//===------ OrcLazyJIT.cpp - Basic Orc-based JIT for lazy execution -------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "OrcLazyJIT.h"
#include "llvm/ExecutionEngine/Orc/OrcArchitectureSupport.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/DynamicLibrary.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/TypeBuilder.h"
#include "llvm/IR/Constants.h"
#include "llvm/Transforms/Instrumentation.h"
#include "llvm/IR/Type.h"
#include <cstdio>
#include <system_error>
#include <vector>

using namespace llvm;

int numFunctions;

namespace {

  enum class DumpKind { NoDump, DumpFuncsToStdOut, DumpModsToStdErr,
                        DumpModsToDisk };

  cl::opt<DumpKind> OrcDumpKind("orc-lazy-debug",
                                cl::desc("Debug dumping for the orc-lazy JIT."),
                                cl::init(DumpKind::NoDump),
                                cl::values(
                                  clEnumValN(DumpKind::NoDump, "no-dump",
                                             "Don't dump anything."),
                                  clEnumValN(DumpKind::DumpFuncsToStdOut,
                                             "funcs-to-stdout",
                                             "Dump function names to stdout."),
                                  clEnumValN(DumpKind::DumpModsToStdErr,
                                             "mods-to-stderr",
                                             "Dump modules to stderr."),
                                  clEnumValN(DumpKind::DumpModsToDisk,
                                             "mods-to-disk",
                                             "Dump modules to the current "
                                             "working directory. (WARNING: "
                                             "will overwrite existing files)."),
                                  clEnumValEnd),
                                cl::Hidden);

  cl::opt<bool> OrcInlineStubs("orc-lazy-inline-stubs",
                               cl::desc("Try to inline stubs"),
                               cl::init(true), cl::Hidden);
}

std::unique_ptr<OrcLazyJIT::CompileCallbackMgr>
OrcLazyJIT::createCompileCallbackMgr(Triple T) {
  switch (T.getArch()) {
    default: return nullptr;

    case Triple::x86: {
      typedef orc::LocalJITCompileCallbackManager<orc::OrcI386> CCMgrT;
      return llvm::make_unique<CCMgrT>(0);
    }

    case Triple::x86_64: {
      typedef orc::LocalJITCompileCallbackManager<orc::OrcX86_64> CCMgrT;
      return llvm::make_unique<CCMgrT>(0);
    }
  }
}

OrcLazyJIT::IndirectStubsManagerBuilder
OrcLazyJIT::createIndirectStubsMgrBuilder(Triple T) {
  switch (T.getArch()) {
    default: return nullptr;

    case Triple::x86:
      return [](){
        return llvm::make_unique<
                       orc::LocalIndirectStubsManager<orc::OrcI386>>();
      };

    case Triple::x86_64:
      return [](){
        return llvm::make_unique<
                       orc::LocalIndirectStubsManager<orc::OrcX86_64>>();
      };
  }
}

//Add instrumentation to count the function invocations.
//This version does not have a global table, so recompilation
//decisions must be made locally based on the raw counter.
//Later we will add a global function table to hold all counters
//so that we can periodically check which functions are the "hottest"
OrcLazyJIT::TransformFtor OrcLazyJIT::insertLocalProfilingCode() {
  return [this](std::unique_ptr<Module> M) { 

    Type *Int32 = Type::getInt32Ty(M->getContext());
    Type *Int64 = Type::getInt64Ty(M->getContext());

    // Add a prototype for the recompile function so that we can call it if
    // this function becomes hot.  Arguments are pointer to JIT and pointer to
    // the Function (cast as int64).  
    // RecompileHot is a call to recompileHotStatic with 
    // these arguments, which in turn calls the recompileHot method of the JIT.
    // with the pointer to the Function object (the thing that represents the IR
    // for the function).
    //
    // The resolver knows that if you look for symbol $recompile_hot, it means
    // you would like to call recompileHotStatic.
    Type* RecompileArgTypes[] = { Int64 , Int64 };
    Function *RecompileHot =
        Function::Create(FunctionType::get(Int64, RecompileArgTypes, false),
                         GlobalValue::ExternalLinkage, "$recompile_hot", M.get());

    for (auto &F : *M) {

        //Declarations don't have bodies
        if (F.isDeclaration())
            continue;

        GlobalVariable* Counter =
            new GlobalVariable(*M, Int32, false, GlobalValue::ExternalLinkage,
                               ConstantInt::get(Int32, 0),
                               F.getName() + "$counter");

        Counter->setAlignment(4);

        //Find the last alloca instruction.  We don't want to insert stuff before these.
        BasicBlock *Entry = &F.front();
        auto I = Entry->begin();
        while (isa<AllocaInst>(I))
            ++I;

        // Split the entry-block after last alloca
        BasicBlock *ContEntryBlock = F.front().splitBasicBlock(I, "entry_cont");

        //Create an extra basic block to branch to if the func is hot
        BasicBlock *RecompileBlock =
            BasicBlock::Create(M->getContext(), "recompile");
        F.getBasicBlockList().push_back(RecompileBlock);

        // In the entry block, add load compare inc and store for the
        // counter for this func
        IRBuilder<> B(Entry);
        Value *CounterVal = B.CreateLoad(Counter, "counter");
        Value *Condition = B.CreateICmpUGT(CounterVal, ConstantInt::get(Int32, 9));
        Value *CounterInc = B.CreateAdd(CounterVal, ConstantInt::get(Int32, 1));
        B.CreateStore(CounterInc, Counter);

        // If the function is hot, jump to recompile block, otherwise just
        // continue at entry_cont block
        B.CreateCondBr(Condition, RecompileBlock, ContEntryBlock);

        // Switch IR Builder to the recompile block
        B.SetInsertPoint(RecompileBlock);

        // insert a compile to compile the hot version
        uint64_t JITAddr = static_cast<uint64_t>(reinterpret_cast<uintptr_t>(this));
        uint64_t FuncAddr = static_cast<uint64_t>(reinterpret_cast<uintptr_t>(&F));

        std::cout << std::string(mangle(F.getName())) << std::endl;
        std::cout << "Instrumenting function with " << FunctionIDs[mangle(F.getName())] << std::endl;
        Value *JITAddrConst = ConstantInt::get(Int64, JITAddr);
        Value *FuncAddrConst = ConstantInt::get(Int64, FunctionIDs[mangle(F.getName())]);
        Value *RecompileHotArgs[] = {JITAddrConst, FuncAddrConst};
        Value *HotFnAddr = B.CreateCall(RecompileHot, RecompileHotArgs);


        // The recompile function returns address of optimized version.
        // Cast it to a function pointer and call it.
        Value *HotFn = B.CreateIntToPtr(HotFnAddr, F.getType());
        std::vector<Value*> CallArgs;
        for (auto &A : F.args())
            CallArgs.push_back(&A);
        CallInst *HotFnCall = B.CreateCall(HotFn, CallArgs);
        HotFnCall->setTailCall(true);

        if (F.getReturnType()->isVoidTy())
            B.CreateRetVoid();
        else
            B.CreateRet(HotFnCall);
    }
    return M; 
  };
}


OrcLazyJIT::TransformFtor OrcLazyJIT::insertProfilingCode() {
  return [](std::unique_ptr<Module> M) { 

    // Make a prototype for printf.  Assume it's already linked from environment.
    FunctionType *printf_type =
        TypeBuilder<int(char *, ...), false>::get(getGlobalContext());

    Function *printf_func = cast<Function>(M->getOrInsertFunction(
                "printf", printf_type,
                AttributeSet().addAttribute(M->getContext(), 1U, Attribute::NoAlias)));

    ArrayType *CounterTy;
    GlobalVariable *Counters;

    bool firstfunc = true;

    for (Function &F : *M) {
        if (!F.empty()) {
            
            //Declare the counter table global if this is a module that has actual functions 
            //(as opposed to a module with just declarations)
            if (firstfunc) {
              CounterTy = ArrayType::get(Type::getInt64Ty(M->getContext()), numFunctions);
              Counters =
                  new GlobalVariable(*M, CounterTy, false,
                          GlobalValue::CommonLinkage,
                          nullptr,
                          "__llvm_func_ctr");
              Counters->setAlignment(16);
              firstfunc = false;
            }

            //Make a global String value with the functions name
            Constant *string_to_print = ConstantDataArray::getString(M->getContext(), F.getName().str().append("\n"), true);
            Type* StringType = string_to_print->getType();
            GlobalVariable* gvar_array__str = new GlobalVariable(
                    /*Module*/*M, 
                    /*Type*/StringType,
                    /*isConstant*/true,
                    /*Linkage*/GlobalValue::PrivateLinkage,
                    /*Initializer*/0, // has initializer, specified below
                    /*Name prefix*/".str");
            gvar_array__str->setAlignment(1);

            /*Constant zeros for array offsets*/
            ConstantInt* const_int32 = ConstantInt::get(M->getContext(), APInt(32, StringRef("0"), 10));
            std::vector<Constant*> const_ptr_indices;
            const_ptr_indices.push_back(const_int32);
            const_ptr_indices.push_back(const_int32);

            /*Construct getelementptr llvm*/
            Constant* const_ptr = ConstantExpr::getGetElementPtr(StringType, gvar_array__str, const_ptr_indices);
            gvar_array__str->setInitializer(string_to_print);

            /*Construct arglist for printf*/
            std::vector<Value*> args;
            args.push_back(const_ptr);

            /*Insert a call instruction to call print at the beginning of the first basic block of this function*/
            Instruction& pi = F.front().front();
            Instruction* ni = CallInst::Create(printf_func, args, "printf", &pi);
        }
    }

    return M; 
  };
}

OrcLazyJIT::TransformFtor OrcLazyJIT::createPreDebugDumper() {

  OrcDumpKind = DumpKind::NoDump; 
  switch (OrcDumpKind) {
  case DumpKind::NoDump:
    return [](std::unique_ptr<Module> M) { return M; };

  case DumpKind::DumpFuncsToStdOut:
    return [](std::unique_ptr<Module> M) {
      printf("[ ");

      for (const auto &F : *M) {
        if (F.isDeclaration())
          continue;

        if (F.hasName()) {
          std::string Name(F.getName());
          printf("%s ", Name.c_str());
        } else
          printf("<anon> ");
      }

      printf("]\n");
      return M;
    };

  case DumpKind::DumpModsToStdErr:
    return [](std::unique_ptr<Module> M) {
             dbgs() << "----- Module Start -----\n" << *M
                    << "----- Module End -----\n";
             dbgs().flush();
             return M;
           };

  case DumpKind::DumpModsToDisk:
    return [](std::unique_ptr<Module> M) {
             std::error_code EC;
             raw_fd_ostream Out(M->getModuleIdentifier() + ".ll", EC,
                                sys::fs::F_Text);
             if (EC) {
               errs() << "Couldn't open " << M->getModuleIdentifier()
                      << " for dumping.\nError:" << EC.message() << "\n";
               exit(1);
             }
             Out << *M;
             return M;
           };
  }
  llvm_unreachable("Unknown DumpKind");
}


OrcLazyJIT::TransformFtor OrcLazyJIT::createDebugDumper() {

  OrcDumpKind = DumpKind::DumpModsToStdErr; 
  switch (OrcDumpKind) {
  case DumpKind::NoDump:
    return [](std::unique_ptr<Module> M) { return M; };

  case DumpKind::DumpFuncsToStdOut:
    return [](std::unique_ptr<Module> M) {
      printf("[ ");

      for (const auto &F : *M) {
        if (F.isDeclaration())
          continue;

        if (F.hasName()) {
          std::string Name(F.getName());
          printf("%s ", Name.c_str());
        } else
          printf("<anon> ");
      }

      printf("]\n");
      return M;
    };

  case DumpKind::DumpModsToStdErr:
    return [](std::unique_ptr<Module> M) {
             dbgs() << "----- Module Start -----\n" << *M
                    << "----- Module End -----\n";
             dbgs().flush();
             return M;
           };

  case DumpKind::DumpModsToDisk:
    return [](std::unique_ptr<Module> M) {
             std::error_code EC;
             raw_fd_ostream Out(M->getModuleIdentifier() + ".ll", EC,
                                sys::fs::F_Text);
             if (EC) {
               errs() << "Couldn't open " << M->getModuleIdentifier()
                      << " for dumping.\nError:" << EC.message() << "\n";
               exit(1);
             }
             Out << *M;
             return M;
           };
  }
  llvm_unreachable("Unknown DumpKind");
}

// Defined in lli.cpp.
CodeGenOpt::Level getOptLevel();


template <typename PtrTy>
static PtrTy fromTargetAddress(orc::TargetAddress Addr) {
  return reinterpret_cast<PtrTy>(static_cast<uintptr_t>(Addr));
}

int llvm::runOrcLazyJIT(std::unique_ptr<Module> M, int ArgC, char* ArgV[]) {
  // Add the program's symbols into the JIT's search space.
  if (sys::DynamicLibrary::LoadLibraryPermanently(nullptr)) {
    errs() << "Error loading program symbols.\n";
    return 1;
  }

  // Grab a target machine and try to build a factory function for the
  // target-specific Orc callback manager.
  EngineBuilder EB;
  EB.setOptLevel(getOptLevel());
  auto TM = std::unique_ptr<TargetMachine>(EB.selectTarget());
  auto CompileCallbackMgr =
    OrcLazyJIT::createCompileCallbackMgr(Triple(TM->getTargetTriple()));

  // If we couldn't build the factory function then there must not be a callback
  // manager for this target. Bail out.
  if (!CompileCallbackMgr) {
    errs() << "No callback manager available for target '"
           << TM->getTargetTriple().str() << "'.\n";
    return 1;
  }

  auto IndirectStubsMgrBuilder =
    OrcLazyJIT::createIndirectStubsMgrBuilder(Triple(TM->getTargetTriple()));

  // If we couldn't build a stubs-manager-builder for this target then bail out.
  if (!IndirectStubsMgrBuilder) {
    errs() << "No indirect stubs manager available for target '"
           << TM->getTargetTriple().str() << "'.\n";
    return 1;
  }

  // Everything looks good. Build the JIT.
  OrcLazyJIT J(std::move(TM), std::move(CompileCallbackMgr),
               std::move(IndirectStubsMgrBuilder),
               OrcInlineStubs);

  //Make a global counters array
  numFunctions = M->size();
  ArrayType *CounterTy =
      ArrayType::get(Type::getInt64Ty(M->getContext()), numFunctions);
  GlobalVariable *Counters =
      new GlobalVariable(*M, CounterTy, false,
              GlobalValue::CommonLinkage,
              Constant::getNullValue(CounterTy),
              "__llvm_func_ctr");
  Counters->setAlignment(16);

  // Insert the profiling initializer code into the main function of module
  Function *MainFunc = M->getFunction("main");

  // Add the module, look up main and run it.
  std::cout << "Bout to add some modules!" << std::endl;
  auto MainHandle = J.addModule(std::move(M));
  auto MainSym = J.findSymbolIn(MainHandle, "main");

  if (!MainSym) {
    errs() << "Could not find main function.\n";
    return 1;
  }

  typedef int (*MainFnPtr)(int, char*[]);
  auto Main = fromTargetAddress<MainFnPtr>(MainSym.getAddress());
  return Main(ArgC, ArgV);
}
