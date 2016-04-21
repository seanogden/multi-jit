//===--- OrcLazyJIT.h - Basic Orc-based JIT for lazy execution --*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Simple Orc-based JIT. Uses the compile-on-demand layer to break up and
// lazily compile modules.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_TOOLS_LLI_ORCLAZYJIT_H
#define LLVM_TOOLS_LLI_ORCLAZYJIT_H

#include "llvm/ADT/Triple.h"
#include "CompileOnDemandLayer.h"
#include "llvm/ExecutionEngine/Orc/CompileUtils.h"
#include "llvm/ExecutionEngine/Orc/ExecutionUtils.h"
#include "IRCompileLayer.h"
#include "IRTransformLayer.h"
#include "ObjectLinkingLayer.h"
#include "llvm/ExecutionEngine/RTDyldMemoryManager.h"

using namespace llvm;
using namespace llvm::orc;
namespace llvm {

class OrcLazyJIT {
public:

  typedef std::function<std::unique_ptr<Module>(std::unique_ptr<Module>)> TransformFtor;
  typedef orc::JITCompileCallbackManager CompileCallbackMgr;
  typedef orc::ObjectLinkingLayer<> ObjLayerT;
  typedef orc::IRCompileLayer<ObjLayerT> CompileLayerT;
  typedef orc::IRCompileLayer<ObjLayerT> HotCompileLayerT;
  typedef orc::IRTransformLayer<CompileLayerT, TransformFtor> IRDumpLayerT;
  typedef orc::IRTransformLayer<IRDumpLayerT, TransformFtor> ProfilingLayerT;
  typedef orc::IRTransformLayer<ProfilingLayerT, TransformFtor> PreDumpLayerT;
  typedef orc::CompileOnDemandLayer<PreDumpLayerT, CompileCallbackMgr> CODLayerT;
  typedef CODLayerT::IndirectStubsManagerBuilderT IndirectStubsManagerBuilder;
  typedef CODLayerT::ModuleSetHandleT ModuleHandleT;

  OrcLazyJIT(std::unique_ptr<TargetMachine> TM,
             std::unique_ptr<CompileCallbackMgr> CCMgr,
             IndirectStubsManagerBuilder IndirectStubsMgrBuilder,
             bool InlineStubs)
      : TM(std::move(TM)), DL(this->TM->createDataLayout()),
	CCMgr(std::move(CCMgr)),
	ObjectLayer(),
        CompileLayer(ObjectLayer, orc::SimpleCompiler(*this->TM)),
        HotCompileLayer(ObjectLayer, orc::SimpleCompiler(*this->TM)),
        IRDumpLayer(CompileLayer, createDebugDumper()),
        ProfilingLayer(IRDumpLayer, insertLocalProfilingCode()),
        PreDumpLayer(ProfilingLayer, createPreDebugDumper()),
        CODLayer(PreDumpLayer, extractSingleFunction, *this->CCMgr,
                 std::move(IndirectStubsMgrBuilder), InlineStubs),
        CXXRuntimeOverrides(
            [this](const std::string &S) { return mangle(S); }),
        RecompileName(mangle("$recompile_hot")),
        FunctionStubTable() {}

  ~OrcLazyJIT() {
    // Run any destructors registered with __cxa_atexit.
    CXXRuntimeOverrides.runDestructors();
    // Run any IR destructors.
    for (auto &DtorRunner : IRStaticDestructorRunners)
      DtorRunner.runViaLayer(CODLayer);
  }

  static std::unique_ptr<CompileCallbackMgr> createCompileCallbackMgr(Triple T);
  static IndirectStubsManagerBuilder createIndirectStubsMgrBuilder(Triple T);

  ModuleHandleT addModule(std::unique_ptr<Module> M) {
    // Attach a data-layout if one isn't already present.
    if (M->getDataLayout().isDefault())
      M->setDataLayout(DL);

    // Record the static constructors and destructors. We have to do this before
    // we hand over ownership of the module to the JIT.
    std::vector<std::string> CtorNames, DtorNames;
    for (auto Ctor : orc::getConstructors(*M))
      CtorNames.push_back(mangle(Ctor.Func->getName()));
    for (auto Dtor : orc::getDestructors(*M))
      DtorNames.push_back(mangle(Dtor.Func->getName()));

    // Add the module to the JIT.
    std::vector<std::unique_ptr<Module>> S;
    S.push_back(std::move(M));
    auto H = CODLayer.addModuleSet(std::move(S),
				   llvm::make_unique<SectionMemoryManager>(),
				   createResolver());

    // Run the static constructors, and save the static destructor runner for
    // execution when the JIT is torn down.
    orc::CtorDtorRunner<CODLayerT> CtorRunner(std::move(CtorNames), H);
    CtorRunner.runViaLayer(CODLayer);

    IRStaticDestructorRunners.emplace_back(std::move(DtorNames), H);

    return H;
  }

  orc::JITSymbol findSymbol(const std::string &Name) {
    return CODLayer.findSymbol(mangle(Name), true);
  }

  orc::JITSymbol findSymbolIn(ModuleHandleT H, const std::string &Name) {
    return CODLayer.findSymbolIn(H, mangle(Name), true);
  }

  JITSymbol findUnmangledSymbol(const std::string &Name) {
    return findSymbol(mangle(Name));
  }

  JITSymbol findUnmangledSymbolIn(ModuleHandleT H, const std::string &Name) {
    return findSymbolIn(H, mangle(Name));
  }

private:

  std::unique_ptr<RuntimeDyld::SymbolResolver> createResolver() {
      // We need a memory manager to allocate memory and resolve symbols for this
      // new module. Create one that resolves symbols by looking back into the
      // JIT.
      // Symbol resolution order:
      //   0) If recompile, return the recompiler function's address
      //   1) Search the JIT symbols.
      //   2) Check for C++ runtime overrides.
      //   3) Search the host process (LLI)'s symbol table.


      return createLambdaResolver(
        [&](const std::string &Name) {
          if (Name == RecompileName) {
            auto RecompileAddr = static_cast<TargetAddress>(
              reinterpret_cast<uintptr_t>(OrcLazyJIT::recompileHotStatic));
            return RuntimeDyld::SymbolInfo(RecompileAddr, JITSymbolFlags::Exported);
                      
          }

          if (auto Sym = CODLayer.findSymbol(Name, true))
            return RuntimeDyld::SymbolInfo(Sym.getAddress(), Sym.getFlags());
                      
          if (auto Sym = CXXRuntimeOverrides.searchOverrides(Name))
            return Sym;

          if (auto Addr = RTDyldMemoryManager::getSymbolAddressInProcess(Name))
            return RuntimeDyld::SymbolInfo(Addr, JITSymbolFlags::Exported);

          return RuntimeDyld::SymbolInfo(nullptr);
        },
        [](const std::string &Name) {
          return RuntimeDyld::SymbolInfo(nullptr);
        }
      );
  }

  TargetAddress recompileHot(Function *F) {
    //TODO:  Add an argument to this to pass a function identifier so that
    //       we can lookup the original function's IR (and the handle to the stub
    //       we generated so we have something to actually compile, and so we can
    //       get the pointer to the body of the function and replace it with our
    //       hot function body's pointer.
      
    // Recompile the function with a different compile layer that has higher optimization set.
    std::vector<std::unique_ptr<Module>> S;
    S.push_back(std::unique_ptr<Module>(F->getParent()));
    auto H = HotCompileLayer.addModuleSet(std::move(S),
            llvm::make_unique<SectionMemoryManager>(),
            createResolver());

    std::string FuncName = F->getName();

    // Look up the optimized function body.
    auto HotFnSym =
        HotCompileLayer.findSymbolIn(H, mangle(FuncName), true);
    TargetAddress HotFnAddr = HotFnSym.getAddress();

    // Find the function body pointer and update it to point at the optimized
    // version.
    // TODO:  'I->second.second' below should be a Module handle to the the
    //        module containing the stub for our function.
    auto BodyPtrSym =
        findUnmangledSymbolIn(I->second.second, FuncName + "$address");
    auto BodyPtr = reinterpret_cast<void*>(
            static_cast<uintptr_t>(BodyPtrSym.getAddress()));
    memcpy(BodyPtr, &HotFnAddr, sizeof(uintptr_t));

    // Return the address for the hot function body so that the cold function
    // (which called us) can finish by calling it.
    return HotFnAddr; 
  }

  // Static helper.
  static TargetAddress recompileHotStatic(OrcLazyJIT *J, Function *F) {
      return J->recompileHot(F);
  }

  std::string mangle(const std::string &Name) {
    std::string MangledName;
    {
      raw_string_ostream MangledNameStream(MangledName);
      Mangler::getNameWithPrefix(MangledNameStream, Name, DL);
    }
    return MangledName;
  }

  static std::set<Function*> extractSingleFunction(Function &F) {
    std::set<Function*> Partition;
    Partition.insert(&F);
    return Partition;
  }

  static TransformFtor createDebugDumper();
  static TransformFtor createPreDebugDumper();
  static TransformFtor insertProfilingCode();
  TransformFtor insertLocalProfilingCode();

  std::unique_ptr<TargetMachine> TM;
  DataLayout DL;
  SectionMemoryManager CCMgrMemMgr;

  std::unique_ptr<CompileCallbackMgr> CCMgr;
  ObjLayerT ObjectLayer;
  CompileLayerT CompileLayer;
  HotCompileLayerT HotCompileLayer;
  IRDumpLayerT IRDumpLayer;
  ProfilingLayerT ProfilingLayer;
  PreDumpLayerT PreDumpLayer;
  CODLayerT CODLayer;

  orc::LocalCXXRuntimeOverrides CXXRuntimeOverrides;
  std::vector<orc::CtorDtorRunner<CODLayerT>> IRStaticDestructorRunners;
  std::string RecompileName;
  std::map<Function*, ModuleHandleT> FunctionStubTable;
};

int runOrcLazyJIT(std::unique_ptr<Module> M, int ArgC, char* ArgV[]);

} // end namespace llvm

#endif
