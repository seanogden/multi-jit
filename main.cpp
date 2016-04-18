#include "OrcLazyJIT.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/Bitcode/ReaderWriter.h"
#include "llvm/IR/Module.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/Memory.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/Process.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/TargetSelect.h"
#include <cerrno>

using namespace llvm;

namespace {
  cl::opt<std::string>
  InputFile(cl::desc("<input bitcode>"), cl::Positional, cl::init("-"));

  cl::opt<char>
  OptLevel("O",
           cl::desc("Optimization level. [-O0, -O1, -O2, or -O3] "
                    "(default = '-O2')"),
           cl::Prefix,
           cl::ZeroOrMore,
           cl::init(' '));
}

CodeGenOpt::Level getOptLevel() {
  switch (OptLevel) {
  default:
    errs() << "lli: Invalid optimization level.\n";
    exit(1);
  case '0': return CodeGenOpt::None;
  case '1': return CodeGenOpt::Less;
  case ' ':
  case '2': return CodeGenOpt::Default;
  case '3': return CodeGenOpt::Aggressive;
  }
  llvm_unreachable("Unrecognized opt level.");
}

int main(int argc, char **argv, char * const *envp) {
  sys::PrintStackTraceOnErrorSignal();
  PrettyStackTraceProgram X(argc, argv);

  LLVMContext &Context = getGlobalContext();
  atexit(llvm_shutdown);  

  // If we have a native target, initialize it to ensure it is linked in and
  // usable by the JIT.
  InitializeNativeTarget();
  InitializeNativeTargetAsmPrinter();
  InitializeNativeTargetAsmParser();

  cl::ParseCommandLineOptions(argc, argv,
                              "llvm interpreter & dynamic compiler\n");

  // Load the bitcode...
  SMDiagnostic Err;
  std::unique_ptr<Module> Owner = parseIRFile(InputFile, Err, Context);
  Module *Mod = Owner.get();
  if (!Mod) {
    Err.print(argv[0], errs());
    return 1;
  }

  //Mod->dump();
  return runOrcLazyJIT(std::move(Owner), argc, argv);
  //return 0;
}
