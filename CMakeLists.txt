cmake_minimum_required(VERSION 2.8.8)
project(jitcompiler)

find_package(LLVM REQUIRED CONFIG)
set( CMAKE_EXPORT_COMPILE_COMMANDS 1 ) 
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

# Set your project compile flags.
# E.g. if using the C++ header files
# you will need to enable C++11 support
# for your compiler.

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

set(CMAKE_CXX_FLAGS "-fno-rtti -std=c++11")

# Now build our tools
add_executable(jitcompile main.cpp OrcLazyJIT.cpp)

# Find the libraries that correspond to the LLVM components
# that we wish to use
#include "llvm/Support/Process.h"
llvm_map_components_to_libnames(llvm_libs core 
                                          codegen 
                                          orcjit 
                                          irreader 
                                          executionengine 
                                          object 
                                          runtimedyld 
                                          support 
                                          native)
#llvm_map_components_to_libnames(llvm_libs core mcjit native)

message(STATUS "Linking these libraries: ${llvm_libs}")
# Link against LLVM libraries
target_link_libraries(jitcompile ${llvm_libs})
