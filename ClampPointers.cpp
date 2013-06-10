/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The Original Contributor of this Source Code Form is Nokia Research
 * Center Tampere (http://webcl.nokiaresearch.com).
 */

// [Go directly to algorithm](#runOnModule) TODO: Write complete "proof summary" here and refer later sections

#include "llvm/Pass.h"
#include "llvm/Function.h"
#include "llvm/Module.h"
#include "llvm/Constants.h"
#include "llvm/Instructions.h"
#include "llvm/Intrinsics.h"
#include "llvm/User.h"
#include "llvm/IRBuilder.h"
#include "llvm/Operator.h"

#include "llvm/Support/CallSite.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Analysis/MemoryDependenceAnalysis.h"

#include <vector>
#include <map>
#include <set>
#include <iostream>
#include <sstream>
#include <cstdio>
#include <iterator>

using namespace llvm;

// Declares **-allow-unsafe-exceptions** switch for the pass. Makes it possible to run normal C programs with external dependencies through this pass (only for testing).
static cl::opt<bool>
RunUnsafeMode("allow-unsafe-exceptions",
        cl::desc("Will not change main() function signature allowing program to be ran. Adds main function arguments to safe exceptions list and allows calling external functions / extern variables."),
        cl::init(false), cl::Hidden);

// Fast assert macro, which will not dump stack-trace to make tests run faster.
#define fast_assert( condition, message ) do {                       \
    if ( (condition) == false ) {                                    \
      dbgs() << "\nOn line: " << __LINE__ << " " << message << "\n"; \
      exit(1);                                                       \
    }                                                                \
  } while(0)

namespace {
  template <class T> void dumpContainer(const T& container) {
    int n = 0;
    for (typename T::const_iterator it = container.begin();
         it != container.end();
         ++it, ++n) {
      dbgs() << "\t[" << n << "]\t" *it << "\n";
    }
  }

  template <class T> void dumpContainer2(const T& container) {
    int n = 0;
    for (typename T::const_iterator it = container.begin();
         it != container.end();
         ++it, ++n) {
      dbgs() << "\t[" << n << "]\t" << **it << "\n";
    }
  }
}

#define DUMP(contents) DEBUG( dbgs() << __FILE__ << ":" << __LINE__ << ": " << __FUNCTION__ << " DUMP(" << (#contents) << ") = " << (contents) << "\n"; )
#define DUMP_CONTAINER(contents) DEBUG( dbgs() << __FILE__ << ":" << __LINE__ << ": " << __FUNCTION__ << " DUMP(" << (#contents) << ") = " << "\n"; dumpContainer(contents); )
#define DUMP_CONTAINER2(contents) DEBUG( dbgs() << __FILE__ << ":" << __LINE__ << ": " << __FUNCTION__ << " DUMP(" << (#contents) << ") = " << "\n"; dumpContainer2(contents); )

// LLVM 3.2 didn't support ConstantExpt::getAsInstruction() yet
// so for now we have copypasted it from trunk. This will be removed in future llvm.
Instruction *getAsInstruction(ConstantExpr *expr) {
  SmallVector<Value*,4> ValueOperands;
  for (ConstantExpr::op_iterator I = expr->op_begin(), E = expr->op_end(); I != E; ++I)
    ValueOperands.push_back(cast<Value>(I));
  ArrayRef<Value*> Ops(ValueOperands);
  switch (expr->getOpcode()) {
  case Instruction::Trunc:
  case Instruction::ZExt:
  case Instruction::SExt:
  case Instruction::FPTrunc:
  case Instruction::FPExt:
  case Instruction::UIToFP:
  case Instruction::SIToFP:
  case Instruction::FPToUI:
  case Instruction::FPToSI:
  case Instruction::PtrToInt:
  case Instruction::IntToPtr:
  case Instruction::BitCast:
    return CastInst::Create((Instruction::CastOps)expr->getOpcode(), Ops[0], expr->getType());
  case Instruction::Select:
    return SelectInst::Create(Ops[0], Ops[1], Ops[2]);
  case Instruction::InsertElement:
    return InsertElementInst::Create(Ops[0], Ops[1], Ops[2]);
  case Instruction::ExtractElement:
    return ExtractElementInst::Create(Ops[0], Ops[1]);
  case Instruction::InsertValue:
    return InsertValueInst::Create(Ops[0], Ops[1], expr->getIndices());
  case Instruction::ExtractValue:
    return ExtractValueInst::Create(Ops[0], expr->getIndices());
  case Instruction::ShuffleVector:
    return new ShuffleVectorInst(Ops[0], Ops[1], Ops[2]);
  case Instruction::GetElementPtr:
    if (cast<GEPOperator>(expr)->isInBounds())
      return GetElementPtrInst::CreateInBounds(Ops[0], Ops.slice(1));
    else
      return GetElementPtrInst::Create(Ops[0], Ops.slice(1));
  case Instruction::ICmp:
  case Instruction::FCmp:
    return CmpInst::Create((Instruction::OtherOps)expr->getOpcode(), expr->getPredicate(), Ops[0], Ops[1]);
  default:
    assert(expr->getNumOperands() == 2 && "Must be binary operator?");
    BinaryOperator *BO =
      BinaryOperator::Create((Instruction::BinaryOps)expr->getOpcode(), Ops[0], Ops[1]);
    if (isa<OverflowingBinaryOperator>(BO)) {
      assert(false && "Not supported hopefully never needed until llvm 3.3 is out.");
    }
    if (isa<PossiblyExactOperator>(BO)) {
      assert(false && "Not supported hopefully never needed until llvm 3.3 is out.");
    }
    return BO;
  }
}

// # WebCL to OpenCL instrumentation
// Detailed description of the algorithm is documented in [virtual bool runOnModule( Module &M )](#runOnModule)
namespace WebCL {

  // maybe we could avoid using the numbers if we always allocate
  // address spaces, whose variables are in global scope from global scope
  // and address spaces allocated with alloca in function scope
  
  // Numbers according to SPIR target
  //const unsigned privateAddressSpaceNumber  = 0;
  //const unsigned globalAddressSpaceNumber   = 1;
  //const unsigned constantAddressSpaceNumber = 2;
  //const unsigned localAddressSpaceNumber    = 3;

  // std::string addressSpaceLabel(unsigned as) {
  //   switch (as) {
  //   case 0: return "Private";
  //   case 1: return "Global";
  //   case 2: return "Constant";
  //   case 3: return "Local";
  //   }
  //   return "Unknown";
  // }

  // Numbers of NVPTX backend: http://llvm.org/docs/NVPTXUsage.html
  const unsigned privateAddressSpaceNumber  = 0;
  const unsigned globalAddressSpaceNumber   = 1;
  const unsigned constantAddressSpaceNumber = 4;
  const unsigned localAddressSpaceNumber    = 3; // Shared address space

  std::string addressSpaceLabel(unsigned as) {
    switch (as) {
    case 0: return "Private";
    case 1: return "Global";
    case 4: return "Constant";
    case 3: return "Local";
    }
    return "Unknown";
  }
  
  // ### Common helper functions
  
  // Returns demangled function name without argument type prefix.
  //
  // Mangled symbol format is _Z{name_length}{function_name}{prefix} e.g. for
  // `_Z7vstore4Dv4_fyPU10AS16776960f` function will return `vstore4`
  //
  // `std::string name` Mangled function name or non-mangled
  // `returns` Demangled function name or the passed argument if mangling is not recognized.
  std::string extractItaniumDemangledFunctionName(std::string name) {
    bool isMangled = name.find("_Z") == 0;
    std::string retVal = name;
    if (isMangled) {
      size_t lastIndex = name.find_first_not_of("0123456789", 2);
      std::string functionNameLength = name.substr(2, lastIndex-2); 
      fast_assert(functionNameLength.find_first_not_of("0123456789") == std::string::npos, 
                  "Error when trying to demangle: " + name);
      retVal = name.substr(lastIndex, atoi(functionNameLength.c_str()));
    }
    //DEBUG( dbgs() << "Demangled: " << name << " to " << retVal << "\n" );
    return retVal;
  }

  // useful for getting the next-of-iterator in an expression
  template <typename T>
  T next(T v) {
    ++v;
    return v;
  }

  // used for skipping programAllocationsArgument. This label is useful for reading and searching the code.
  Function::arg_iterator skipPaa(Function::arg_iterator it) {
    return next(it);
  }

  // distance from an intertor to another. b must be reachable from a.
  template <typename Iterator>
  int iteratorDistance(Iterator a, Iterator b) {
    int n = 0;
    while (a != b) {
      ++n;
      ++a;
    }
    return n;
  }

  // Creates mangled name (own mangling scheme) to be able to select correct safe builtin function implementations to call.
  // All calls to functions with names mangled by this algorithm should be inlined and removed afterwards by later optimizations.
  //
  // Scheme steals mangle suffix from original Itanium Mangled function call and add it to our version which
  // is safe to call. This way we get an unique name for each function which for we need to write safe implementation and know
  // easily to which implementation to call when converting builtin calls to safe-builtin calls.
  //
  // `Function* function` The function whose argument list prefix should be added to the returned name.
  // `std::string base` Base where mangle suffix will be added.
  // `returns` Function name which implements safe builtin implementation for the `function` arg.
  std::string customMangle(Function* function, std::string base) {
    std::stringstream ss;
    std::string origName = function->getName();
    std::string demangledOrig = extractItaniumDemangledFunctionName(function->getName());
    size_t namePos = origName.find(demangledOrig);
    size_t prefixChars = namePos + demangledOrig.length();
    std::string itaniumMangleSuffix = origName.erase(0, prefixChars);
    ss << base << itaniumMangleSuffix;
    DEBUG( dbgs() << "Orig: " << function->getName() << " new: " << ss.str() << "\n" ); 
    return ss.str();
  }
  
  // Helper to create 32bit ConstantInt
  ConstantInt* getConstInt(LLVMContext &context, int i) {
    return ConstantInt::get(Type::getInt32Ty(context), i);
  }

  // Helpers to create on the fly vectors from integers
  template <class T>
  std::vector<T> genIntVector(LLVMContext &context, int i1) {
    std::vector<T> temp;
    temp.push_back(getConstInt(context, i1));
    return temp;
  }

  template <class T>
  std::vector<T> genIntVector(LLVMContext &context, int i1, int i2) {
    std::vector<T> temp;
    temp.push_back(getConstInt(context, i1));
    temp.push_back(getConstInt(context, i2));
    return temp;
  }    
  
  template <class T>
  std::vector<T> genIntVector(LLVMContext &context, int i1, int i2, int i3) {
    std::vector<T> temp;
    temp.push_back(getConstInt(context, i1));
    temp.push_back(getConstInt(context, i2));
    temp.push_back(getConstInt(context, i3));
    return temp;
  }
  
  template <class T>
  std::vector<T> genIntVector(LLVMContext &context, int i1, int i2, int i3, int i4) {
    std::vector<T> temp;
    temp.push_back(getConstInt(context, i1));
    temp.push_back(getConstInt(context, i2));
    temp.push_back(getConstInt(context, i3));
    temp.push_back(getConstInt(context, i4));
    return temp;
  }
  
  // Helpers to create array refs of any type of Values
  template <class T>
  std::vector<T> genVector(T v1) {
    std::vector<T> temp;
    temp.push_back(v1);
    return temp;
  }

  template <class T>
  std::vector<T> genVector(T v1, T v2) {
    std::vector<T> temp;
    temp.push_back(v1);
    temp.push_back(v2);
    return temp;
  }    
  
  template <class T>
  std::vector<T> genVector(T v1, T v2, T v3) {
    std::vector<T> temp;
    temp.push_back(v1);
    temp.push_back(v2);
    temp.push_back(v3);
    return temp;
  }    

  template <class T>
  std::vector<T> genVector(T v1, T v2, T v3, T v4) {
    std::vector<T> temp;
    temp.push_back(v1);
    temp.push_back(v2);
    temp.push_back(v3);
    temp.push_back(v4);
    return temp;
  }    
  
  // Creates SmartPointer struct type for given pointer type. This structure type
  // is used to pass pointer with limits to the functions.
  StructType* getSmartStructType( LLVMContext& c, Type* t ) {
    return StructType::get( c, genVector<Type*>(t,t,t) );
  }
 

  template<typename T>
  struct LocationKind {
    enum { initAtEnd = 0 };
    static Instruction* AllocInstLocation(BasicBlock* block) {
      return &block->front();
    }
    static Function* getParent(T* instruction) {
      return instruction->getParent()->getParent();
    }
  };

  template<>
  struct LocationKind<llvm::BasicBlock> {
    enum { initAtEnd = 1 };
    static BasicBlock* AllocInstLocation(BasicBlock* block) {
      return block;
    }
    static Function* getParent(BasicBlock* block) {
      return block->getParent();
    }
  };

  namespace {
    std::string unsafeBuiltins_tmp[] = {
      "dumpAddress", // a debugging tool
      "fract", "frexp", "lgamma_r", "modf", "remquo", "sincos", 
      "vload2", "vload3", "vload4", "vload8", "vload16", 
      "vstore2", "vstore3", "vstore4", "vstore8", "vstore16",
      "async_work_group_copy",
      "async_work_group_strided_copy",
      "wait_group_events",
      "atomic_add", "atomic_sub", "atomic_xchg", 
      "atomic_inc", "atomic_dec", "atomic_cmpxchg", 
      "atomic_min", "atomic_max",
      "atomic_and", "atomic_or", "atomic_xor",
    };
    std::set<std::string> unsafeBuiltins(unsafeBuiltins_tmp, unsafeBuiltins_tmp + sizeof(unsafeBuiltins_tmp) / sizeof(unsafeBuiltins_tmp[0]));

    std::string unsupportedUnsafeBuiltins_tmp[] = {
      "vload_half", "vload_half2", "vload_half3", "vload_half4", "vload_half8", "vload_half16", 
      "vloada_half2", "vloada_half3", "vloada_half4", "vloada_half8", "vloada_half16", 
      "vstore_half", "vstore_half2", "vstore_half3", "vstore_half4", "vstore_half8", "vstore_half16", 
      "vstore_half_rte", "vstore_half2_rte", "vstore_half3_rte", "vstore_half4_rte", "vstore_half8_rte", "vstore_half16_rte",
      "vstore_half_rtz", "vstore_half2_rtz", "vstore_half3_rtz", "vstore_half4_rtz", "vstore_half8_rtz", "vstore_half16_rtz",
      "vstore_half_rtp", "vstore_half2_rtp", "vstore_half3_rtp", "vstore_half4_rtp", "vstore_half8_rtp", "vstore_half16_rtp",
      "vstore_half_rtn", "vstore_half2_rtn", "vstore_half3_rtn", "vstore_half4_rtn", "vstore_half8_rtn", "vstore_half16_rtn",
      "vstorea_half2", "vstorea_half3", "vstorea_half4", "vstorea_half8", "vstorea_half16", 
      "vstorea_half2_rte", "vstorea_half3_rte", "vstorea_half4_rte", "vstorea_half8_rte","vstorea_half16_rte",
      "vstorea_half2_rtz", "vstorea_half3_rtz", "vstorea_half4_rtz", "vstorea_half8_rtz","vstorea_half16_rtz",
      "vstorea_half2_rtp", "vstorea_half3_rtp", "vstorea_half4_rtp", "vstorea_half8_rtp","vstorea_half16_rtp",
      "vstorea_half2_rtn", "vstorea_half3_rtn", "vstorea_half4_rtn", "vstorea_half8_rtn","vstorea_half16_rtn"
    };
    std::set<std::string> unsupportedUnsafeBuiltins(unsupportedUnsafeBuiltins_tmp, unsupportedUnsafeBuiltins_tmp + sizeof(unsupportedUnsafeBuiltins_tmp) / sizeof(unsupportedUnsafeBuiltins_tmp[0]));
  }

  /** returns true if an argument list has a pointer argument */
  bool argsHasPointer( const llvm::Function::ArgumentListType& args ) {
    bool result = false;
    for ( llvm::Function::ArgumentListType::const_iterator argIt = args.begin();
          !result && argIt != args.end();
          ++argIt ) {
      result = argIt->getType()->isPointerTy();
    }
    return result;
  }

  typedef std::vector< Type* > TypeVector;
  static TypeVector typesOfArgumentList( llvm::Function::ArgumentListType& args );

  /** returns true if an argument list looks like it might contain a
   * manually written (in C) safe pointer; it searches for three
   * sequential pointers of the same type. It is by no means a certain
   * indicator, you should use it only for builtins where there is no
   * chance of mistake . */
  bool argsHasSafePointer( llvm::Function::ArgumentListType& args ) {
    bool result = false;
    TypeVector types = typesOfArgumentList(args);
    for ( signed i = 0; 
          !result && i <= int(types.size()) - 3;
          ++i ) {
      llvm::PointerType* pt1 = dyn_cast<PointerType>(types[i]);
      llvm::PointerType* pt2 = dyn_cast<PointerType>(types[i + 1]);
      llvm::PointerType* pt3 = dyn_cast<PointerType>(types[i + 2]);
      result = ( pt1 == pt2 && pt2 == pt3 );
    }
    return result;
  }

  /* **SafeArgTypes** is the operation for making a smartptrized
   * version of a function signature. Look at the constructor 
   * for more documentation.
   *
   * This class needs to be defined outside the ClampPointers as
   * Signature depends on it. (Although this order dependency could
   * be avoided, but probably most of the stuff should be moved
   * outside the ClampPointers struct..) */
  struct SafeArgTypes {
    TypeVector        argTypes;     // resulting argument types
    std::set<int>     safeArgNos;   // 0 is the first argument

    /* Given a list of function argument types
     * (typesOfArgumentList(F->getArgumentList())) constructs a
     * vector of the types of the arguments wrapped into safe
     * pointers, if they need to be. Otherwise the types are
     * returned as-is.
     *
     * @param c The LLVM Context
     * @param types The function argument types
     * @param dontTouchArguments Simply return the types as is, perform no wrapping
     */
    SafeArgTypes( LLVMContext& c,
                  const TypeVector& types,
                  bool dontTouchArguments,
                  Type* programAllocationsType ) {
        
      int argNo = 0;
      if (programAllocationsType) {
        argTypes.push_back( programAllocationsType );
      }
      for( TypeVector::const_iterator typeIt = types.begin(); typeIt != types.end(); ++typeIt, ++argNo ) {
        Type* t = *typeIt;

        // TODO: assert not supported arguments (e.g. some int**, struct etc... or at least verify cases we can allow)
        
        if( !dontTouchArguments && t->isPointerTy() ) {
          Type* smart_array_struct = getSmartStructType( c, t );
          argTypes.push_back( smart_array_struct );
          safeArgNos.insert( argNo );
        } else {
          fast_assert( (!t->isArrayTy()), "Passing array in arguments is not implemented." );
          argTypes.push_back( t );
        }
      }
    }
  };

  /** builds a TypeVector out of function arguments; useful for
   * dealing with safeArgTypes
   *
   * This class needs to be defined outside the ClampPointers as
   * Signature depends on it. (Although this order dependency could
   * be avoided, but probably most of the stuff should be moved
   * outside the ClampPointers struct..)
   */
  static TypeVector typesOfArgumentList( llvm::Function::ArgumentListType& args )
  {
    TypeVector v;
    for ( llvm::Function::ArgumentListType::iterator it = args.begin();
          it != args.end();
          ++it ) {
      v.push_back( it->getType() );
    }
    return v;
  }

  /** **Signature** class contains the part of the function
   * signature that identifies it: its demangled name and its
   * argument list. This is used for associating unsafe functions
   * with their safe corresponding functions.  It can also be copied
   * and it provides a comparison operator so it can be put into a
   * std::set or a std::map.
   *
   * This class needs to be defined outside the ClampPointers struct 
   * so that operator<< can be defined for it.
   */
  struct Signature {
    std::string       name;
    TypeVector        argTypes;

    Signature() {}

    Signature( llvm::Function* f ) :
      name( extractItaniumDemangledFunctionName(f->getName().str()) ),
      argTypes( typesOfArgumentList(f->getArgumentList()) ) {
      // nothing        
    }

    bool operator<( const Signature& other ) const {
      if ( name < other.name ) {
        return true;
      } else if ( name > other.name ) {
        return false;
      } else {
        return argTypes < other.argTypes;
      }
    }

    Signature safe( LLVMContext& c, Type* programAllocationsType ) {
      SafeArgTypes safeAt( c, argTypes, false, programAllocationsType );
      Signature s;
      s.name = name;
      s.argTypes = safeAt.argTypes;
      return s;
    }
  };

  llvm::raw_ostream& operator<<(llvm::raw_ostream& stream, const Signature& sig) {
    stream << sig.name << "(";
    bool first = true;
    for ( TypeVector::const_iterator it = sig.argTypes.begin();
          it != sig.argTypes.end();
          first = false, ++it ) {
      if ( !first ) {
        stream << ", ";
      }
      stream << **it;
    }
    stream << ")";
    return stream;
  }

  typedef std::map< Value*, int > ValueIndexMap;
  typedef std::set< Value* > ValueSet;
  typedef std::set< GlobalValue* > GlobalValueSet;
  typedef std::map< Function*, Function* > FunctionMap;
  typedef std::list< Function* > FunctionList;
  typedef std::map< Argument*, Argument* > ArgumentMap;
  typedef std::set< Function* > FunctionSet;
  typedef std::set< Argument* > ArgumentSet;
  typedef std::set< Instruction* > InstrSet;
  typedef std::set< CallInst* > CallInstrSet;
  typedef std::set< AllocaInst* > AllocaInstrSet;
  typedef std::set< GetElementPtrInst* > GepInstrSet;
  typedef std::set< LoadInst* > LoadInstrSet;
  typedef std::set< StoreInst* > StoreInstrSet;
  typedef std::set< int > IntSet;
  typedef std::set< unsigned > UIntSet;
  typedef std::vector< Value* > ValueVector;
  typedef std::map< unsigned, ValueVector > ValueVectorByAddressSpaceMap;
  typedef std::map< unsigned, GlobalValue* > AddressSpaceStructByAddressSpaceMap;
  typedef std::map< GlobalValue*, GlobalValue* > GlobalValueMap;
  class AreaLimitBase;
  typedef std::set< AreaLimitBase* > AreaLimitSet;
  typedef std::map< unsigned, AreaLimitSet > AreaLimitSetByAddressSpaceMap;
  typedef std::map< Value*, AreaLimitBase* > AreaLimitByValueMap;

  class FunctionManager;
  FunctionMap makeUnsafeToSafeMapping( LLVMContext& c, const FunctionManager& functionManager );

  // keeps track of function replacements and builtin functions
  class FunctionManager {
  public:
    FunctionManager(Module& M) :
      M(M) {
      // nothing
    }
    ~FunctionManager() {
      // nothing
    }

    void replaceFunction(Function *orig, Function *replacement) {
      replacedFunctions[orig] = replacement;
    }
    std::insert_iterator<FunctionMap> replaceFunctionInserter() {
      return std::inserter(replacedFunctions, replacedFunctions.begin());
    }
    const FunctionMap &getReplacedFunctions() const {
      return replacedFunctions;
    }

    void replaceArguments(Argument *orig, Argument *replacement) {
      replacedArguments.insert(std::make_pair(orig, replacement));
    }
    std::insert_iterator<ArgumentMap> replaceArgumentInserter() {
      return std::inserter(replacedArguments, replacedArguments.begin());
    }
    const ArgumentMap &getReplacedArguments() const {
      return replacedArguments;
    }

    void replaceArguments(const ArgumentMap& replacements) {
      replacedArguments.insert(replacements.begin(), replacements.end());
    }

    const CallInstrSet& getInternalCalls() const {
      return internalCalls;
    }
    void addInternalCall(CallInst* call) {
      internalCalls.insert(call);
      allCalls.insert(call);
    }

    const CallInstrSet& getExternalCalls() const {
      return externalCalls;
    }
    void addExternalCall(CallInst* call) {
      externalCalls.insert(call);
      allCalls.insert(call);
    }
    const CallInstrSet& getAllCalls() const {
      return allCalls;
    }

    void addAlloca(AllocaInst* call) {
      allocas.insert(call);
    }

    void addStore(StoreInst* call) {
      stores.insert(call);
    }

    void addLoad(LoadInst* call) {
      loads.insert(call);
    }

    const AllocaInstrSet &getAllocas() const {
      return allocas;
    }

    const StoreInstrSet &getStores() const {
      return stores;
    }

    const LoadInstrSet &getLoads() const {
      return loads;
    }

    void addSafeBuiltinFunction(Function* function) {
      safeBuiltinFunctions.insert(function);
    }

    void addUnsafeBuiltinFunction(Function* function) {
      unsafeBuiltinFunctions.insert(function);
    }

    const FunctionSet& getSafeBuiltinFunctions() const {
      return safeBuiltinFunctions;
    }

    const FunctionSet& getUnsafeBuiltinFunctions() const {
      return unsafeBuiltinFunctions;
    }

    const FunctionMap getUnsafeToSafeBuiltin() const {
      return makeUnsafeToSafeMapping(M.getContext(), *this);
    }

  private:
    // doesn't exist: the class is not copyable
    void operator=(FunctionManager& other);

    Module&         M;

    // Functions which has been replaced with new ones when signatures are modified.
    FunctionMap    replacedFunctions;
    // Function arguments mapping to find replacement arguments for old function arguments.
    ArgumentMap    replacedArguments;

    CallInstrSet   internalCalls;
    CallInstrSet   externalCalls;
    CallInstrSet   allCalls;
    AllocaInstrSet allocas;
    StoreInstrSet  stores;
    LoadInstrSet   loads;

    FunctionSet    unsafeBuiltinFunctions;
    FunctionSet    safeBuiltinFunctions;
  };

  /** Given a list of unsafe builtin functions and safe builtin
   * functions returns an association from the unsafe functions to
   * matching safe functions. Matching is implemented by generating
   * a safe version of the unsafe signature and checking if a
   * matching signature can be found from the list of safe builtin
   * functions.
   */
  FunctionMap makeUnsafeToSafeMapping( LLVMContext& c, 
                                       const FunctionManager& functionManager ) {
    FunctionMap mapping;
    std::map<Signature, Function*> safeSignatureMap;
      
    const FunctionSet& safeBuiltinFunctions = functionManager.getSafeBuiltinFunctions();
    for ( FunctionSet::const_iterator safeIt = safeBuiltinFunctions.begin();
          safeIt != safeBuiltinFunctions.end();
          ++safeIt ) {
      safeSignatureMap[Signature(*safeIt)] = *safeIt;
    }

    const FunctionSet& unsafeBuiltinFunctions = functionManager.getUnsafeBuiltinFunctions();
    for ( FunctionSet::const_iterator unsafeIt = unsafeBuiltinFunctions.begin();
          unsafeIt != unsafeBuiltinFunctions.end();
          ++unsafeIt ) {
      Signature origSig = Signature(*unsafeIt);
      Signature safeSig = origSig.safe(c, 0);
      std::map<Signature, Function*>::const_iterator safeSigIt = 
        safeSignatureMap.find(safeSig);
      if ( safeSigIt != safeSignatureMap.end() ) {
        mapping[*unsafeIt] = safeSigIt->second;
        DEBUG( dbgs() << "Mapped " << origSig << " => " << safeSig << "\n"; );
      }
    }
      
    return mapping;
  }

  class AreaLimitBase {
  public:
    AreaLimitBase() {}
    virtual ~AreaLimitBase() {}

    virtual void validAddressBoundsFor(Type *type, Instruction *checkStart, Value *&first, Value *&last) = 0;
    virtual void print(llvm::raw_ostream& stream) const = 0;
    virtual void getBounds(Function *F, IRBuilder<> &blockBuilder, Value *&min, Value *&max) { assert(0); }
  };

  // **AreaLimit** class holds information of single memory area allocation. Limits of the area
  // can be stored directly as constant expressions for *min* and *max* or they can be indirect
  // references to the limits. In case of indirect memory area, the *min* and *max* contains memory
  // addresses where limit addresses are stored.
  struct AreaLimit: public AreaLimitBase {
  private:
    Value* min; // Contains first valid address
    Value* max; // Contains last valid address
    bool indirect; // true if min and max are indirect pointers (requires load for getting address)

    AreaLimit( Value* _min, Value* _max, bool _indirect ) :
      min( _min ),
      max( _max ),
      indirect( _indirect ) {
    }
    
    // **AreaLimit::getValidAddressFor** Returns valid address relative to val and offset for given type of memory access access.
    //
    // If val is indirect, first add load instruction to get indirect address value and then
    // do the pointer cast / address arithmetic to get the correct address for given type.
    // basically does following: `return ((type)(isIndirect ? *val : val) + offset);`
    //
    // `Value *val` Direct or indirect base address which from `offset` is computed.
    // `bool isIndirect` True if we require adding a load instruction before doing address computing.
    // `int offset` Positive or negative offset how many addresses we roll from val.
    // `Type type` Type which kind of pointer we are going to access.
    // `Instruction *checkStart` Position where necessary loads and pointer arithmetics will be added.
    Value* getValidAddressFor(Value *val, bool isIndirect, int offset, Type *type, Instruction *checkStart) {
      Function *F = checkStart->getParent()->getParent();
      LLVMContext &c = F->getContext();
        
      Value *limit = val;
      if (isIndirect) {
        limit = new LoadInst(val, "", checkStart);
      }
                
      Value *ret_limit = NULL;
        
      if ( Instruction *inst = dyn_cast<Instruction>(limit) ) {
        /* bitcast can be removed by later optimizations if not necessary */
        CastInst *type_fixed_limit = BitCastInst::CreatePointerCast(inst, type, "", checkStart);
        ret_limit = GetElementPtrInst::Create(type_fixed_limit, genIntVector<Value*>(c, offset), "", checkStart);
          
      } else if ( Constant *constant = dyn_cast<Constant>(limit) ) {
        Constant *type_fixed_limit = ConstantExpr::getBitCast(constant, type);
        ret_limit = ConstantExpr::getGetElementPtr( type_fixed_limit, getConstInt(c, offset) );
          
      } else {
        fast_assert(false, "Couldnt resolve type of the limit value.");
      }

      std::stringstream ss; ss << offset;
      ret_limit->setName(val->getName() + "." + ss.str());
        
      return ret_limit;
    }

  public:

    // **AreaLimit::Create** AreaLimit factory. TODO: add bookkeeping and cleanup for freeing allocated memory.
    static AreaLimit* Create( Value* _min, Value* _max, bool _indirect) {
      DEBUG( dbgs() << "Creating limits:\nmin: "; _min->print(dbgs());
             dbgs() << "\nmax: "; _max->print(dbgs()); dbgs() << "\nindirect: " << _indirect << "\n"; );
      return new AreaLimit(_min, _max, _indirect);
    }

    // **AreaLimit::firstValidAddressFor and lastValidAddressFor** Returns first and last valid address
    // inside of AreaLimit for given type of memory access.
    //
    // Min and max are first loaded (in case of indirect limits) and then casted to given type
    // for being able to get the exact correct address. Actually it is possible that
    // `lastValidAddressFor < firstValidAddressFor` which means that requested type actually cannot
    // be accessed because it is too big. Limits are inclusive.
    //
    // `Type *type` Type of memory access which is going to be done inside these limits.
    // `Instruction *checkStart` We add new instructions before this if necessary.

    void print(llvm::raw_ostream& stream) const {
      stream << "### min: " << *min << "\n";
      stream << "### max: " << *max << "\n";
    }
      
    void validAddressBoundsFor(Type *type, Instruction *checkStart, Value *&first, Value *&last) {
      first = getValidAddressFor(max, indirect, -1, type, checkStart);
      last = getValidAddressFor(max, indirect, -1, type, checkStart);
    }
  };

  llvm::raw_ostream& operator<<(llvm::raw_ostream& stream, const AreaLimitBase& areaLimit) {
    areaLimit.print(stream);
    return stream;
  }

  // handles creating and bookkeeping address space info objects operations that require creating types must
  // be called after all information has been inserted. This is enforced by setting a 'fixed' flag upon such
  // operations, and if the flag is set, the object cannot be mutated.
  class AddressSpaceInfoManager {
  public:
    typedef void (AddressSpaceInfoManager::*GetLimitsFunc)(Function *F, IRBuilder<> &blockBuilder, int n, Value*& min, Value*& max) const;

    class ASAreaLimit: public AreaLimitBase {
    public:
      ASAreaLimit(AddressSpaceInfoManager& infoManager, GetLimitsFunc limitsFunc, int asNumber, int asIndex) :
        infoManager(infoManager), limitsFunc(limitsFunc), asNumber(asNumber), asIndex(asIndex) {
        // nothing
      }
      ~ASAreaLimit() {}
      
      void validAddressBoundsFor(Type *type, Instruction *at, Value *&first, Value *&last) {
        Function* F = at->getParent()->getParent();
        LLVMContext& c = F->getContext();
        IRBuilder<> blockBuilder(at);
        Value* min;
        Value* max;
        (infoManager.*limitsFunc)(F, blockBuilder, asIndex, min, max);

        first = BitCastInst::CreatePointerCast(min, type, "", at);

        /* bitcast can be removed by later optimizations if not necessary */
        CastInst *type_fixed_limit = BitCastInst::CreatePointerCast(max, type, "", at);
        last = GetElementPtrInst::Create(type_fixed_limit, genIntVector<Value*>(c, -1), "", at);
      }

      void getBounds(Function *F, IRBuilder<> &blockBuilder, Value *&min, Value *&max) {
        (infoManager.*limitsFunc)(F, blockBuilder, asIndex, min, max);
      }

      void print(llvm::raw_ostream& stream) const {
        stream << "### ASAreaLimit(asNumber " << asNumber << ", asIndex "  << asIndex << ")\n";
      }
    private:
      AddressSpaceInfoManager& infoManager;
      GetLimitsFunc            limitsFunc;
      unsigned                 asNumber;
      int                      asIndex;
    };

    AddressSpaceInfoManager(Module& M) : 
      M(M),
      programAllocationsType(0),
      constantLimitsType(0),
      globalLimitsType(0),
      localLimitsType(0),
      localAllocations(0),
      constantAllocations(0),
      fixed(false) {
      // these address spaces have allocations in the compilation unit
      asAreaLimits[constantAddressSpaceNumber].insert(new ASAreaLimit(*this, getASLimitsFunc(constantAddressSpaceNumber), constantAddressSpaceNumber, 0));
      asAreaLimits[privateAddressSpaceNumber].insert(new ASAreaLimit(*this, getASLimitsFunc(privateAddressSpaceNumber), privateAddressSpaceNumber, 0));
      asAreaLimits[localAddressSpaceNumber].insert(new ASAreaLimit(*this, getASLimitsFunc(localAddressSpaceNumber), localAddressSpaceNumber, 0));
    }
    // not implemented: cannot be copy-constructed
    AddressSpaceInfoManager(const AddressSpaceInfoManager&);
    ~AddressSpaceInfoManager() {
      for (AddressSpaceAllocAreaLimitSetMap::iterator it = asAreaLimits.begin();
           it != asAreaLimits.end();
           ++it) {
        for (AreaLimitSet::iterator alIt = it->second.begin();
             alIt != it->second.end();
             ++alIt) {
          delete *alIt;
        }
      }
      for (GlobalValueSet::iterator it = deleteGlobalValues.begin();
           it != deleteGlobalValues.end();
           ++it) {
        delete *it;
      }
    }
    
    // add new replacement to bookkeeping
    void addReplacement(Value* original, Value* replacement) {
      DEBUG( dbgs() << "ASInfoManager bookkeeping, added value replacement: "; original->print(dbgs()); dbgs() << " replacement: "; replacement->print(dbgs()); dbgs() << "\n"; );
      originalValues[replacement] = original;
      replacedValues[original] = replacement;
    }
    
    // get corresponding value after replacing static allocations with structs and
    // moving functions to new signature
    Value* getReplacedValue(Value *original) {
      Value *replaced = replacedValues[original];
      if (!replaced) {
        // here we should not assume that replaced value is same, because this query is currently done only
        // for globalvariables / allocas 
        dbgs() << "Could not find replacement for: "; original->print (dbgs()); dbgs() << "\n";
        fast_assert(replaced, "Please implement bookkeeping where you add mapping between old and new value.");
      }
      return replaced;
    }
    
    // get corresponding original value for the value after transformations
    Value* getOriginalValue(Value *replaced) {
      Value *original = originalValues[replaced];
      if (!original) {
        // if exception is not found from bookkeeping, assume that original and new value is the same
        return replaced;
      }
      return original;
    }
    
    void addAddressSpace(unsigned asNumber, bool isGlobalScope, const ArrayRef<Value*> &values, const ArrayRef<Constant*> &dataInit) {
      // TODO: make copy of values and all other data..
      // TODO: implement!
      assert(!fixed);
      for (ArrayRef<Value*>::const_iterator it = values.begin();
           it != values.end();
           ++it) {
        valueASMapping.insert(std::make_pair(*it, ValueASIndex(asNumber, asValues[asNumber].size())));
        asValues[asNumber].push_back(*it);
      }
      std::copy(dataInit.begin(), dataInit.end(), std::back_inserter(asInits[asNumber]));
    }
    int nthArgAreaLimitIndex(unsigned asNumber, int n) {
      // these two address spaces have a special limit for all allocations as the first argument, skip that
      if (asNumber == constantAddressSpaceNumber || asNumber == localAddressSpaceNumber) {
        ++n;
      }
      return n;
    }
    void addDynamicLimitRange(Argument* arg) {
      // TODO: kernel is disregarded for now
      // TODO: implement, add enough info to be able to calculate worst case scenario how many limit areas we should use.
      assert(!fixed);
      PointerType* type = cast<PointerType>(arg->getType());
      unsigned asNumber = type->getAddressSpace();
      int idx = dynamicRanges[asNumber].size();
      dynamicRanges[asNumber].push_back(arg);
      ASAreaLimit* areaLimit = new ASAreaLimit(*this, getASLimitsFunc(asNumber), asNumber, nthArgAreaLimitIndex(asNumber, idx));
      asAreaLimits[asNumber].insert(areaLimit);
      argumentAreaLimits[arg] = areaLimit;
    }
    // for an argument, return its area limits if it is a known argument, otherwise return an empty set
    AreaLimitSet getArgumentLimits(Argument* arg) {
      AreaLimitSet limits;
      if (argumentAreaLimits.count(arg)) {
        limits.insert(argumentAreaLimits.find(arg)->second);
      }
      return limits;

    }
    AreaLimitSet getASLimits(unsigned asNumber) {
      AddressSpaceAllocAreaLimitSetMap::const_iterator it = asAreaLimits.find(asNumber);
      if (it != asAreaLimits.end()) {
        return it->second;
      } else {
        return AreaLimitSet();
      }
    }
    GlobalVariable* getLocalAllocations() {
      fixed = true;
      if (!localAllocations) {
        localAllocations = new GlobalVariable
          (M, getASAllocationsType(localAddressSpaceNumber), false, GlobalValue::InternalLinkage, 
           ConstantAggregateZero::get(getASAllocationsType(localAddressSpaceNumber)), 
           "localAllocations", NULL, GlobalVariable::NotThreadLocal, localAddressSpaceNumber);
      }
      return localAllocations;
    }
    GlobalVariable* getConstantAllocations() {
      fixed = true;
      if (!constantAllocations) {
        constantAllocations = new GlobalVariable
          (M, getASAllocationsType(constantAddressSpaceNumber), true, GlobalValue::InternalLinkage, 
           ConstantStruct::get(getASAllocationsType(constantAddressSpaceNumber), asInits[constantAddressSpaceNumber]),
           "constantAllocations", NULL, GlobalVariable::NotThreadLocal, constantAddressSpaceNumber);
      }
      return constantAllocations;
    }
    Value* generateProgramAllocationCode(Function* F, IRBuilder<> &blockBuilder) {
      fixed = true;
      Value* paa = blockBuilder.CreateAlloca(dyn_cast<PointerType>(getProgramAllocationsType())->getTypeAtIndex(0u),
                                             0,
                                             "ProgramAllocationsRoot");
        
      getLocalAllocations();
      getConstantAllocations();

      generateASLimitsInit(constantAddressSpaceNumber, blockBuilder, getConstantLimitsField(F, blockBuilder));
      generateASLimitsInit(globalAddressSpaceNumber, blockBuilder, getGlobalLimitsField(F, blockBuilder));
      generateASLimitsInit(localAddressSpaceNumber, blockBuilder, getLocalLimitsField(F, blockBuilder));
      //generatePrivateAllocationsInit(blockBuilder, getPrivateAllocationsField(F, blockBuilder));

      //Function* kernel = blockBuilder.GetInsertPoint()->getParent()->getParent();

      return paa;
    }
    // slightly hazardous; you need to be sure you've called non-const getProgramAllocationsType before using this
    PointerType* getProgramAllocationsType() const {
      assert(fixed);
      return const_cast<AddressSpaceInfoManager&>(*this).getProgramAllocationsType();
    }
    PointerType* getProgramAllocationsType() {
      fixed = true;
      if (!programAllocationsType) {
        LLVMContext& c = M.getContext();

        programAllocationsType =
          PointerType::get(StructType::create(c,
                                              genVector<Type*>(getASLimitsType(constantAddressSpaceNumber),
                                                               getASLimitsType(globalAddressSpaceNumber),
                                                               getASLimitsType(localAddressSpaceNumber),
                                                               getASAllocationsType(privateAddressSpaceNumber)),
                                              "ProgramAllocationsType"),
                           privateAddressSpaceNumber);
      }
      return programAllocationsType;
    }
    void replaceUsesOfOriginalVariables() {
      fixed = true;
      // TODO: go through value mappings of every address space that we have created and replace all uses with.
      for (ValueASIndexMap::const_iterator it = valueASMapping.begin();
           it != valueASMapping.end();
           ++it) {
        Value* const value = it->first;
        const ValueASIndex& index = it->second;
        
        DEBUG( dbgs() << "Replacing:"; value->print(dbgs());
               dbgs() << " with AddressSpaceRef: " << index.asNumber << "," << index.index << "\n"; );
        
        if (Instruction* inst = dyn_cast<Instruction>(value)) {
          IRBuilder<> blockBuilder(inst);
          Function* F = inst->getParent()->getParent();
          Value* replacement = getValueReplacement(F, blockBuilder, value);
          assert(replacement != value);
          // store replacement to bookkeeping to be able to trace between original and new values
          addReplacement(value, replacement);
          value->replaceAllUsesWith(replacement);

        } else if (GlobalVariable* global = dyn_cast<GlobalVariable>(value)) {
          Constant *constGEP = ConstantExpr::getGetElementPtr(global, genIntVector<Constant*>(global->getParent()->getContext(), 0, 1));
          global->replaceAllUsesWith(constGEP);
          addReplacement(global, constGEP);
          deleteGlobalValues.insert(global);
          
        } else {
          assert(0);
        }
      }
    }

    Value* getValueReplacement(Function* F, IRBuilder<> &blockBuilder, Value *value) {
      if (!valueASMapping.count(value)) {
        return value;
      }
      const ValueASIndex& index = valueASMapping.find(value)->second;
      Value* replacement;
      if (index.asNumber == privateAddressSpaceNumber) {
        replacement = getPrivateAllocationsField(F, blockBuilder, index.index);
      } else if (index.asNumber == constantAddressSpaceNumber) {
        replacement = getConstantAllocationsField(F, blockBuilder, index.index);
      } else if (index.asNumber == localAddressSpaceNumber) {
        replacement = getLocalAllocationsField(F, blockBuilder, index.index);
      } else if (index.asNumber == globalAddressSpaceNumber) {
        replacement = value;
      }
      assert(replacement);
      replacement->setName(replacement->getName() + "." + value->getName());
      return replacement;
    }

    Constant* getASFieldGEP(unsigned asNumber, int index) {
      LLVMContext& c = M.getContext();
      if (asNumber == privateAddressSpaceNumber) {
        assert(0);
        return 0;
      } else if (asNumber == constantAddressSpaceNumber) {
        return ConstantExpr::getGetElementPtr( getConstantAllocations(), genIntVector<Constant*>(c, 0, index) );
      } else if (asNumber == localAddressSpaceNumber) {
        return ConstantExpr::getGetElementPtr( getLocalAllocations(), genIntVector<Constant*>(c, 0, index) );
      } else if (asNumber == globalAddressSpaceNumber) {
        assert(0);
      } else {
        assert(0);
      }
    }

    /** Given a function, retrieve the value for the program allocations value passed as the function's first
        parameter. */
    Value* getProgramAllocations(Function& F) const {
      Value* paa = 0;
      Type* paaType = getProgramAllocationsType();
      if (F.arg_begin() != F.arg_end()) {
        Argument& arg = *F.arg_begin();
        if (PointerType* pointerType = dyn_cast<PointerType>(arg.getType())) {
          if (pointerType == paaType) {
            paa = &arg;
          }
        }
      }
      if (!paa) {
        // find an allocation for ProgramAllocationsType from the entry block
        if (F.begin() != F.end()) {
          BasicBlock& block = *F.begin();
          for (BasicBlock::iterator it = block.begin();
               it != block.end() && !paa;
               ++it) {
            if (AllocaInst* alloca = dyn_cast<AllocaInst>(it)) {
              if (PointerType* pointerType = dyn_cast<PointerType>(alloca->getType())) {
                if (pointerType == paaType) {
                  paa = alloca;
                }
              }
            }
          }
        }
      }
      assert(paa);
      return paa;
    }
    
    // TODO: also needs to get replaced calls I suppose...
    void addAReplacementsToBookkeepping(FunctionManager &functionManager) {
      const ArgumentMap& replacedArgs = functionManager.getReplacedArguments();
      for (ArgumentMap::const_iterator a = replacedArgs.begin(); a != replacedArgs.end(); ++a) {
        Argument *original = a->first;
        Argument *replacement = a->second;
        addReplacement(original, replacement);
      }
    }
    
  private:
    Module& M;
    PointerType* programAllocationsType;
    typedef std::map<unsigned, StructType*> AddressSpaceStructTypeMap;
    typedef std::vector<PointerType*> PointerTypeVector;
    typedef std::vector<Argument*> ArgumentVector;
    typedef std::map<Argument*, AreaLimitBase*> ArgumentAreaLimitMap;
    typedef std::map<unsigned, AreaLimitSet> AddressSpaceAllocAreaLimitSetMap;
    typedef std::map<unsigned, ArgumentVector> AddressSpaceArgumentVectorMap;
    typedef std::vector<Constant*> ConstantValueVector; // not to be confused with llvm's ConstantVector..
    typedef std::map<unsigned, ConstantValueVector> ConstantValueVectorByAddressSpaceMap;
    AddressSpaceStructTypeMap allocationsTypes;
    AddressSpaceArgumentVectorMap dynamicRanges;
    ArgumentAreaLimitMap argumentAreaLimits;
    AddressSpaceAllocAreaLimitSetMap asAreaLimits;
    AddressSpaceStructTypeMap asLimitsTypes;
    StructType* constantLimitsType;
    StructType* globalLimitsType;
    StructType* localLimitsType;
    GlobalVariable* localAllocations;
    GlobalVariable* constantAllocations;
    std::map<Value*, Value*> replacedValues;
    std::map<Value*, Value*> originalValues;
    GlobalValueSet deleteGlobalValues; // the set of globals to be deleted at destructor
    bool fixed;               // once fixed cannot become unfixed.
    struct ValueASIndex {
      unsigned asNumber;
      int      index; // index of the value in the address space struct
      ValueASIndex(unsigned asNumber, int index) :
        asNumber(asNumber), index(index) {
        // nothing
      }
    };
    typedef std::map<Value*, ValueASIndex> ValueASIndexMap;
    ValueASIndexMap valueASMapping;

    ValueVectorByAddressSpaceMap asValues;
    ConstantValueVectorByAddressSpaceMap asInits;

    static GetLimitsFunc getASLimitsFunc(unsigned asNumber) {
      if (asNumber == globalAddressSpaceNumber) {
        return &AddressSpaceInfoManager::getGlobalLimits;
      } else if (asNumber == localAddressSpaceNumber) {
        return &AddressSpaceInfoManager::getLocalLimits;
      } else if (asNumber == constantAddressSpaceNumber) {
        return &AddressSpaceInfoManager::getConstantLimits;
      } else if (asNumber == privateAddressSpaceNumber) {
        return &AddressSpaceInfoManager::getPrivateLimits;
      } else {
        assert(0);
      }
    }
    int getNumASLimits(unsigned asNumber) const {
      if (asNumber == globalAddressSpaceNumber) {
        return (dynamicRanges.count(globalAddressSpaceNumber) ? dynamicRanges.find(globalAddressSpaceNumber)->second.size() : 0); // args
      } else if (asNumber == localAddressSpaceNumber) {
        return (dynamicRanges.count(globalAddressSpaceNumber) ? dynamicRanges.find(globalAddressSpaceNumber)->second.size() : 0) + 1; // args + local allocations
      } else if (asNumber == constantAddressSpaceNumber) {
        return (dynamicRanges.count(constantAddressSpaceNumber) ? dynamicRanges.find(constantAddressSpaceNumber)->second.size() : 0) + 1; // args + constant allocations
      } else if (asNumber == privateAddressSpaceNumber) {
        return 1; // only private allocations
      } else {
        assert(0);
      }
    }
    GetElementPtrInst* getConstantLimitsField(Function* F, IRBuilder<> &blockBuilder) const {
      LLVMContext& c = M.getContext();
      Value* paa = getProgramAllocations(*F);
      GetElementPtrInst* value = cast<GetElementPtrInst>(blockBuilder.CreateGEP(paa, genIntVector<Value*>(c, 0, 0)));
      value->setName("constantLimits");
      return value;
    }
    void getConstantLimits(Function *F, IRBuilder<> &blockBuilder, int n, Value*& min, Value*& max) const {
      LLVMContext& c = M.getContext();
      Value* paa = getProgramAllocations(*F);
      min = blockBuilder.CreateGEP(paa, genIntVector<Value*>(c, 0, 0, 2 * n + 0));
      min->setName("constantLimits.min");
      max = blockBuilder.CreateGEP(paa, genIntVector<Value*>(c, 0, 0, 2 * n + 1));
      max->setName("constantLimits.max");
    }
    GetElementPtrInst* getGlobalLimitsField(Function *F, IRBuilder<> &blockBuilder) const {
      LLVMContext& c = M.getContext();
      Value* paa = getProgramAllocations(*F);
      GetElementPtrInst* value = cast<GetElementPtrInst>(blockBuilder.CreateGEP(paa, genIntVector<Value*>(c, 0, 1)));
      value->setName("globalLimits");
      return value;
    }
    void getGlobalLimits(Function *F, IRBuilder<> &blockBuilder, int n, Value*& min, Value*& max) const {
      LLVMContext& c = M.getContext();
      Value* paa = getProgramAllocations(*F);
      min = blockBuilder.CreateGEP(paa, genIntVector<Value*>(c, 0, 1, 2 * n + 0));
      min->setName("globalLimits.min");
      max = blockBuilder.CreateGEP(paa, genIntVector<Value*>(c, 0, 1, 2 * n + 1));
      max->setName("globalLimits.max");
    }
    GetElementPtrInst* getLocalLimitsField(Function *F, IRBuilder<> &blockBuilder) const {
      LLVMContext& c = M.getContext();
      Value* paa = getProgramAllocations(*F);
      GetElementPtrInst* value = cast<GetElementPtrInst>(blockBuilder.CreateGEP(paa, genIntVector<Value*>(c, 0, 2)));
      value->setName("localLimits");
      return value;
    }
    void getLocalLimits(Function *F, IRBuilder<> &blockBuilder, int n, Value*& min, Value*& max) const {
      LLVMContext& c = M.getContext();
      Value* paa = getProgramAllocations(*F);
      min = blockBuilder.CreateGEP(paa, genIntVector<Value*>(c, 0, 2, 2 * n + 0));
      min->setName("localLimits.min");
      max = blockBuilder.CreateGEP(paa, genIntVector<Value*>(c, 0, 2, 2 * n + 1));
      max->setName("localLimits.max");
    }
    GetElementPtrInst* getPrivateAllocationsField(Function *F, IRBuilder<> &blockBuilder) const {
      LLVMContext& c = M.getContext();
      Value* paa = getProgramAllocations(*F);
      GetElementPtrInst* value = cast<GetElementPtrInst>(blockBuilder.CreateGEP(paa, genIntVector<Value*>(c, 0, 3)));
      value->setName("privateAllocs");
      return value;
    }
    GetElementPtrInst* getPrivateAllocationsField(Function *F, IRBuilder<> &blockBuilder, int n) const {
      LLVMContext& c = M.getContext();
      Value* paa = getProgramAllocations(*F);
      GetElementPtrInst* value = cast<GetElementPtrInst>(blockBuilder.CreateGEP(paa, genIntVector<Value*>(c, 0, 3, n)));
      value->setName("privateAllocs");
      return value;
    }
    void getPrivateLimits(Function *F, IRBuilder<> &blockBuilder, int n, Value*& min, Value*& max) const {
      LLVMContext& c = M.getContext();
      Value* paa = getProgramAllocations(*F);
      min = blockBuilder.CreateGEP(paa, genIntVector<Value*>(c, 0, 3));
      min->setName("privateLimits.min");
      // TODO: return proper maximum
      max = blockBuilder.CreateGEP(paa, genIntVector<Value*>(c, 0, 3));
      max->setName("privateLimits.max");
    }
    ConstantExpr* getConstantAllocationsField(Function *F, IRBuilder<> &blockBuilder, int n) {
      LLVMContext& c = M.getContext();
      GlobalVariable* root = getConstantAllocations();
      Value* v = blockBuilder.CreateGEP(root, genIntVector<Value*>(c, 0, n));
      ConstantExpr* value = cast<ConstantExpr>(v);
      value->setName("constantAllocs");
      return value;
    }
    ConstantExpr* getLocalAllocationsField(Function *F, IRBuilder<> &blockBuilder, int n) {
      LLVMContext& c = M.getContext();
      GlobalVariable* root = getLocalAllocations();
      ConstantExpr* value = cast<ConstantExpr>(blockBuilder.CreateGEP(root, genIntVector<Value*>(c, 0, n)));
      value->setName("localAllocs");
      return value;
    }


    Value* getConstantAllocationsInit(Function *F, IRBuilder<> &blockBuilder) {
      LLVMContext& c = M.getContext();
      // TODO
      return ConstantStruct::get(getASAllocationsType(constantAddressSpaceNumber), genIntVector<Constant*>(c, 0));
    }

    Value* getLocalAllocationsInit(Function *F, IRBuilder<> &blockBuilder) {
      LLVMContext& c = M.getContext();
      // TODO
      return ConstantStruct::get(getASAllocationsType(localAddressSpaceNumber), genIntVector<Constant*>(c, 0));
    }

    StructType* getASAllocationsType(int asNumber) {
      if (!allocationsTypes[asNumber]) {
        LLVMContext& c = M.getContext();
        std::vector<Type*> fields;
        const ValueVector& values = asValues[asNumber];
        // TODO: should the types be resolved earlier? This dereference conversion is done because global
        // variables are pointers
        for (ValueVector::const_iterator it = values.begin(); it != values.end(); ++it) {
          Type* type = (*it)->getType();
          assert(isa<PointerType>(type));
          fields.push_back(dyn_cast<PointerType>(type)->getTypeAtIndex(0u));
        }
        allocationsTypes[asNumber] = StructType::create(c, fields, addressSpaceLabel(asNumber) + "AllocationsType");
      }
      return allocationsTypes[asNumber];
    }

    Value* generatePrivateAllocationsInit(IRBuilder<> &blockBuilder) {
      // LLVMContext& c = M.getContext();
      // ValueVector values = asValues[privateAddressSpaceNumber];
      // std::vector<Constant*> initValues;
      // for (int c = 0; c < values.length(); ++c) {
      //   initValues.push_back();
      // }
      // // TODO
      // return ConstantStruct::get(getPrivateAllocationsType(), genIntVector<Constant*>(c, 0));
      return 0;
    }

    StructType* getConstantLimitsType() {
      if (!constantLimitsType) {
        constantLimitsType = getASLimitsType(constantAddressSpaceNumber);
      }
      return constantLimitsType;
    }

    StructType* getASLimitsType(unsigned asNumber) {
      if (!asLimitsTypes.count(asNumber)) {
        LLVMContext& c = M.getContext();
        std::vector<Type*> fields;
        Type* allocationsType = getASAllocationsType(asNumber);
        if (asNumber != globalAddressSpaceNumber) {
          fields.push_back(PointerType::get(allocationsType, asNumber)); // xxxAllocations_min
          fields.push_back(PointerType::get(allocationsType, asNumber)); // xxxAllocations_max
        }
        const ArgumentVector& dynamic = dynamicRanges[asNumber];
        for (ArgumentVector::const_iterator it = dynamic.begin();
             it != dynamic.end();
             ++it) {
          PointerType* type = cast<PointerType>((*it)->getType());
          fields.push_back(type); // min
          fields.push_back(type); // max
        }
        asLimitsTypes[asNumber] = StructType::create(c, fields, addressSpaceLabel(asNumber) + "LimitsType");
      }
      return asLimitsTypes[asNumber];
    }

    void generateASLimitsInit(unsigned asNumber, IRBuilder<> &blockBuilder, Value* results) {
      // TODO
      if (asNumber == globalAddressSpaceNumber) {
          
      } else {
        StructType* t = getASLimitsType(asNumber);
        std::vector<Constant*> init;
        for (StructType::element_iterator it = t->element_begin();
             it != t->element_end();
             ++it) {
          init.push_back(Constant::getNullValue(*it));
        }
        blockBuilder.CreateStore(ConstantStruct::get(getASLimitsType(asNumber), init), results);
      }
    }
  };

  // TODO: probably we could use more complete graph info
  //       which allows to later trace all dependencies and
  //       to find out e.g. all loads, stores and calls of
  //       certain Value ( TODO TODO: check if llvm has
  //       where we can query this kind of information directly)
  class DependenceAnalyser {
  private:
    InstrSet needChecks;

    typedef std::pair< Instruction*, Value* > DepValue;
    typedef std::pair< Instruction*, Value* > IndirectAccess;
    typedef std::pair< int, Value* > DepKey;
    typedef std::set< DepValue > DepValueSet;
    
    std::map< DepKey, DepValueSet > dependencies;
    std::set< IndirectAccess > indirectAccesses;
    
    ValueSet usedInCallOperand;
    ValueSet usedInRelativeMemPtrOperand;
    ValueSet usedInStoreValOperand;
    
    // values which finally limits the operand
    std::map< Value*, Value* > limitingDeps;

    /**
     * Traces value ancestors until the base address is found.
     * Could be int -> pointer cast, load, alloca or global variable.
     *
     * TODO: could be probably refactored with resolveAncestors
     */
    Value *getBase(Value* val, int recursion_level = 0) {

      Value *next = NULL;
      DEBUG( for (int i = 0; i < recursion_level; i++ ) dbgs() << "  "; );
        
      if ( GetElementPtrInst *gep = dyn_cast<GetElementPtrInst>(val) ) {
        DEBUG( dbgs() << "Found GEP: "; val->print(dbgs()); dbgs() << " tracing to baseval.\n"; );
        next = gep->getPointerOperand();
      } else if ( LoadInst *load = dyn_cast<LoadInst>(val) ) {
        DEBUG( dbgs() << "Returning LOAD: "; val->print(dbgs()); dbgs() << "\n"; );
      } else if ( isa<StoreInst>(val) ) {
        fast_assert(false, "No way! I dont have any idea how code can reach this point. Store cannot be used as operand.");
      } else if ( CastInst* cast = dyn_cast<CastInst>(val) ) {
        // if cast is not from pointer to pointer in same address space, cannot resolve
        if (!cast->getType()->isPointerTy() ||
            dyn_cast<PointerType>(cast->getType())->getAddressSpace() != dyn_cast<PointerType>(val->getType())->getAddressSpace()) {
            DEBUG( dbgs() << "returning untraceable cast\n" );
        } else {
          DEBUG( dbgs() << " tracing to src op.\n" );
          next = cast->getOperand(0);
        }
      } else if ( ConstantExpr *expr = dyn_cast<ConstantExpr>(val) ) {
        Instruction* inst = getAsInstruction(expr);
        if ( GetElementPtrInst *gep = dyn_cast<GetElementPtrInst>(inst) ) {
          DEBUG( dbgs() << "... constant GEP, following to baseval.\n"; );
          next = gep->getPointerOperand();
        } else {
          DEBUG( dbgs() << "... returning unhandled constant expr.\n"; );
        }
        delete inst;
      }
        
      if (next) {
        return getBase(next, recursion_level + 1);
      }
      return val;
    }
  
    int getIndirection(Type* type) {
      int retVal = 0;
      while (type->isPointerTy()) {
        retVal++;
        type = type->getPointerElementType();
      }
      return retVal;
    }
    
    Value* getBaseDep(Value *op) {
      Value *current = op;
      Value *next = NULL;
      // TODO: make sure our tree cannot have circular dependencies..
      while (true) {
        next = getDependency(current);
        if (current == next) break;
        current = next;
      }
      return next;
    }
    
  public:
      
    InstrSet& needCheck() {
      return needChecks;
    }

    void addCheck(Instruction *inst) {
      needChecks.insert(inst);
    }
    
    void analyseOperands(Instruction *inst) {     
      // if inst is call / store / load check pointer, resolve some data about
      // how their operands were used...
    
      // TODO: this thing could also resolve if relative mem operand is constant and in bounds
      
      if (inst != NULL) {
        Value *memPtrOp = NULL;
        if (CallInst *call = dyn_cast<CallInst>(inst) ) {
          for (CallInst::op_iterator op = call->op_begin(); op != call->op_end(); ++op) {
            Value *opVal = *op;
            if (opVal->getType()->isPointerTy()) {
              Value *base = getBase(opVal);
              DEBUG(dbgs() << "Used in call operand: "; inst->print(dbgs());
                    dbgs() << " base: "; base->print(dbgs()); dbgs() << "\n";);
              usedInCallOperand.insert(base);
              if (dyn_cast<LoadInst>(base)) indirectAccesses.insert( IndirectAccess(inst, opVal) );
            }
          }
        } else if (StoreInst *store = dyn_cast<StoreInst>(inst)) {
          memPtrOp = store->getPointerOperand();
          if (store->getValueOperand()->getType()->isPointerTy()) {
            Value *base = getBase(store->getValueOperand());
            DEBUG(dbgs() << "Used in store val operand: "; inst->print(dbgs());
                  dbgs() << " base: "; base->print(dbgs()); dbgs() << "\n";);
            usedInStoreValOperand.insert(base);
          }
        } else if (LoadInst *load = dyn_cast<LoadInst>(inst)) {
          memPtrOp = load->getPointerOperand();
        }
        
        if (memPtrOp) {
          // if not direct store without any gep / casting shenanigans assume dangerous
          if (!dyn_cast<AllocaInst>(memPtrOp)) {
            Value *base = getBase(memPtrOp);
            DEBUG(dbgs() << "Used in relative mem ptr operand: "; inst->print(dbgs());
                  dbgs() << " base: "; base->print(dbgs()); dbgs() << "\n";);
            usedInRelativeMemPtrOperand.insert(base);
            if (dyn_cast<LoadInst>(base)) indirectAccesses.insert( IndirectAccess(inst, memPtrOp) );
          }
        }
      }      
    }
    
    // TODO: maybe we should also trace ancestors of pointer -> non pointer casts to make sure
    //       that those allocas will be also put to struct? For now we just don't support
    //       int -> ptr casts (and then passing pointer to call or accessing data). 
    // @param indirection number of pointers before reading actual value stored in address
    //                    needed to be able to trace dependencies
    void addDependency(int indirection,
                       Instruction* applyLocation,
                       Value* newVal, Value *whoseLimitsToRespect) {

      DepKey key = DepKey(indirection, newVal);
      
      dbgs() << "\nAdding dependency[" << indirection << ": "; newVal->print(dbgs()); dbgs() << "] = "; whoseLimitsToRespect->print(dbgs()); dbgs() << "\n";
      
      dependencies[key].insert( DepValue(applyLocation, whoseLimitsToRespect) );
    }
    
    Value* getDependency(Value *value) {
      
      if (LoadInst *load = dyn_cast<LoadInst>(value)) {
        value = load->getPointerOperand();
      }

      int indirection = getIndirection(value->getType());

      dbgs() << "Dep for: [" << indirection << ":"; value->print(dbgs()); dbgs() << "]\n";
      DepKey key = DepKey(indirection, value);
      DepValueSet &depSet = dependencies[key];

      if (depSet.size() == 0) return NULL;

      if (depSet.size() > 1) {
        // TODO: if single depenedency return it... add different getter to analyse where to get
        //       limits for load / store / call operand. Correct one can be resolved from DepValue location
        //       and value location.
        
        for (DepValueSet::iterator dv = depSet.begin(); dv != depSet.end(); ++dv) {
          dbgs() << "Dep: "; dv->second->print(dbgs());
          dbgs() << "\nWas set in: "; dv->first->print(dbgs()); dbgs() << "\n";
        }
        
        fast_assert(false, "More than 1 possible dependencies. Add here some more algorithm to resolve which one is the correct.");
      }
      
      dbgs() << "returning: "; (*depSet.begin()).second->print(dbgs()); dbgs() << "\n";
      
      return (*depSet.begin()).second;
    }
    
    bool hasOnlySafeAccesses(Value *ptrVal) {
      if (ptrVal == NULL || !ptrVal->getType()->isPointerTy()) {
        return false;
      }
      return (usedInCallOperand.count(ptrVal) == 0 &&
              usedInRelativeMemPtrOperand.count(ptrVal) == 0 &&
              usedInStoreValOperand.count(ptrVal) == 0);
    }

    // returns the memory allocation whose limits accessing this value should respect
    Value* getLimitingDependency(Instruction *inst, Value *operand) {
      // get from cache..
      if (limitingDeps.count(operand) > 0) {
        return limitingDeps[operand];
      }
      
      bool isIndirect = indirectAccesses.count( IndirectAccess(inst, operand) ) > 0;
      Value *limitingDep = NULL;
      if (isIndirect) {
        // if indirect access scan through getDepency hirarchy
        limitingDep = getBaseDep(operand);
      } else {
        // for direct access, get base with getBase... we could have stored that earlier..
        limitingDep = getBase(operand);
      }
      DEBUG( dbgs() << "LIMIT: "; limitingDep->print(dbgs()); dbgs() << "\n"; );
      limitingDeps[operand] = limitingDep;
      return limitingDep;
    }
    
    // pre resolve all limiting dependencies to cache 
    // before use replacements makes dependece graph inusable
    void resolveLimitsForAllChecks() {
      for (InstrSet::iterator inst = needCheck().begin(); inst != needCheck().end(); ++inst) {
        DEBUG( dbgs() << "\nRESOLVING OPERAND LIMITS FOR: "; (*inst)->print(dbgs()); dbgs() << "\n"; );
        Value *memOperand = NULL;
        if (CallInst *call = dyn_cast<CallInst>(*inst)) {
          for (CallInst::op_iterator op = call->op_begin(); op != call->op_end(); ++op) {
            Value *opVal = *op;
            if (opVal->getType()->isPointerTy()) {
              Value *opLimits = getLimitingDependency(call, opVal);
            }
          }
        } else if (LoadInst *load = dyn_cast<LoadInst>(*inst)) {
          memOperand = load->getPointerOperand();
        } else if (StoreInst *store = dyn_cast<StoreInst>(*inst)) {
          memOperand = store->getPointerOperand();
        }
        
        if (memOperand) {
          Value *opLimits = getLimitingDependency(*inst, memOperand);
        }
      }
    }

  };

  // handles creating limits according to information found from address space info manager and dependency analyser
  class AreaLimitManager {
  public:
    AreaLimitManager(AddressSpaceInfoManager& infoManager, DependenceAnalyser& dependenceAnalyser) :
      infoManager(infoManager),
      dependenceAnalyser(dependenceAnalyser) {
      // nothing
    }

    // does not exist
    AreaLimitManager(const AreaLimitManager& other);

    ~AreaLimitManager() {}

    AreaLimitSet getAreaLimits(Instruction* inst, Value* ptrOperand) {
      Value* current = ptrOperand;
      AreaLimitSet limits;

      // get first limit from address space
      assert(isa<PointerType>(ptrOperand->getType()));
      PointerType* pointerType = cast<PointerType>(ptrOperand->getType());
      int asNumber = pointerType->getAddressSpace();
      limits = infoManager.getASLimits(asNumber);

      // if there was not single limits try to check limits from dependence manager
      if (limits.size() != 1) {
        // get original values to be able to query DependenceAnalyser (original != inst only for calls)
        Instruction *originalInst = dyn_cast<Instruction>(infoManager.getOriginalValue(inst));
        Value *originalPtrOperand = infoManager.getOriginalValue(ptrOperand);
      
        Value* base = dependenceAnalyser.getLimitingDependency(originalInst, originalPtrOperand);

        // get replaced value of limit to be able to resolve if it is argument or inside
        // static allocation of some address space
        Value *replacedVal = infoManager.getReplacedValue(base);
      
        dbgs() << "Getting limits of inst: "; inst->print(dbgs());
        dbgs() << " op: "; ptrOperand->print(dbgs()); dbgs() << "\n";
        dbgs() << "BASE: "; replacedVal->print(dbgs()); dbgs() << "\n";
/*
        // find out limits from mappings if it is an safepointer argument or
        AreaLimitBase *limit = getValueLimit(replacedVal);

        // replace old set
        if (limit) {
          limits.clear();
          limits.insert(limit);
        }
 */
      }
      return limits;
    }
    
  private:
    AddressSpaceInfoManager& infoManager;
    DependenceAnalyser& dependenceAnalyser;

    AreaLimitSetByAddressSpaceMap asAreaLimits;
    AreaLimitByValueMap valueAreaLimits;

    // does not exist
    void operator=(const AreaLimitManager&);
    
    // find which limits the value respects, value should be pretty straight forward to
    // trace until some argument or struct field
    AreaLimitBase* getValueLimit(Value* val) {
      fast_assert(false, "Needs implementation, this will finally return the AreaLimitBase object of value.");
      return NULL;
    }
    
  };

  // Function signatures (where needed)
  Function* transformSafeArguments( Function& F, ArgumentMap& argumentMapping );

  Function* createWebClKernel(Module &M, Function *origKernel, Function *smartKernel,
                              AddressSpaceInfoManager &infoManager);

  template <typename Location> Value* convertArgumentToSmartStruct(Value* origArg, Value* minLimit, Value* maxLimit, bool isIndirect, Location* location);

  bool isSafeAddressToLoad(Value *operand);

  void addChecks(Value *ptrOperand, Instruction *inst, AreaLimitByValueMap &valLimits, const AreaLimitSetByAddressSpaceMap &asLimits, ValueSet &safeExceptions);

  void createLimitCheck(Value *ptr, const AreaLimitSet &limits, Instruction *meminst);

  void convertCallToUseSmartPointerArgs(CallInst *call, Function *newFun,
                                        const ArgumentMap &replacedArguments, AreaLimitByValueMap &valLimits,
                                        bool useProgramAllocationsArgument,
                                        const AddressSpaceInfoManager& infoManager);

  Function* createNewFunctionSignature(Function *F,  
                                       std::insert_iterator<FunctionMap> functionMappingInserter,
                                       std::insert_iterator<ArgumentMap> argumentMappingInserter,
                                       Type* programAllocationsType);

  // Function implementations
  void collectBuiltinFunctions(Module& M, 
                               FunctionManager& functionManager) {
    // **Analyze all original functions.** Goes through all functions in module
    // and creates new function signature for them and collects information of instructions
    // that we will need in later transformations.
    // If function is intrinsic or WebCL builtin declaration (we know how it will behave) we
    // just skip it. If function is unknown external call compilation will fail.
    for( Module::iterator i = M.begin(); i != M.end(); ++i ) {

      if ( unsafeBuiltins.count(extractItaniumDemangledFunctionName(i->getName().str())) ) {
        if ( i->isDeclaration() && argsHasPointer(i->getArgumentList()) ) {
          functionManager.addUnsafeBuiltinFunction(i);
        } else if ( !i->isDeclaration() && argsHasSafePointer(i->getArgumentList()) ) {
          ArgumentMap replacedArguments;
          Function* newFunction = transformSafeArguments(*i, replacedArguments);
          functionManager.replaceArguments(replacedArguments);
          functionManager.replaceFunction(&*i, newFunction);
          functionManager.addSafeBuiltinFunction(newFunction);
        } else {
          // skip this case, just some other function
        }
        continue;          
      }
    }
  }

  /** Given a manually written safeptr C function returns a function
   * that follows the regular safe pointer calling conventions:
   * instead of passing a pointer to a three-pointers, pass a single
   * three-struct in a register.
   *
   * The actual code contents is not modified here but in
   * **moveOldFunctionImplementationsToNewSignatures**, which takes
   * into account that the function already uses smart pointers.
   *
   * Adjusted arguments are placed into the argumentMapping
   * associative container.
   */        
  Function* transformSafeArguments( Function& F,
                                    ArgumentMap& argumentMapping ) {
    LLVMContext& c = F.getContext();
    FunctionType *functionType = F.getFunctionType();
    TypeVector origTypes = std::vector<Type*>( functionType->param_begin(),
                                               functionType->param_end() );
    TypeVector newTypes;

    const llvm::AttrListPtr& origAttributes = F.getAttributes();
    llvm::AttrListPtr newAttributes;

    // Construct new attributes and new types. There may be fewer
    // arguments than in the original as three pointer arguments are
    // folded into one three-struct.
    {
      int newArgIdx = 1;
      int origArgIdx = 1;
      for ( Function::arg_iterator 
              origArgIt = F.arg_begin(),
              E = F.arg_end();
            origArgIt != E;
            ++origArgIt, ++newArgIdx, ++origArgIdx ) {
        llvm::Attributes attribs = origAttributes.getParamAttributes(origArgIdx);
        newAttributes = newAttributes.addAttr(c, newArgIdx, attribs);
        bool byval = attribs.hasAttribute(llvm::Attributes::ByVal);

        if ( !byval && isa<PointerType>(origArgIt->getType()) ) {
          PointerType* pt0 = dyn_cast<PointerType>(origArgIt->getType());
          ++origArgIt;
          fast_assert( origArgIt != E, "Insufficient arguments for a safe pointer, 3 required" );
          PointerType* pt1 = dyn_cast<PointerType>( origArgIt->getType() );
          ++origArgIt;
          fast_assert( origArgIt != E, "Insufficient arguments for a safe pointer, 3 required" );
          PointerType* pt2 = dyn_cast<PointerType>( origArgIt->getType() );
          fast_assert( pt0 == pt1, "Types 0 and 1 are not the same" );
          fast_assert( pt1 == pt2, "Types 1 and 2 are not the same" );
          newTypes.push_back( getSmartStructType( c, pt0 ) );
          origArgIdx += 2;
        } else {
          newTypes.push_back( origArgIt->getType() );
        }
      }
    }

    FunctionType *newFunctionType = FunctionType::get( functionType->getReturnType(), 
                                                       newTypes, 
                                                       false );

    Function *newFunction = Function::Create( newFunctionType, F.getLinkage() );
    newFunction->setCallingConv( F.getCallingConv() );
    newFunction->setAttributes( newAttributes );
    if ( F.hasGC() ) {
      newFunction->setGC( F.getGC() );
    }
    
    F.getParent()->getFunctionList().insert( &F, newFunction );
    newFunction->setName( F.getName() );

    for( Function::arg_iterator 
           origArgIt  = F.arg_begin(), 
           E          = F.arg_end(), 
           newArgIt   = newFunction->arg_begin();
         origArgIt != E;
         ++origArgIt, ++newArgIt ) {
      bool byval = origAttributes.getParamAttributes(origArgIt->getArgNo() + 1).hasAttribute(llvm::Attributes::ByVal);
      // remove attribute which does not make sense for non-pointer argument
      // getArgNo() starts from 0, but removeAttribute assumes them starting from 1 ( arg index 0 is the return value ).
      int argIdx = newArgIt->getArgNo()+1;
      newFunction->removeAttribute( argIdx, Attributes::get(c, genVector(llvm::Attributes::NoCapture)) );
      newFunction->removeAttribute( argIdx, Attributes::get(c, genVector(llvm::Attributes::ByVal)) );

      // it is a smart pointer: we can skip three of the original arguments
      if (!byval && isa<PointerType>(origArgIt->getType())) {
        newArgIt->setName( origArgIt->getName() + ".SmartArg" );
        ++origArgIt; assert(origArgIt != E);
        ++origArgIt; assert(origArgIt != E);
      } else {
        newArgIt->setName( origArgIt->getName() );
      }
      
      argumentMapping.insert( std::pair< Argument*, Argument* >( origArgIt, newArgIt ) ); 
    }

    return newFunction;
  }

  /**
   * Resolves uses of value and limits that it should respect
   *
   * Does also simple data dependency analysis to be able to
   * resolve limits which values should respect in case of same
   * address space has more than allocated 1 areas.
   *
   * Follows uses of val recursively. Also if val is stored and val is pointer type, 
   * follows uses of pointer operand of the store.
   *
   * TODO: needs more clear implementation
   *
   * @param indirection tells how many levels we have pointers, if we follow store 
   */
  void resolveUses(Value *val, DependenceAnalyser &dependenceAnalyser, int indirection = 1, int recursion_level = 0) {
      
    // check all uses of value until cannot trace anymore
    for( Value::use_iterator i = val->use_begin(); i != val->use_end(); ++i ) {
      Value *use = *i;
      int indirAfter = indirection;

      // ----- continue to next use if cannot be sure about the limits
      if ( dyn_cast<GetElementPtrInst>(use) ) {
        DEBUG( for (int i = 0; i < recursion_level; i++ ) dbgs() << "  "; );
        DEBUG(dbgs() << "Found GEP: "; use->print(dbgs());
              dbgs() << "  ## Preserving original limits KEEP ON TRACKING\n"; );
      
      } else if ( isa<LoadInst>(use) ) {
        DEBUG( for (int i = 0; i < recursion_level; i++ ) dbgs() << "  "; );
        DEBUG( dbgs() << "Found LOAD: "; use->print(dbgs()); dbgs() << "  ## follow if loading from indirect pointer\n"; );
        
        indirAfter--;
        // if we already loaded value no need to trace any more
        if (indirAfter == 0) continue;
        
      } else if ( StoreInst *store = dyn_cast<StoreInst>(use) ) {
        DEBUG( for (int i = 0; i < recursion_level; i++ ) dbgs() << "  "; );
        DEBUG( dbgs() << "Found STORE: "; use->print(dbgs()); dbgs() << "  ## If we are storing pointer, also pass VAL limits to destination address.\n" );
          
        // first check if use is actually in value operand and in that case set limits for destination pointer
        if (store->getValueOperand() == val) {
          // we don't really care if value is stored... only pointer stores are interesting
          if (val->getType()->isPointerTy()) {
            // adds also place where limit was set to be able to trace, which limit
            // is valid in which place
            indirAfter++;
            dependenceAnalyser.addDependency(indirAfter, store, store->getPointerOperand(), val);
            resolveUses(store->getPointerOperand(), dependenceAnalyser, indirAfter,
                        recursion_level + 1);
          }
        } else if (store->getPointerOperand() == val) {
          // TODO: should we trace also storePtrOperand uses in case if indirection > 1?
          //       maybe not because if there is pointer stored as value we should trace its
          //       ancestor, not uses...
        }
        
        continue;
          
      } else if ( CastInst* cast = dyn_cast<CastInst>(use) ) {
        // if cast is not from pointer to pointer in same address space, cannot resolve
        if (!cast->getType()->isPointerTy() ||
            dyn_cast<PointerType>(cast->getType())->getAddressSpace() != dyn_cast<PointerType>(val->getType())->getAddressSpace()) {
          DEBUG( for (int i = 0; i < recursion_level; i++ ) dbgs() << "  "; );
          DEBUG( dbgs() << "  ## Found cast that cannot preserve limits.\n" );
          continue;
        }
        DEBUG( for (int i = 0; i < recursion_level; i++ ) dbgs() << "  "; );
        DEBUG( dbgs() << "  ## Found valid pointer cast, keep on tracking.\n" );
          
      } else {
        // notify about unexpected cannot be resolved cases for debug
        DEBUG( for (int i = 0; i < recursion_level; i++ ) dbgs() << "  "; );
        DEBUG( dbgs() << "  #### Cannot resolve limit for: "; use->print(dbgs()); dbgs() << "\n");
        continue;
      }
        
      // limits of use are directly derived from value
      dependenceAnalyser.addDependency(indirAfter, dyn_cast<Instruction>(use), use, val);
      resolveUses(use, dependenceAnalyser, indirAfter, recursion_level + 1);
    }
  }

      
  /**
   * Traces from leafs to root if dependency if found then adds dependency to each step.
   */
  bool resolveAncestors(Value *val, DependenceAnalyser &dependenceAnalyser, int recursion_level = 0) {
    if (dependenceAnalyser.getDependency(val) != NULL ) return true;

    Value *next = NULL;
    DEBUG( for (int i = 0; i < recursion_level; i++ ) dbgs() << "  "; );
      
    if ( GetElementPtrInst *gep = dyn_cast<GetElementPtrInst>(val) ) {
      DEBUG( dbgs() << "Found GEP: "; val->print(dbgs()); dbgs() << " tracing to baseval.\n"; );
      next = gep->getPointerOperand();
    } else if ( LoadInst *load = dyn_cast<LoadInst>(val) ) {
      DEBUG( dbgs() << "Found LOAD: "; val->print(dbgs()); dbgs() << " tracing to memaddr.\n"; );
      next = load->getPointerOperand();
    } else if ( isa<StoreInst>(val) ) {
      DEBUG( dbgs() << "Found STORE: "; val->print(dbgs()); dbgs() << " cant be, store does not return value.\n" );
      fast_assert(false, "No way! I dont have any idea how code can reach this point.");
    } else if ( CastInst* cast = dyn_cast<CastInst>(val) ) {
      // if cast is not from pointer to pointer in same address space, cannot resolve
      if (!cast->getType()->isPointerTy() ||
          dyn_cast<PointerType>(cast->getType())->getAddressSpace() != dyn_cast<PointerType>(val->getType())->getAddressSpace()) {
        DEBUG( dbgs() << "  ## non pointer result or wrong address space.\n" );
        return false;
      } else {
        DEBUG( dbgs() << " tracing to src op.\n" );
        next = cast->getOperand(0);
      }
    } else if ( ConstantExpr *expr = dyn_cast<ConstantExpr>(val) ) {
          
      Instruction* inst = getAsInstruction(expr);
      if ( GetElementPtrInst *gep = dyn_cast<GetElementPtrInst>(inst) ) {
        DEBUG( dbgs() << "... constant GEP, following to baseval.\n"; );
        next = gep->getPointerOperand();
      } else {
        DEBUG( dbgs() << "... unhandled const expr, maybe could be supported if implemented\n"; );
      }
      delete inst;
    }
      
    if (next) {
      if (dependenceAnalyser.getDependency(next) != NULL) {
        dependenceAnalyser.addDependency(1, NULL, val, dependenceAnalyser.getDependency(next));
        return true;
      } else {
        if (resolveAncestors(next, dependenceAnalyser, recursion_level + 1)) {
          dependenceAnalyser.addDependency(1, NULL, val, dependenceAnalyser.getDependency(next));
          return true;
        }
      }
    }
    return false;
  }
  
  /** Returns 'true' if a constant is a simple one. Currently simple constants are null values, integers,
   * floats, or arrays, structs of expressions that are built of simpleConstants, but it could be anything
   * that doesn't depend on other values.
   */
  bool simpleConstant( Constant* value ) {
    if (ConstantExpr* expr = dyn_cast<ConstantExpr>(value)) {        
      SmallVector<Constant*,4> ConstantOperands;
      bool isConstant = true;
      for (ConstantExpr::op_iterator I = expr->op_begin(), E = expr->op_end(); isConstant && I != E; ++I) {
        if (!isa<Constant>(I)) {
          isConstant = false;
        } else {
          ConstantOperands.push_back(cast<Constant>(I));
        }
      }
      if (isConstant) {
        ArrayRef<Constant*> Ops(ConstantOperands);
        switch (expr->getOpcode()) {
        case Instruction::IntToPtr: {
          isConstant = simpleConstant(Ops[0]);
        } break;
        }
      }
      return isConstant;
    } else if (ConstantArray* ar = dyn_cast<ConstantArray>(value)) {
      bool isConstant = true;
      ArrayType* type = ar->getType();
      for (size_t c = 0; isConstant && c < type->getNumElements(); ++c) {
        isConstant = simpleConstant(ar->getAggregateElement(c));
      }
      return isConstant;
    } else if (ConstantDataSequential* seq = dyn_cast<ConstantDataSequential>(value)) {
      bool isConstant = true;
      for (size_t c = 0; isConstant && c < seq->getNumElements(); ++c) {
        isConstant = simpleConstant(seq->getElementAsConstant(c));
      }
      return isConstant;
    } else if (ConstantStruct* st = dyn_cast<ConstantStruct>(value)) {
      bool isConstant = true;
      StructType* type = st->getType();
      for (size_t c = 0; isConstant && c < type->getNumElements(); ++c) {
        isConstant = simpleConstant(st->getAggregateElement(c));
      }
      return isConstant;
    } else {
      return value->isNullValue() || isa<ConstantInt>(value) || isa<ConstantFP>(value);
    }
  }

  void scanKernelArguments( Module &M, AddressSpaceInfoManager &infoManager ) {
    NamedMDNode* oclKernels = M.getNamedMetadata("opencl.kernels");
    if (oclKernels != NULL) {
      for (unsigned int op = 0; op < oclKernels->getNumOperands(); op++) {
        MDNode* md = oclKernels->getOperand(op);
        DEBUG( dbgs() << "Scanning arguments of " << op << ": "; md->print(dbgs()); dbgs() << " -->\n" );
        Function* kernel = dyn_cast<Function>(md->getOperand(0));
          
        for( Function::arg_iterator a = kernel->arg_begin(); a != kernel->arg_end(); ++a ) {
          Argument* arg = a;
          Type* t = arg->getType();
          if ( t->isPointerTy() ) {
            infoManager.addDynamicLimitRange(arg);
          }
        }
      }
    }
  }

  /**
   * Collect all allocas and global values for each address space and create one struct for each
   * address space.
   */
  void scanStaticMemory(Module &M, AddressSpaceInfoManager &infoManager,
                        DependenceAnalyser &dependenceAnalyser) {
      
    LLVMContext& c = M.getContext();
      
    ValueVectorByAddressSpaceMap staticAllocations;
    // set of address spaces which we need to allocate from global scope
    UIntSet globalScopeAdressSpaces;
            
    for (Module::global_iterator g = M.global_begin(); g != M.global_end(); g++) {
      // collect only named linked addresses (for unnamed there cannot be relative references anywhere) externals are allowed only in special case.
      DEBUG( dbgs()  << "Found global: "; g->print(dbgs());
             dbgs() << " of address space: " << g->getType()->getAddressSpace() << "\n";  );

      fast_assert(!g->hasInitializer() || simpleConstant(g->getInitializer()),
                  "Unsupported: Globals cannot have complex initalizers");

      if ( g->hasUnnamedAddr() ) {
        DEBUG( dbgs() << " ### Ignored because unnamed address \n"; );
      } else if ( g->hasExternalLinkage() && g->isDeclaration() ) {
        DEBUG( dbgs() << " ### Ignored because extern linkage \n"; );
      } else {
        DEBUG( dbgs() << " ### Collected to address space structure " << g->getType()->getAddressSpace() << "\n"; );
        staticAllocations[g->getType()->getAddressSpace()].push_back(g);
        globalScopeAdressSpaces.insert(g->getType()->getAddressSpace());
      }
    }

    // all 'alloca's are considered private
    for (Module::iterator f = M.begin(); f != M.end(); f++) {
      // skip declarations (they does not even have entry blocks)
      // skip builtins
      if (f->isDeclaration() || unsafeBuiltins.count(extractItaniumDemangledFunctionName(f->getName().str()))) 
        continue;
      BasicBlock &entry = f->getEntryBlock();
      for (BasicBlock::iterator i = entry.begin(); i != entry.end(); i++) {
        AllocaInst *alloca = dyn_cast<AllocaInst>(i);
        if (alloca != NULL) {
          if ( dependenceAnalyser.hasOnlySafeAccesses(alloca) ) {
            DEBUG( dbgs() << "Skipping alloca from private address space beacuse it does not have any relative / indirect accesses:"; alloca->print(dbgs()); dbgs() << "\n"; );
          } else {
            DEBUG( dbgs() << "Collecting: "; alloca->print(dbgs()); dbgs() << "\n"; );
            staticAllocations[alloca->getType()->getAddressSpace()].push_back(alloca);
          }
        }
      }
    }
      
    // simple fix of alignment of mem intrinsics because some alignments might change when variables are moved to struct
    for (Module::iterator f = M.begin(); f != M.end(); f++) {
      if (f->isIntrinsic()) {
        if (f->getName().find("llvm.mem") == 0) {
          for ( Function::use_iterator use = f->use_begin(); use != f->use_end(); use++ ) {
            if ( CallInst *call = dyn_cast<CallInst>(*use) ) {
              // we can set alignment argument to 1 which is always valid argument, later optimization passes sets
              // alignment back to optimal value
              call->setOperand(3, getConstInt(c, 1));
              DEBUG( dbgs() << "After: "; call->print(dbgs()); dbgs() << "\n"; );
            };
          }
        }
      }
    }
      
    // create struct for each address space, currently not doing any special ordering
    for (ValueVectorByAddressSpaceMap::iterator i = staticAllocations.begin(); i != staticAllocations.end(); i++) {
      unsigned addressSpace = i->first;
      std::vector<Value*> &values = i->second;
      // TODO: sort types by alignment and size to minimize padding

      // create struct type
      std::vector< Type* > structElementTypes;
      std::vector< Constant* > structInitData;
      for (size_t valIndex = 0; valIndex < values.size(); valIndex++) {

        // element type 
        Value* val = values[valIndex];
        structElementTypes.push_back(dyn_cast<PointerType>(val->getType())->getElementType());
          
        // initializer
        Type* elementType = NULL;
        Constant* initializer = NULL;
        if ( AllocaInst* alloca = dyn_cast<AllocaInst>(val) ) {
          elementType = alloca->getType()->getElementType();
        } else if ( GlobalVariable* global = dyn_cast<GlobalVariable>(val) ) {
          elementType = global->getType()->getElementType();
          if (global->hasInitializer()) {
            initializer = global->getInitializer();
            // TODO: disable initializer removal for now (so it compiles)
            //global->setInitializer(NULL);
          }
        } else {
          dbgs() << "Got unexpected static allocation: "; val->print(dbgs()); dbgs() << "\n";
          fast_assert(false, "Unexpected type static allocation.");
        }

        if (!initializer) {
          if (elementType->isAggregateType()) {
            structInitData.push_back(ConstantAggregateZero::get(elementType));
          } else {
            structInitData.push_back(Constant::getNullValue(elementType));
          }
        } else {
          structInitData.push_back(initializer);
        }
      }

      // just add collected data to our info manager, which can later on create necessary requi
      infoManager.addAddressSpace(addressSpace, globalScopeAdressSpaces.count(addressSpace) > 0, values, structInitData);
    }
  }
       
  /**
   * Checks if given function declaration is one of webcl builtins
   * 
   * NOTE: This check and validity that only builtins are called can be implemented easier
   * by setting compiler to give an error if call to undefined function is made. All builtins
   * are defined already in some implicit kernel header.
   *
   * Since this is does not really matter here, return always true
   */
  bool isWebClBuiltin(Function *F) {
    return true;
  }

  /**
   * Goes through kernels metadata entries and creates webcl compliant kernel signature.
   *
   * If signature has no pointers, then do nothing, if there was pinter arguments, add 
   * count parameter after each pointer to tell how many elements pointer has. Take original
   * kernel name and add implementation, that just resolves the last address of array and passes
   * it as limit to safepointer version of original kernel.
   */
  void createKernelEntryPoints(Module &M, const FunctionMap &replacedFunctions,
                               AddressSpaceInfoManager &infoManager) {

    NamedMDNode* oclKernels = M.getNamedMetadata("opencl.kernels");
    if (oclKernels != NULL) {
      for (unsigned int op = 0; op < oclKernels->getNumOperands(); op++) {
        MDNode* md = oclKernels->getOperand(op);
        DEBUG( dbgs() << "Fixing entry point " << op << ": "; md->print(dbgs()); dbgs() << " -->\n" );
        Function* oldFun = dyn_cast<Function>(md->getOperand(0));
        Function *newKernelEntryFunction = NULL;
          
        // If there is need, create new kernel wrapper and replace old kernel reference with new WebCl
        // compatible version.
        if (replacedFunctions.count(oldFun) > 0) {
          Function *smartKernel = replacedFunctions.find(oldFun)->second;
          newKernelEntryFunction = createWebClKernel(M, oldFun, smartKernel, infoManager);
          // make smartKernel to be internal linkage to allow better optimization
          smartKernel->setLinkage(GlobalValue::InternalLinkage);
          // TODO: if found nvptx_kernel attribute, move it to new kernel
          md->replaceOperandWith(0, newKernelEntryFunction);
        }

        DEBUG( md->print(dbgs()); dbgs() << "\n" );
      }
    }
  }

  void createMainEntryPoint(Module& M,
                            const FunctionMap &replacedFunctions,
                            ValueSet& safeExceptions) {
    Function* main = 0;
    for (FunctionMap::const_iterator funIt = replacedFunctions.begin();
         !main && funIt != replacedFunctions.end();
         ++funIt) {
      if (funIt->first->getName() == "main") {
        main = funIt->second;
      }
    }
    if (main) {
      LLVMContext& c = M.getContext();
      IRBuilder<> blockBuilder( main->getEntryBlock().begin() );
    }
  }

  /**
   * Creates new WebCl kernel compliant function, which has element count parameter for each
   * pointer parameter and can be called from host.
   *
   * New function will be given name of the original kernel, but after each pointer parameter,
   * count parameter will be added which is used to pass information how many elements are
   * reserved in pointer. Function implementation will convert (pointer, count) to corresponding
   * smart pointer, which is used to make call to smartKernel.
   */
  Function* createWebClKernel(Module &M, Function *origKernel, Function *smartKernel,
                              AddressSpaceInfoManager &infoManager) {

    LLVMContext &c = M.getContext();

    // create argument list for WebCl kernel
    std::vector< Type* > paramTypes;
    for( Function::arg_iterator a = origKernel->arg_begin(); a != origKernel->arg_end(); ++a ) {
      Argument* arg = a;
      Type* t = arg->getType();
      paramTypes.push_back( t );
        
      if( t->isPointerTy() ) {
        Type* arraySizeType = Type::getInt32Ty(c);
        paramTypes.push_back( arraySizeType );          
      }
    }

    // creating new function with WebCl compatible arguments
    FunctionType *functionType = origKernel->getFunctionType();
    FunctionType *newFunctionType = FunctionType::get( functionType->getReturnType(), paramTypes, false );
    Function *webClKernel = dyn_cast<Function>( M.getOrInsertFunction("", newFunctionType) );
    webClKernel->takeName( origKernel );

    // create basic block and builder
    BasicBlock* kernelBlock = BasicBlock::Create( c, "entry", webClKernel );
    IRBuilder<> blockBuilder( kernelBlock );

    // TODO: tell address space info manager that it should generate the programAllocations structure and its init code here
    // NOTE: this also generates GlobalScope address space structures on demand (they are needed to pass some limits).
    Value* programAllocationsArgument = infoManager.generateProgramAllocationCode(webClKernel, blockBuilder);
      
    std::vector<Value*> args;
    args.push_back(programAllocationsArgument);

      
    //TODO: fix calling smart kernel.. probably one can ask limits or even safe pointer directly from manager... 
 
    Function::arg_iterator origArg = origKernel->arg_begin();
    for( Function::arg_iterator a = webClKernel->arg_begin(); a != webClKernel->arg_end(); ++a ) {
      Argument* arg = a;
      arg->setName(origArg->getName());
      Type* t = arg->getType();
        
      if ( t->isPointerTy() ) {
        AreaLimitSet limits = infoManager.getArgumentLimits(origArg);
        assert(limits.size() == 1);
        AreaLimitBase& limit = **limits.begin();

        Value* min;
        Value* max;
        DUMP(*webClKernel);
        limit.getBounds(webClKernel, blockBuilder, min, max);

        Value* elementCount = (++a);
        a->setName(origArg->getName() + ".size");
        GetElementPtrInst *lastLimit = dyn_cast<GetElementPtrInst>(blockBuilder.CreateGEP(arg, elementCount));

        blockBuilder.CreateStore(arg, min);
        blockBuilder.CreateStore(lastLimit, max);
          
        // create smart pointer alloca to entry block of function, which is used as a argument to
        // function call
        Value* newArgument = convertArgumentToSmartStruct( arg,  arg, lastLimit, false, kernelBlock);

        args.push_back(newArgument);

      } else {
        args.push_back(arg);
      }
      origArg++;
    }

    DEBUG( dbgs() << "\nCreated arguments: "; 
           for ( size_t i = 0; i < args.size(); i++ ) { 
             args[i]->getType()->print(dbgs()); dbgs() << " "; 
           } dbgs() << "\n"; ) ;
    DEBUG( dbgs() << "Function arguments: "; 
           for ( Function::arg_iterator a = smartKernel->arg_begin(); a != smartKernel->arg_end(); ++a ) { 
             a->getType()->print(dbgs()); dbgs() << " ";
           } dbgs() << "\n"; ) ;

    blockBuilder.CreateCall(smartKernel, args);
    blockBuilder.CreateRetVoid();

    DEBUG( webClKernel->print(dbgs()) );
    return webClKernel;
  }

  /**
   * Converts old argument to SafePointer with limits added.
   *
   * TODO: maybe we could generalize this and add some bookkeeping even that these really are 
   *       used only inside function scope. But Would be nice to say that update smart pointer
   *       which will store new values to argument struct just before function call.
   *
   *       Maybe it wont be needed and I can just skip messing with function signatures altogether..
   *
   * TODO: or just maybe we could create unnamed global variable and pass it to prevent polluting entry block too much
   */
  template <typename Location>
  Value* convertArgumentToSmartStruct(Value* origArg, Value* minLimit, Value* maxLimit, bool isIndirect, Location* location) {
    typedef LocationKind<Location> LK;

    DEBUG( dbgs() << (LK::initAtEnd ? "2" : "1") << "-Converting arg: "; origArg->print(dbgs());
           dbgs() << "\nmin: "; minLimit->print(dbgs());
           dbgs() << "\nmax: "; maxLimit->print(dbgs()); dbgs() << "\n"; );

    fast_assert(origArg->getType()->isPointerTy(), "Cannot pass non pointer as smart pointer.");
      
    // create alloca to entry block of function for the value
    Function* argFun = LK::getParent(location);
    BasicBlock &entryBlock = argFun->getEntryBlock();
    LLVMContext &c = argFun->getContext();
    Type *smarArgType = getSmartStructType(c, origArg->getType());
    AllocaInst *smartArgStructAlloca = new AllocaInst(smarArgType, origArg->getName() + ".SmartPassing", LK::AllocInstLocation(&entryBlock));
      
    // create temp smart pointer struct and initialize it with correct values
    GetElementPtrInst *curGEP = GetElementPtrInst::CreateInBounds(smartArgStructAlloca, genIntVector<Value*>(c, 0, 0), "", location);
    GetElementPtrInst *minGEP = GetElementPtrInst::CreateInBounds(smartArgStructAlloca, genIntVector<Value*>(c, 0, 1), "", location);
    GetElementPtrInst *maxGEP = GetElementPtrInst::CreateInBounds(smartArgStructAlloca, genIntVector<Value*>(c, 0, 2), "", location);
    Value *minValue = minLimit;
    Value *maxValue = maxLimit;
    // if indirect limit, we need to load value first
    if (isIndirect) {
      minValue = new LoadInst(minLimit, "", location);
      maxValue = new LoadInst(maxLimit, "", location);
    }
    CastInst *castedMinAddress = BitCastInst::CreatePointerCast(minValue, origArg->getType(), "", location);
    CastInst *castedMaxAddress = BitCastInst::CreatePointerCast(maxValue, origArg->getType(), "", location);
    new StoreInst(origArg, curGEP, location);
    new StoreInst(castedMinAddress, minGEP, location);
    new StoreInst(castedMaxAddress, maxGEP, location);
    LoadInst *smartArgVal = new LoadInst(smartArgStructAlloca, "", location);
    return smartArgVal;
  }

  /**
   * Paint all uses of argv of main function as safe ones, which does not require checks.
   *
   * NOTE: should never be called for opencl code
   */
  void resolveArgvUses(Value *val, ValueSet &safeExceptions) {
    for( Value::use_iterator i = val->use_begin(); i != val->use_end(); ++i ) {
      Value *use = *i;
        
      // ----- continue to next use if cannot be sure about if still safe
      if ( dyn_cast<GetElementPtrInst>(use) || dyn_cast<LoadInst>(use) ) {
        DEBUG( dbgs() << "Use: "; use->print(dbgs()); dbgs() << " is safe!\n" );
        safeExceptions.insert(use);
        resolveArgvUses(use, safeExceptions);                
      } else if ( StoreInst *store = dyn_cast<StoreInst>(use) ) {

        // dont care about store, but try to find it's destinations uses too
        if (safeExceptions.count(store->getPointerOperand()) == 0 && store->getPointerOperand()->getName() == "argv.addr") {
          DEBUG( dbgs() << "store has no uses, but follow its destination's uses: "; use->print(dbgs()); dbgs() << "\n" );
          DEBUG( dbgs() << "follow: "; store->getPointerOperand()->print(dbgs()); dbgs() << "\n" );
          safeExceptions.insert(store->getPointerOperand());
          resolveArgvUses(store->getPointerOperand(), safeExceptions);
        }
      } else { 
        // notify about unexpected cannot be resolved cases for debug
        DEBUG( dbgs() << "Cannot resolve if still safe for: "; use->print(dbgs()); dbgs() << "\n" );
        continue;
      }
    }
  }

  /**
   * Resolving from GEP if it is safe.
   * 
   * NOTE: really bad algorithm. find out proper analysis for this later. probably some analysis pass could be exploited.
   */
  bool isSafeGEP(GetElementPtrInst *gep) {
    DEBUG( dbgs() << "GEP: resolving limits.. "; );
    if (!gep->hasAllConstantIndices()) {
      DEBUG( dbgs() << "not constant indices\n"; );
      return false;
    }

    if (!gep->isInBounds()) {
      DEBUG( dbgs() << "not inbounds\n"; );
      return false;
    }

    // TODO: try validity of this check... naive case where one clearly overindexes types with constant indices
    if ( GlobalValue *baseVal = dyn_cast<GlobalValue>(gep->getPointerOperand()) ) {
      DEBUG( dbgs() << "hasExternalLinkage: " << baseVal->hasExternalLinkage() << "\n"; );
      return ( !(baseVal->hasExternalLinkage() && baseVal->isDeclaration()) || RunUnsafeMode);
    }
      
    // check recursively if safe based on safe value....
    if ( !isSafeAddressToLoad(gep->getPointerOperand()) ) {
      DEBUG( dbgs() << ".. unknown baseval type, some general resolving method would be nice"; );
      return false;
    }
      
    return true;
  }
    
  /** 
   * This might be possible to refactor with findAncestors...
   */
  bool isSafeAddressToLoad(Value *operand) {
    bool isSafe = false;
      
    DEBUG( dbgs() << "Checking if safe to access: "; operand->print(dbgs()); dbgs() << " ... "; );

    if ( ConstantExpr *constExpr = dyn_cast<ConstantExpr>(operand) ) {
        
      Instruction* inst = getAsInstruction(constExpr);
      if ( GetElementPtrInst *gep = dyn_cast<GetElementPtrInst>(inst) ) {
        isSafe = isSafeGEP(gep);
      } else {
        DEBUG( dbgs() << "... unhandled const expr, maybe could be supported if implemented"; );
      }
      delete inst;

    } else if ( isa<GlobalAlias>(operand) ) {
      DEBUG( dbgs() << "loading directly global alias.. "; );
      isSafe = true;      
    } else if ( isa<GlobalVariable>(operand) ) {
      DEBUG( dbgs() << "loading directly global variable .. "; );
      isSafe = true;
    } else if ( isa<AllocaInst>(operand) ) {
      DEBUG( dbgs() << "loading directly from alloca variable .. "; );
      isSafe = true;
    } else if ( isa<ConstantStruct>(operand) ) {
      DEBUG( dbgs() << "ConstantStruct value.. maybe if support implemented"; );
    } else if ( isa<ConstantVector>(operand) ) {
      DEBUG( dbgs() << "ConstantVector value.. maybe if support implemented"; );
    } else if ( isa<ConstantArray>(operand) ) {
      DEBUG( dbgs() << "ConstantArray value.. maybe if support implemented"; );
    } else if ( isa<ConstantDataSequential>(operand) ) {
      DEBUG( dbgs() << "ConstantDataSequential value.. maybe if support implemented"; );
    } else if ( GetElementPtrInst *gep = dyn_cast<GetElementPtrInst>(operand) ) {
      isSafe = isSafeGEP(gep);
    } else {
      DEBUG( dbgs() << "unhandled case"; );
    }
      
    DEBUG( dbgs() << "... returning: " << (isSafe ? "safe!" : "unsafe") << "\n"; );
    return isSafe;
  }
  
 
  /**
   * Adds boundary check for given pointer
   *
   * ==== Changes e.g.
   * 
   * %0 = load i32** %some_label
   * %1 = load i32* %0
   *
   * ==== To
   *
   *   %0 = load i32** %some_label
   *   ; this checks if value is direct or indirect and does required casting and gets last valid address for clamp
   *   %1 = AreaLimit.getMaxFor(%some_label)
   *   %2 = AreaLimit.getMinFor(%some_label)
   *   %3 = icmp ugt i32* %0, %1
   *   br i1 %3, label %boundary.check.fail, label %check.first.limit
   * check.first.limit:      
   *   %4 = icmp ult i32* %0, %2
   *   br i1 %4, label %boundary.check.fail, label %boundary.check.ok
   * boundary.check.ok:
   *   %5 = load i32* %0
   *   br %if.end
   * boundary.check.fail:
   *   br %if.end
   * if.end:
   *   %6 = phi i32* [ 0, %boundary.check.fail ], [ %5, %boundary.check.ok ]
   *
   * ==== for store instruction phi node is not generated (instruction is just skipped)
   * 
   * @param ptr Address whose limits are checked
   * @param limits Smart pointer, whose limits pointer should respect
   * @param meminst Instruction which for check is injected
   */
  void createLimitCheck(Value *ptr, const AreaLimitSet &limits, Instruction *meminst) {
      
    DEBUG( dbgs() << "Creating limit check for: "; ptr->print(dbgs()); dbgs() << " of type: "; ptr->getType()->print(dbgs()); dbgs() << "\n" );
    static int id = 0;
    id++;
    char postfix_buf[64];
    
    if ( dyn_cast<LoadInst>(meminst) ) {
      sprintf(postfix_buf, "load.%d", id);
    } else {
      sprintf(postfix_buf, "store.%d", id);
    }
    std::string postfix = postfix_buf;
      
    DEBUG( dbgs() << " Possible limits to check: \n" );
    for (AreaLimitSet::const_iterator i = limits.begin(); i != limits.end(); i++) {
      DEBUG( dbgs() << **i; );
    }
    fast_assert(limits.size() == 1, "Current boundary check generation does not support multiple limits checking.");
    AreaLimitBase *limit = *(limits.begin());
      
    BasicBlock *BB = meminst->getParent();
    Function *F = BB->getParent();
    LLVMContext& c = F->getContext();

    // ------ this block is destination of all places where limit check fails, needs unconditional just branch to if.end block
    BasicBlock* boundary_fail_block = BasicBlock::Create( c, "boundary.check.failed." + postfix, F );
    IRBuilder<> boundary_fail_builder( boundary_fail_block );

    // ------ block for minimum value check
    BasicBlock* check_first_block = BasicBlock::Create( c, "check.first.limit." + postfix, F );
    IRBuilder<> check_first_builder( check_first_block );

    // ------ get limits if require loading indirect address

    // *   %1 = instruction or value returning last valid value
    Value *last_value_for_type;
    // *   %2 = value to compare to get first valid address
    Value *first_valid_pointer;
    limit->validAddressBoundsFor(ptr->getType(), meminst, first_valid_pointer, last_value_for_type);

    // ------ add max boundary check code

    // get limits for this certain type of pointer... basically does "((ptrType)(&last_val[1]))[-1]"
    DEBUG( last_value_for_type->getType()->print(dbgs()); dbgs() << " VS. "; ptr->getType()->print(dbgs()); dbgs() << "\n" );

    // *   %3 = icmp ugt i32* %0, %1
    ICmpInst* cmp = new ICmpInst( meminst, CmpInst::ICMP_UGT, ptr, last_value_for_type, "" );
    // *   br i1 %3, label %boundary.check.failed, label %check.first.limit
    BranchInst::Create( boundary_fail_block, check_first_block, cmp, meminst );

    // ------ break current BB to 3 parts, start, boundary_check_ok and if_end (meminst is left in ok block)

    // ------ this block actually contains the load/store instruction and branch to if.end block
    // dbgs() << "Going to split:\n"; BB->print(dbgs()); dbgs() << "\nfrom: "; meminst->print(dbgs()); dbgs() << "\n";
    BasicBlock* boundary_ok_block = BB->splitBasicBlock(meminst, "boundary.check.ok." + postfix);

    // leave meminst to ok block and split it again to create if.end block
    BasicBlock* end_block =
      boundary_ok_block->splitBasicBlock(boundary_ok_block->begin()->getNextNode(),
                                         "if.end.boundary.check." + postfix);
      
    // erase implicitly added branch from start block to boundary.check.ok
    BB->back().eraseFromParent();

    // and add unconditional branch from boundary_fail_block to if.end 
    BranchInst::Create( end_block, boundary_fail_block );

    // ------ add min boundary check code
    // * check.first.limit:

    // *   %4 = icmp ult i32* %0, %2
    ICmpInst* cmp2 = new ICmpInst( *check_first_block, CmpInst::ICMP_ULT, ptr, first_valid_pointer, "" );

    // *   br i1 %4, label %boundary.check.failed, label %if.end
    BranchInst::Create( boundary_fail_block, boundary_ok_block, cmp2, check_first_block );

    // if meminst == load, create phi node to start of if.end block and replace all uses of meminst with this phi
    if ( dyn_cast<LoadInst>(meminst) ) {
      PHINode* newPhi = PHINode::Create(meminst->getType(), 2, "", &end_block->front());
      meminst->replaceAllUsesWith(newPhi);
      newPhi->addIncoming(meminst, boundary_ok_block);
      newPhi->addIncoming(Constant::getNullValue(meminst->getType()), boundary_fail_block);
    }

    // organize blocks to order shown in comment
    check_first_block->moveAfter(BB);
    boundary_ok_block->moveAfter(check_first_block);
    boundary_fail_block->moveAfter(boundary_ok_block);
    end_block->moveAfter(boundary_fail_block);

    DEBUG( dbgs() << "Created boundary check for: "; meminst->print(dbgs()); dbgs() << "\n"; );
  }

  /**
   * Goes through external function externalCalls and if call is unsafe opencl call convert it to safe webcl
   * implementation which operates with smart pointers
   */
  void makeBuiltinCallsSafe(const CallInstrSet &externalCalls, AreaLimitByValueMap &valLimits, const FunctionMap& unsafeToSafeBuiltin,
                            Type* programAllocationsType, const AddressSpaceInfoManager& infoManager ) {
    // if mapping is needed outside export this to be reference parameter instead of local 
    FunctionMap safeBuiltins;
    ArgumentMap dummyArgMap;

    for (CallInstrSet::const_iterator i = externalCalls.begin(); i != externalCalls.end(); i++) {
      CallInst *call = *i;

      DEBUG( dbgs() << "---- Checking builtin call:"; call->print(dbgs()); dbgs() << "\n" );
        
      Function* oldFun = call->getCalledFunction();

      FunctionMap::const_iterator unsafeToSafeBuiltinIt = unsafeToSafeBuiltin.find( oldFun );

      if ( unsafeToSafeBuiltinIt != unsafeToSafeBuiltin.end() ) {
        Function *newFun = unsafeToSafeBuiltinIt->second;
        ArgumentMap dummyArg;
        convertCallToUseSmartPointerArgs( call, newFun, dummyArg, valLimits, false, infoManager );
      } else if ( isWebClBuiltin(oldFun) ) {

        std::string demangledName = extractItaniumDemangledFunctionName(oldFun->getName().str());

        // if not supported yet assert
        fast_assert( unsupportedUnsafeBuiltins.count(demangledName) == 0, 
                     "Tried to call unsupported builtin: " + oldFun->getName() + " " + demangledName);
          
        // if unsafe fix call
        if ( unsafeBuiltins.count(demangledName) > 0 ) {
            
          // if safe version is not yet generated do it first..
          if ( safeBuiltins.count(oldFun) == 0 ) {
            Function *newFun = createNewFunctionSignature(oldFun,
                                                          std::inserter(safeBuiltins, safeBuiltins.begin()),
                                                          std::inserter(dummyArgMap, dummyArgMap.begin()),
                                                          programAllocationsType);
            // simple name mangler to be able to select, which implementation to call (couldn't find easy way to do Itanium C++ mangling here)
            // luckily the cases that needs mangling are pretty limited so we can keep it simple
            newFun->setName(customMangle(oldFun, demangledName + "__safe__"));
          }
            
          Function *newFun = safeBuiltins[oldFun];
          ArgumentMap dummyArg;
          convertCallToUseSmartPointerArgs(call, newFun, dummyArg, valLimits, false, infoManager);
        }

      } else {
        if ( RunUnsafeMode ) {
          dbgs() << "WARNING: Calling external function, which we cannot guarantee to be safe: "; 
          oldFun->print(dbgs());
          continue;
        } else {
          fast_assert(false, "Aborting since we are in strict mode.");
        }
      }
    }
  }
    
  /**
   * Goes through function calls and change call parameters to be suitable for new function signature.
   *
   * Also updates param.Cur value before making call to make sure that smart pointer has always the latest 
   * value stored.
   */
  void fixCallsToUseChangedSignatures(const FunctionMap &replacedFunctions, 
                                      const ArgumentMap &replacedArguments, 
                                      const CallInstrSet &internalCalls,
                                      AreaLimitByValueMap &valLimits,
                                      const AddressSpaceInfoManager& infoManager) {
    DUMP(iteratorDistance(internalCalls.begin(), internalCalls.end()));
    for (CallInstrSet::const_iterator i = internalCalls.begin(); i != internalCalls.end(); i++) {
      CallInst *call = *i;

      DEBUG( dbgs() << "---- Started fixing:"; call->print(dbgs()); dbgs() << "\n" );
        
      Function* oldFun = call->getCalledFunction();
        
      // if function was not replaced (didn't have pointer parameters)
      if (replacedFunctions.count(oldFun) == 0) {
        continue;
      }

      Function* newFun = replacedFunctions.find(oldFun)->second;
      convertCallToUseSmartPointerArgs(call, newFun, replacedArguments, valLimits, true, infoManager);
    }
  }

  Value* replaceCallArgument(CallInst* call, Value* operand, Value* oldArg, Value* newArg,
                             const ArgumentMap &replacedArguments, bool& removeAttribute, AreaLimitManager &areaLimitManager)
  {
    removeAttribute = false;

    Value* retArg = 0;

    // this argument type has been changed to smart pointer, find out corresponding smart
    if (oldArg->getType() != newArg->getType()) {
      //DEBUG( dbgs() << "- op #" << op << " needs fixing: "; operand->print(dbgs()); dbgs() << "\n" );

      AreaLimitSet limits = areaLimitManager.getAreaLimits(call, operand);
      fast_assert(limits.size() == 1, "Call operands have to be able to resolved to single limits.");
      AreaLimitBase *limit = *limits.begin();
      // TODO: what is currently official limit API?...
      // retArg = convertArgumentToSmartStruct(operand, limit->min, limit->max, limit->indirect, call);

  /* HOPEFULLY THIS IS NOW ENOUGH... add tricks from below if normal llvm optimizations cannot handle them
      retArg = convertArgumentToSmartStruct(operand, operand, operand, false, call);
      removeAttribute = true;
      
      
      if ( Argument* arg = dyn_cast<Argument>(operand) ) {
        // if operand is argument it should be found from replacement map
        DEBUG( dbgs() << "Operand is argument of the same func! Passing it through.\n"; );
        retArg = replacedArguments.find(arg)->second;
          
      } else if (ExtractValueInst *extract = dyn_cast<ExtractValueInst>(operand)) {
        // TODO: REMOVE THIS HACK IT OPENS SECURITY HOLE, ALWAYS GET LIMITS FROM RESULT OF ANALYSIS
        //       THIS WILL ALLOW UNSAFE CODE IF STRUCT IS GIVEN AS ARGUMENT AND THEN ONE ELEMENT OF
        //       IT IS PASSED TO OTHER FUNCTION
        Value* aggregateOp = extract->getAggregateOperand();
        DEBUG( dbgs() << "Operand is extractval of argument of the same func: "; aggregateOp->print(dbgs()); dbgs() << "\n"; );
        // TODO: to make this secure we have to check that operand argument is listed in replaced argument map and is really generated by us (types in replaced arguments must been changed)
        retArg = aggregateOp;
      } else {
        AreaLimitBase *limit = areaLimitManager.getAreaLimits(call, );
        // TODO: Temporarily disabled mechanism to allow running the app through for development purposes.
        // if (valLimits.count(operand) > 0) {
        //   limit = valLimits[operand];
        // } else {
        //   DEBUG( dbgs() << "In basic block: \n"; call->getParent()->print(dbgs()); dbgs() << "\nin call:\n"; call->print(dbgs()); dbgs() << "\nOperand:"; operand->print(dbgs()); dbgs() << "\n"; );
        //   fast_assert(false, "Could not resolve limits for a value passed as operand. Try to make code less obscure, write better limit analysis or do not change signature of this method at all and check against all limits of address space.");
        // }
        // retArg = convertArgumentToSmartStruct(operand, limit->min, limit->max, limit->indirect, call);
        retArg = convertArgumentToSmartStruct(operand, operand, operand, false, call);
        removeAttribute = true;
      }
*/
    } else {
      retArg = operand;
    }
    assert(retArg);

    return retArg;
  }
    
  /**
   * Converts call function to use new function as called function and changes all pointer parameters to be smart pointers.
   */
  void convertCallToUseSmartPointerArgs(CallInst *call, Function *newFun,
                                        const ArgumentMap &replacedArguments,
                                        AreaLimitManager &areaLimitManager,
                                        bool useProgramAllocationsArgument,
                                        AddressSpaceInfoManager& infoManager) {

    Function* oldFun = call->getCalledFunction();
    call->setCalledFunction(newFun);

    DEBUG( dbgs() << "-- Converted call from : "; call->print(dbgs()); dbgs() << "\n" );
      
    // find if function signature changed some Operands and change them to refer smart pointers 
    // instead of pointers directly
    Function::arg_iterator newArgIter = newFun->arg_begin();
    LLVMContext& c = newFun->getContext();
    std::vector<Value*> newCallArguments = std::vector<Value*>(call->value_op_begin(), call->value_op_end());
    newCallArguments.erase(newCallArguments.end() - 1, newCallArguments.end());
    if (useProgramAllocationsArgument) {
      // and insert the programAllocationsArgument to the front
      newCallArguments.insert(newCallArguments.begin(), infoManager.getProgramAllocations(*call->getParent()->getParent()));
      int op = 1;
      newArgIter = skipPaa(newArgIter);
      for( Function::arg_iterator a = oldFun->arg_begin(); a != oldFun->arg_end(); ++a ) {
        Argument* oldArg = a;
        Argument* newArg = newArgIter;
        newArgIter++;

        // NOTE: If we would first expand smart pointer map, we might be able to resolve smart pointer for parameter
        //       a lot easier... if there is need to add more and more special cases here, consider the option...

        bool removeAttribute;
        newCallArguments[op] = replaceCallArgument(call, call->getOperand(op - 1), oldArg, newArg,
                                                   replacedArguments, removeAttribute, areaLimitManager);
        // ignore for now: should copy attributes except for these ones?
        // if (removeAttribute) {
        //   int argIdx = a->getArgNo() + 1; // removeAttribute does know about arg# 0 (the return value), thus +1
        //   call->removeAttribute(argIdx, Attributes::get(c, genVector(llvm::Attributes::ByVal)));
        // }

        op++;
      }

      CallInst* newCall = CallInst::Create(newFun, newCallArguments, "", call);
      call->replaceAllUsesWith(newCall);
      call->removeFromParent();
      infoManager.addReplacement(call, newCall);
      call = newCall;
    } else {
      int op = 0;
      newArgIter = newArgIter;
      // pitää tehdä niin että newCallArguments (esim.) rakennetaan allaolevassa luupissa oikein ja sitten vasta
      //   lopuksi korvataan - jos korvataan? muuten menee smartpointer-kutsut kätöseen.
      for( Function::arg_iterator a = oldFun->arg_begin(); a != oldFun->arg_end(); ++a ) {
        Argument* oldArg = a;
        Argument* newArg = newArgIter;
        newArgIter++;
        
        // NOTE: If we would first expand smart pointer map, we might be able to resolve smart pointer for parameter
        //       a lot easier... if there is need to add more and more special cases here, consider the option...

        bool removeAttribute;
        call->setOperand(op, replaceCallArgument(call, call->getOperand(op), oldArg, newArg, replacedArguments, removeAttribute, areaLimitManager));
        if (removeAttribute) {
          int argIdx = a->getArgNo() + 1; // removeAttribute does know about arg# 0 (the return value), thus +1
          call->removeAttribute(argIdx, Attributes::get(c, genVector(llvm::Attributes::ByVal)));
        }

        op++;
      }
    }
      
    DEBUG( dbgs() << "-- Converted call to : "; call->print(dbgs()); dbgs() << "\n" );
  }
    
  /**
   * Goes through all replaced functions and their arguments.
   *
   * 1. Moves all basic blocks to new function
   * 2. For each argument if necessary adds exctractvalue instruction to get passed pointer value
   * 3. Replaces all uses of old function argument with extractvalue instruction or with new function argument if it was not pointer.
   */
  void moveOldFunctionImplementationsToNewSignatures(const FunctionMap &replacedFunctions, 
                                                     const ArgumentMap &replacedArguments,
                                                     const FunctionSet &safeBuiltinFunctions) {
            
    for (FunctionMap::const_iterator i = replacedFunctions.begin(); i != replacedFunctions.end(); i++) {
      // loop through arguments and if type has changed, then create label to access original arg 
      Function* oldFun = i->first;
      Function* newFun = i->second;
      bool isBuiltin = safeBuiltinFunctions.count(newFun);
        
      // move all instructions to new function
      newFun->getBasicBlockList().splice( newFun->begin(), oldFun->getBasicBlockList() );
      BasicBlock &entryBlock = newFun->getEntryBlock();
 
      DEBUG( dbgs() << "Moved BBs to " << newFun->getName() << "( .... ) and took the final function name.\n" );

      if ( isBuiltin ) {
        const llvm::AttrListPtr& oldAttributes = oldFun->getAttributes();
        // we need to do special operations to fold three safe arguments into one struct
        for( Function::arg_iterator
               oldArgIt = oldFun->arg_begin(),
               newArgIt = newFun->arg_begin();
             oldArgIt != oldFun->arg_end(); 
             ++oldArgIt, ++newArgIt ) {
          bool byval = oldAttributes.getParamAttributes(oldArgIt->getArgNo() + 1).hasAttribute(llvm::Attributes::ByVal);
          if ( !byval && isa<PointerType>(oldArgIt->getType()) ) {
            Argument*   argCur   = oldArgIt;
            ++oldArgIt; assert(oldArgIt != oldFun->arg_end());
            Argument*   argBegin = oldArgIt;
            ++oldArgIt; assert(oldArgIt != oldFun->arg_end());
            Argument*   argEnd   = oldArgIt;
            Argument*   oldArg  = replacedArguments.find(oldArgIt)->second;
            std::string name     = argCur->getName();

            BasicBlock::iterator instAt = entryBlock.begin();
            Instruction* exCur   = ExtractValueInst::Create(oldArg, genVector(0u), name + ".Cur", instAt);
            Instruction* exBegin = ExtractValueInst::Create(oldArg, genVector(1u), name + ".Begin", instAt);
            Instruction* exEnd   = ExtractValueInst::Create(oldArg, genVector(2u), name + ".End", instAt);

            argCur->replaceAllUsesWith(exCur);
            argBegin->replaceAllUsesWith(exBegin);
            argEnd->replaceAllUsesWith(exEnd);
          } else {
            oldArgIt->replaceAllUsesWith(replacedArguments.find(oldArgIt)->second);
          }
        }          
      } else {
        for( Function::arg_iterator a = oldFun->arg_begin(); a != oldFun->arg_end(); ++a ) {
          Argument* oldArg = a;
          Argument* newArg = replacedArguments.find(a)->second;

          DEBUG( dbgs() << "Fixing arg: "; oldArg->print(dbgs()); dbgs() << " : \n" );

          newArg->takeName(oldArg);
          oldArg->setName(newArg->getName() + ".orig");

          // non safe pointer argument... direct replace
          if (oldArg->getType() == newArg->getType()) {
            DEBUG( dbgs() << "type was not changed. Just replacing oldArg uses with newArg.\n" );
            // argument was not tampered, just replace uses to point the new function
            oldArg->replaceAllUsesWith(newArg);
          } else {
            // if argument types are not the same we need to find smart pointer that was generated for
            // argument and create initializations so that existing smart alloca will get correct values

            // argument types are not the same we need to get .Cur element of the passed safe pointer, which is being
            // used in function and replace all uses with that.

            DEBUG( dbgs() << "1 newArg: "; newArg->print(dbgs()); );
            Twine paramName = Twine("") + newArg->getName() + ".SmartArg";
            newArg->setName(paramName);

            // get value of passed smart_pointer.cur and replace all uses of original argument with it
            ExtractValueInst* newArgCur = ExtractValueInst::Create(newArg,
                                                                   genVector<unsigned int>(0),
                                                                   Twine("") + newArg->getName() + ".Cur",
                                                                   entryBlock.begin());

            // this potentially will not work if there is store to arg... probably that case is impossible to happen and smart pointer arguments are read-only
            DEBUG( dbgs() << "Replacing old arg: "; oldArg->getType()->print(dbgs()); dbgs() << " with: "; newArgCur->getType()->print(dbgs()); dbgs() << "\n"; );

            oldArg->replaceAllUsesWith(newArgCur);
          }
        } // -- end arguments for loop
      }  
    }

    // remove old functions
    for (FunctionMap::const_iterator i = replacedFunctions.begin(); i != replacedFunctions.end(); i++) {
      i->first->removeFromParent();
    }
  }
    
  /**
   * Creates new function signatures and mapping between original and new.
   *
   * Function does not modify function yet in any manner.
   *
   * If kernel function is seen, we should assert (@todo needs to be implemented when llvm 3.2 is ready),
   * until we are ready with implementing safe pointer construction from (int*, int) pairs.
   * 
   * If int main(int argc, char *argv[]) add to safe functions and safe arguments or assert because of those parameters for now.
   * 
   */
  Function* createNewFunctionSignature(Function *F,  
                                       std::insert_iterator<FunctionMap> functionMappingInserter,
                                       std::insert_iterator<ArgumentMap> argumentMappingInserter,
                                       Type* programAllocationsType) {
       
    LLVMContext& c = F->getContext();

    // currently returning pointer or array is not supported
    fast_assert( (!F->getFunctionType()->getReturnType()->isPointerTy()), 
                 "Handling function returning pointer is not implemented." );
    fast_assert( (!F->getFunctionType()->getReturnType()->isArrayTy()), 
                 "Handling function returning array type is not implemented." );
    fast_assert( (!F->isVarArg()), "Variable argument functions are not supported.");

    // TODO: check if needed and if necessary to mask from strict version
    // check if main or kernel and in that case do not change signature
    bool dontTouchArguments = false;
    if (RunUnsafeMode && F->getName() == "main") {
      dontTouchArguments = true;
    }
      
    // convert function signature to use pointer structs instead of direct pointers
    SafeArgTypes args(c, typesOfArgumentList(F->getArgumentList()), dontTouchArguments, programAllocationsType);
    TypeVector& param_types = args.argTypes;
    IntSet& safeTypeArgNos = args.safeArgNos; // argument numbers of safe parameters we have generated; used later for deciding when to not remove ByVal attribute

    // creating new function with different prototype 
    FunctionType *function_type = F->getFunctionType();
    FunctionType *new_function_type = FunctionType::get( function_type->getReturnType(), param_types, false );

    Function *new_function = Function::Create( new_function_type, F->getLinkage() );
    new_function->copyAttributesFrom( F );
    new_function->arg_begin()->setName("ProgramAllocations");
 
    F->getParent()->getFunctionList().insert( F, new_function );
    new_function->setName( F->getName() + "__smart_ptrs__" );


    // add new function to book keepig to show what was replaced
    *functionMappingInserter++ = std::make_pair(F, new_function);

    DEBUG( dbgs() << "-- Created new signature for: " << F->getName() << " "; F->getType()->print(dbgs()) );
    DEBUG( dbgs() << "\nnew signature: " << new_function->getName() << " "; new_function->getType()->print(dbgs()); dbgs() << "\n" );

    Function::arg_iterator a_new = skipPaa(new_function->arg_begin());

    // map arguments of original function to new replacements
    for( Function::arg_iterator 
           a = F->arg_begin(), 
           E = F->arg_end();
         a != E;
         ++a, ++a_new ) {             
      // remove attribute which does not make sense for non-pointer argument
      // getArgNo() starts from 0, but removeAttribute assumes them starting from 1 ( arg index 0 is the return value ).
      int argIdx = a_new->getArgNo()+1;
      new_function->removeAttribute(argIdx, Attributes::get(c, genVector(llvm::Attributes::NoCapture)));
      if (safeTypeArgNos.count(a_new->getArgNo())) {
        new_function->removeAttribute(argIdx, Attributes::get(c, genVector(llvm::Attributes::ByVal)));
      }
        
      *argumentMappingInserter++ = std::make_pair(a, a_new);
      DEBUG( dbgs() << "Mapped orig arg: "; a->print(dbgs()); dbgs() << " -----> "; a_new->print(dbgs()); dbgs() << "\n" );
        
    }
    DEBUG( dbgs() << "\nNew signature: "; new_function->print(dbgs()); dbgs() << "\n" );

    return new_function;
  }

  void sortInstructions( Function *F, 
                         FunctionManager& functionManager ) {
      
    DEBUG( dbgs() << "-- Finding interesting instructions from: " << F->getName() << "\n" );

    // find all instructions which should be handled afterwards
    for ( Function::iterator bb = F->begin(); bb != F->end(); bb++) {
      for( BasicBlock::iterator i = bb->begin(); i != bb->end(); ++i ) {      
        Instruction &inst = *i;
       
        if ( CallInst *call = dyn_cast< CallInst >(&inst) ) {
          if (!call->getCalledFunction()->isIntrinsic()) {
            if (call->getCalledFunction()->isDeclaration()) {
              functionManager.addExternalCall(call);
              DEBUG( dbgs() << "Found external call: " );
            } else {
              functionManager.addInternalCall(call);
              DEBUG( dbgs() << "Found internal call: " );
            }
            DEBUG( call->print(dbgs()); dbgs() << "\n" );
          } else {
            DEBUG( dbgs() << "Ignored call to intrinsic\n" );
          }

        } else if ( AllocaInst *alloca = dyn_cast< AllocaInst >(&inst) ) {
            
          // TODO: check if alloca is from smart pointer argument. 
          // ( for these we should no do traditional smart pointer initialization, 
          //  but initialize them from sp read from argument )
            
          functionManager.addAlloca(alloca);
          DEBUG( dbgs() << "Found alloca: "; alloca->print(dbgs()); dbgs() << "\n" );
            
        } else if ( StoreInst *store = dyn_cast< StoreInst >(&inst) ) {
            
          if (dyn_cast<Argument>(store->getValueOperand())) {
            DEBUG( dbgs() << "Skipping store which reads function argument: "; store->print(dbgs()); dbgs() << "\n" );
            continue;
          } 
            
          functionManager.addStore(store);
          DEBUG( dbgs() << "Found store: "; store->print(dbgs()); dbgs() << "\n" );
            
        } else if ( LoadInst *load = dyn_cast< LoadInst >(&inst) ) {            
            
          functionManager.addLoad(load);
          DEBUG( dbgs() << "Found load: "; load->print(dbgs()); dbgs() << "\n" );

        } else if ( dyn_cast<FenceInst>(&inst) || 
                    dyn_cast<VAArgInst>(&inst) ||  
                    dyn_cast<AtomicRMWInst>(&inst) || 
                    dyn_cast<AtomicCmpXchgInst>(&inst) ) {
            
          DEBUG( dbgs() << "Unsafe instruction: "; inst.print(dbgs()); dbgs() << "\n" );
          fast_assert(false, "Instruction is not supported.");
        } 
      }
    }
  }
    
  // ## LLVM Module pass
  struct ClampPointers :
    public ModulePass {
    static char ID;

    ClampPointers() :
      ModulePass( ID ) {
    }
      
#if 0
    virtual void getAnalysisUsage(AnalysisUsage &AU) const {
      // check lib/Analysis/MemDepPrinter.cpp how to use memdap anaylsis
      AU.addRequiredTransitive<AliasAnalysis>();
      AU.addRequiredTransitive<MemoryDependenceAnalysis>();
      AU.setPreservesAll();
      dbgs() << "Analysis usage was actually called.\n";
    }
#endif
      
    // ## <a id="runOnModule"></a> Run On Module
    //
    // This function does the top-level algorithm for instrumentation.
    //
    // 1. Collect static memory allocations from the module and combine them to contiguous area.
    // 2. Collects information of original instructions which are not created by the pass.
    // 3. Create new function signatures and fix calls to use new signatures, which passes also limits for pointers.
    // 4. Analyzes original code to find limits for all load/store/call operand should respect.
    // 5. Analyzes original code and resolves if memory access limits can be verified compile time.
    // 6. Add boundary checks to loads/stores if instruction was not proved to be valid in compile time.
    // 7. Fix calls to unsafe builtin functions to call safe versions instead.
    virtual bool runOnModule( Module &M ) {
      FunctionManager functionManager(M);
      
      ValueSet       resolveLimitsOperands;

      // Bookkeeping of which limits certain value respects.
      AreaLimitByValueMap valueLimits;
      // Bookkeeping of all available limits of address spaces.
      AreaLimitSetByAddressSpaceMap addressSpaceLimits;
      AddressSpaceStructByAddressSpaceMap addressSpaceStructs;
      GlobalValueMap addressSpaceEndPtrs;

      // Set where is collected all values, which will not require boundary checks to
      // memory accesses. These have been resolved to be safe accesses in compile time.
      ValueSet safeExceptions;

      // Collect all allocas and global variables for each address space to struct to be able to resolve
      // static area reference limits easily. See example in [consolidateStaticMemory( Module &M
      // )](#consolidateStaticMemory).

      // Allocations of private memory are of special interest (and they (among others) are stored to
      // addressSpaceStructs along; accesses to them are put into safeExceptions. Later on addressSpaceStructs
      // is used by createWebClKernel to put the required allocations of the local memory regions to the front
      // of the new kernels.
      AddressSpaceInfoManager addressSpaceInfoManager(M);
      DependenceAnalyser dependenceAnalyser;
      AreaLimitManager areaLimitManager(addressSpaceInfoManager, dependenceAnalyser);
      
      // Do the rest of the analysis to be able to resolve all places where we have to do limit
      // checks and where to find limits for it
      // if can be traced to some argument or to some alloca or if we can trace it to single address space
      DEBUG( dbgs() << "\n ---- ANALYZE AND COLLECT INFORMATION ABOUT DEPENDENCIES ------\n" );
      collectDependencyInfo( M, dependenceAnalyser, functionManager );
      dependenceAnalyser.resolveLimitsForAllChecks();
      
      DEBUG( dbgs() << "\n --------------- COLLECT INFORMATION OF STATIC MEMORY ALLOCATIONS --------------\n" );
      scanStaticMemory( M, addressSpaceInfoManager, dependenceAnalyser );

      // Collect rest of the info about address space limits from kernel function arguments
      DEBUG( dbgs() << "\n --------------- COLLECT LIMITS FROM KERNEL ARGUMENTS --------------\n" );
      scanKernelArguments( M, addressSpaceInfoManager );
      
      collectBuiltinFunctions(M, functionManager);

      Type* programAllocationsType = addressSpaceInfoManager.getProgramAllocationsType();

      // **Analyze all original functions.** Goes through all functions in module
      // and creates new function signature for them and collects information of instructions
      // that we will need in later transformations.
      // If function is intrinsic or WebCL builtin declaration (we know how it will behave) we
      // just skip it. If function is unknown external call compilation will fail.
      for( Module::iterator F = M.begin(); F != M.end(); ++F ) {

        if ( unsafeBuiltins.count(extractItaniumDemangledFunctionName(F->getName().str())) ) {
          continue;
        }

        if ( F->isIntrinsic() || F->isDeclaration() ) {
          if (RunUnsafeMode) {
            DEBUG( dbgs() << "Skipping: " << F->getName() << " which is intrinsic and/or declaration\n" );
            continue;
          }
          
          if (!isWebClBuiltin(F)) {
            dbgs() << "Found: " << F->getName() << " which is intrinsic and/or declaration\n";
            fast_assert(false, "Calling external functions is not allowed in strict mode. "
                        "Also intrinsics should be lowered before runnin pass.");
          } else {
            DEBUG( dbgs() << "Recognized builtin: "; F->print(dbgs()); );
            continue;
          }
        }

        /* DISABLED MemoryDependenceAnalysis tests since couldn't get any useful info from it
        if (!F->isIntrinsic()) {
          DEBUG( dbgs() << "\n --------------- PRINTING OUT SOME DEBUG FROM MEMDEP ANALYSIS --------------\n" );
          // this should be wrapped inside DependenceAnalyser
          AliasAnalysis *AA = &getAnalysis<AliasAnalysis>(*F);
          MemoryDependenceAnalysis *MD = &getAnalysis<MemoryDependenceAnalysis>(*F);
          for ( Function::iterator bb = F->begin(); bb != F->end(); bb++) {
            for( BasicBlock::iterator i = bb->begin(); i != bb->end(); ++i ) {
              Instruction &inst = *i;
              
              if (inst.mayReadFromMemory() || inst.mayWriteToMemory()) {
              
                const MemDepResult &result = MD->getDependency(&inst);
                dbgs() << "DEP FOR: "; i->print(dbgs()); dbgs() << "\n";
                
                dbgs() << " isClobber: " << result.isClobber()
                << " isDef: " << result.isDef()
                << " isNonLocal: " << result.isNonLocal()
                << " isNonFuncLocal: " << result.isNonFuncLocal()
                << " isUnknown: " << result.isUnknown();
                
                dbgs() << " getInst(): "; result.getInst()->print(dbgs()); dbgs() << "\n";
              }
            }
          }
        }
        */
        
        // Creates new signatures for internal functions in the program and adds mapping between
        // old and new functions. Also creates mapping between old and new function arguments.
        // [Function* createNewFunctionSignature(Function *F, FunctionMap &functionMapping,
        // ArgumentMap &argumentMapping )](#createNewFunctionSignature).
        DEBUG( dbgs() << "\n --------------- CREATING NEW FUNCTION SIGNATURE --------------\n" );
        createNewFunctionSignature( F, functionManager.replaceFunctionInserter(), functionManager.replaceArgumentInserter(),
                                    programAllocationsType );
      }

      // **End of analyze phase.** After this `replacedFunctions`, `replacedArguments`, `internalCalls`, `externalCalls`,
      // `allCalls`, `allocas`, `stores`, `loads` and `resolveLimitsOperands` should not be changed, but only used for lookup.
      
      // Moves function instructions / basic blocks from original functions to new ones and fixes uses of original function arguments
      // to point new arguments. After this function behavior should be back to original, except if function has call to another
      // function whose signature was changed. [moveOldFunctionImplementationsToNewSignatures(FunctionMap &replacedFunctions,
      // ArgumentMap &replacedArguments)](#moveOldFunctionImplementationsToNewSignatures).
      // Manually written safe implementations of unsafe builtin functions are handled slightly differently, so a list of them
      // is passed as an argument.
      DEBUG( dbgs() << "\n ----------- CONVERTING OLD FUNCTIONS TO NEW ONES AND FIXING SMART POINTER ARGUMENT PASSING  ----------\n" );
      moveOldFunctionImplementationsToNewSignatures(functionManager.getReplacedFunctions(), 
                                                    functionManager.getReplacedArguments(),
                                                    functionManager.getSafeBuiltinFunctions());
      
      // Finds out kernel functions from Module metadata and creates WebCL kernels from them. `kernel void
      // foo(global float *bar)` -> `kernel void foo(global float *bar, size_t bar_size)`. Also calculates and
      // creates run-time limits for passed kernel arguments and adds limits to `addressSpaceLimits`
      // bookkeeping and calls original kernel implementation which has been changed earlier to use safe
      // pointer arguments. [createKernelEntryPoints(Module &M, FunctionMap &replacedFunctions,
      // AreaLimitSetByAddressSpaceMap &asLimits)](#createKernelEntryPoints). addressSpaceStructs is used for
      // putting the allocations of private memory structs to the beginning of the kernels.
      DEBUG( dbgs() << "\n --------------- CREATE KERNEL ENTRY POINTS AND GET ADDITIONAL LIMITS FROM KERNEL ARGUMENTS --------------\n" );
      createKernelEntryPoints(M, functionManager.getReplacedFunctions(), addressSpaceInfoManager );

      // The same but for only 'main' functions; currently only handles the allocation of private structs
      if (RunUnsafeMode) {
        createMainEntryPoint(M, functionManager.getReplacedFunctions(), safeExceptions);
      }

      
      // fix all old alloca and globals uses to point new variables (required to be able to get limits correctly for call replacement ?)
      DEBUG( dbgs() << "\n --------------- FIX REFRENCES OF OLD ALLOCAS AND GLOBALS TO POINT ADDRESS SPACE STRUCT FIELDS --------------\n" );
      addressSpaceInfoManager.addAReplacementsToBookkeepping(functionManager);
      addressSpaceInfoManager.replaceUsesOfOriginalVariables();
      
      
      // Fixes all call instructions in the program to call new safe implementations so that program is again in functional state.
      // [fixCallsToUseChangedSignatures(...)](#fixCallsToUseChangedSignatures)
      DEBUG( dbgs() << "\n --------------- FIX CALLS TO USE NEW SIGNATURES --------------\n" );
      fixCallsToUseChangedSignatures(functionManager.getReplacedFunctions(),
                                     functionManager.getReplacedArguments(), 
                                     functionManager.getInternalCalls(), valueLimits,
                                     addressSpaceInfoManager);

      
      // Goes through all memory accesses and creates instrumentation to prevent any invalid accesses. NOTE: if opecl frontend actually
      // creates some memory intrinsics we might need to take care of checking their operands as well.
      // [addBoundaryChecks( ... )](#addBoundaryChecks)
      DEBUG( dbgs() << "\n --------------- ADDING BOUNDARY CHECKS --------------\n" );
      // TODO: wrap to addBoundaryChecks function....
      for (InstrSet::iterator inst = dependenceAnalyser.needCheck().begin(); inst != dependenceAnalyser.needCheck().end(); inst++) {
        Value *ptrOperand = NULL;
        if (LoadInst *load = dyn_cast<LoadInst>(*inst)) {
          ptrOperand = load->getPointerOperand();
        } else if (StoreInst *store = dyn_cast<StoreInst>(*inst)) {
          ptrOperand = store->getPointerOperand();
        } else {
          fast_assert(false, "Can add check only for load or store");
        }
        
        areaLimitManager.getAreaLimits(*inst, ptrOperand);
        // createLimitCheck(ptrOperand, areaLimitManager.getAreaLimits(*inst, ptrOperand), *inst);
      }

      // Goes through all builtin WebCL calls and if they are unsafe (has pointer arguments), converts instruction to call safe
      // version of it instead. Value limits are required to be able to resolve which limit to pass to safe builtin call.
      // [makeBuiltinCallsSafe( ... )](#makeBuiltinCallsSafe)
      DEBUG( dbgs() << "\n --------------- FIX BUILTIN CALLS TO CALL SAFE VERSIONS IF NECESSARY --------------\n" );
      makeBuiltinCallsSafe(functionManager.getExternalCalls(), valueLimits, functionManager.getUnsafeToSafeBuiltin(),
                           programAllocationsType, addressSpaceInfoManager);

      // Helps to print out resulted LLVM IR code if pass fails before writing results
      // on pass output validation
      dbgs() << "\n --------------- FINAL OUTPUT --------------\n";
      M.print(dbgs(), NULL);
      dbgs() << "\n --------------- FINAL OUTPUT END --------------\n";
      return true;
    }
      
    void collectDependencyInfo( Module &M, DependenceAnalyser &dependenceAnalyser, FunctionManager &functionManager ) {
      ValueSet resolveLimitsOperands;
      
      // add each global value and alloca to be final dependency for them selves
      for ( Module::global_iterator gv = M.global_begin(); gv != M.global_end(); ++gv ) {
        GlobalVariable &global = *gv;
        dependenceAnalyser.addDependency(1, NULL, &global, &global);
        resolveUses(&global, dependenceAnalyser);
      }

      for ( Module::iterator F = M.begin(); F != M.end(); ++F) {

        if ( F->isIntrinsic() || F->isDeclaration() ) {
            continue;
        }
        
        // Runs through all instructions in function and collects instructions.
        // [sortInstructions( Function *F, ... ) ](#sortInstructions).

        DEBUG( dbgs() << "\n --------------- FINDING INTERESTING INSTRUCTIONS --------------\n" );
        sortInstructions( F,  functionManager );

        // each alloca is final dep
        const AllocaInstrSet& allocs = functionManager.getAllocas();
        for (AllocaInstrSet::iterator i = allocs.begin(); i!= allocs.end(); i++) {
          AllocaInst *alloca = *i;
          dependenceAnalyser.addDependency(1, alloca, alloca, alloca);
        }
        
        const LoadInstrSet& loads = functionManager.getLoads();
        for (LoadInstrSet::iterator i = loads.begin(); i!= loads.end(); i++) {
          LoadInst *load = *i;
          dependenceAnalyser.analyseOperands(load);
          resolveLimitsOperands.insert(load->getPointerOperand());
        }
        
        const StoreInstrSet& stores = functionManager.getStores();
        for (StoreInstrSet::iterator i = stores.begin(); i != stores.end(); i++) {
          StoreInst *store = *i;
          dependenceAnalyser.analyseOperands(store);
          resolveLimitsOperands.insert(store->getPointerOperand());
        }
        
        const CallInstrSet& allCalls = functionManager.getAllCalls();
        for (CallInstrSet::iterator i = allCalls.begin(); i != allCalls.end(); i++) {
          CallInst *call = *i;
          dependenceAnalyser.analyseOperands(call);
          for (size_t op = 0; op < call->getNumOperands(); op++) {
            Value *operand = call->getOperand(op);
            /* ignore function pointers operands (not allowed in opencl)... no need to check them, but add all other pointer operands */
            if ( operand->getType()->isPointerTy() && !operand->getType()->getPointerElementType()->isFunctionTy() ) {
              resolveLimitsOperands.insert(operand);
            }
          }
        }
        
        // go through function arguments and trace all uses of them and add
        // information for each instruction which argument limits they ultimately respects
        for( Function::arg_iterator a = F->arg_begin(); a != F->arg_end(); ++a ) {
          Argument &arg = *a;
          
          // arg does respect its own limits
          dependenceAnalyser.addDependency(1, NULL, &arg, &arg);
          // if pointer argument, trace uses
          if ( arg.getType()->isPointerTy() ) {
            resolveUses(&arg, dependenceAnalyser);
          }
        }

        DEBUG( dbgs() << "----- Tracing call/load/store operands: \n"; );
        for (ValueSet::iterator limitOperand = resolveLimitsOperands.begin();
             limitOperand != resolveLimitsOperands.end() ; limitOperand++) {

          Value* val = *limitOperand;
          DEBUG( dbgs() << "Tracing dependency for: "; val->print(dbgs()); dbgs() << "\n"; );

          if ( resolveAncestors(val, dependenceAnalyser) ) {
            DEBUG( dbgs() << "Traced limits successful!\n"; );
            fast_assert( dependenceAnalyser.getDependency(val) != NULL,
                         "Got true from resolve. Obviously limits should have been added to set.");
          } else {
            DEBUG( dbgs() << "Could not trace the dependency!\n"; );
          }
        }
        
        DEBUG( dbgs() << "\n --------------- Collect list of instructions to check --------------\n" );
        ValueSet safeExceptions;
        for (ValueSet::iterator limitOperand = resolveLimitsOperands.begin();
             limitOperand != resolveLimitsOperands.end() ; limitOperand++) {
          Value *operand = *limitOperand;
          if (isSafeAddressToLoad(operand)) {
            safeExceptions.insert(operand);
          }
        }
        
        if (RunUnsafeMode) {
          if (F->getName() == "main") {
            for( Function::arg_iterator a = F->arg_begin(); a != F->arg_end(); ++a ) {
              Argument* arg = a;
              if (arg->getName() == "argv") {
                resolveArgvUses(arg, safeExceptions);
              }
            }
          }
        }

        // finally create set of required checks
        for (LoadInstrSet::iterator i = loads.begin(); i!= loads.end(); i++) {
          LoadInst *load = *i;
          if ( safeExceptions.count(load->getPointerOperand()) == 0) {
            dependenceAnalyser.addCheck(load);
          }
        }
        for (StoreInstrSet::iterator i = stores.begin(); i != stores.end(); i++) {
          StoreInst *store = *i;
          if ( safeExceptions.count(store->getPointerOperand()) == 0) {
            dependenceAnalyser.addCheck(store);
          }
        }
                
      }
    }

  };
}
  
char WebCL::ClampPointers::ID = 0;
static RegisterPass<WebCL::ClampPointers> 
X("clamp-pointers", "Adds dynamic checks to prevent accessing memory outside of allocated area.", 
  false, false);

