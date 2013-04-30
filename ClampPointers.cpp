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

#include <vector>
#include <map>
#include <set>
#include <iostream>
#include <sstream>
#include <cstdio>

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
    DEBUG( dbgs() << "Demangled: " << name << " to " << retVal << "\n" );
    return retVal;
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

  std::string unsafeBuiltins_tmp[] = {
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

  // ## LLVM Module pass
  struct ClampPointers :
    public ModulePass {
    static char ID;

    ClampPointers() :
      ModulePass( ID ) {
    }

    typedef std::vector< Type* > TypeVector;

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
                    bool dontTouchArguments ) {
        
        int argNo = 0;
        for( TypeVector::const_iterator typeIt = types.begin(); typeIt != types.end(); ++typeIt, ++argNo ) {
          Type* t = *typeIt;

          // TODO: assert not supported arguments (e.g. some int**, struct etc... or at least verify cases we can allow)
        
          if( !dontTouchArguments && t->isPointerTy() ) {
            Type* smart_array_struct = getSmartStructType( c, t );
            argTypes.push_back( smart_array_struct );
            safeArgNos.insert(argNo);
          } else {
            fast_assert( (!t->isArrayTy()), "Passing array in arguments is not implemented." );
            argTypes.push_back( t );
          }
        }
      }
    };
    
    /** builds a TypeVector out of function arguments; useful for
     * dealing with safeArgTypes */
    static TypeVector typesOfArgumentList( llvm::Function::ArgumentListType& args )
    {
      TypeVector v;
      for ( llvm::Function::ArgumentListType::iterator it = args.begin();
            it != args.end();
            ++it ) {
        v.push_back(it->getType());
      }
      return v;
    }

    // **AreaLimit** class holds information of single memory area allocation. Limits of the area
    // can be stored directly as constant expressions for *min* and *max* or they can be indirect
    // references to the limits. In case of indirect memory area, the *min* and *max* contains memory
    // addresses where limit addresses are stored.
    struct AreaLimit {
    private:
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
        
        LLVMContext &c = checkStart->getParent()->getParent()->getContext();
        
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
        
        return ret_limit;
      }

    public:
      Value* min; // Contains first valid address
      Value* max; // Contains last valid address
      bool indirect; // true if min and max are indirect pointers (requires load for getting address)

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
      Value* firstValidAddressFor(Type *type, Instruction *checkStart) {
        return getValidAddressFor(min, indirect, 0, type, checkStart);
      }

      Value* lastValidAddressFor(Type *type, Instruction *checkStart) {
        return getValidAddressFor(max, indirect, -1, type, checkStart);
      }

    };
    
    typedef std::map< Function*, Function* > FunctionMap;
    typedef std::map< Argument*, Argument* > ArgumentMap;
    typedef std::set< Function* > FunctionSet;
    typedef std::set< Argument* > ArgumentSet;
    typedef std::set< CallInst* > CallInstrSet;
    typedef std::set< AllocaInst* > AllocaInstrSet;
    typedef std::set< GetElementPtrInst* > GepInstrSet;
    typedef std::set< LoadInst* > LoadInstrSet;
    typedef std::set< StoreInst* > StoreInstrSet;
    typedef std::set< Value* > ValueSet;
    typedef std::set< int > IntSet;
    typedef std::vector< Value* > ValueVector;
    typedef std::map< unsigned, ValueVector > ValueVectorByAddressSpaceMap;
    typedef std::set< AreaLimit* > AreaLimitSet;
    typedef std::map< unsigned, AreaLimitSet > AreaLimitSetByAddressSpaceMap;
    typedef std::map< Value*, AreaLimit* > AreaLimitByValueMap;
      
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
      
      // Functions which has been replaced with new ones when signatures are modified.
      FunctionMap replacedFunctions;
      // Function arguments mapping to find replacement arguments for old function arguments.
      ArgumentMap replacedArguments;

      // Sets of different type of instructions we are interested in.
      CallInstrSet internalCalls;
      CallInstrSet externalCalls;
      CallInstrSet allCalls;
      AllocaInstrSet allocas;
      StoreInstrSet stores;
      LoadInstrSet loads;
      
      // All values which might require that limits are resolved. Basically all load/store/call
      // pointer operands are added here. Call pointer operands needs to be resolved, because we need
      // to know which limits we have to pass with a pointer.
      ValueSet resolveLimitsOperands;

      // Bookkeeping of which limits certain value respects.
      AreaLimitByValueMap valueLimits;
      // Bookkeeping of all available limits of address spaces.
      AreaLimitSetByAddressSpaceMap addressSpaceLimits;

      // Set where is collected all values, which will not require boundary checks to
      // memory accesses. These have been resolved to be safe accesses in compile time.
      ValueSet safeExceptions;

      // Collect all allocas and global variables for each address space to struct to be able to
      // resolve static area reference limits easily. See example in [consolidateStaticMemory( Module &M )](#consolidateStaticMemory).
      DEBUG( dbgs() << "\n --------------- COLLECT ALLOCAS AND GLOBALS AND CREATE ONE BIG STRUCT --------------\n" );
      consolidateStaticMemory( M );

      // Find out static limits of each address space structure and adds limits to `addressSpaceLimits`
      // map sorted by address space number. Also adds the address space struct to value limits so that
      // if lookups traces limits all the way up to address space allocation struct, then limits are
      // found from limit map as normally `valueLimits[pointerOperand]`. [findAddressSpaceLimits( Module &M,
      // AreaLimitByValueMap &valLimits, AreaLimitSetByAddressSpaceMap &asLimits )](#findAddressSpaceLimits).
      DEBUG( dbgs() << "\n --------------- FIND LIMITS FOR EACH ADDRESS SPACE --------------\n" );
      findAddressSpaceLimits( M, valueLimits, addressSpaceLimits );

      
      // **Analyze all original functions.** Goes through all functions in module
      // and creates new function signature for them and collects information of instructions
      // that we will need in later transformations.
      // If function is intrinsic or WebCL builtin declaration (we know how it will behave) we
      // just skip it. If function is unknown external call compilation will fail.
      for( Module::iterator i = M.begin(); i != M.end(); ++i ) {

        if ( i->isIntrinsic() || i->isDeclaration() ) {
          if (RunUnsafeMode) {
            DEBUG( dbgs() << "Skipping: " << i->getName() << " which is intrinsic and/or declaration\n" );
            continue;
          }
          if (!isWebClBuiltin(i)) {
            dbgs() << "Found: " << i->getName() << " which is intrinsic and/or declaration\n";
            fast_assert(false, "Calling external functions is not allowed in strict mode. "
                        "Also intrinsics should be lowered before runnin pass.");
          } else {
            DEBUG( dbgs() << "Recognized builtin: "; i->print(dbgs()); );
            continue;
          }
        }

        // Creates new signatures for internal functions in the program and adds mapping between
        // old and new functions. Also creates mapping between old and new function arguments.
        // [Function* createNewFunctionSignature(Function *F, FunctionMap &functionMapping,
        // ArgumentMap &argumentMapping )](#createNewFunctionSignature).
        DEBUG( dbgs() << "\n --------------- CREATING NEW FUNCTION SIGNATURE --------------\n" );
        createNewFunctionSignature( i, replacedFunctions, replacedArguments );

        // Runs through all instructions in function and collects instructions.
        // [sortInstructions( Function *F, ... ) ](#sortInstructions).
        DEBUG( dbgs() << "\n --------------- FINDING INTERESTING INSTRUCTIONS --------------\n" );
        sortInstructions( i,  internalCalls, externalCalls, allocas, stores, loads );

        // Initialize the additional `allCalls` and `resolveLimitsOperands` sets which are useful in later phases.
        allCalls.insert(internalCalls.begin(), internalCalls.end());
        allCalls.insert(externalCalls.begin(), externalCalls.end());
        
        for (LoadInstrSet::iterator i = loads.begin(); i != loads.end(); i++) {
          LoadInst *load = *i;
          resolveLimitsOperands.insert(load->getPointerOperand());
        }
        
        for (StoreInstrSet::iterator i = stores.begin(); i != stores.end(); i++) {
          StoreInst *store = *i;
          resolveLimitsOperands.insert(store->getPointerOperand());
        }
        
        for (CallInstrSet::iterator i = allCalls.begin(); i != allCalls.end(); i++) {
          CallInst *call = *i;
          for (size_t op = 0; op < call->getNumOperands(); op++) {
            Value *operand = call->getOperand(op);
            /* ignore function pointers operands (not allowed in opencl)... no need to check them, but add all other pointer operands */
            if ( operand->getType()->isPointerTy() && !operand->getType()->getPointerElementType()->isFunctionTy() ) {
              resolveLimitsOperands.insert(operand);
            }
          }
        }
      }
      
      // **End of analyze phase.** After this `replacedFunctions`, `replacedArguments`, `internalCalls`, `externalCalls`,
      // `allCalls`, `allocas`, `stores`, `loads` and `resolveLimitsOperands` should not be changed, but only used for lookup.
      
      // Moves function instructions / basic blocks from original functions to new ones and fixes uses of original function arguments
      // to point new arguments. After this function behavior should be back to original, except if function has call to another
      // function whose signature was changed. [moveOldFunctionImplementationsToNewSignatures(FunctionMap &replacedFunctions,
      // ArgumentMap &replacedArguments)](#moveOldFunctionImplementationsToNewSignatures).
      DEBUG( dbgs() << "\n ----------- CONVERTING OLD FUNCTIONS TO NEW ONES AND FIXING SMART POINTER ARGUMENT PASSING  ----------\n" );
      moveOldFunctionImplementationsToNewSignatures(replacedFunctions, replacedArguments);
      
      // Finds out kernel functions from Module metadata and creates WebCL kernels from them. `kernel void foo(global float *bar)` ->
      // `kernel void foo(global float *bar, size_t bar_size)`. Also calculates and creates run-time limits for passed kernel
      // arguments and adds limits to `addressSpaceLimits` bookkeeping and calls original kernel implementation which has been changed
      // earlier to use safe pointer arguments. [createKernelEntryPoints(Module &M, FunctionMap &replacedFunctions,
      // AreaLimitSetByAddressSpaceMap &asLimits)](#createKernelEntryPoints).
      DEBUG( dbgs() << "\n --------------- CREATE KERNEL ENTRY POINTS AND GET ADDITIONAL LIMITS FROM KERNEL ARGUMENTS --------------\n" );
      createKernelEntryPoints(M, replacedFunctions, addressSpaceLimits);

      // Traces limits for all instructions and values in the module and adds them to `valueLimits`. After this we should be able
      // to get min and max addresses for all instructions / globals that we are interested in. [findLimits(FunctionMap &replacedFunctions,
      // ValueSet &checkOperands, AreaLimitByValueMap &valLimits, AreaLimitSetByAddressSpaceMap &asLimits)](#findLimits).
      DEBUG( dbgs() << "\n --------------- FIND LIMITS OF EVERY REQUIRED OPERAND --------------\n" );
      findLimits(replacedFunctions, resolveLimitsOperands, valueLimits, addressSpaceLimits);
      
      // Fixes all call instructions in the program to call new safe implementations so that program is again in functional state.
      // [fixCallsToUseChangedSignatures(...)](#fixCallsToUseChangedSignatures)
      DEBUG( dbgs() << "\n --------------- FIX CALLS TO USE NEW SIGNATURES --------------\n" );
      fixCallsToUseChangedSignatures(replacedFunctions, replacedArguments, internalCalls, valueLimits);
      
      // Analyze code and find out the cases where we can be sure that memory access is safe in compile time and check can be omitted.
      // NOTE: better place for this could be already before any changes has been made to original code.
      // [collectSafeExceptions(resolveLimitsOperands, replacedFunctions, safeExceptions)](#collectSafeExceptions)
      DEBUG( dbgs() << "\n --------------- ANALYZING CODE TO FIND SPECIAL CASES WHERE CHECKS ARE NOT NEEDED --------------\n" );
      collectSafeExceptions(resolveLimitsOperands, replacedFunctions, safeExceptions);

      // Goes through all memory accesses and creates instrumentation to prevent any invalid accesses. NOTE: if opecl frontend actually
      // creates some memory intrinsics we might need to take care of checking their operands as well.
      // [addBoundaryChecks( ... )](#addBoundaryChecks)
      DEBUG( dbgs() << "\n --------------- ADDING BOUNDARY CHECKS --------------\n" );
      addBoundaryChecks(stores, loads, valueLimits, addressSpaceLimits, safeExceptions);

      // Goes through all builtin WebCL calls and if they are unsafe (has pointer arguments), converts instruction to call safe
      // version of it instead. Value limits are required to be able to resolve which limit to pass to safe builtin call.
      // [makeBuiltinCallsSafe( ... )](#makeBuiltinCallsSafe)
      DEBUG( dbgs() << "\n --------------- FIX BUILTIN CALLS TO CALL SAFE VERSIONS IF NECESSARY --------------\n" );
      makeBuiltinCallsSafe(externalCalls, valueLimits);

      // Helps to print out resulted LLVM IR code if pass fails before writing results
      // on pass output validation
      /*
      dbgs() << "\n --------------- FINAL OUTPUT --------------\n";
      M.print(dbgs(), NULL);
      dbgs() << "\n --------------- FINAL OUTPUT END --------------\n";
      */
      return true;
    }
    
    /**
     * Resolves uses of value and limits that it should respect
     *
     * Does also simple data dependency analysis to be able to 
     * resolve limits which values should respect in case of same 
     * address space has more than allocated 1 areas.
     *
     * Follows uses of val and in case of storing to memory, keep track if
     * there is always only single limits for that location.
     *
     * TODO: needs more clear implementation
     */
    void resolveUses(Value *val, AreaLimitByValueMap &valLimits, int recursion_level = 0) {
      
      // check all uses of value until cannot trace anymore
      for( Value::use_iterator i = val->use_begin(); i != val->use_end(); ++i ) {
        Value *use = *i;
       
        // ----- continue to next use if cannot be sure about the limits
        if ( dyn_cast<GetElementPtrInst>(use) ) {
          DEBUG( for (int i = 0; i < recursion_level; i++ ) dbgs() << "  "; );
          DEBUG( dbgs() << "Found GEP: "; use->print(dbgs()); dbgs() << "  ## Preserving original limits KEEP ON TRACKING\n"; );
        } else if ( LoadInst *load = dyn_cast<LoadInst>(use) ) {
          DEBUG( for (int i = 0; i < recursion_level; i++ ) dbgs() << "  "; );
          DEBUG( dbgs() << "Found LOAD: "; use->print(dbgs()); dbgs() << "  ## If we reached here we should have already resolved limits of pointer operand from somewhere.\n"; );
          
        } else if ( StoreInst *store = dyn_cast<StoreInst>(use) ) {
          DEBUG( for (int i = 0; i < recursion_level; i++ ) dbgs() << "  "; );
          DEBUG( dbgs() << "Found STORE: "; use->print(dbgs()); dbgs() << "  ## If we are storing pointer, also pass VAL limits to destination address.\n" );
          
          // first check if use is actually in value operand and in that case set limits for destination pointer
          if (store->getValueOperand() == val) {
            if (valLimits.count(store->getPointerOperand()) != 0) {
              fast_assert(valLimits[store->getPointerOperand()] == valLimits[val],
                          "Dependency analysis cannot resolve single limits for a memory address. This is a bit nasty problem to resolve, since we cannot pass multiple possible limits to functions safe pointer argument. SPIR + removing all safe pointer argument hassling could help this some day. For now avoid assigning pointers from different ranges to the same variable.");
            }
            valLimits[store->getPointerOperand()] = valLimits[val];
            resolveUses(store->getPointerOperand(), valLimits, recursion_level + 1);
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
        valLimits[use] = valLimits[val];
        resolveUses(use, valLimits, recursion_level + 1);
      }
    }
    
    /**
     * Traces from leafs to root if limit is found and then adds limits to each step.
     */
    bool resolveAncestors(Value *val, AreaLimitByValueMap &valLimits, int recursion_level = 0) {
      Value *next = NULL;
      DEBUG( for (int i = 0; i < recursion_level; i++ ) dbgs() << "  "; );
      
      if ( GetElementPtrInst *gep = dyn_cast<GetElementPtrInst>(val) ) {
        DEBUG( dbgs() << "Found GEP: "; val->print(dbgs()); dbgs() << " tracing to baseval.\n"; );
        next = gep->getPointerOperand();
      } else if ( LoadInst *load = dyn_cast<LoadInst>(val) ) {
        DEBUG( dbgs() << "Found LOAD: "; val->print(dbgs()); dbgs() << " tracing to memaddr.\n"; );
        next = load->getPointerOperand();
      } else if ( StoreInst *store = dyn_cast<StoreInst>(val) ) {
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
        if (valLimits.count(next) > 0) {
          valLimits[val] = valLimits[next];
          return true;
        } else {
          if (resolveAncestors(next, valLimits, recursion_level + 1)) {
            valLimits[val] = valLimits[next];
            return true;
          }
        }
      }
      return false;
    }
  
    /**
     * Goes through all relevant parts in program and traces limits for those values.
     *
     * Call operands are not a problem anymore, since they has been converted to pass structs, not direct pointers.
     */
    void findLimits(FunctionMap &replacedFunctions,
                    ValueSet &checkOperands,
                    AreaLimitByValueMap &valLimits,
                    AreaLimitSetByAddressSpaceMap &asLimits) {
    
      // first trace all uses of function arguments to find their limits
      DEBUG( dbgs() << "----- Tracing function pointer argument uses \n"; );
      for ( FunctionMap::iterator i = replacedFunctions.begin(); i != replacedFunctions.end(); i++ )  {
        Function *originalFunc = i->first;
        Function *safePointerFunction = i->second;
        Function::arg_iterator originalArgIter = originalFunc->arg_begin();

        for( Function::arg_iterator a = safePointerFunction->arg_begin(); a != safePointerFunction->arg_end(); ++a ) {
          Argument &originalArg = *originalArgIter;
          Argument &replaceArg = *a;
          
          // if safe pointer argument, trace uses
          if (originalArg.getType() != replaceArg.getType()) {

            fast_assert(replaceArg.getNumUses() == 1, "Safe pointer argument should have only one extractval use as far as expected currently... (the original use of arg)");
            ExtractValueInst *cur = dyn_cast<ExtractValueInst>(*replaceArg.use_begin());
            fast_assert(cur, "Found invalid type of use. Maybe passed directly to other function.");
            
            // Adding extract value instructions to entry block to have direct limits stored.
            BasicBlock &entry = safePointerFunction->getEntryBlock();
            ExtractValueInst *minLimit = ExtractValueInst::Create( &replaceArg, genVector<unsigned int>(1), replaceArg.getName() + ".min", &(*entry.begin()) );
            ExtractValueInst *maxLimit = ExtractValueInst::Create( &replaceArg, genVector<unsigned int>(2), replaceArg.getName() + ".max", &(*entry.begin()) );
            
            // Init direct limits for current and do some analysis to resolve derived limits
            valLimits[cur] = AreaLimit::Create(minLimit, maxLimit, false);
            resolveUses(cur, valLimits);
          }
          
          originalArgIter++;
        }
      }
      
      // optimize single area address space limits
      DEBUG( dbgs() << "----- Tracing call/load/store operands: \n"; );
      for (ValueSet::iterator i = checkOperands.begin(); i != checkOperands.end(); i++) {
        Value* val = *i;
        DEBUG( dbgs() << "Tracing limits for: "; val->print(dbgs()); dbgs() << "\n"; );
        PointerType *t = dyn_cast<PointerType>(val->getType());
        AreaLimitSet &limitSet = asLimits[t->getAddressSpace()];
        // allow no limit values in unsafe mode (e.g. externals)
        if (limitSet.size() == 0 && RunUnsafeMode) {
          DEBUG( dbgs() << "unrestricted mode and no limits found... skipping\n"; );
          continue;
        }

        fast_assert(limitSet.size() > 0, "Pointer to address space without allocations.");
        if ( limitSet.size() == 1 ) {
          DEBUG( dbgs() << "Found single limits for AS: " << t->getAddressSpace() << "\n"; );
          valLimits[val] = *(limitSet.begin());
          continue;
        }
        
        if ( resolveAncestors(val, valLimits) ) {
          DEBUG( dbgs() << "Traced limits successful!\n"; );
          fast_assert( valLimits.count(val) > 0, "Obviously limits should have been added to set.");
        } else {
          DEBUG( dbgs() << "Could not trace the limits!\n"; );
        }
      }
    }
      
    /**
     * Goes through global variables and adds limits to bookkeepping
     */
    void findAddressSpaceLimits( Module &M, AreaLimitByValueMap &valLimits, AreaLimitSetByAddressSpaceMap &asLimits ) {
      LLVMContext& c = M.getContext();
      for (Module::global_iterator g = M.global_begin(); g != M.global_end(); g++) {
        
        // for now unnamed addresses are kept ouside from general address space limits, because they might pollute it
        // unnecessarily. If unnamed address requires limits, they are created on demand.
        // this should work, because there shouldn't be any other than direct references to this kind of globals
        if ( g->hasUnnamedAddr() ) {
          DEBUG( dbgs() << "Found unnamed address, adding limits to bookkeeping\n"; );
          Constant *firstValid = ConstantExpr::getGetElementPtr(g, genIntVector<Constant*>(c, 0, 0));
          // NOTE: this works, but could be safer to check element type of global and get limits from number of element
          Constant *firstInvalid = ConstantExpr::getGetElementPtr(g, genIntVector<Constant*>(c, 1, 0));
          valLimits[g] = AreaLimit::Create(firstValid, firstInvalid, false);
        }
        
        // collect only named addresses which are not externs
        if (!g->hasUnnamedAddr() && !(g->hasExternalLinkage() && g->isDeclaration())) {
          DEBUG( dbgs() << "AS: " << g->getType()->getAddressSpace() << " Added global: "; g->print(dbgs()); dbgs() << "\n"; );
          std::stringstream aliasName;
          aliasName << "AS" << g->getType()->getAddressSpace();
          PointerType *castType = PointerType::get(Type::getFloatTy(c), g->getType()->getAddressSpace());
          // pointercast all limits to float* to make result more readable
          Constant *firstValid = ConstantExpr::getGetElementPtr(g, getConstInt(c,0));
          Constant *firstInvalid = ConstantExpr::getGetElementPtr(g, getConstInt(c,1));
          AreaLimit *gvLimits = AreaLimit::Create(firstValid, firstInvalid, false);
          asLimits[g->getType()->getAddressSpace()].insert(gvLimits);
          // make sure that references to this global variable always respects its own limits
          valLimits[g] = gvLimits;
          /* GlobalAlias does not support GEP... if the support is added, then uncommenting this will improve readability of produced code greatly
          // requires an extra alias for GEP and for Cast or llvm-as throws error: "Aliasee should be either GlobalValue or bitcast of GlobalValue"
          GlobalAlias *firstInvalidAliasTemp = new GlobalAlias(firstInvalid->getType(), GlobalValue::InternalLinkage,
                                                               aliasName.str() + ".temp", firstInvalid, &M);
          GlobalAlias *firstValidAlias = new GlobalAlias(firstValid->getType(), GlobalValue::InternalLinkage, aliasName.str() + ".min",
                                                         ConstantExpr::getPointerCast(firstValid, castType), &M);
          GlobalAlias *firstInvalidAlias = new GlobalAlias(castType, GlobalValue::InternalLinkage, aliasName.str() + ".max",
                                                           ConstantExpr::getPointerCast(firstInvalidAliasTemp, castType), &M);
          asLimits[g->getType()->getAddressSpace()].insert(AreaLimit::Create(firstValidAlias, firstInvalidAlias, false));
          */
        }
      }
    }
      
    /**
     * Collect all allocas and global values for each address space and create one struct for each
     * address space.
     */
    void consolidateStaticMemory( Module &M ) {
      LLVMContext& c = M.getContext();
      
      ValueVectorByAddressSpaceMap staticAllocations;

      for (Module::global_iterator g = M.global_begin(); g != M.global_end(); g++) {
        // collect only named linked addresses (for unnamed there cannot be relative references anywhere) externals are allowed only in special case.
        DEBUG( dbgs()  << "Found global: "; g->print(dbgs());
               dbgs() << " of address space: " << g->getType()->getAddressSpace(); );

        if ( g->hasUnnamedAddr() ) {
          DEBUG( dbgs() << " ### Ignored because unnamed address \n"; );
        } else if ( g->hasExternalLinkage() && g->isDeclaration() ) {
          DEBUG( dbgs() << " ### Ignored because extern linkage \n"; );
        } else {
          DEBUG( dbgs() << " ### Collected to address space structure \n"; );
          staticAllocations[g->getType()->getAddressSpace()].push_back(g);
        }
      }

      for (Module::iterator f = M.begin(); f != M.end(); f++) {
        // skip declarations (they does not even have entry blocks)
        if (f->isDeclaration()) continue;
        BasicBlock &entry = f->getEntryBlock();
        for (BasicBlock::iterator i = entry.begin(); i != entry.end(); i++) {
          AllocaInst *alloca = dyn_cast<AllocaInst>(i);
          if (alloca != NULL) {
            staticAllocations[alloca->getType()->getAddressSpace()].push_back(alloca);
          }
        }
      }
      
      // simple fix of alignment of mem intrinsics
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
              global->setInitializer(NULL);
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

        ArrayRef< Type* > structElementTypesArrayRef( structElementTypes );
        ArrayRef<Constant*> structElementData( structInitData );

        
        std::stringstream structName;
        structName << "AddressSpace" << addressSpace << "StaticData";
        StructType *addressSpaceStructType = StructType::get( c, structElementTypesArrayRef );
        // NOTE: cant give name to struct literal type, would be nice to have for readability
        // addressSpaceStructType->setName(structName.str() + "Type");

        // create struct of generated type and add to module
        Constant* addressSpaceDataInitializer = ConstantStruct::get( addressSpaceStructType, structElementData );
        GlobalVariable *aSpaceStruct = new GlobalVariable
          (M, addressSpaceStructType, false, GlobalValue::InternalLinkage, addressSpaceDataInitializer, 
           structName.str(), NULL, GlobalVariable::NotThreadLocal, addressSpace);
        
        // replace all uses of old allocas and globals value with new constant geps and remove original values
        for (size_t valIndex = 0; valIndex < values.size(); valIndex++) {
          Value* origVal = values[valIndex];

          // get field of struct
          Constant *structVal = ConstantExpr::getInBoundsGetElementPtr
            (dyn_cast<Constant>(aSpaceStruct), genIntVector<Constant*>( c, 0, valIndex) );
          
          // Currently LLVM IR does not support GEP in alias. If support is added, uncommenting this will greatly improve readability of produced code
          // for alias: GlobalAlias *fieldAlias = new GlobalAlias(structVal->getType(), GlobalValue::InternalLinkage, "", structVal, &M);
          
          DEBUG( dbgs() << "Orig val type: "; origVal->getType()->print(dbgs()); 
                 dbgs() << " new val type: "; structVal->getType()->print(dbgs()); dbgs() << "\n"; );
          DEBUG( dbgs() << "Orig val: "; origVal->print(dbgs()); 
                 dbgs() << " new val: "; structVal->print(dbgs()); dbgs() << "\n"; );

          // use alias everywhere
          // for alias: origVal->replaceAllUsesWith(fieldAlias);
          origVal->replaceAllUsesWith(structVal);
          
          // set name according to original value type and remove original
          if ( AllocaInst* alloca = dyn_cast<AllocaInst>(origVal) ) {
            // for alias: fieldAlias->setName(alloca->getParent()->getParent()->getName() + "." + origVal->getName());
            structVal->setName(alloca->getParent()->getParent()->getName() + "." + origVal->getName());
            alloca->eraseFromParent();
          } else if ( GlobalVariable* global = dyn_cast<GlobalVariable>(origVal) ) {
            // for alias: fieldAlias->setName(aSpaceStruct->getName() + "." + origVal->getName());
            structVal->setName(aSpaceStruct->getName() + "." + origVal->getName());
            global->eraseFromParent();
          }
          
          //DEBUG( dbgs() << "Alias: "; fieldAlias->print(dbgs()); dbgs() << "\n"; );
        }
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
    void createKernelEntryPoints(Module &M, FunctionMap &replacedFunctions, AreaLimitSetByAddressSpaceMap &asLimits) {
      NamedMDNode* oclKernels = M.getNamedMetadata("opencl.kernels");
      if (oclKernels != NULL) {
        for (unsigned int op = 0; op < oclKernels->getNumOperands(); op++) {
          MDNode* md = oclKernels->getOperand(op);
          DEBUG( dbgs() << "Fixing entry point " << op << ": "; md->print(dbgs()); dbgs() << " --> " );
          Function* oldFun = dyn_cast<Function>(md->getOperand(0));
          Function *newKernelEntryFunction = NULL;
          
          // If there is need, create new kernel wrapper and replace old kernel reference with new WebCl
          // compatible version.
          if (replacedFunctions.count(oldFun) > 0) {
            Function *smartKernel = replacedFunctions[oldFun];
            newKernelEntryFunction = createWebClKernel(M, oldFun, smartKernel, asLimits);
            // make smartKernel to be internal linkage to allow better optimization
            smartKernel->setLinkage(GlobalValue::InternalLinkage);
            // TODO: if found nvptx_kernel attribute, move it to new kernel
            md->replaceOperandWith(0, newKernelEntryFunction);
          }

          DEBUG( md->print(dbgs()); dbgs() << "\n" );
        }
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
    Function* createWebClKernel(Module &M, Function *origKernel, Function *smartKernel, AreaLimitSetByAddressSpaceMap &asLimits) {
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

      std::vector<Value*> args;
      Function::arg_iterator origArg = origKernel->arg_begin();
      for( Function::arg_iterator a = webClKernel->arg_begin(); a != webClKernel->arg_end(); ++a ) {
        Argument* arg = a;
        arg->setName(origArg->getName());
        Type* t = arg->getType();
        
        if ( t->isPointerTy() ) {
          // create global unnamed variables for each limits got from kernel arguments
          GlobalVariable *globalMin = new GlobalVariable( M, t, false, GlobalValue::PrivateLinkage, Constant::getNullValue(t) );
          GlobalVariable *globalMax = new GlobalVariable( M, t, false, GlobalValue::PrivateLinkage, Constant::getNullValue(t) );
          globalMin->setUnnamedAddr(true);
          globalMax->setUnnamedAddr(true);
          PointerType *pointerType = dyn_cast<PointerType>(t);
          
          // add addresses to limit set for the address space
          DEBUG( dbgs() << "AS: " << pointerType->getAddressSpace() << " Adding indirect limits from kernel parameter: "; arg->print(dbgs()); dbgs() << "\n"; );
          asLimits[pointerType->getAddressSpace()].insert( AreaLimit::Create(globalMin, globalMax, true) );
          
          Value* elementCount = (++a);
          a->setName(origArg->getName() + ".size");
          GetElementPtrInst *lastLimit = dyn_cast<GetElementPtrInst>(blockBuilder.CreateGEP(arg, elementCount));

          blockBuilder.CreateStore(arg, globalMin);
          blockBuilder.CreateStore(lastLimit, globalMax);
          
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
      
      DEBUG( dbgs() << "Checking if safe to load: "; operand->print(dbgs()); dbgs() << " ... "; );

      if ( ConstantExpr *constExpr = dyn_cast<ConstantExpr>(operand) ) {
        
        Instruction* inst = getAsInstruction(constExpr);
        if ( GetElementPtrInst *gep = dyn_cast<GetElementPtrInst>(inst) ) {
          isSafe = isSafeGEP(gep);
        } else {
          DEBUG( dbgs() << "... unhandled const expr, maybe could be supported if implemented"; );
        }
        delete inst;

      } else if ( GlobalAlias *globalAlias = dyn_cast<GlobalAlias>(operand) ) {
        DEBUG( dbgs() << "loading directly global alias.. "; );
        isSafe = true;      
      } else if ( GlobalVariable *globalVal = dyn_cast<GlobalVariable>(operand) ) {
        DEBUG( dbgs() << "loading directly global variable .. "; );
        isSafe = true;
      } else if ( ConstantStruct *constStruct = dyn_cast<ConstantStruct>(operand) ) {
        DEBUG( dbgs() << "ConstantStruct value.. maybe if support implemented"; );
      } else if ( ConstantVector *constVec = dyn_cast<ConstantVector>(operand) ) {
        DEBUG( dbgs() << "ConstantVector value.. maybe if support implemented"; );
      } else if ( ConstantArray *constArr = dyn_cast<ConstantArray>(operand) ) {
        DEBUG( dbgs() << "ConstantArray value.. maybe if support implemented"; );
      } else if ( ConstantDataSequential *constData = dyn_cast<ConstantDataSequential>(operand) ) {
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
     * Collects values, which can be handled without modifying.
     * 
     * e.g. main function arguments (int8** is not currently supported 
     * and won't be in the first place).
     *
     * Note: this is quite dirty symbol name based hack...
     */
    void collectSafeExceptions(ValueSet &checkOperands, FunctionMap &replacedFunctions, ValueSet &safeExceptions) {

      for ( ValueSet::iterator i = checkOperands.begin(); i != checkOperands.end(); i++) {
        Value *operand = *i;

        if (isSafeAddressToLoad(operand)) {
          safeExceptions.insert(operand);
        }
      }
      
      if (RunUnsafeMode) {
        for ( FunctionMap::iterator i = replacedFunctions.begin(); i != replacedFunctions.end(); i++ )  {
          Function *check = i->second;
          if (check->getName() == "main__smart_ptrs__") {
            check->takeName(i->first);
            for( Function::arg_iterator a = check->arg_begin(); a != check->arg_end(); ++a ) {
              Argument* arg = a;
              if (arg->getName() == "argv") {
                resolveArgvUses(arg, safeExceptions);
              }
            }
          }
        }

        // TODO: don't check loading externals...
        // for ( ValueSet::iterator i = checkOperands.begin(); i != checkOperands.end(); i++) {
        //   Value *operand = *i;
        // }
      }
    }
    
    /**
     * Checks if store stores data to smart pointer and updates also smart pointer accordingly.
     */
    void addBoundaryChecks(StoreInstrSet &stores, LoadInstrSet &loads, AreaLimitByValueMap &valLimits, AreaLimitSetByAddressSpaceMap &asLimits, ValueSet &safeExceptions) {
      // check load instructions... 
      for (LoadInstrSet::iterator i = loads.begin(); i != loads.end(); i++) {
        addChecks((*i)->getPointerOperand(), *i, valLimits, asLimits, safeExceptions);
      }   
      // check store instructions
      for (StoreInstrSet::iterator i = stores.begin(); i != stores.end(); i++) {
        addChecks((*i)->getPointerOperand(), *i, valLimits, asLimits, safeExceptions);
      }
    }

    /**
     * If val touching pointer operand needs checks, then inject boundary check code.
     */
    void addChecks(Value *ptrOperand, Instruction *inst, AreaLimitByValueMap &valLimits, AreaLimitSetByAddressSpaceMap &asLimits, ValueSet &safeExceptions) {
      
      // If no need to add checks, just skip
      if (safeExceptions.count(ptrOperand)) {
        DEBUG( dbgs() << "Skipping op that was listed in safe exceptions: "; inst->print(dbgs()); dbgs() << "\n" );        
        return;
      }
      
      AreaLimitSet limits;
      if ( valLimits.count(ptrOperand) != 0 ) {
        limits.insert(valLimits[ptrOperand]);
      } else {
        unsigned int addressSpace = dyn_cast<PointerType>(ptrOperand->getType())->getAddressSpace();
        limits.insert( asLimits[addressSpace].begin(), asLimits[addressSpace].end() );
      }
      
      createLimitCheck(ptrOperand, limits, inst);
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
    void createLimitCheck(Value *ptr, AreaLimitSet &limits, Instruction *meminst) {
      
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
      for (AreaLimitSet::iterator i = limits.begin(); i != limits.end(); i++) {
        DEBUG( dbgs() << "### min: "; (*i)->min->print(dbgs()); dbgs() << "\n"; );
        DEBUG( dbgs() << "### max: "; (*i)->max->print(dbgs()); dbgs() << "\n"; );
      }
      fast_assert(limits.size() == 1, "Current boundary check generation does not support multiple limits checking.");
      AreaLimit *limit = *(limits.begin());
      
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
      Value *last_value_for_type = limit->lastValidAddressFor(ptr->getType(), meminst);
      // *   %2 = value to compare to get first valid address
      Value *first_valid_pointer = limit->firstValidAddressFor(ptr->getType(), meminst);

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
     * Goes through external function calls and if call is unsafe opencl call convert it to safe webcl implementation
     * which operates with smart pointers
     */
    void makeBuiltinCallsSafe(CallInstrSet &calls, AreaLimitByValueMap &valLimits) {
      // if mapping is needed outside export this to be reference parameter instead of local 
      FunctionMap safeBuiltins;
      ArgumentMap dummyArgMap;

      for (CallInstrSet::iterator i = calls.begin(); i != calls.end(); i++) {
        CallInst *call = *i;

        DEBUG( dbgs() << "---- Checking builtin call:"; call->print(dbgs()); dbgs() << "\n" );
        
        Function* oldFun = call->getCalledFunction();
        
        if ( isWebClBuiltin(oldFun) ) {

          std::string demangledName = extractItaniumDemangledFunctionName(oldFun->getName().str());

          // if not supported yet assert
          fast_assert( unsupportedUnsafeBuiltins.count(demangledName) == 0, 
                       "Tried to call forbidden builtin: " + oldFun->getName() + " " + demangledName);
          
          // if unsafe fix call
          if ( unsafeBuiltins.count(demangledName) > 0 ) {
            
            // if safe version is not yet generated do it first..
            if ( safeBuiltins.count(oldFun) == 0 ) {
              Function *newFun = createNewFunctionSignature(oldFun, safeBuiltins, dummyArgMap);
              // simple name mangler to be able to select, which implementation to call (couldn't find easy way to do Itanium C++ mangling here)
              // luckily the cases that needs mangling are pretty limited so we can keep it simple
              newFun->setName(customMangle(oldFun, demangledName + "__safe__"));
            }
            
            Function *newFun = safeBuiltins[oldFun];
            ArgumentMap dummyArg;
            convertCallToUseSmartPointerArgs(call, newFun, dummyArg, valLimits);
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
    void fixCallsToUseChangedSignatures(FunctionMap &replacedFunctions, 
                                        ArgumentMap &replacedArguments, 
                                        CallInstrSet &calls,
                                        AreaLimitByValueMap &valLimits) {

      for (CallInstrSet::iterator i = calls.begin(); i != calls.end(); i++) {
        CallInst *call = *i;

        DEBUG( dbgs() << "---- Started fixing:"; call->print(dbgs()); dbgs() << "\n" );
        
        Function* oldFun = call->getCalledFunction();
        
        // if function was not replaced (didn't have pointer parameters)
        if (replacedFunctions.count(oldFun) == 0) {
          continue;
        }

        Function* newFun = replacedFunctions[oldFun];
        convertCallToUseSmartPointerArgs(call, newFun, replacedArguments, valLimits);
      }
    }
    
    /**
     * Converts call function to use new function as called function and changes all pointer parameters to be smart pointers.
     */
    void convertCallToUseSmartPointerArgs(CallInst *call, Function *newFun, ArgumentMap &replacedArguments, AreaLimitByValueMap &valLimits) {

      Function* oldFun = call->getCalledFunction();
      call->setCalledFunction(newFun);
      
      // find if function signature changed some Operands and change them to refer smart pointers 
      // instead of pointers directly
      int op = 0;
      Function::arg_iterator newArgIter = newFun->arg_begin();
      for( Function::arg_iterator a = oldFun->arg_begin(); a != oldFun->arg_end(); ++a ) {
        LLVMContext& c = newFun->getContext();
        Argument* oldArg = a;
        Argument* newArg = newArgIter;
        newArgIter++;
        
        // NOTE: If we would first expand smart pointer map, we might be able to resolve smart pointer for parameter
        //       a lot easier... if there is need to add more and more special cases here, consider the option...

        // this argument type has been changed to smart pointer, find out corresponding smart
        if (oldArg->getType() != newArg->getType()) {
          Value* operand = call->getArgOperand(op);
          
          DEBUG( dbgs() << "- op #" << op << " needs fixing: "; operand->print(dbgs()); dbgs() << "\n" );
          
          if ( Argument* arg = dyn_cast<Argument>(operand) ) {
            // if operand is argument it should be found from replacement map
            DEBUG( dbgs() << "Operand is argument of the same func! Passing it through.\n"; );
            call->setOperand( op, replacedArguments[arg] );

          } else if (ExtractValueInst *extract = dyn_cast<ExtractValueInst>(operand)) {
            // TODO: REMOVE THIS HACK IT OPENS SECURITY HOLE, ALWAYS GET LIMITS FROM RESULT OF ANALYSIS
            //       THIS WILL ALLOW UNSAFE CODE IF STRUCT IS GIVEN AS ARGUMENT AND THEN ONE ELEMENT OF
            //       IT IS PASSED TO OTHER FUNCTION
            Value* aggregateOp = extract->getAggregateOperand();
            DEBUG( dbgs() << "Operand is extractval of argument of the same func: "; aggregateOp->print(dbgs()); dbgs() << "\n"; );
            // TODO: to make this secure we have to check that operand argument is listed in replaced argument map and is really generated by us (types in replaced arguments must been changed)
            call->setOperand( op, aggregateOp );
          
          } else {
            AreaLimit *limit = NULL;
            if (valLimits.count(operand) > 0) {
              limit = valLimits[operand];
            } else {
              DEBUG( dbgs() << "In basic block: \n"; call->getParent()->print(dbgs()); dbgs() << "\nin call:\n"; call->print(dbgs()); dbgs() << "\nOperand:"; operand->print(dbgs()); dbgs() << "\n"; );
              fast_assert(false, "Could not resolve limits for a value passed as operand. Try to make code less obscure, write better limit analysis or do not change signature of this method at all and check against all limits of address space.");
            }
            call->setOperand( op, convertArgumentToSmartStruct(operand, limit->min, limit->max, limit->indirect, call) );
            int argIdx = a->getArgNo() + 1; // removeAttribute does know about arg# 0 (the return value), thus +1
            call->removeAttribute(argIdx, Attributes::get(c, genVector(llvm::Attributes::ByVal)));
          }
        }
        op++;
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
    void moveOldFunctionImplementationsToNewSignatures(FunctionMap &replacedFunctions, 
                                                       ArgumentMap &replacedArguments) {
            
      for (FunctionMap::iterator i = replacedFunctions.begin(); i != replacedFunctions.end(); i++) {
        // loop through arguments and if type has changed, then create label to access original arg 
        Function* oldFun = i->first;
        Function* newFun = i->second;
        
        LLVMContext& c = oldFun->getContext();
        
        // move all instructions to new function
        newFun->getBasicBlockList().splice( newFun->begin(), oldFun->getBasicBlockList() );
        BasicBlock &entryBlock = newFun->getEntryBlock();
 
        DEBUG( dbgs() << "Moved BBs to " << newFun->getName() << "( .... ) and took the final function name.\n" );

        for( Function::arg_iterator a = oldFun->arg_begin(); a != oldFun->arg_end(); ++a ) {
          Argument* oldArg = a;
          Argument* newArg = replacedArguments[a];

          DEBUG( dbgs() << "Fixing arg: "; oldArg->print(dbgs()); dbgs() << " : " );

          newArg->takeName(oldArg);
          oldArg->setName(newArg->getName() + ".orig");
          
          // non safe pointer argument... direct replace
          if (oldArg->getType() == newArg->getType()) {
            DEBUG( dbgs() << "type was not changed. Just replacing oldArg uses with newArg.\n" );
            // argument was not tampered, just replace uses to point the new function
            oldArg->replaceAllUsesWith(newArg);
            continue;
          }

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
                    
        } // -- end arguments for loop
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
    virtual Function* createNewFunctionSignature( 
      Function *F,  
      FunctionMap &functionMapping, 
      ArgumentMap &argumentMapping ) {
       
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
      SafeArgTypes args(c, typesOfArgumentList(F->getArgumentList()), dontTouchArguments);
      TypeVector& param_types = args.argTypes;
      IntSet& safeTypeArgNos = args.safeArgNos; // argument numbers of safe parameters we have generated; used later for deciding when to not remove ByVal attribute

      // creating new function with different prototype 
      FunctionType *function_type = F->getFunctionType();
      FunctionType *new_function_type = FunctionType::get( function_type->getReturnType(), param_types, false );

      Function *new_function = Function::Create( new_function_type, F->getLinkage() );
      new_function->copyAttributesFrom( F );
      
      F->getParent()->getFunctionList().insert( F, new_function );
      new_function->setName( F->getName() + "__smart_ptrs__" );

      // add new function to book keepig to show what was replaced
      functionMapping.insert( std::pair< Function*, Function* >( F, new_function ) );

      DEBUG( dbgs() << "-- Created new signature for: " << F->getName() << " "; F->getType()->print(dbgs()) );
      DEBUG( dbgs() << "\nnew signature: " << new_function->getName() << " "; new_function->getType()->print(dbgs()); dbgs() << "\n" );

      // map arguments of original function to new replacements
      for( Function::arg_iterator 
             a = F->arg_begin(), 
             E = F->arg_end(), 
             a_new = new_function->arg_begin(); a != E; ++a, ++a_new ) {             

        // remove attribute which does not make sense for non-pointer argument
        // getArgNo() starts from 0, but removeAttribute assumes them starting from 1 ( arg index 0 is the return value ).
        int argIdx = a_new->getArgNo()+1;
        new_function->removeAttribute(argIdx, Attributes::get(c, genVector(llvm::Attributes::NoCapture)));
        if (safeTypeArgNos.count(a_new->getArgNo())) {
          new_function->removeAttribute(argIdx, Attributes::get(c, genVector(llvm::Attributes::ByVal)));
        }
        
        argumentMapping.insert( std::pair< Argument*, Argument* >( a, a_new ) ); 
        DEBUG( dbgs() << "Mapped orig arg: "; a->print(dbgs()); dbgs() << " -----> "; a_new->print(dbgs()); dbgs() << "\n" );
        
      }
      DEBUG( dbgs() << "\nNew signature: "; new_function->print(dbgs()); dbgs() << "\n" );

      return new_function;
    }

    virtual void sortInstructions( Function *F, 
                                   CallInstrSet &internalCalls, 
                                   CallInstrSet &externalCalls, 
                                   AllocaInstrSet &allocas,
                                   StoreInstrSet &stores,
                                   LoadInstrSet &loads) {
      
      DEBUG( dbgs() << "-- Finding interesting instructions from: " << F->getName() << "\n" );

      // find all instructions which should be handled afterwards
      for ( Function::iterator bb = F->begin(); bb != F->end(); bb++) {
        for( BasicBlock::iterator i = bb->begin(); i != bb->end(); ++i ) {      
          Instruction &inst = *i;
       
          if ( CallInst *call = dyn_cast< CallInst >(&inst) ) {
            if (!call->getCalledFunction()->isIntrinsic()) {
              if (call->getCalledFunction()->isDeclaration()) {
                externalCalls.insert(call);
                DEBUG( dbgs() << "Found external call: " );
              } else {
                internalCalls.insert(call);
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
            
            allocas.insert(alloca);
            DEBUG( dbgs() << "Found alloca: "; alloca->print(dbgs()); dbgs() << "\n" );
            
          } else if ( StoreInst *store = dyn_cast< StoreInst >(&inst) ) {
            
            if (dyn_cast<Argument>(store->getValueOperand())) {
              DEBUG( dbgs() << "Skipping store which reads function argument: "; store->print(dbgs()); dbgs() << "\n" );
              continue;
            } 
            
            stores.insert(store);
            DEBUG( dbgs() << "Found store: "; store->print(dbgs()); dbgs() << "\n" );
            
          } else if ( LoadInst *load = dyn_cast< LoadInst >(&inst) ) {            
            
            loads.insert(load);
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
  };
}
  
char WebCL::ClampPointers::ID = 0;
static RegisterPass<WebCL::ClampPointers> 
X("clamp-pointers", "Adds dynamic checks to prevent accessing memory outside of allocated area.", 
  false, false);

