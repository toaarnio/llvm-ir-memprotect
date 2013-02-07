/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The Original Contributor of this Source Code Form is Nokia Research
 * Center Tampere (http://webcl.nokiaresearch.com).
 */

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
#include "llvm/Transforms/Utils/BasicBlockUtils.h"

#include <vector>
#include <map>
#include <set>
#include <iostream>
#include <cstdio>

// #define STRICT_CHECKS 1

#if STRICT_CHECKS == 0
#undef STRICT_CHECKS
#endif

#define UNUSED( x ) \
  (void)x;

#define fast_assert( condition, message ) do {                       \
    if ( condition == false ) {                                      \
      dbgs() << "\nOn line: " << __LINE__ << " " << message << "\n"; \
      exit(1);                                                       \
    }                                                                \
  } while(0)

using namespace llvm;

/**
 * LLVM 3.2 didn't support getAsInstruction() yet
 * so for now we have copypasted it from trunk
 */
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
    return CastInst::Create((Instruction::CastOps)expr->getOpcode(),
                            Ops[0], expr->getType());
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
    return CmpInst::Create((Instruction::OtherOps)expr->getOpcode(),
                           expr->getPredicate(), Ops[0], Ops[1]);
    
  default:
    assert(expr->getNumOperands() == 2 && "Must be binary operator?");
    BinaryOperator *BO =
      BinaryOperator::Create((Instruction::BinaryOps)expr->getOpcode(),
                             Ops[0], Ops[1]);
    if (isa<OverflowingBinaryOperator>(BO)) {
      assert(false && "Not supported hopefully never needed until llvm 3.3 is out.");
      //BO->setHasNoUnsignedWrap(expr->SubclassOptionalData &
      //                         OverflowingBinaryOperator::NoUnsignedWrap);
      //BO->setHasNoSignedWrap(expr->SubclassOptionalData &
      //                       OverflowingBinaryOperator::NoSignedWrap);
    }
    if (isa<PossiblyExactOperator>(BO)) {
      assert(false && "Not supported hopefully never needed until llvm 3.3 is out.");
      // BO->setIsExact(expr->SubclassOptionalData & PossiblyExactOperator::IsExact);
    }
    return BO;
  }
}

namespace WebCL {

  /// Module pass that implements algorithm for restricting memory
  /// accesses to locally reserved addresses.  
  ///
  /// TODO: add detailed description what is done...
  /// 

  struct ClampPointers :
    public ModulePass {
    static char ID;

    ClampPointers() :
      ModulePass( ID ) {
    }

    /// An allocation of contiguous array of memory.
    struct SmartPointer {
      SmartPointer( Value* _current, Value* _min, Value* _max, Value* _smart, Value* _smart_ptr ) :
        cur( _current ),
        min( _min ),
        max( _max ),
        smart( _smart ),
        smart_ptr( _smart_ptr ) {
      }

      Value* cur; // Used only to be able to pass pointer value with limits to function call
      Value* min; // First valid address
      Value* max; // Last valid address (not last  valid + 1)
      Value* smart;
      Value* smart_ptr;
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
    typedef std::map< Value*, SmartPointer* > SmartPointerByValueMap;

    /// Helper function for generating a single-index GEP instruction from a value.
    GetElementPtrInst* generateGEP( LLVMContext& ctx, Value* ptr, int a, Instruction* i, Twine t = "" ) {
      Twine name = t;
      ConstantInt* c_0 = ConstantInt::get( Type::getInt32Ty(ctx), a );
      std::vector< Value* > values;
      values.push_back( c_0 );
      ArrayRef< Value* > ref( values );
      GetElementPtrInst* gep = GetElementPtrInst::Create( ptr, ref, name, i );
      return gep;
    }

    /// Helper function for generating a two-index GEP instruction from a value.
    GetElementPtrInst* generateGEP( LLVMContext& c, Value* ptr, int a, int b, Instruction* i, Twine t = "" ) {
      Twine name = t;
      ConstantInt* c_0 = ConstantInt::get( Type::getInt32Ty(c), a );
      ConstantInt* c_1 = ConstantInt::get( Type::getInt32Ty(c), b );
      std::vector< Value* > values;
      values.push_back( c_0 );
      values.push_back( c_1 );
      ArrayRef< Value* > ref( values );
      GetElementPtrInst* gep = GetElementPtrInst::Create( ptr, ref, name, i );
      return gep;
    }

    /// Helper function for generating smart pointer struct pointer type
    Type* getSmartPointerType( LLVMContext& c, Type* t ) {
      StructType* s = this->getSmartStructType( c, t );
      Type* smart_array_pointer_type = PointerType::getUnqual( s );
      return smart_array_pointer_type;
    }

    /// Helper function for generating smart pointer struct type
    StructType* getSmartStructType( LLVMContext& c, Type* t ) {
      std::vector< Type* > types;
      types.push_back( t );
      types.push_back( t );
      types.push_back( t );

      ArrayRef< Type* > tref( types );

      StructType *smart_array_struct = StructType::get( c, tref );
      assert( smart_array_struct );
      return smart_array_struct;
    }

    /**
     * First analyze original code and then do transformations, 
     * until code is again runnable with using smart pointers.
     * 
     * Then analyse uses of smart pointers to get limits for them and 
     * add boundary checks.
     */
    virtual bool runOnModule( Module &M ) {
      
      // map of functions which has been replaced with new ones
      FunctionMap replacedFunctions;
      // map of function arguments, which are replaced with new ones
      ArgumentMap replacedArguments;
      
      // set of values, which won't need boundary checks to memory accesses
      ValueSet safeExceptions;
      
      // set of different interesting instructions in the program
      // maybe could be just one map of sets sorted by opcode
      CallInstrSet calls;
      AllocaInstrSet allocas;
      StoreInstrSet stores;
      LoadInstrSet loads;
      
      // smartpointers sorted by value e.g. i32* %a ->  {i32*, i32*, i32*}* %safe_a
      SmartPointerByValueMap smartPointers;

      // we can create smart pointers only for local allocas
      DEBUG( dbgs() << "\n --------------- NORMALIZE GLOBAL VARIABLE USES --------------\n" );
      normalizeGlobalVariableUses( M );

      // ####### Analyze all functions
      for( Module::iterator i = M.begin(); i != M.end(); ++i ) {

        // allow calling external functions with original signatures (this pass should be ran just for fully linked code)
        if ( i->isIntrinsic() || i->isDeclaration() ) {
#ifndef STRICT_CHECKS
          DEBUG( dbgs() << "Skipping: " << i->getName() << " which is intrinsic and/or declaration\n" );
          continue;
#else
           dbgs() << "Found: " << i->getName() << " which is intrinsic and/or declaration\n";
           fast_assert(false, "Calling external functions is not allowed in strict mode. Also intrinsics should be lowered before runnin pass.");
#endif
        }

        // some optimizations causes removal of passing argument %arg to %arg.addr alloca
        // this construct is needed by later phases, so we need to make sure that arguments are fine
        // before starting... has something to do with nocapture argument attribute...
        DEBUG( dbgs() << "\n --------------- NORMALIZE ARGUMENT PASSING --------------\n" );
        normalizeArgumentPassing( i );

        // actually this should not touch functions yet at all, just collect data which functions needs to be changed
        // now it still creates new function signatures and steals old function's name
        DEBUG( dbgs() << "\n --------------- CREATING NEW FUNCTION SIGNATURE --------------\n" );
        createNewFunctionSignature( i, replacedFunctions, replacedArguments );

        DEBUG( dbgs() << "\n --------------- FINDING INTERESTING INSTRUCTIONS --------------\n" );
        sortInstructions( i,  calls, allocas, stores, loads );
      }


      // add smart allocas and generate initial smart pointer data
      DEBUG( dbgs() << "\n --------------- CREATING SMART ALLOCAS FOR EVERYONE --------------\n" );
      createSmartAllocas( allocas, smartPointers );

      // some extra hint of debug...
      /*
      for (FunctionMap::iterator i = replacedFunctions.begin(); i != replacedFunctions.end(); i++) {
        // loop through arguments and if type has changed, then create label to access original arg 
        i->first->print(dbgs());
        dbgs() << "\n";
      }
      */

      // TODO: create smart pointer initializations for kernel function where we expect (int*, int) input

      DEBUG( dbgs() << "\n ---------- FIXING SMART POINTER STORE OPERATIONS TO ALSO KEEP .Cur, .First and .Last UP-TO-DATE ------\n" );
      fixStoreInstructions(stores, smartPointers);

      DEBUG( dbgs() << "\n ----------- CONVERTING OLD FUNCTIONS TO NEW ONES AND FIXING SMART POINTER ARGUMENT PASSING  ----------\n" );
      // gets rid of old functions, replaces calls to old functions and fix call arguments to 
      // use smart pointers in call parameters 
      moveOldFunctionImplementationsToNewSignatures(replacedFunctions, replacedArguments, smartPointers);

      DEBUG( dbgs() << "\n --------------- FIX CALLS TO USE NEW SIGNATURES --------------\n" );
      fixCallsToUseChangedSignatures(replacedFunctions, replacedArguments, calls, smartPointers);

      // ##########################################################################################
      // #### At this point code should be again perfectly executable and runs with new function 
      // #### signatures and has cahnged pointers to smart ones
      // ##########################################################################################

      // expand smart pointers to be able to find limits for uses of smart pointers (loads and geps mostly)
      DEBUG( dbgs() << "\n ------- EXPANDING SMARTPOINTER MAP TO ALSO CONTAIN LIMITS FOR USES OF ORIGINAL POINTERS -----------\n" );
      expandSmartPointersResolvingToCoverUses(smartPointers);

      DEBUG( dbgs() << "\n --------------- ANALYZING CODE TO FIND SPECIAL CASES WHERE CHECKS ARE NOT NEEDED --------------\n" );
      collectSafeExceptions(replacedFunctions, safeExceptions);

      DEBUG( dbgs() << "\n --------------- ADDING BOUNDARY CHECKS --------------\n" );
      addBoundaryChecks(stores, loads, smartPointers, safeExceptions);

      /*
      for (FunctionMap::iterator i = replacedFunctions.begin(); i != replacedFunctions.end(); i++) {
        // loop through arguments and if type has changed, then create label to access original arg 
        i->second->print(dbgs());
        dbgs() << "\n";
      }
      */

      return true;
    }
    
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
     * Collects values, which can be handled without modifying.
     * 
     * e.g. main function arguments (int8** is not currently supported 
     * and won't be in the first place).
     *
     * Note: this is quite dirty symbol name based hack...
     */
    void collectSafeExceptions(FunctionMap &replacedFunctions, ValueSet &safeExceptions) {
#ifndef STRICT_CHECKS
      for ( FunctionMap::iterator i = replacedFunctions.begin(); i != replacedFunctions.end(); i++)  {
        Function *check = i->second;
        if (check->getName() == "main") {
          for( Function::arg_iterator a = check->arg_begin(); a != check->arg_end(); ++a ) {
            Argument* arg = a;
            if (arg->getName() == "argv") {
              resolveArgvUses(arg, safeExceptions);
            }
          }
        }
      }
#else
      DEBUG( dbgs() << "Skipping allowance to use int main(argc, argv) arguments freely. \n" );
#endif
    }

    /**
     * Pass globals always through local variables.
     *
     * To be able to create SmartPointers with limits for global variables, we need to 
     * pass them first through local variables.
     * 
     * NOTE: this is slow, hopefulle post optimizations fixes the overhead and 
     *       avoid using globals
     */ 
    void normalizeGlobalVariableUses(Module &M) {
      for (Module::global_iterator g = M.global_begin(); g != M.global_end(); g++) {
#ifdef STRICT_CHECKS
        if (! g->isDeclaration() ) {
          dbgs() << "Global variables are not supported in strict mode: "; g->print(dbgs()); dbgs() << "\n";
          fast_assert( false, "Global/external variables are not supported in strict mode.");
        }
#endif
        DEBUG( dbgs() << "Found global: "; g->print(dbgs()); dbgs() << "\n" );
        if (!g->hasExternalLinkage()) {
          DEBUG( dbgs() << "-- It is known global var. We should be able to resolve limits when used. \n" );
          continue;
        }

        int id = 0;
        for( Value::use_iterator i = g->use_begin(); i != g->use_end(); ++i ) {
          DEBUG( dbgs() << "Found use: "; i->print(dbgs()); dbgs() << " : " );

          Instruction *useAsInst = dyn_cast<Instruction>(*i);
          
          // skipping does not mean that stuff is always fine... it just means that
          // we don't know how to handle the use and if it is bad, compilation will abort later
          if (!useAsInst || dyn_cast<StoreInst>(useAsInst) || dyn_cast<LoadInst>(useAsInst)) {
            DEBUG( dbgs() << "## skipping\n" );
            continue;
          }
          
          DEBUG( dbgs() << "## fixing\n" );

          id++;
          char postfix_buf[64];
          sprintf(postfix_buf, "%d", id);
          std::string postfix = postfix_buf;

          // before each use copy it to local variable
          AllocaInst *alloca = new AllocaInst(g->getType(), g->getName() + ".use.alloca." + postfix, useAsInst);
          new StoreInst(g, alloca, useAsInst);
          LoadInst *load = new LoadInst(alloca, "", useAsInst);
          i->replaceUsesOfWith(g, load);
          DEBUG( dbgs() << "with: "; i->print(dbgs()); dbgs() << "\n" );
        }
      }
    }


    /**
     * Makes sure that each function argument has structure: 
     * 
     * NOTE: This must be run, before starting ClampPointers pass... could be 
     * created as separate pass.
     * 
     * define i32 @foo(i32* %local_alloca) {
     * entry:
     *   %local_alloca.addr = alloca i32*, align 8* %arg.addr = alloca type
     *   store i32* %local_alloca, i32** %local_alloca.addr
     *   %first_use = load i32* %local_alloca.addr
     *
     * opt -O0 has this kind of entries already.
     */ 
    void normalizeArgumentPassing(Function *F) {
      for( Function::arg_iterator a = F->arg_begin(); a != F->arg_end(); ++a ) {
        Argument* arg = a;
        DEBUG( dbgs() << "Checking argument: "; arg->print(dbgs()); dbgs() << " : " );
        if ( arg->hasOneUse() ) {
          if ( StoreInst *store = dyn_cast<StoreInst>(arg->use_back()) ) {
            if ( AllocaInst *alloca = dyn_cast<AllocaInst>(store->getPointerOperand()) ) {
              // ok, this argument looks fine.
              DEBUG( dbgs() << "found alloca: "; alloca->print(dbgs()); dbgs() << "\n" );
              continue;
            }
          }
        }
        
        DEBUG( dbgs() << "Creating entry code: " );
        Instruction &entryPoint = arg->getParent()->getEntryBlock().front();
        AllocaInst *argAlloca = new AllocaInst(arg->getType(), arg->getName() + ".addr", &entryPoint);
        DEBUG( argAlloca->print(dbgs()); dbgs() << " original arg: "; arg->print(dbgs()); dbgs() << "\n" );  
        LoadInst *load = new LoadInst(argAlloca, "", &entryPoint);
        arg->replaceAllUsesWith(load);
        new StoreInst(arg, argAlloca, load);
      }
    }


    /**
     * Checks if store stores data to smart pointer and updates also smart pointer accordingly.
     */
    void addBoundaryChecks(StoreInstrSet &stores, LoadInstrSet &loads, SmartPointerByValueMap &smartPointers, ValueSet &safeExceptions) {
      // check load instructions... 
      for (LoadInstrSet::iterator i = loads.begin(); i != loads.end(); i++) {
        addChecks((*i)->getPointerOperand(), *i, smartPointers, safeExceptions);
      }   
      // check store instructions
      for (StoreInstrSet::iterator i = stores.begin(); i != stores.end(); i++) {
        addChecks((*i)->getPointerOperand(), *i, smartPointers, safeExceptions);
      }
    }

    /**
     * If val touching pointer operand needs checks, then inject boundary check code.
     */
    void addChecks(Value *ptrOperand, Instruction *inst, SmartPointerByValueMap &smartPointers, ValueSet &safeExceptions) {
      
      // TODO: maybe all of these cases could be added directly to safeExceptions map...
      if (safeExceptions.count(ptrOperand)) {
        DEBUG( dbgs() << "Skipping op that was listed in safe exceptions: "; inst->print(dbgs()); dbgs() << "\n" );        
        return;
      } else if ( dyn_cast<AllocaInst>(ptrOperand) ) {
        DEBUG( dbgs() << "Skipping direct alloca op: "; inst->print(dbgs()); dbgs() << "\n" );
        return;
      
      } else if ( dyn_cast<GlobalValue>(ptrOperand) ) {
        DEBUG( dbgs() << "Skipping direct load from global value: "; inst->print(dbgs()); dbgs() << "\n" );
        return;
        
      } else if ( ConstantExpr *constGep = dyn_cast<ConstantExpr>(ptrOperand) ) {        
        Instruction* inst = getAsInstruction(constGep);
        bool isGep = false;
        bool isInBounds = false;
        
        if ( GetElementPtrInst *gep = dyn_cast<GetElementPtrInst>(inst) ) {
          isGep = true;
          isInBounds = gep->isInBounds();
          // if we refer non array global / external variable, we assume that we have to refer always to 0 element
          if ( !gep->getPointerOperand()->getType()->getSequentialElementType()->isArrayTy() ) {
            isInBounds = gep->hasAllZeroIndices();
          }
        }
        
        delete inst;
        
        if (isGep) {
          if (!isInBounds) {
            dbgs() << "Cannot verify that expression is in limits: "; inst->print(dbgs());
            fast_assert(isInBounds, "Constant expression out of bounds");
          }
          DEBUG( dbgs() << "Skipping constant expression, which is in limits: "; inst->print(dbgs()); dbgs() << "\n" );
          return;
        }
      }
 
      // find out which limits load has to respect and add boundary checks
      if ( smartPointers.count(ptrOperand) == 0 ) {
        // TODO: add recognition for loading data from global table... test case try to clamp optimized version of performace test sources.
        dbgs() << "When verifying: "; inst->print(dbgs()); dbgs() << "\n";
        fast_assert(false, "Could not find limits to create protection!");
      }

      SmartPointer *limits = smartPointers[ptrOperand];
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
     *   %1 = load i32** %some_lable.Smart.Last
     *   %2 = icmp ugt i32* %0, %1
     *   br i1 %2, label %boundary.check.fail, label %check.first.limit
     * check.first.limit:      
     *   %3 = load i32** %some_label.Smart.First
     *   %4 = icmp ult i32* %0, %3
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
    void createLimitCheck(Value *ptr, SmartPointer *limits, Instruction *meminst) {
      static int id = 0;
      id++;
      char postfix_buf[64];
      if ( dyn_cast<LoadInst>(meminst) ) {
        sprintf(postfix_buf, "load.%d", id);
      } else {
        sprintf(postfix_buf, "store.%d", id);
      }
      std::string postfix = postfix_buf;

      BasicBlock *BB = meminst->getParent();
      Function *F = BB->getParent();
      LLVMContext& c = F->getContext();

      // ------ this block is destination of all places where limit check fails, needs unconditional just branch to if.end block
      BasicBlock* boundary_fail_block = BasicBlock::Create( c, "boundary.check.failed." + postfix, F );
      IRBuilder<> boundary_fail_builder( boundary_fail_block );

      // ------ block for minimum value check
      BasicBlock* check_first_block = BasicBlock::Create( c, "check.first.limit." + postfix, F );
      IRBuilder<> check_first_builder( check_first_block );
      
      // ------ add max boundary check code 

      // *   %1 = load i32** %some_lable.Smart.Last
      LoadInst* last_val = new LoadInst( limits->max, "", meminst );
      // *   %2 = icmp ugt i32* %0, %1
      ICmpInst* cmp = new ICmpInst( meminst, CmpInst::ICMP_UGT, ptr, last_val, "" );
      // *   br i1 %2, label %boundary.check.failed, label %check.first.limit
      BranchInst::Create( boundary_fail_block, check_first_block, cmp, meminst );

      // ------ break current BB to 3 parts, start, boundary_check_ok and if_end (meminst is left in ok block)

      // ------ this block actually contains the load/store instruction and branch to if.end block
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
      // *   %3 = load i32** %some_label.Smart.First
      LoadInst* first_val = new LoadInst( limits->min, "", check_first_block );
      // *   %4 = icmp ult i32* %0, %3
      ICmpInst* cmp2 = new ICmpInst( *check_first_block, CmpInst::ICMP_ULT, ptr, first_val, "" );
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

      DEBUG( dbgs() << "Created boundary check for: "; meminst->print(dbgs()); dbgs() << "\n" );
    }
    
    void expandSmartPointersResolvingToCoverUses(SmartPointerByValueMap &smartPointers) {
      SmartPointerByValueMap derivedUses;
      for (SmartPointerByValueMap::iterator i = smartPointers.begin(); i != smartPointers.end(); i++) {
        resolveUses(i->first, i->second, derivedUses);
      }
      
      // expand smartPointers with derivedUses
      for (SmartPointerByValueMap::iterator i = derivedUses.begin(); i != derivedUses.end(); i++) {
        smartPointers[i->first] = i->second;
      }      
    }
    
    /**
     * Traverses through uses of safe pointer and adds limits to derivedUses map.
     *
     * TODO: better description how uses derived uses are collected. e.g. some kind of tree...
     *
     */
    void resolveUses(Value *val, SmartPointer *limits, SmartPointerByValueMap &derivedUses) {
      for( Value::use_iterator i = val->use_begin(); i != val->use_end(); ++i ) {
        Value *use = *i;
        
        // ----- continue to next use if cannot be sure about the limits
        if ( dyn_cast<GetElementPtrInst>(use) ) {
          // all good for GEPs
        } else if ( LoadInst *load = dyn_cast<LoadInst>(use) ) {
          // we cannot be sure about limits, if load is not directly from alloca
          if ( ! dyn_cast<AllocaInst>(load->getPointerOperand()) ) {
            continue;
          }
        } else if ( dyn_cast<StoreInst>(use) ) {
          // never care about stores... they does not return anything
          continue;
        } else { 
          // notify about unexpected cannot be resolved cases for debug
          DEBUG( dbgs() << "Cannot resolve limit for: "; use->print(dbgs()); dbgs() << "\n" );
          continue;
        }

        DEBUG( dbgs() << "Use: "; use->print(dbgs()); dbgs() << " respects limits of: "; limits->smart->print(dbgs()); dbgs() << "\n" );
        derivedUses[use] = limits;
        resolveUses(use, limits, derivedUses);                
      }
    }

    /**
     * Traces recursively up in SSA tree until finds element, which has information about limits.
     *
     * Currently stops if alloca or global value is found or a load.
     *
     * NOTE: this function does not feel safe enough... approach should be validated or reimplemented
     * 
     * @param trashcan When one gets getAsInstruction from constant expression, it creates dangling
     *   instruction. Dangling instructions will crash the pass on exit, so we add created instruction
     *   before trashcan instruction and let later optimization passes clean these up.
     */
    Value* findLimitingFactor(Value *op, Instruction *trashcan) {
      if (dyn_cast<AllocaInst>(op) || dyn_cast<GlobalValue>(op)) {
        return op;

      } else if (GetElementPtrInst *gep = dyn_cast<GetElementPtrInst>(op)) {
        return findLimitingFactor(gep->getPointerOperand(), trashcan);

      } else if (ConstantExpr *constExp = dyn_cast<ConstantExpr>(op)) {

        constExp->isGEPWithNoNotionalOverIndexing();
        Instruction *newInst = getAsInstruction(constExp);
        newInst->insertBefore(trashcan);
        return findLimitingFactor(newInst, trashcan);

      } else if (LoadInst *load = dyn_cast<LoadInst>(op)) {

        // we are finding limiting factor for store to smart pointer, so we can accept that safe pointer of load can be it.
        return load;

      } else {
        dbgs() << "Handling value: "; op->print(dbgs()); dbgs() << "\n";
        dyn_cast<Constant>(op)->print(dbgs()); dbgs() << "\n";
        fast_assert(false, "Don't know how to trace limiting operand for this structure.");
      }
    }

    /**
     * Checks if store stores data to smart pointer and updates also smart pointer accordingly.
     * 
     * NOTE: maybe there should be better way to do this than used here... or generic version...
     */
    void fixStoreInstructions(StoreInstrSet &stores, SmartPointerByValueMap &smartPointers) {
      
      for (StoreInstrSet::iterator i = stores.begin(); i != stores.end(); i++) {
        StoreInst* store = *i;        
        LLVMContext& c = store->getParent()->getContext();
        
        Value *src = store->getValueOperand();
        Value *dest = store->getPointerOperand();
        
        if ( dest->getType()->isPointerTy() && dyn_cast<PointerType>(dest->getType())->getElementType()->isPointerTy() ) {

          DEBUG( dbgs() << "Found store to fix: "; store->print(dbgs()); dbgs() << "\n" );
          DEBUG( src->print(dbgs()); dbgs() << "\n" );

          fast_assert (smartPointers.count(dest) > 0, "Cannot find smart pointer for destination");
          SmartPointer *smartDest = smartPointers[dest];
 
          Value* limits = findLimitingFactor(src, store->getParent()->begin());
          Value* first = NULL;
          Value* last = NULL;

          if (smartPointers.count(limits) > 0) {

            SmartPointer *smartSrc = smartPointers[limits];
            LoadInst *firstLoad = new LoadInst(smartSrc->min);
            LoadInst *lastLoad = new LoadInst(smartSrc->max);
            firstLoad->insertBefore(store);
            lastLoad->insertBefore(store);
            last = lastLoad;
            first = firstLoad;

          } else if ( LoadInst *load = dyn_cast<LoadInst>(limits) ) {

            // external or global src of load... local ones has smart pointers created
            if ( !dyn_cast<GlobalValue>(load->getPointerOperand()) || 
                 !dyn_cast<GlobalValue>(load->getPointerOperand())->hasExternalLinkage() ) {
              dbgs() << "Unexpected load selected to be limiting factor: "; load->print(dbgs()); dbgs() << "\n";
              fast_assert(false, "Unexpected load instruction");
            }

            first = load;
            last = load;
            
          } else if ( GlobalValue *globalVal = dyn_cast<GlobalValue>(limits) ) {
            DEBUG( dbgs() << "Handling global: "; globalVal->print(dbgs()); dbgs() << " : " );
            
            // if array type of global
            if (globalVal->getType()->getElementType()->isArrayTy()) {
              DEBUG( dbgs() << "applying array limits.\n" );
              int array_size = dyn_cast<ArrayType>(globalVal->getType()->getElementType())->getNumElements();
              first = generateGEP(c, globalVal, 0, 0, store, "");
              last = generateGEP(c, globalVal, 0, array_size-1, store, "");

            } else if (globalVal->getType()->getElementType()->isStructTy()) {
              dbgs() << "While handling: "; globalVal->print(dbgs()); dbgs() << "\n";
              fast_assert(false, "Getting limits from struct type is not implemented.");

            } else {
              first = globalVal;
              last = globalVal;
            }

          } else {
            fast_assert(false, "Could not resolve limits from source operand, for smart pointer assignment.");
          }
          
          // fix limits and cur in smart pointer assignment
          DEBUG( dbgs() << "#### FOUND LIMITS:\n" );
          DEBUG( first->print(dbgs()); dbgs() << " type: "; first->getType()->print(dbgs()); 
                 dbgs() << " ---> "; 
                 smartDest->min->print(dbgs()); dbgs() << " type: "; smartDest->min->getType()->print(dbgs()); dbgs() << "\n" );
          DEBUG( last->print(dbgs()); dbgs() << " type: "; last->getType()->print(dbgs()); 
                 dbgs() << " ---> "; 
                 smartDest->max->print(dbgs()); dbgs() << " type: "; smartDest->max->getType()->print(dbgs()); dbgs() << "\n" );

          StoreInst* firstStore = new StoreInst(first, smartDest->min);
          StoreInst* lastStore = new StoreInst(last, smartDest->max);
          StoreInst* curStore = new StoreInst(store->getValueOperand(), smartDest->cur);          

          lastStore->insertAfter(store);
          firstStore->insertAfter(store);
          curStore->insertAfter(store);

          DEBUG( dbgs() << "-- Created smart store:\n" );
          DEBUG( store->print(dbgs()); dbgs() << "\n" );
          DEBUG( curStore->print(dbgs()); dbgs() << "\n" );
          DEBUG( firstStore->print(dbgs()); dbgs() << "\n" );
          DEBUG( lastStore->print(dbgs()); dbgs() << "\n" );
        }
      }
    }
    
    /**
     * Creates smart pointer, before given alloca.
     *
     * For e.g. an alloca: 
     *
     * %int_table = alloca [8 x i32]
     *
     * Generate smart pointer stuff:
     * 
     * %int_table.Smart = alloca { i32*, i32*, i32* }
     * %int_table.SmartPtr = alloca { i32*, i32*, i32* }*
     * store { i32*, i32*, i32* }* %int_table.Smart, { i32*, i32*, i32* }** %int_table.SmartPtr
     * %int_table.Smart.Cur = getelementptr { i32*, i32*, i32* }* %int_table.Smart, i32 0, i32 0
     * %int_table.Smart.First = getelementptr { i32*, i32*, i32* }* %int_table.Smart, i32 0, i32 1
     * %int_table.Smart.Last = getelementptr { i32*, i32*, i32* }* %int_table.Smart, i32 0, i32 2
     * %int_table = alloca [8 x i32], align 16
     * %int_table.Last = getelementptr [8 x i32]* %int_table, i32 0, i32 7 ( this is given in parameter for function )
     * store i32* %int_table.Last, i32** %int_table.Smart.Last
     * %int_table.First = getelementptr [8 x i32]* %int_table, i32 0, i32 0 ( this is given in parameter for function )
     * store i32* %int_table.First, i32** %int_table.Smart.First
     * store i32* %int_table.First, i32** %int_table.Smart.Cur
     * 
     * @param elementType Type which is used to generate smart pointer internal elements.
     */
    SmartPointer* createSmartPointer(Type* elementType, AllocaInst* alloca, Instruction* first, Instruction* last) {

          std::vector< Type* > types;
          types.push_back( elementType );
          types.push_back( elementType );
          types.push_back( elementType );
          ArrayRef< Type* > tref( types );

          LLVMContext& c = alloca->getParent()->getContext();
          StringRef allocaName = alloca->getName();

          StructType *smart_ptr_struct_type = StructType::get( c, tref );
          assert( smart_ptr_struct_type );
          // I'm sorry.. 
          Type* smart_ptr_struct_ptr_type = PointerType::getUnqual( smart_ptr_struct_type );

          Twine name_data = allocaName + ".Smart";
          AllocaInst* smart_ptr_struct_alloca = new AllocaInst( smart_ptr_struct_type, 0, name_data, alloca );
          Twine name_ptr = allocaName + ".SmartPtr";
          AllocaInst* smart_ptr_struct_ptr_alloca = new AllocaInst( smart_ptr_struct_ptr_type, 0, name_ptr, alloca );
          StoreInst* smart_ptr_struct_ptr_store = new StoreInst( smart_ptr_struct_alloca, smart_ptr_struct_ptr_alloca );
          smart_ptr_struct_ptr_store->insertAfter( smart_ptr_struct_ptr_alloca );
          
          Twine nameCur = Twine( "" ) + allocaName + ".Smart.Cur";
          Twine nameFirst = Twine( "" ) + allocaName + ".Smart.First";
          Twine nameLast = Twine( "" ) + allocaName + ".Smart.Last";
          GetElementPtrInst* last_gep = generateGEP( c, smart_ptr_struct_alloca, 0, 2, alloca, nameLast );
          GetElementPtrInst* first_gep = generateGEP( c, smart_ptr_struct_alloca, 0, 1, alloca, nameFirst );
          GetElementPtrInst* cur_gep = generateGEP( c, smart_ptr_struct_alloca, 0, 0, alloca, nameCur );
                    
          SmartPointer* s = new SmartPointer( cur_gep, first_gep, last_gep, smart_ptr_struct_alloca, smart_ptr_struct_ptr_alloca );

          StoreInst* init_cur_store = new StoreInst( first, cur_gep );
          StoreInst* init_first_store = new StoreInst( first, first_gep );
          StoreInst* init_last_store = new StoreInst( last, last_gep );
          
          init_last_store->insertAfter( last );
          init_first_store->insertAfter( last );
          init_cur_store->insertAfter( last );

          return s;
    }

    /**
     * Find all allocas that can be converted to smart pointers and add them to function
     * 
     * TODO: Generate safe pointers for everything
     * 
     * NOTE: replacing original alloca with smart.Cur everywhere is bad idea, dont do it.
     * 
     * e.g. 
     *   %0 = gep %smart.Cur, 0, 4
     *   call %0
     *
     * Would mean that we have to create copy of the smart pointer to pass it to function to 
     * maintain previous functionality
     */
    void createSmartAllocas(AllocaInstrSet &allocas, SmartPointerByValueMap &smartPointers) {
      
      for (AllocaInstrSet::iterator i = allocas.begin(); i != allocas.end(); i++) {

        AllocaInst* alloca = *i;
        
        LLVMContext& c = alloca->getParent()->getContext();
        
        Type* t = alloca->getAllocatedType();

        DEBUG( dbgs() << "Creating smart pointer structures for: "; alloca->print(dbgs()); dbgs() << " ---- " );

        // Treat array alloca special way, otherwise just treat pointer allocas.....
        Type *ptrType = NULL;
        Instruction *first = NULL;
        Instruction *last = NULL;

        if( ArrayType* a = dyn_cast< ArrayType >( t ) ) {
          DEBUG( dbgs() << "It's an array!\n" );

          Type* element_type = a->getElementType();
          fast_assert( ( element_type->isIntegerTy() || element_type->isFloatingPointTy() ),
                       "Currently pass supports only integer or float arrays.");

          unsigned int array_size = a->getArrayNumElements();

          ptrType = PointerType::getUnqual( element_type );
          first = generateGEP(c, alloca, 0, 0, 0, "");
          last = generateGEP(c, alloca, 0, array_size - 1, 0, "");
          last->insertAfter(alloca);
          first->insertAfter(alloca);

        } else if (t->isPointerTy()) {

          DEBUG( dbgs() << "It's a pointer!\n" );
          LoadInst *alloca_load = new LoadInst(alloca);
          alloca_load->insertAfter(alloca);
          ptrType = t;
          first = alloca_load;
          last = alloca_load;

        } else if (t->isIntegerTy() || t->isFloatingPointTy()) {

          DEBUG( dbgs() << "It an integer or float!\n" );
          ptrType = PointerType::getUnqual( t );
          first = alloca;
          last = alloca;
          
        } else {
          DEBUG( dbgs() << "It is an unhandled type, some day we need to implement this to make system work correctly!\n" );
        }

        if (ptrType) {
          SmartPointer* newSmartPointer = createSmartPointer(ptrType, alloca, first, last);
          smartPointers.insert( std::pair< Value*, SmartPointer* >( alloca, newSmartPointer ) );
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
                                        SmartPointerByValueMap &smartPointers) {

      for (CallInstrSet::iterator i = calls.begin(); i != calls.end(); i++) {
        CallInst *call = *i;

        DEBUG( dbgs() << "---- Started fixing:"; call->print(dbgs()); dbgs() << "\n" );
        
        Function* oldFun = call->getCalledFunction();
        
        if (oldFun->isDeclaration() && replacedFunctions.count(oldFun) == 0) {
          dbgs() << "WARNING: Calling external function, which we cannot guarantee to be safe: "; oldFun->print(dbgs());
#ifdef STRICT_CHECKS
          fast_assert(false, "Aborting since we are in strict mode.");
#endif
          continue;
        }

        Function* newFun = replacedFunctions[oldFun];
        
        call->setCalledFunction(newFun);
        
        // find if function signature changed some Operands and change them to refer smart pointers instead of pointers directly
        int op = 0;
        for( Function::arg_iterator a = oldFun->arg_begin(); a != oldFun->arg_end(); ++a ) {
          Argument* oldArg = a;
          Argument* newArg = replacedArguments[oldArg];

          // this argument type has been changed to smart pointer, find out corresponding smart
          if (oldArg->getType() != newArg->getType()) {
            Value* operand = call->getArgOperand(op);

            DEBUG( dbgs() << "- op #" << op << " needs fixing: "; operand->print(dbgs()); dbgs() << "\n" );

            // !!! TODO !!! We probably should traverse SSA tree further up to find smart pointer
            // in that case we should also update .Cur to oldParam to have calculated 
            // value passed to function. This method could work universally to all cases here...
              
            // use findLimitingElement and it should have smart pointer

            // get load, which actually loads smart pointer from the end of label
            if ( LoadInst* loadToFix = dyn_cast<LoadInst>(operand) ) {
              

              Value* oldParam = loadToFix->getOperand(0);

              // we should not load value from end of pointer, but directly pass smart pointer instead...
              if (smartPointers.count(oldParam) > 0) {
                call->setOperand( op, smartPointers[oldParam]->smart );
              } else {
                dbgs() << "In function: ---- \n";
                call->getParent()->print(dbgs());
                dbgs() << "\n\nError while converting call:\n";
                loadToFix->print(dbgs());
                dbgs() << "\n";
                call->print(dbgs());
                dbgs() << " cannot find smart pointer for op: " << op << "\n"; 
                fast_assert(false, "Cannot find smart pointer for the call operand. Maybe you tried to pass extern variable?");
              }
              
            } else if ( GetElementPtrInst* gepToFix = dyn_cast<GetElementPtrInst>(operand) ) {
              
              // fixing passing parameter got from GEP e.g. passing table
              Value *sourcePointer = gepToFix->getPointerOperand();
              if (smartPointers.count(sourcePointer) == 0 ) {
                // hack for one current special case should be fixed as described in start of function
                if (LoadInst* load = dyn_cast<LoadInst>(sourcePointer)) {
                  sourcePointer = load->getPointerOperand();
                  if (smartPointers.count(sourcePointer) == 0) sourcePointer = NULL;
                } else {
                  sourcePointer = NULL;
                }

                if (!sourcePointer) {
                  dbgs() << "In BB:\n"; gepToFix->getParent()->print(dbgs());
                  dbgs() << "\nConverting: "; gepToFix->getPointerOperand()->print(dbgs()); dbgs() << "\n";
                  fast_assert(false, "Could not find smart pointer for GEP, which should have smart pointer..");
                }
              }
              
              SmartPointer *smartPtr = smartPointers[sourcePointer];
              // update smartPointer .Cur to match the  
              StoreInst *updateCur = new StoreInst(gepToFix, smartPtr->cur);
              updateCur->insertAfter(gepToFix);
              call->setOperand(op, smartPtr->smart);

            } else if ( AllocaInst* allocaSrc = dyn_cast<AllocaInst>(operand) ) {
              
              // value can be also e.g. alloca, which is read from caller function's arguments %param.addr... 
              if (smartPointers.count(allocaSrc) > 0) {
                SmartPointer *smartPtr = smartPointers[allocaSrc];
                call->setOperand(op, smartPtr->smart);
              } else {
                // check if alloca is not integer type lets fail
                dbgs() << "\n\nIn BB:\n";
                call->getParent()->print(dbgs());
                dbgs() << "\n\nError while converting call:\n";
                allocaSrc->print(dbgs());
                dbgs() << "\n";
                call->print(dbgs());
                dbgs() << "\nOld argument type:::\n";
                oldArg->print(dbgs());
                dbgs() << "\nNew argument type:::\n";
                newArg->print(dbgs());
                dbgs() << " cannot find smart pointer for alloca op: " << op << "\n"; 
                fast_assert(false, "Could not find smart pointer for alloca");
              }
              
            } else {

              // NOTE: if we find lots of cases where earlier does not work we could think of adding 
              // normalizing pass which would fix constructs to be suitable for this pass. 
              // In llvm there might be already passes, which may work for doing it.
              dbgs() << "In BB:\n";
              call->getParent()->print(dbgs());
              dbgs() << "\n\nError while converting call:\n";
              call->print(dbgs());
              dbgs() << " not able to find smart pointer for call operand: " << op << "  argument op: ";
              call->getArgOperand(op)->print(dbgs());
              dbgs() << "\n";
              fast_assert(false, "Could not find smart pointer for op for converting call.");

            }
          }
          op++;
        }

        DEBUG( dbgs() << "-- Converted call to : "; call->print(dbgs()); dbgs() << "\n" );
      }
    }
    
    /**
     * Goes through all replaced functions and their arguments.
     * 
     * 1. For each function argument that was converted to smart pointer, fix smart pointer initialization.
     *
     * 2. Go through all call instructions in program. If there has been used replaced safe pointer, fix call so that 
     *    smart pointer is passed instead of normal pointer. And make sure that SmartPtr.Cur in passed struct has updated value.
     *
     * result: after this, old functions and their arguments should be able to be thrown away.
     *
     * TODO: get some example transformation from basic calls test case and split to 2 pieces
     *
     * define int32 foo(i32* %a) {
     * entry: 
     *   %a.addr = alloca i32*
     *   store i32* %a, i32** %a.addr
     *   %0 = im_using %a.addr
     *   %1 = load *i32 %a.addr
     *   %call = tail call i32 @some_func(i32* %1, i32 %0)
     *
     * -- converts to something like --- >
     * 
     * define i32 foo({i32*, i32*, i32*}* %a.Smart) {
     * entry:
     *   %a.addr = alloca i32*
     * 
     *   ; fixing %a.addr initialization and %a.addr.Smart.Cur
     *   %0 = getelementpointer {i32*, i32*, i32*}* %a.Smart, i32 0, i32 0
     *   store i32* %0, i32** %a.addr
     *   store i32* %0, i32** %a.addr.Smart.Cur
     * 
     *   ; fixing %a.addr.Smart.First and %a.addr.Smart.Last values
     *   %1 = getelementpointer {i32*, i32*, i32*}* %a.Smart, i32 0, i32 1
     *   store i32* %1, i32** %a.addr.Smart.First
     *   %2 = getelementpointer {i32*, i32*, i32*}* %a.Smart, i32 0, i32 2
     *   store i32* %2, i32** %a.addr.Smart.Last
     *   
     *   ; maybe we just could replace all uses of original a.addr with a.addr.Smart.Cur
     *   %call = tail call i32 @some_func({i32*,i32*,i32*} %a.Smart, i32 %0)
     * 
     */
    void moveOldFunctionImplementationsToNewSignatures(FunctionMap &replacedFunctions, 
                                                       ArgumentMap &replacedArguments, 
                                                       SmartPointerByValueMap &smartPointers) {
            
      for (FunctionMap::iterator i = replacedFunctions.begin(); i != replacedFunctions.end(); i++) {
        // loop through arguments and if type has changed, then create label to access original arg 
        Function* oldFun = i->first;
        Function* newFun = i->second;
        
        LLVMContext& c = oldFun->getContext();
        
        // move all instructions to new function
        newFun->getBasicBlockList().splice( newFun->begin(), oldFun->getBasicBlockList() );
        BasicBlock &entryBlock = newFun->getEntryBlock();
        newFun->takeName(oldFun);

        // TODO: if old function is kernel, then fix its parameters and name for allowing to call function from outside. 
        //       Internal calls to kernels already has been fixed
 
        DEBUG( dbgs() << "Moved BBs to " << newFun->getName() << "( .... ) and took the final function name.\n" );

        for( Function::arg_iterator a = oldFun->arg_begin(); a != oldFun->arg_end(); ++a ) {
          Argument* oldArg = a;
          Argument* newArg = replacedArguments[a];

          DEBUG( dbgs() << "Fixing arg: "; oldArg->print(dbgs()); dbgs() << " : " );

          newArg->takeName(oldArg);
          oldArg->setName(newArg->getName() + "_replaced_arg");

          // if argument types are not the same we need to find smart pointer that was generated for 
          // argument and create initializations so that existing smart alloca will get correct values
          if (oldArg->getType() == newArg->getType()) {
            DEBUG( dbgs() << "type was not changed. Just replacing oldArg uses with newArg.\n" );
            // argument was not tampered, just replace uses to point the new function
            oldArg->replaceAllUsesWith(newArg);
            continue;
          }

          // create GEPs for initializing smart pointer
          DEBUG( dbgs() << "1 " );
          Twine paramName = Twine("") + newArg->getName() + ".SmartArg";
          newArg->setName(paramName);
            
          // create GEPs for reading .Cur .First and .Last of parameter for initializations
          DEBUG( dbgs() << "2 " );
          GetElementPtrInst* paramLast = generateGEP(c, newArg, 0, 2, entryBlock.begin(), Twine("") + newArg->getName() + ".Last");
          GetElementPtrInst* paramFirst = generateGEP(c, newArg, 0, 1, entryBlock.begin(), Twine("") + newArg->getName() + ".First");
          GetElementPtrInst* paramCur = generateGEP(c, newArg, 0, 0, entryBlock.begin(), Twine("") + newArg->getName() + ".Cur");

          DEBUG( dbgs() << "3 " );
          LoadInst* paramLastVal = new LoadInst(paramLast, Twine("") + paramLast->getName() + ".LoadedVal");
          paramLastVal->insertAfter(paramLast);
          LoadInst* paramFirstVal = new LoadInst(paramFirst, Twine("") + paramFirst->getName() + ".LoadedVal");
          paramFirstVal->insertAfter(paramFirst);
          LoadInst* paramCurVal = new LoadInst(paramCur, Twine("") + paramCur->getName() + ".LoadedVal");
          paramCurVal->insertAfter(paramCur);

          // find smart pointer of the %param.addr alloca which was generated by compiler
          // it should be the only use of the original argument. 
            
          // We need to fix construct:
          // define i32 @foo(i32* %orig_ptr_param) {
          // entry:
          // %orig_ptr_param.addr = alloca i32*
          // store i32* %orig_ptr_param, i32** %orig_ptr_param.addr
          // 
          // To ---------> 
          // define i32 @foo({ i32*, i32*, i32* }* %orig_ptr_param.SmartArg) { 
          // entry:
          // %orig_ptr_param.SmartArg.Cur = getelementptr { i32*, i32*, i32* }* %orig_ptr_param.SmartArg, i32 0, i32 0
          // %orig_ptr_param.SmartArg.Cur.LoadedVal = load i32** %orig_ptr_param.SmartArg.Cur
          // %orig_ptr_param.SmartArg.First = getelementptr { i32*, i32*, i32* }* %orig_ptr_param.SmartArg, i32 0, i32 1
          // %orig_ptr_param.SmartArg.First.LoadedVal = load i32** %orig_ptr_param.SmartArg.First
          // %orig_ptr_param.SmartArg.Last = getelementptr { i32*, i32*, i32* }* %orig_ptr_param.SmartArg, i32 0, i32 2
          // %orig_ptr_param.SmartArg.Last.LoadedVal = load i32** %orig_ptr_param.SmartArg.Last
          // 
          // ; --- some smart pointer initializations for alloca has been done here ---
          // 
          // %orig_ptr_param.addr = alloca i32*
          // store i32* %orig_ptr_param.SmartArg.Cur.LoadedVal, i32** %orig_ptr_param.addr
          // store i32* %orig_ptr_param.SmartArg.Cur.LoadedVal, i32** %orig_ptr_param.addr.Smart.Cur
          // store i32* %orig_ptr_param.SmartArg.First.LoadedVal, i32** %orig_ptr_param.addr.Smart.First
          // store i32* %orig_ptr_param.SmartArg.Last.LoadedVal, i32** %orig_ptr_param.addr.Smart.Last

          fast_assert(oldArg->hasOneUse(), 
                 "Unknown construct where pointer argument has > 1 uses. (expecting just store instruction to %param.addr)");
            
          DEBUG( dbgs() << "4 " );
          if (StoreInst* store = dyn_cast<StoreInst>( (*oldArg->use_begin()) )) {
            AllocaInst *origAddrAlloca = dyn_cast<AllocaInst>(store->getPointerOperand());
            assert(origAddrAlloca);
            
            DEBUG( dbgs() << "5 " );
            StoreInst* newStore = new StoreInst( paramCurVal, origAddrAlloca );
            ReplaceInstWithInst( store, newStore );
              
            // find smart pointer of %param.addr and initialize .Cur, .First and .Last elements
            if (smartPointers.count(origAddrAlloca) == 0) {
              dbgs() << "While handling\n";            
              origAddrAlloca->print(dbgs()); dbgs() << "\n";
              store->print(dbgs()); dbgs() << "\n";
              fast_assert(false, "Could not find smart pointer for alloca.");
            }

            DEBUG( dbgs() << "6 " );
            SmartPointer* smartAlloca = smartPointers[origAddrAlloca];
            StoreInst* initCurStore = new StoreInst( paramCurVal, smartAlloca->cur);
            StoreInst* initFirstStore = new StoreInst( paramFirstVal, smartAlloca->min);
            StoreInst* initLastStore = new StoreInst( paramLastVal, smartAlloca->max);              

            initLastStore->insertAfter(newStore);
            initFirstStore->insertAfter(newStore);
            initCurStore->insertAfter(newStore);

            DEBUG( dbgs() << "done! \n" );

          } else {
            dbgs() << "\n"; oldArg->use_begin()->print(dbgs()); dbgs() << "\n";
            fast_assert(false, 
                        "Unknown construct where pointer argument's use is not StoreInst");
          }
                
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
                   "Handling function retruning array type is not implemented." );
      fast_assert( (!F->isVarArg()), "Variable argument functions are not supported.");

      // check if main or kernel and in that case do not change signature 
      bool dontTouchArguments = false;
      if (F->getName() == "main") {
        dontTouchArguments = true;
      }
      
      // convert function signature to use pointer structs instead of direct pointers
      std::vector< Type* > param_types;
      for( Function::arg_iterator a = F->arg_begin(); a != F->arg_end(); ++a ) {
        Argument* arg = a;
        Type* t = arg->getType();

        // TODO: assert not supported arguments (e.g. some int**, struct etc... or at least verify cases we can allow)
        
        if( !dontTouchArguments && t->isPointerTy() ) {
          Type* smart_array_struct = getSmartPointerType( c, t );
          param_types.push_back( smart_array_struct );          
        } else {
          fast_assert( (!t->isArrayTy()), "Passing array in arguments is not implemented." );
          param_types.push_back( t );
        }
      }

      // creating new function with different prototype 
      FunctionType *function_type = F->getFunctionType();
      FunctionType *new_function_type = FunctionType::get( function_type->getReturnType(), param_types, false );

      Function *new_function = Function::Create( new_function_type, F->getLinkage() );
      new_function->copyAttributesFrom( F );      
      F->getParent()->getFunctionList().insert( F, new_function );
      new_function->setName( F->getName() + "___tobe_replacement___" );
      
      // add new function to book keepig to show what was replaced
      functionMapping.insert( std::pair< Function*, Function* >( F, new_function ) );

      DEBUG( dbgs() << "-- Created new signature for: " << F->getName() << " "; F->getType()->print(dbgs()) );
      DEBUG( dbgs() << "\nnew signature: " << new_function->getName() << " "; new_function->getType()->print(dbgs()); dbgs() << "\n" );

      // map arguments of original function to new replacements
      for( Function::arg_iterator 
             a = F->arg_begin(), 
             E = F->arg_end(), 
             a_new = new_function->arg_begin(); a != E; ++a, ++a_new ) {     
        
        argumentMapping.insert( std::pair< Argument*, Argument* >( a, a_new ) ); 
        DEBUG( dbgs() << "Mapped orig arg: "; a->print(dbgs()); dbgs() << " -----> "; a_new->print(dbgs()); dbgs() << "\n" );
      }

      return new_function;
    }

    virtual void sortInstructions( Function *F, 
                                   CallInstrSet &calls, 
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
              calls.insert(call);
              DEBUG( dbgs() << "Found call: "; call->print(dbgs()); dbgs() << "\n" );
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

  private:
    GlobalVariable* mGlobalNull;

  };
}
  
char WebCL::ClampPointers::ID = 0;
static RegisterPass<WebCL::ClampPointers> X("clamp-pointers", "Safe array accesses using clamp.", false, false);
