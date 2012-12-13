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

#include "llvm/Support/CallSite.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Debug.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"

#include "llvm/IRBuilder.h"

#include <vector>
#include <map>
#include <set>
#include <iostream>
#include <cstdio>

#define UNUSED( x ) \
  (void)x;

using namespace llvm;

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
     * Is there really any cases where this might be needed and how this will 
     * prevent uses after delete anyways, since pointers are passed as copies?
     * Remember that when pointer is passed, passed value is actually copy of pointer's
     * memory address. When smart pointer is passed, the same way copy of min, max and cur
     * pointed pointers values are passed. 
    virtual void freeMemory( Function *F ) {
      FunctionSmartPointerList::iterator sp_source = mFunctionSmartPointers.find( F );
      //assert( sp_source != mFunctionSmartPointers.end() );

      if( sp_source == mFunctionSmartPointers.end() ) {
        // no smart pointers to free
        return;
      }

      for( Function::iterator f = F->begin(), ef = F->end(); f != ef; ++f) {
        for( BasicBlock::iterator i = f->begin(), eb = f->end(); i != eb; ++i) {
          if( llvm::AllocaInst* a = dyn_cast< llvm::AllocaInst >(i) ) {
            UNUSED( a );
            //errs() << "RET" << *ret;
          }
        }

        for( BasicBlock::iterator i = f->begin(), eb = f->end(); i != eb; ++i) {
          if( llvm::ReturnInst* ret = dyn_cast< llvm::ReturnInst >(i) ) {
            SmartPointerList* sp_list = sp_source->second;
            assert( sp_list != 0 );

            for( SmartPointerList::iterator i = sp_list->begin(); i != sp_list->end(); ++i ) {
              SmartPointer* sp = *i;

              assert( sp != 0 );

              StoreInst* s_cur = new StoreInst( mGlobalNull, sp->cur );
              s_cur->insertBefore( ret );

              StoreInst* s_min = new StoreInst( mGlobalNull, sp->min );
              s_min->insertBefore( ret );

              StoreInst* s_max = new StoreInst( mGlobalNull, sp->max );
              s_max->insertBefore( ret );

            }
          }
        }
      }
    }
     */

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
      
      // sets for functions and their arguments, whose signatures are ok, even if they contain pointers
      FunctionSet safeFunctions;
      ArgumentSet safeArguments;

      // set of different interesting instructions in the program
      // maybe could be just one map of sets sorted by opcode
      CallInstrSet calls;
      AllocaInstrSet allocas;
      StoreInstrSet stores;
      LoadInstrSet loads;
      
      // smartpointers sorted by value e.g. i32* %a ->  {i32*, i32*, i32*}* %safe_a
      SmartPointerByValueMap smartPointers;

      // Analyze all functions
      for( Module::iterator i = M.begin(); i != M.end(); ++i ) {
        if (i->isIntrinsic()) {
          DEBUG(dbgs() << "Skipping: " << i->getName() << " which is intrinsic\n");
          continue;
        }
        // actually this should not touch functions yet at all, just collect data which functions needs to be changed
        // now it still creates new function signatures and steals old function's name
        DEBUG(dbgs() << "\n --------------- CREATING NEW FUNCTION SIGNATURE --------------\n");
        createNewFunctionSignature( i, replacedFunctions, replacedArguments, safeFunctions, safeArguments );
        DEBUG(dbgs() << "\n --------------- FINDING INTERESTING INSTRUCTIONS --------------\n");
        sortInstructions( i,  calls, allocas, stores, loads );
      }

      // add smart allocas and generate initial smart pointer data
      DEBUG(dbgs() << "\n --------------- CREATING SMART ALLOCAS FOR EVERYONE --------------\n");
      createSmartAllocas(allocas, smartPointers);

      // some extra hint of debug...
      /*
      for (FunctionMap::iterator i = replacedFunctions.begin(); i != replacedFunctions.end(); i++) {
        // loop through arguments and if type has changed, then create label to access original arg 
        i->first->print(dbgs());
        dbgs() << "\n";
      }
      */

      // TODO: create smart pointer initializations for main function where we expect (int count, int8* argv[]) input
      // TODO: create smart pointer initializations for kernel function where we expect (int*, int) input

      DEBUG(dbgs() << "\n --------------- FIXING SMART POINTER STORE OPERATIONS TO ALSO KEEP .Cur, .First and .Last UP-TO-DATE --------------\n");
      fixStoreInstructions(stores, smartPointers);

      DEBUG(dbgs() << "\n --------------- CONVERTING OLD FUNCTIONS TO NEW ONES AND FIXING SMART POINTER ARGUMENT PASSING  --------------\n");
      // gets rid of old functions, replaces calls to old functions and fix call arguments to 
      // use smart pointers in call parameters 
      fixCodeToUseReplacedFunctions(replacedFunctions, replacedArguments, calls, smartPointers);

      // dbgs() << "\n --------------- FIX CALLS TO USE NEW SIGNATURES --------------\n";
      // split last part of fixCodeToUseReplacedFunctions(replacedFunctions, replacedArguments, calls, smartPointers);
      // to be separate step

      // ##########################################################################################
      // #### At this point code should be again perfectly executable and runs with new function 
      // #### signatures and has cahnged pointers to smart ones
      // ##########################################################################################

      // expand smart pointers to be able to find limits for uses of smart pointers (loads and geps mostly)
      DEBUG(dbgs() << "\n --------------- EXPANDING SMARTPOINTER MAP TO ALSO CONTAIN LIMITS FOR USES OF ORIGINAL POINTERS --------------\n");
      expandSmartPointersResolvingToCoverUses(smartPointers);

      DEBUG(dbgs() << "\n --------------- ADDING BOUNDARY CHECKS --------------\n");
      addBoundaryChecks(stores, loads, smartPointers);

      /*
      for (FunctionMap::iterator i = replacedFunctions.begin(); i != replacedFunctions.end(); i++) {
        // loop through arguments and if type has changed, then create label to access original arg 
        i->second->print(dbgs());
        dbgs() << "\n";
      }
      */

      return true;
    }


    /**
     * Checks if store stores data to smart pointer and updates also smart pointer accordingly.
     */
    void addBoundaryChecks(StoreInstrSet &stores, LoadInstrSet &loads, SmartPointerByValueMap &smartPointers) {
      // check load instructions... 
      for (LoadInstrSet::iterator i = loads.begin(); i != loads.end(); i++) {
        addChecks((*i)->getPointerOperand(), *i, smartPointers);
      }   
      // check store instructions
      for (StoreInstrSet::iterator i = stores.begin(); i != stores.end(); i++) {
        addChecks((*i)->getPointerOperand(), *i, smartPointers);
      }
    }

    /**
     * If val touching pointer operand needs checks, then inject boundary check code.
     */
    void addChecks(Value *ptrOperand, Instruction *inst, SmartPointerByValueMap &smartPointers) {
      if ( dyn_cast<AllocaInst>(ptrOperand) ) {
        DEBUG(dbgs() << "Skipping direct alloca op: "; inst->print(dbgs()); dbgs() << "\n");
        return;
      } else if ( dyn_cast<GlobalValue>(ptrOperand) ) {
        DEBUG(dbgs() << "Skipping direct load from global value: "; inst->print(dbgs()); dbgs() << "\n");
        return;
      }

      // find out which limits load has to respect and add boundary checks
      if ( smartPointers.count(ptrOperand) == 0 ) {
        dbgs() << "When verifying: "; inst->print(dbgs()); dbgs() << "\n";
        assert(false && "Could not find limits to create protection!");
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
     *   br i1 %2, label %boundary.check.failed, label %check.first.limit
     * boundary.check.failed:
     *   tail call void @llvm.trap()
     *   unreachable
     * check.first.limit:      
     *   %3 = load i32** %some_label.Smart.First
     *   %4 = icmp ult i32* %0, %3
     *   br i1 %4, label %boundary.check.failed, label %if.end
     * if.end:
     *   %5 = load i32* %0
     * 
     */
    void createLimitCheck(Value *ptr, SmartPointer *limits, Instruction *before) {
      static int id = 0;
      id++;
      char postfix_buf[64];
      sprintf(postfix_buf, "%d", id);
      std::string postfix = postfix_buf;

      BasicBlock *BB = before->getParent();
      Function *F = BB->getParent();
      LLVMContext& c = F->getContext();

      Twine boundary_fail_name = Twine("boundary.check.failed");

      // find exit on fail block
      BasicBlock* boundary_fail_block = NULL;
      for (Function::iterator bbi = F->begin(); bbi != F->end(); bbi++) {
        BasicBlock *bb = &(*bbi); // iterator seems to return reference...
        if (bb->getName() == boundary_fail_name.getSingleStringRef()) {
          boundary_fail_block = bb;
          break;
        }
      }
      
      // fail block not created yet... create it
      if (!boundary_fail_block) {
        DEBUG(dbgs() << "Creating fail block to function: " << F->getName() << "\n");
        boundary_fail_block = BasicBlock::Create( c, boundary_fail_name, F );
        IRBuilder<> boundary_fail_builder( boundary_fail_block );
        Function *trapFn = Intrinsic::getDeclaration(F->getParent(), Intrinsic::trap);
        CallInst *trapCall = CallInst::Create(trapFn, "", boundary_fail_block);
        trapCall->setDebugLoc(before->getDebugLoc());
        new UnreachableInst (c, boundary_fail_block);
      }
 
      BasicBlock* check_first_block = BasicBlock::Create( c, "check.first.limit." + postfix, F );
      IRBuilder<> check_first_builder( check_first_block );

      // ------ add max boundary check code 

      // *   %1 = load i32** %some_lable.Smart.Last
      LoadInst* last_val = new LoadInst( limits->max, "", before );
      // *   %2 = icmp ugt i32* %0, %1
      ICmpInst* cmp = new ICmpInst( before, CmpInst::ICMP_UGT, ptr, last_val, "" );
      // *   br i1 %2, label %boundary.check.failed, label %check.first.limit
      BranchInst::Create( boundary_fail_block, check_first_block, cmp, before );

      // ------ break current BB to 2 after branch and name later one to be if.end

      BasicBlock* end_block = BB->splitBasicBlock(before, "if.end." + postfix);
      // remove implicitly added branch..
      BB->back().eraseFromParent();

      // ------ add min boundary check code 

      // * check.first.limit:      
      // *   %3 = load i32** %some_label.Smart.First
      LoadInst* first_val = new LoadInst( limits->min, "", check_first_block );
      // *   %4 = icmp ult i32* %0, %3
      ICmpInst* cmp2 = new ICmpInst( *check_first_block, CmpInst::ICMP_ULT, ptr, first_val, "" );
      // *   br i1 %4, label %boundary.check.failed, label %if.end
      BranchInst::Create( boundary_fail_block, end_block, cmp2, check_first_block );

      DEBUG(dbgs() << "Created boundary check for: "; before->print(dbgs()); dbgs() << "\n");
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
          DEBUG(dbgs() << "Cannot resolve limit for: "; use->print(dbgs()); dbgs() << "\n");
          continue;
        }

        DEBUG(dbgs() << "Use: "; use->print(dbgs()); dbgs() << " respects limits of: "; limits->smart->print(dbgs()); dbgs() << "\n");
        derivedUses[use] = limits;
        resolveUses(use, limits, derivedUses);                
      }
    }

    /**
     * Traces recursively up in SSA tree until finds element, which has information about limits.
     *
     * Currently stops if alloca or global value is found.
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
        Instruction *newInst = constExp->getAsInstruction();
        newInst->insertBefore(trashcan);
        return findLimitingFactor(newInst, trashcan);

      } else {
        dbgs() << "Handling value: "; op->print(dbgs()); dbgs() << "\n";
        dyn_cast<Constant>(op)->print(dbgs()); dbgs() << "\n";
        assert(false && "Don't know how to trace limiting operand for this structure.");
      }
    }

    /**
     * Checks if store stores data to smart pointer and updates also smart pointer accordingly.
     */
    void fixStoreInstructions(StoreInstrSet &stores, SmartPointerByValueMap &smartPointers) {
      
      for (StoreInstrSet::iterator i = stores.begin(); i != stores.end(); i++) {
        StoreInst* store = *i;        
        LLVMContext& c = store->getParent()->getContext();
        
        Value *src = store->getValueOperand();
        Value *dest = store->getPointerOperand();
        
        if ( dest->getType()->isPointerTy() && dyn_cast<PointerType>(dest->getType())->getElementType()->isPointerTy() ) {

          DEBUG(dbgs() << "Found store to fix: "; store->print(dbgs()); dbgs() << "\n");
          DEBUG(src->print(dbgs()); dbgs() << "\n");

          assert (smartPointers.count(dest) > 0 && "Cannot find smart pointer for destination");
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

          } else if ( GlobalValue *globalVal = dyn_cast<GlobalValue>(limits) ) {

            if (globalVal->hasExternalLinkage()) {
              // if ext array init, get last and first from there, otherwise just use global val..
              if (globalVal->getType()->getElementType()->isArrayTy()) {
                int array_size = dyn_cast<ArrayType>(globalVal->getType()->getElementType())->getNumElements();
                first = generateGEP(c, globalVal, 0, 0, store, "");
                last = generateGEP(c, globalVal, 0, array_size-1, store, "");
              } else {
                first = globalVal;
                last = globalVal;
              }
            } else {
              assert(false && "Unsupported use of global value.");
            }

          } else {
            assert(false && "Could not resolve limits from source operand, for smart pointer assignment.");
          }
          
          // fix limits and cur in smart pointer assignment
          DEBUG(dbgs() << "#### FOUND LIMITS:\n");
          DEBUG(first->print(dbgs()); dbgs() << "\n");
          DEBUG(last->print(dbgs()); dbgs() << "\n");
          
          StoreInst* firstStore = new StoreInst(first, smartDest->min);
          StoreInst* lastStore = new StoreInst(last, smartDest->max);
          StoreInst* curStore = new StoreInst(store->getValueOperand(), smartDest->cur);          

          lastStore->insertAfter(store);
          firstStore->insertAfter(store);
          curStore->insertAfter(store);

          DEBUG(dbgs() << "-- Created smart store:\n");
          DEBUG(store->print(dbgs()); dbgs() << "\n");
          DEBUG(curStore->print(dbgs()); dbgs() << "\n");
          DEBUG(firstStore->print(dbgs()); dbgs() << "\n");
          DEBUG(lastStore->print(dbgs()); dbgs() << "\n");
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

        DEBUG(dbgs() << "Creating smart pointer structures for: "; alloca->print(dbgs()); dbgs() << " ---- ");

        // Treat array alloca special way, otherwise just treat pointer allocas.....
        Type *ptrType = NULL;
        Instruction *first = NULL;
        Instruction *last = NULL;

        if( ArrayType* a = dyn_cast< ArrayType >( t ) ) {
          DEBUG(dbgs() << "It's an array!\n");

          Type* element_type = a->getElementType();
          assert(element_type->isIntegerTy(32) && "Currently pass supports only i32 type of alloca arrays.");

          unsigned int array_size = a->getArrayNumElements();

          ptrType = PointerType::getUnqual( element_type );
          first = generateGEP(c, alloca, 0, 0, 0, "");
          last = generateGEP(c, alloca, 0, array_size - 1, 0, "");
          last->insertAfter(alloca);
          first->insertAfter(alloca);

        } else if (t->isPointerTy()) {

          DEBUG(dbgs() << "It's a pointer!\n");
          LoadInst *alloca_load = new LoadInst(alloca);
          alloca_load->insertAfter(alloca);
          ptrType = t;
          first = alloca_load;
          last = alloca_load;

        } else if (t->isIntegerTy()) {

          DEBUG(dbgs() << "It's an integer!\n");
          ptrType = PointerType::getUnqual( t );
          first = alloca;
          last = alloca;
          
        } else {
          DEBUG(dbgs() << "It is an unhandled type, some day we need to implement this to make system work correctly!\n");
        }

        if (ptrType) {
          SmartPointer* newSmartPointer = createSmartPointer(ptrType, alloca, first, last);
          smartPointers.insert( std::pair< Value*, SmartPointer* >( alloca, newSmartPointer ) );
        }
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
    void fixCodeToUseReplacedFunctions(FunctionMap &replacedFunctions, 
                                       ArgumentMap &replacedArguments, 
                                       CallInstrSet &callInstructions,
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
   
        for( Function::arg_iterator a = oldFun->arg_begin(); a != oldFun->arg_end(); ++a ) {
          Argument* oldArg = a;
          Argument* newArg = replacedArguments[a];
          
          newArg->takeName(oldArg);
          oldArg->setName(newArg->getName() + "_replaced_arg");

          // if argument types are not the same we need to find smart pointer that was generated for 
          // argument and create initializations so that existing smart alloca will get correct values
          if (oldArg->getType() == newArg->getType()) {
            // argument was not tampered, just replace uses to point the new function
            oldArg->replaceAllUsesWith(newArg);
            continue;
          }

          // create GEPs for initializing smart pointer
          Twine paramName = Twine("") + newArg->getName() + ".SmartArg";
          newArg->setName(paramName);
            
          // create GEPs for reading .Cur .First and .Last of parameter for initializations
          GetElementPtrInst* paramLast = generateGEP(c, newArg, 0, 2, entryBlock.begin(), Twine("") + newArg->getName() + ".Last");
          GetElementPtrInst* paramFirst = generateGEP(c, newArg, 0, 1, entryBlock.begin(), Twine("") + newArg->getName() + ".First");
          GetElementPtrInst* paramCur = generateGEP(c, newArg, 0, 0, entryBlock.begin(), Twine("") + newArg->getName() + ".Cur");

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

          assert(oldArg->hasOneUse() && 
                 "Unknown construct where pointer argument has > 1 uses. (expecting just store instruction to %param.addr)");
            
          if (StoreInst* store = dyn_cast<StoreInst>( (*oldArg->use_begin()) )) {
            AllocaInst *origAddrAlloca = dyn_cast<AllocaInst>(store->getPointerOperand());
            assert(origAddrAlloca);

            StoreInst* newStore = new StoreInst( paramCurVal, origAddrAlloca );
            ReplaceInstWithInst( store, newStore );
              
            // find smart pointer of %param.addr and initialize .Cur, .First and .Last elements
            SmartPointer* smartAlloca = smartPointers[origAddrAlloca];
            StoreInst* initCurStore = new StoreInst( paramCurVal, smartAlloca->cur);
            StoreInst* initFirstStore = new StoreInst( paramFirstVal, smartAlloca->min);
            StoreInst* initLastStore = new StoreInst( paramLastVal, smartAlloca->max);              
            initLastStore->insertAfter(newStore);
            initFirstStore->insertAfter(newStore);
            initCurStore->insertAfter(newStore);
              
          } else {
            dbgs() << "\n"; oldArg->use_begin()->print(dbgs()); dbgs() << "\n";
            assert(false && 
                   "Unknown construct where pointer argument's use is not StoreInst");
          }
                
        } // -- end arguments for loop
      }  
      
      
      // --------- FIXING CALLS FROM OLD STYLE TO USE NEW FUNCTIONS -----------
      // ---------- basically all arguments etc. needs to be fixed ------------
      DEBUG(dbgs() << "------------- FIXING CALLS TO USE NEW SIGNATURES ------------------\n");

      for (CallInstrSet::iterator i = callInstructions.begin(); i != callInstructions.end(); i++) {
        CallInst *call = *i;

        DEBUG(dbgs() << "---- Started fixing:"; call->print(dbgs()); dbgs() << "\n");

        Function* oldFun = call->getCalledFunction();
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

            DEBUG(dbgs() << "- op #" << op << " needs fixing: "; operand->print(dbgs()); dbgs() << "\n");

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
                assert(false && "Cannot find smart pointer for the call operand. Maybe you tried to pass extern variable?");
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
                  assert(false && "Could not find smart pointer for GEP, which should have smart pointer..");
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
                assert(false && "Could not find smart pointer for alloca");
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
              assert(false && "Could not find smart pointer for op for converting call.");

            }
          }
          op++;
        }

        DEBUG(dbgs() << "-- Converted call to : "; call->print(dbgs()); dbgs() << "\n");
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
     * @param safeFunctions Set of function, which does not need to be replaced
     * @param safeArguments Set of arguments, which shouldn't need extra caring even if they are any pointer types etc.
     *
     */
    virtual Function* createNewFunctionSignature( 
      Function *F,  
      FunctionMap &functionMapping, 
      ArgumentMap &argumentMapping, 
      FunctionSet &safeFunctions,
      ArgumentSet &safeArguments ) {
       
      LLVMContext& c = F->getContext();

      // currently returning pointer or array is not supported
      assert( (!F->getFunctionType()->getReturnType()->isPointerTy()) && 
              "Handling function returning pointer is not implemented." );
      assert( (!F->getFunctionType()->getReturnType()->isArrayTy()) && 
              "Handling function retruning array type is not implemented." );
      assert( (!F->isVarArg()) && "Variable argument functions are not supported.");

      // TODO: check if main or kernel and in that case do not change signature 

      // convert function signature to use pointer structs instead of direct pointers
      std::vector< Type* > param_types;
      for( Function::arg_iterator a = F->arg_begin(); a != F->arg_end(); ++a ) {
        Argument* arg = a;
        Type* t = arg->getType();
        
        if( t->isPointerTy() ) {
          Type* smart_array_struct = getSmartPointerType( c, t );
          param_types.push_back( smart_array_struct );          
        } else {
          assert( (!t->isArrayTy()) && "Passing array in arguments is not implemented." );
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

      DEBUG(dbgs() << "-- Created new signature for: " << F->getName() << " "; F->getType()->print(dbgs()));
      DEBUG(dbgs() << "\nnew signature: " << new_function->getName() << " "; new_function->getType()->print(dbgs()); dbgs() << "\n");

      // map arguments of original function to new replacements
      for( Function::arg_iterator 
             a = F->arg_begin(), 
             E = F->arg_end(), 
             a_new = new_function->arg_begin(); a != E; ++a, ++a_new ) {     
        
        argumentMapping.insert( std::pair< Argument*, Argument* >( a, a_new ) ); 
        DEBUG(dbgs() << "Mapped orig arg: "; a->print(dbgs()); dbgs() << " -----> "; a_new->print(dbgs()); dbgs() << "\n");
      }

      return new_function;
    }

    virtual void sortInstructions( Function *F, 
                                   CallInstrSet &calls, 
                                   AllocaInstrSet &allocas,
                                   StoreInstrSet &stores,
                                   LoadInstrSet &loads) {
      
      DEBUG(dbgs() << "-- Finding interesting instructions from: " << F->getName() << "\n");

      // find all instructions which should be handled afterwards
      for ( Function::iterator bb = F->begin(); bb != F->end(); bb++) {
        for( BasicBlock::iterator i = bb->begin(); i != bb->end(); ++i ) {      
          Instruction &inst = *i;
       
          if ( CallInst *call = dyn_cast< CallInst >(&inst) ) {
            if (!call->getCalledFunction()->isIntrinsic()) {
              calls.insert(call);
              DEBUG(dbgs() << "Found call: "; call->print(dbgs()); dbgs() << "\n");
            } else {
              DEBUG(dbgs() << "Ignored call to intrinsic\n");
            }

          } else if ( AllocaInst *alloca = dyn_cast< AllocaInst >(&inst) ) {
            
            // TODO: check if alloca is from smart pointer argument. 
            // ( for these we should no do traditional smart pointer initialization, 
            //  but initialize them from sp read from argument )
            
            allocas.insert(alloca);
            DEBUG(dbgs() << "Found alloca: "; alloca->print(dbgs()); dbgs() << "\n");
            
          } else if ( StoreInst *store = dyn_cast< StoreInst >(&inst) ) {
            
            if (dyn_cast<Argument>(store->getValueOperand())) {
              DEBUG(dbgs() << "Skipping store which reads function argument: "; store->print(dbgs()); dbgs() << "\n");
              continue;
            } 
            
            stores.insert(store);
            DEBUG(dbgs() << "Found store: "; store->print(dbgs()); dbgs() << "\n");
            
          } else if ( LoadInst *load = dyn_cast< LoadInst >(&inst) ) {            
            
            loads.insert(load);
            DEBUG(dbgs() << "Found load: "; load->print(dbgs()); dbgs() << "\n");
          } 
          
          //    TODO: if unsupported instruction is seen (inttoptr, fence, va_arg, cmpxchg,atomicrmw, 
          //           landingpad, memintrinsics) , abort
        }
      }
    }

  private:
    GlobalVariable* mGlobalNull;

  };
}
  
char WebCL::ClampPointers::ID = 0;
static RegisterPass<WebCL::ClampPointers> X("clamp-pointers", "Safe array accesses using clamp.", false, false);