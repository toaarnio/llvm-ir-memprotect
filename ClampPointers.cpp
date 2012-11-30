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
#include "llvm/User.h"

#include "llvm/Support/CallSite.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"

#include "llvm/Support/IRBuilder.h"

#include <vector>
#include <map>
#include <set>
#include <iostream>

#define UNUSED( x ) \
  (void)x;

using namespace llvm;

namespace WebCL {

  /// Module pass that implements algorithm for restricting memory
  /// accesses to locally reserved addresses.  This is done
  /// dynamically by tracking minimum and maximum addresses of each
  /// memory allocation.  The address is clamped to a valid memory
  /// region before the pointer is dereferenced.  When memory
  /// allocation goes out of scope, both upper and lower limits are
  /// set to null preventing any attempt to access memory at such
  /// addresses.  The algorithm itself consists of several
  /// IR-transforming phases, first of which is to find static memory
  /// allocations.  For this purpose, an instance of smart pointer
  /// struct is created after every llvm::AllocaInst instruction that
  /// contains the dynamic information about the current address of
  /// the pointer along its minimum and maximum addresses.  In the
  /// next phase, algorithm iterates over the def-use chain of the
  /// each previously found memory allocation and replaces all of its
  /// uses with a smart pointer.  A third phase is needed for
  /// transforming the function arguments and call sites.  At first,
  /// each function declaration is inspected and pointer parameters
  /// are replaced with smart pointers and corresponding
  /// transformation is done on the function body.  Then, each
  /// llvm::CallInst checked and if it was previously transformed, its
  /// arguments are replaced by corresponding smart pointers.
  /// Finally, a software implementation of a clamp function is
  /// inserted if the platform does not support it on the hardware and
  /// call for it is added for each pointer dereferencing operation
  /// (identified by llvm::GetElementPtrInst).

  struct ClampPointers :
    public ModulePass {
    static char ID;

    ClampPointers() :
      ModulePass( ID ),
      mClampFunction( 0 ) {
    }

    /// An allocation of contiguous array of memory.
    struct SmartPointer {
      SmartPointer( Value* _current, Value* _min, Value* _max, Value* _ptr ) :
        cur( _current ),
        min( _min ),
        max( _max ),
        ptr( _ptr ) {
      }

      Value* cur;
      Value* min;
      Value* max;
      Value* ptr;
    };

    void indent( unsigned int depth ) {
      for( unsigned int i = 0; i < depth; ++i )
        errs() << "- ";
    }

    /// Create a load instruction from a value.
    LoadInst* generateLoad( Value* from, Instruction* i, Twine name ) {
      Twine name_load = Twine("") + name;
      LoadInst* load_inst = new LoadInst( from, name_load );
      load_inst->insertAfter( i );
      return load_inst;
    }

    /// Create a store instruction from a value.
    StoreInst* generateStore( Value* from, Value* to, Instruction* i, Twine name ) {
      StoreInst* store_inst = new StoreInst( from, to );
      store_inst->insertAfter( i );
      return store_inst;
    }

    /// Create a single-index GEP instruction from a value.
    GetElementPtrInst* generateGEP( LLVMContext& ctx, Value* ptr, int a, Instruction* i, Twine t = "gep" ) {
      Twine name = t;
      ConstantInt* c_0 = ConstantInt::get( Type::getInt32Ty(ctx), a );
      std::vector< Value* > values;
      values.push_back( c_0 );
      ArrayRef< Value* > ref( values );
      GetElementPtrInst* gep = GetElementPtrInst::Create( ptr, ref, name, i );
      return gep;
    }

    /// Create a two-index GEP instruction from a value.
    GetElementPtrInst* generateGEP( LLVMContext& c, Value* ptr, int a, int b, Instruction* i, Twine t = "gep" ) {
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

    void generateSmartPointerFree( SmartPointer* sp, Instruction* i ) {
    }

    void generateDeref( Module* module ) {
      LLVMContext& c = module->getContext();

      Type* i32 = Type::getInt32Ty(c);
      Type* i32p = PointerType::getUnqual( Type::getInt32Ty(c) );
      Type* smart = getSmartPointerType( c, i32p );

      Constant* p = module->getOrInsertFunction( "deref",
                                                 i32p,
                                                 i32p,
                                                 smart,
                                                 i32,
                                                 NULL );

      Function* deref = cast<Function>(p);
      deref->setCallingConv(CallingConv::C);
			
      mDerefFunction = deref;

      Function::arg_iterator args = deref->arg_begin();

      Value* pointer_a = args++;
      pointer_a->setName( "pointer" );

      Value* range_a = args++;
      range_a->setName( "range" );

      Value* index_a = args++;
      index_a->setName( "index" );

      // new deref implementation written based on .ll from compiling and optimizing C
      // struct SmartPointer { int32_t cur; int32_t* first; int32_t* last; };
      // int32_t* deref(int32_t *ptr, struct SmartPointer *range, int32_t index) {
      //  int32_t *ptr_index = ptr + index;
      //  if (ptr_index > range->last) return range->last;
      //  if (ptr_index < range->first) return range->first;
      //  return ptr_index;
      //}
      //
      // ---- >
      //
      // define i32* @deref(i32* %ptr, %struct.SmartPointer* nocapture %range, i32 %index) nounwind uwtable readonly optsize ssp minsize {
      // entry:
      //   %add.ptr = getelementptr inbounds i32* %ptr, i32 %index
      //   %last = getelementptr inbounds %struct.SmartPointer* %range, i32 0, i32 2
      //   %0 = load i32** %last, align 4
      //   %cmp = icmp ugt i32* %add.ptr, %0
      //   br i1 %cmp, label %return, label %if.end
      // if.end:                                           ; preds = %entry
      //   %first = getelementptr inbounds %struct.SmartPointer* %range, i32 0, i32 1
      //   %1 = load i32** %first, align 4
      //   %cmp2 = icmp ult i32* %add.ptr, %1
      //   %.add.ptr = select i1 %cmp2, i32* %1, i32* %add.ptr
      //   br label %return
      // return:                                           ; preds = %if.end, %entry
      //   %retval.0 = phi i32* [ %0, %entry ], [ %.add.ptr, %if.end ]
      //   ret i32* %retval.0
      // }


      BasicBlock* entry_block = BasicBlock::Create( c, "entry", deref );
      BasicBlock* if_end_block = BasicBlock::Create( c, "if.end", deref );
      BasicBlock* return_block = BasicBlock::Create( c, "return", deref );
      IRBuilder<> entry_builder( entry_block );
      IRBuilder<> if_end_builder( if_end_block );
      IRBuilder<> return_builder( return_block );

      // dummy instructions to be able to create GEPs with helpers 
      // (didn't wan't to overload generateGEP more to support adding to basic block)
      // will be removed after function is ready
      Twine name_ifend_start = "ifend_dummy";
      AllocaInst* ifend_start = new AllocaInst( i32, 0, name_ifend_start, if_end_block );

      // entry:
      //   %add.ptr = getelementptr inbounds i32* %ptr, i32 %index
      std::vector< Value* > values;
      values.push_back( index_a );
      ArrayRef< Value* > ref( values );
      GetElementPtrInst* add_ptr_gep = GetElementPtrInst::Create( pointer_a, ref, "add.ptr", entry_block );

      //   %last = getelementptr inbounds %struct.SmartPointer* %range, i32 0, i32 2
      GetElementPtrInst* last_gep = this->generateGEP( c,  range_a, 0, 2, add_ptr_gep, "last" );

      //   %0 = load i32** %last, align 4
      LoadInst* last_ptr_load = new LoadInst( last_gep, "last.prt.load", entry_block );
      
      //   %cmp = icmp ugt i32* %add.ptr, %0
      ICmpInst* cmp = new ICmpInst( *entry_block, CmpInst::ICMP_UGT, add_ptr_gep, last_ptr_load, "cmp" );

      //   br i1 %cmp, label %return, label %if.end
      BranchInst* if_gt_bra = BranchInst::Create( return_block, if_end_block, cmp, entry_block );

      // if.end:                                           ; preds = %entry
      //   %first = getelementptr inbounds %struct.SmartPointer* %range, i32 0, i32 1
      GetElementPtrInst* first_gep = this->generateGEP( c,  range_a, 0, 1, ifend_start, "first" );

      //   %1 = load i32** %first, align 4
      LoadInst* first_ptr_load = new LoadInst( first_gep, "first.ptr.load", if_end_block );

      //   %cmp2 = icmp ult i32* %add.ptr, %1
      ICmpInst* cmp2 = new ICmpInst( *if_end_block, CmpInst::ICMP_ULT, add_ptr_gep, first_ptr_load, "cmp2" );

      //   %.add.ptr = select i1 %cmp2, i32* %1, i32* %add.ptr
      SelectInst* _add_ptr = SelectInst::Create(cmp2, first_ptr_load, add_ptr_gep, ".add.ptr", if_end_block);

      //   br label %return
      BranchInst* br_end = BranchInst::Create( return_block, if_end_block );

      // return:                                           ; preds = %if.end, %entry
      //   %retval.0 = phi i32* [ %0, %entry ], [ %.add.ptr, %if.end ]
      PHINode* retval_0 = PHINode::Create(_add_ptr->getType(), 2, "retval.0", return_block);
      retval_0->addIncoming(last_ptr_load, entry_block);
      retval_0->addIncoming(_add_ptr, if_end_block);

      //   ret i32* %retval.0
      ReturnInst* ret = ReturnInst::Create( c, retval_0, return_block );

      // remove dummy place holder
      ifend_start->eraseFromParent();

      UNUSED( if_gt_bra );
      UNUSED( br_end );
      UNUSED( ret );
    }

    void generateClamp( Module* module ) {
      LLVMContext& c = module->getContext();

      Type* i32 = Type::getInt32Ty(c);
      Type* i32p = PointerType::getUnqual(Type::getInt32Ty(c));
      Type* smart = getSmartPointerType( c, i32p );

      mGlobalNull = new GlobalVariable( *module, i32, true, GlobalValue::ExternalLinkage, 0, "NULL_i32" );

      Constant* p = module->getOrInsertFunction( "clamp",
                                                 i32p,
                                                 smart,
                                                 NULL );

      Function* clamp = cast< Function >(p);
      clamp->setCallingConv( CallingConv::C );
			
      mClampFunction = clamp;

      Function::arg_iterator args = clamp->arg_begin();
      Value* a = args++;
      a->setName( "smart" );

      BasicBlock* entry_block = BasicBlock::Create( c, "entry", clamp );
      BasicBlock* continue_block = BasicBlock::Create( c, "continue", clamp );
      IRBuilder<> entry_builder( entry_block );

      BasicBlock* min_block = BasicBlock::Create( c, "clamp_to_min", clamp );
      BasicBlock* max_block = BasicBlock::Create( c, "clamp_to_max", clamp );

      BasicBlock* exit_block = BasicBlock::Create( c, "exit", clamp );
      IRBuilder<> exit_builder( exit_block );

      // in
      Twine name_data = "smart_alloca";
      AllocaInst* smart_alloca = new AllocaInst( smart, 0, name_data, entry_block );
      StoreInst* smart_store = new StoreInst( a, smart_alloca, entry_block );

      UNUSED( smart_store );

      // out
      Twine name_ret_alloca = "clamped_ptr";
      AllocaInst* ret_alloca = new AllocaInst( i32p, 0, name_ret_alloca, entry_block );

      Twine name_ret_load = "ret_load";
      LoadInst* ret_load = new LoadInst( ret_alloca, name_ret_load, exit_block );

      GetElementPtrInst* cur_gep = this->generateGEP( c, a, 0, 0, ret_alloca, "cur_gep" );
      Twine name_load_cur = "cur";
      LoadInst* cur_load = new LoadInst( cur_gep, name_load_cur );
      cur_load->insertAfter( cur_gep );

      // default output
      StoreInst* cur_store = new StoreInst( cur_load, ret_alloca, entry_block );

      UNUSED( cur_store );

      GetElementPtrInst* min_gep = this->generateGEP( c, a, 0, 1, ret_alloca, "min_gep" );
      Twine name_load_min = "min";
      LoadInst* min_load = new LoadInst( min_gep, name_load_min );
      min_load->insertAfter( min_gep );

      GetElementPtrInst* max_gep = this->generateGEP( c, a, 0, 2, ret_alloca, "max_gep" );
      Twine name_load_max = "max";
      LoadInst* max_load = new LoadInst( max_gep, name_load_max );
      max_load->insertAfter( max_gep );

      StoreInst* ret_store = new StoreInst( ret_load, cur_gep, exit_block );
      ReturnInst* ret = ReturnInst::Create( c, ret_load, exit_block );

      UNUSED( ret_store );
      UNUSED( ret );

      StoreInst* min_store = new StoreInst( min_load, ret_alloca, min_block );
      StoreInst* max_store = new StoreInst( max_load, ret_alloca, max_block );

      UNUSED( min_store );
      UNUSED( max_store );

      BranchInst* min_exit_bra = BranchInst::Create( exit_block, min_block );
      BranchInst* max_exit_bra = BranchInst::Create( exit_block, max_block );

      UNUSED( min_exit_bra );
      UNUSED( max_exit_bra );

      ICmpInst* min_cmp = new ICmpInst( *entry_block, CmpInst::ICMP_ULT, cur_load, min_load, "less_than" );
      ICmpInst* max_cmp = new ICmpInst( *entry_block, CmpInst::ICMP_UGT, cur_load, max_load, "greater_than" );

      BranchInst* min_bra = BranchInst::Create( min_block, continue_block, min_cmp, entry_block );
      BranchInst* max_bra = BranchInst::Create( max_block, exit_block, max_cmp, continue_block );

      UNUSED( min_bra );
      UNUSED( max_bra );
    }

    Type* getSmartPointerType( LLVMContext& c, Type* t ) {
      StructType* s = this->getSmartStructType( c, t );
      Type* smart_array_pointer_type = PointerType::getUnqual( s );
      return smart_array_pointer_type;
    }

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

    virtual void generateSmartPointerLoads( Function *F ) {
      for( Function::iterator f = F->begin(), ef = F->end(); f != ef; ++f) {
        for( BasicBlock::iterator i = f->begin(), eb = f->end(); i != eb; ++i) {
          if( llvm::LoadInst* l = dyn_cast< llvm::LoadInst >(i) ) {
            Value* source = l->getPointerOperand();

            SmartPointerMap::iterator spi = mSmartPointers.find( source );
            if( spi == mSmartPointers.end() ) {
              continue;
            }

            SmartPointer* sp = spi->second;
            //Value* ptr = sp->cur;

            std::vector< Value* > values;
            values.push_back( sp->ptr );
            ArrayRef< Value* > vref( values );

            CallInst* clamp_call = CallInst::Create( mClampFunction, vref, "clamped" );
            clamp_call->insertBefore( l );

            //source->replaceAllUsesWith( clamp_call );
            l->replaceAllUsesWith( clamp_call );

            errs() << "sp" << *(sp->cur) << "\n";
            errs() << "load" << *l << "\n";
            errs() << "alloca" << *source << "\n";
          }
        }
      }
    }

    virtual void generateSmartPointerAssignment( SmartPointer* source, SmartPointer* target, Instruction* root ) {
      // load source
      LoadInst* source_min_load = this->generateLoad( source->min, root, Twine("source.First") );
      LoadInst* source_max_load = this->generateLoad( source->max, root, Twine("source.Last") );
      LoadInst* source_cur_load = this->generateLoad( source->cur, root, Twine("source.Current") );

      root = source_min_load;

      assert( mClampFunction != 0 );

      // store source -> terget
      StoreInst* target_min_store = this->generateStore( source_min_load, target->min, root, Twine("target.First") );
      StoreInst* target_max_store = this->generateStore( source_max_load, target->max, root, Twine("target.Last") );
      StoreInst* target_cur_store = this->generateStore( source_cur_load, target->cur, root, Twine("target.Current") );

      UNUSED( target_min_store );
      UNUSED( target_max_store );
      UNUSED( target_cur_store );
    }

    virtual void findPointerAssignments( Value* root ) {
      for( Value::use_iterator i = root->use_begin(), e = root->use_end(); i != e; ++i ) {
        if( StoreInst *store = dyn_cast< StoreInst >(*i) ) {
          Value* source = store->getValueOperand();
          Value* target = store->getPointerOperand();

          // find target registers
          SmartPointerMap::iterator sp_target = mSmartPointers.find( target );

          if( sp_target == mSmartPointers.end() ) {
            errs() << "FATAL ERROR: Cannot find target ->" << *target << "'\n";
            assert( false );
          }

          // if source is gep
          if( GetElementPtrInst *gep = dyn_cast< GetElementPtrInst >( source ) ) {

            Value* gep_source = gep->getPointerOperand();
            SmartPointerMap::iterator sp_source = mSmartPointers.find( gep_source );

            if( sp_source == mSmartPointers.end() ) {
              errs() << "FATAL ERROR: Cannot find GEP source ->" << *gep_source << "'\n";
              assert( false );
            }

            this->generateSmartPointerAssignment( sp_source->second, sp_target->second, store );
          } else if( LoadInst* load = dyn_cast< LoadInst >( source ) ) {

            Value* load_source = load->getPointerOperand();
            SmartPointerMap::iterator sp_source = mSmartPointers.find( load_source );

            if( sp_source == mSmartPointers.end() ) {
              errs() << "FATAL ERROR: Cannot find LOAD source ->" << *load_source << "'\n";
              continue;
              //assert( false );
            }

            this->generateSmartPointerAssignment( sp_source->second, sp_target->second, store );
          } else if( Argument* a = dyn_cast< Argument > ( source ) ) {
            UNUSED( a );
            errs() << "FATAL WARNING: is argument" << "\n";
            //assert( false );
          } else {
            errs() << "WARNING: Unknown instruction type ignored ->" << *source << "\n";
            //assert( false );
          }

        }
      }
    }


    virtual void findStores( Value* root ) {
      for( Value::use_iterator i = root->use_begin(), e = root->use_end(); i != e; ++i ) {
        if( StoreInst *store = dyn_cast< StoreInst >(*i) ) {
          Value* source = store->getValueOperand();
          Value* target = store->getPointerOperand();

          if( target == root ) {
            // write to smart pointer, get source data
            LoadInst* load = this->findLoad( source );
            assert( load );

            Value* source = load->getPointerOperand();
            SmartPointerMap::iterator is = mSmartPointers.find( source );
            errs() << *source;
            assert( is != mSmartPointers.end() );

            if( !load->hasName() ) {
              Value* source = load->getPointerOperand();
              StringRef source_name = source->getName();
              load->setName( Twine("") + source_name + Twine(".Load") );
            }

            StringRef loadName = load->getName();

            Value* cur = is->second->cur;
            Value* min = is->second->min;
            Value* max = is->second->max;

            Twine name_min = Twine("") + loadName + Twine( ".First" );
            LoadInst* load_min = new LoadInst( min, name_min );
            load_min->insertAfter( load );

            Twine name_max = Twine("") + loadName + Twine( ".Last" );
            LoadInst* load_max = new LoadInst( max, name_max );
            load_max->insertAfter( load );

            Twine name_cur = Twine("") + loadName + Twine( ".Cur" );
            LoadInst* load_cur = new LoadInst( cur, name_cur );
            load_cur->insertAfter( load );

            SmartPointerMap::iterator it = mSmartPointers.find( target );
            assert( it != mSmartPointers.end() );

            Value* tcur = it->second->cur;
            Value* tmin = it->second->min;
            Value* tmax = it->second->max;

            StoreInst* store_max = new StoreInst( load_max, tmax );
            store_max->insertAfter( load_max );

            StoreInst* store_min = new StoreInst( load_min, tmin );
            store_min->insertAfter( load_min );

            StoreInst* store_cur = new StoreInst( load_cur, tcur );
            store_cur->insertAfter( load_cur );

          }
        }
      }
    }

    virtual LoadInst* findLoad( Value* root ) {
      if( LoadInst *load = dyn_cast< LoadInst >(root) ) {
        return load;
      } else if( GetElementPtrInst *gep = dyn_cast< GetElementPtrInst >( root ) ) {
        Value* source = gep->getPointerOperand();
        return findLoad( source );
      } else {
        return 0;
      }
    }


    virtual void modifyArrayPointers( Value* array, Value* limit ) {
      for( User::use_iterator u = array->use_begin(), e = array->use_end(); u != e; ++u ) {
        if( LoadInst* load = dyn_cast< llvm::LoadInst >(*u) ) {
          modifyArrayPointers( load, limit );
        } else if( GetElementPtrInst* gep = dyn_cast< llvm::GetElementPtrInst >(*u) ) {
          bool mod = false;

          unsigned int index = 0;
          for( User::op_iterator o = gep->idx_begin(); o != gep->idx_end(); ++o ) {
            Value* v2 = o->get();

            index++;
            if( index == 2 ) {
              if( !(isa< Constant >(v2)) ) {
                if( isa< SExtInst >(v2) ) {
                  v2 = cast< SExtInst >(v2)->getOperand(0);
                }

                Instruction* place = dyn_cast< Instruction >(v2);
                assert( place );

                //BinaryOperator* or1 = BinaryOperator::Create( Instruction::URem, limit, limit, "safe_idx", gep );
                BinaryOperator* or1 = BinaryOperator::Create( Instruction::URem, limit, limit, "safe_idx" );
                or1->insertAfter( place );

                v2->replaceAllUsesWith( or1 );
                or1->setOperand( 0, v2 );

                mod = true;
              } else {
                mod = true;

                Value* idx = gep->getOperand( 2 );
                Value* ptr = gep->getPointerOperand();
                Type* type = gep->getPointerOperandType();

                UNUSED( idx );
                UNUSED( type );

                if( llvm::AllocaInst* alloca = dyn_cast< llvm::AllocaInst >(ptr) ) {
                  Type* t = alloca->getAllocatedType();

                  if( ArrayType* a = dyn_cast< ArrayType >(t) ) {
                    unsigned int max = a->getArrayNumElements();
                    unsigned int req = cast< ConstantInt >(v2)->getLimitedValue();

                    if( req >= max ) {
                      Type* i32 = v2->getType();
                      //unsigned int new_size = req % max;
                      unsigned int new_size = max - 1;
                      Constant* constSize = ConstantInt::get( i32, new_size );

                      gep->setOperand( 2, constSize );
                    }
                  }
                }
              }
            }
          }

          if( !mod ) {
            Value* idx = gep->getOperand( 1 );
            BinaryOperator* or1 = BinaryOperator::Create( Instruction::URem, idx, limit, "safe_idx", gep );
            gep->setOperand( 1, or1 );
          }
        }
      }
    }

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


    virtual bool runOnModule( Module &M ) {

      // create smart pointers
      for( Module::iterator i = M.begin(); i != M.end(); ++i ) {
        this->runOnFunction( i );
      }

      generateClamp( &M );
      generateDeref( &M );

      // find all pointer assigments
      for( SmartPointerMap::iterator i = mSmartPointers.begin(); i != mSmartPointers.end(); ++i ) {
        this->findPointerAssignments( i->first );
      }

      for( Module::iterator i = M.begin(); i != M.end(); ++i ) {
        this->generateSmartPointerLoads( i );
      }

      // modify calls
      for( Module::iterator i = M.begin(); i != M.end(); ++i ) {
        this->modifyCalls( i );
      }

      // create nulls
      for( Module::iterator i = M.begin(); i != M.end(); ++i ) {
        this->freeMemory( i );
      }

      return true;
    }

    virtual bool modifyCalls( Function *F ) {

      std::vector< Value* > args;

      while( !F->use_empty() ) {
        FunctionMap::iterator fi = mReplacedFunctions.find( F );

        if( fi == mReplacedFunctions.end() )
          return false;

        assert( fi != mReplacedFunctions.end() );

        Function* new_function = fi->second;

        CallSite call_site( F->use_back() );
        Instruction *call_inst = call_site.getInstruction();

        for( CallSite::arg_iterator a = call_site.arg_begin(); a != call_site.arg_end(); ++a ) {
          Value* value = *a;
          Type* t = value->getType();

          if( t->isPointerTy() ) {

            if( GetElementPtrInst *gep = dyn_cast< GetElementPtrInst >( value ) ) {
              Value* gep_source = gep->getPointerOperand();
              SmartPointerMap::iterator sp_source = mSmartPointers.find( gep_source );

              Value* source = sp_source->second->ptr;

              args.push_back( source );

              /* @todo Crash?
                 if( sp_source == mSmartPointers.end() ) {
                 errs() << "FATAL ERROR: Cannot find GEP source ->" << *gep_source << "'\n";
                 assert( false );
                 }
              */
            } else {
              /// @todo Handle other values.
              assert( false );
            }
          } else {
            args.push_back( *a );
          }
        }

        AttrListPtr attributes = call_site.getAttributes();

        Instruction *new_inst;
        if( InvokeInst *invoke_inst = dyn_cast< InvokeInst >( call_inst ) ) {
          new_inst = InvokeInst::Create( new_function, invoke_inst->getNormalDest(), invoke_inst->getUnwindDest(), args, "", call_inst );
          cast< InvokeInst >( new_inst )->setCallingConv( call_site.getCallingConv() );
          cast< InvokeInst >( new_inst )->setAttributes( attributes );
        } else {
          new_inst = CallInst::Create( new_function, args, "", call_inst );
        }
        new_inst->setDebugLoc( call_inst->getDebugLoc() );

        args.clear();
        if( !call_inst->use_empty() )
          call_inst->replaceAllUsesWith( new_inst );

        new_inst->takeName(call_inst);
        call_inst->eraseFromParent();

        break;
      }

      return true;
    }

    virtual bool runOnFunction( Function *F ) {
      assert( F != 0 );
      
      LLVMContext& c = F->getContext();
     
      // convert function signature
      std::vector< Type* > param_types;
      for( Function::arg_iterator a = F->arg_begin(); a != F->arg_end(); ++a ) {
        Argument* arg = a;
        Type* t = arg->getType();

        if( t->isPointerTy() ) {
          Type* smart_array_struct = getSmartPointerType( c, t );
          param_types.push_back( smart_array_struct );
        } else {
          param_types.push_back( t );
        }
      }

      FunctionType *function_type = F->getFunctionType();
      FunctionType *new_function_type = FunctionType::get( function_type->getReturnType(), param_types, false );

      Function *new_function = Function::Create( new_function_type, F->getLinkage() );
      new_function->copyAttributesFrom( F );
      F->getParent()->getFunctionList().insert( F, new_function );
      new_function->takeName( F );

      new_function->getBasicBlockList().splice( new_function->begin(), F->getBasicBlockList() );

      mReplacedFunctions.insert( std::pair< Function*, Function* >( F, new_function ) );

      SmartPointerList* sp_list = new SmartPointerList();
      mFunctionSmartPointers.insert( std::pair< Function*, SmartPointerList* >( new_function, sp_list ) );

      for( Function::arg_iterator a = F->arg_begin(), E = F->arg_end(), a2 = new_function->arg_begin();
           a != E; ++a, ++a2 ) {
        Argument* arg = a;
        Type* t = arg->getType();

        if( t->isPointerTy() ) {
          for( Value::use_iterator u = a->use_begin(), e = a->use_end(); u != e; ++u ) {
            Value* temp = *u;
            assert( temp );

            if( llvm::StoreInst* store = dyn_cast< llvm::StoreInst >(*u) ) {

              Value* target = store->getPointerOperand();

              if( llvm::AllocaInst* alloca = dyn_cast< llvm::AllocaInst >(target) ) {
                Type* smart_array_struct = getSmartStructType( c, t );
                StringRef allocaName = alloca->getName();
                Twine name_data = allocaName + ".SmartPtr";
                AllocaInst* smart_array_ptr_alloca = new AllocaInst( PointerType::getUnqual(smart_array_struct), 0, name_data, alloca );

                StoreInst* smart_array_ptr_store = new StoreInst( a2, smart_array_ptr_alloca );
                smart_array_ptr_store->insertAfter( smart_array_ptr_alloca );

                GetElementPtrInst* gep = this->generateGEP( c, a2, 0, 0, 0 );
                gep->insertAfter( smart_array_ptr_store );

                Twine name_load_cur = Twine( "" ) + allocaName + ".Data";
                LoadInst* load_cur = new LoadInst( gep, name_load_cur );
                load_cur->insertAfter( gep );

                Twine name_alloca = allocaName + ".Arg";
                AllocaInst* arg_ptr_inst = new AllocaInst( alloca->getType()->getPointerElementType(), 0, name_alloca );

                StoreInst* store_addr = new StoreInst( load_cur, arg_ptr_inst );
                ReplaceInstWithInst( store, store_addr );
                ReplaceInstWithInst( alloca, arg_ptr_inst );

                assert( alloca );
                mBlackList.insert( arg_ptr_inst );
                mBlackList.insert( smart_array_ptr_alloca );
              }

            }
          }

          a2->takeName(a);
        } else {
          // just copy old uses
          a->replaceAllUsesWith(a2);
          a2->takeName(a);
        }
      }

      // find all array/pointer allocations and convert them to smart arrays
      BasicBlock& block = F->getEntryBlock(); 
      for( BasicBlock::iterator i = block.begin(); i != block.end(); ++i ) {
        if( llvm::AllocaInst* alloca = dyn_cast< llvm::AllocaInst >(i) ) {
          AllocaSet::iterator bl = mBlackList.find( alloca );
          if( bl != mBlackList.end() ) {
            continue;
          }

          // check if it is used to store parameter
          bool isParam = false;
          for( Value::use_iterator u = alloca->use_begin(), e = alloca->use_end(); u != e; ++u ) {
            if( StoreInst *store = dyn_cast< StoreInst >(*u) ) {
              Value* source = store->getValueOperand();
              if( Argument* a = dyn_cast< Argument > ( source ) ) {
                UNUSED( a );

                // skip argument
                isParam = true;
                break;
              }
            }
          }

          // do not convert arguments
          if( isParam )
            continue;

          Type* t = alloca->getAllocatedType();

          static unsigned int id = 0;
          StringRef allocaName = alloca->getName();

          if( ArrayType* a = dyn_cast< ArrayType >( t ) ) {
            id++;
            Type* element_type = a->getElementType();

            /// @todo: Array of pointers.

            std::vector< Type* > types;
            types.push_back( PointerType::getUnqual( element_type ) );
            types.push_back( PointerType::getUnqual( element_type ) );
            types.push_back( PointerType::getUnqual( element_type ) );
            ArrayRef< Type* > tref( types );

            StructType *smart_array_struct = StructType::get( c, tref );
            assert( smart_array_struct );
            Type* smart_array_pointer_type = PointerType::getUnqual( smart_array_struct );

            Twine name_data = allocaName + ".Smart";
            AllocaInst* smart_array_alloca = new AllocaInst( smart_array_struct, 0, name_data, alloca );
            Twine name_ptr = allocaName + ".SmartPtr";
            AllocaInst* smart_array_ptr_alloca = new AllocaInst( smart_array_pointer_type, 0, name_ptr, alloca );
            StoreInst* smart_array_ptr_store = new StoreInst( smart_array_alloca, smart_array_ptr_alloca );
            smart_array_ptr_store->insertAfter( smart_array_ptr_alloca );

            Twine name_00 = Twine( "" ) + allocaName + ".Smart.Cur";
            ConstantInt* c_0 = ConstantInt::get( Type::getInt32Ty(c), 0 );
            std::vector< Value* > values_00;
            values_00.push_back( c_0 );
            values_00.push_back( c_0 );
            ArrayRef< Value* > ref_00( values_00 );
            GetElementPtrInst* array_gep = GetElementPtrInst::Create( smart_array_alloca, ref_00, name_00, alloca );

            Twine name_01 = Twine( "" ) + allocaName + ".Smart.First";
            ConstantInt* c_1 = ConstantInt::get( Type::getInt32Ty(c), 1 );
            std::vector< Value* > values_01;
            values_01.push_back( c_0 );
            values_01.push_back( c_1 );
            ArrayRef< Value* > ref_01( values_01 );
            GetElementPtrInst* first_gep = GetElementPtrInst::Create( smart_array_alloca, ref_01, name_01, alloca );

            Twine name_02 = Twine( "" ) + allocaName + ".Smart.Last";
            ConstantInt* c_2 = ConstantInt::get( Type::getInt32Ty(c), 2 );
            std::vector< Value* > values_02;
            values_02.push_back( c_0 );
            values_02.push_back( c_2 );
            ArrayRef< Value* > ref_02( values_02 );
            GetElementPtrInst* last_gep = GetElementPtrInst::Create( smart_array_alloca, ref_02, name_02, alloca );

            SmartPointer* s = new SmartPointer( array_gep, first_gep, last_gep, smart_array_alloca );
            mSmartPointers.insert( std::pair< Value*, SmartPointer* >( alloca, s ) );
            sp_list->push_back( s );

            mSmartPointerVector.push_back( alloca );
            //errs() << "push back" << *alloca << "\n";

            Twine name_first = Twine( "" ) + allocaName + ".First";
            GetElementPtrInst* array_data_gep = GetElementPtrInst::Create( alloca, ref_00, name_first );
            array_data_gep->insertAfter( alloca );
            StoreInst* data_store = new StoreInst( array_data_gep, array_gep );
            data_store->insertAfter( array_data_gep );
            StoreInst* first_store = new StoreInst( array_data_gep, first_gep );
            first_store->insertAfter( array_data_gep );

            Twine name_last = Twine( "" ) + allocaName + ".Last";
            unsigned int array_size = a->getArrayNumElements();
            ConstantInt* c_last = ConstantInt::get( Type::getInt32Ty(c), array_size - 1 );
            std::vector< Value* > values_last;
            values_last.push_back( c_0 );
            values_last.push_back( c_last );
            ArrayRef< Value* > ref_last( values_last );
            GetElementPtrInst* array_last_gep = GetElementPtrInst::Create( alloca, ref_last, name_last );
            array_last_gep->insertAfter( alloca );
            StoreInst* last_store = new StoreInst( array_last_gep, last_gep );
            last_store->insertAfter( array_last_gep );
          } else {
            // pointers only
            if( !t->isPointerTy() )
              continue;

            id++;

            std::vector< Type* > types;
            types.push_back( t );
            types.push_back( t );
            types.push_back( t );

            ArrayRef< Type* > tref( types );

            StructType *smart_array_struct = StructType::get( c, tref );
            assert( smart_array_struct );
            Type* smart_array_pointer_type = PointerType::getUnqual( smart_array_struct );

            Twine name_data = allocaName + ".Smart";
            AllocaInst* smart_array_alloca = new AllocaInst( smart_array_struct, 0, name_data, alloca );
            Twine name_ptr = allocaName + ".SmartPtr";
            AllocaInst* smart_array_ptr_alloca = new AllocaInst( smart_array_pointer_type, 0, name_ptr, alloca );
            StoreInst* smart_array_ptr_store = new StoreInst( smart_array_alloca, smart_array_ptr_alloca );
            smart_array_ptr_store->insertAfter( smart_array_ptr_alloca );

            Twine name_00 = Twine( "" ) + allocaName + ".Smart.Cur";
            ConstantInt* c_0 = ConstantInt::get( Type::getInt32Ty(c), 0 );
            std::vector< Value* > values_00;
            values_00.push_back( c_0 );
            values_00.push_back( c_0 );
            ArrayRef< Value* > ref_00( values_00 );
            GetElementPtrInst* array_gep = GetElementPtrInst::Create( smart_array_alloca, ref_00, name_00, alloca );

            Twine name_01 = Twine( "" ) + allocaName + ".Smart.First";
            ConstantInt* c_1 = ConstantInt::get( Type::getInt32Ty(c), 1 );
            std::vector< Value* > values_01;
            values_01.push_back( c_0 );
            values_01.push_back( c_1 );
            ArrayRef< Value* > ref_01( values_01 );
            GetElementPtrInst* first_gep = GetElementPtrInst::Create( smart_array_alloca, ref_01, name_01, alloca );

            Twine name_02 = Twine( "" ) + allocaName + ".Smart.Last";
            ConstantInt* c_2 = ConstantInt::get( Type::getInt32Ty(c), 2 );
            std::vector< Value* > values_02;
            values_02.push_back( c_0 );
            values_02.push_back( c_2 );
            ArrayRef< Value* > ref_02( values_02 );
            GetElementPtrInst* last_gep = GetElementPtrInst::Create( smart_array_alloca, ref_02, name_02, alloca );

            SmartPointer* s = new SmartPointer( array_gep, first_gep, last_gep, smart_array_alloca );
            mSmartPointers.insert( std::pair< Value*, SmartPointer* >( alloca, s ) );
            sp_list->push_back( s );

            mSmartPointerVector.push_back( alloca );
            //errs() << "push back" << *alloca << "\n";
          }
        }
      }

      return false;
    }

  private:
    typedef std::map< Value*, SmartPointer* > SmartPointerMap;
    SmartPointerMap mSmartPointers;

    typedef std::vector< Value* > SmartPointerVector;
    SmartPointerVector mSmartPointerVector;

    typedef std::set< AllocaInst* > AllocaSet;
    AllocaSet mBlackList;

    typedef std::map< Function*, Function* > FunctionMap;
    FunctionMap mReplacedFunctions;

    typedef std::vector< SmartPointer* > SmartPointerList;
    typedef std::map< Function*, SmartPointerList* > FunctionSmartPointerList;

    FunctionSmartPointerList mFunctionSmartPointers;

    Function* mClampFunction;
    Function* mDerefFunction;

    GlobalVariable* mGlobalNull;

  };
}
  
char WebCL::ClampPointers::ID = 0;
static RegisterPass<WebCL::ClampPointers> X("clamp-pointers", "Safe array accesses using clamp.", false, false);
