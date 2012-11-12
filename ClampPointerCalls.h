#ifndef CLAMPPOINTERCALLS_H
#define CLAMPPOINTERCALLS_H 

	struct Leafs
	{
		Leafs() :
			count( 0 )
		{
		}

		void dump()
		{
			errs() << "calls:" << "\n";
			for( unsigned int i = 0; i < calls.size(); ++i )
			{
				errs() <<  *(calls.at( i )) << "\n";
			}

			errs() << "stores:" << "\n";
			for( unsigned int i = 0; i < stores.size(); ++i )
			{
				errs() <<  *(stores.at( i )) << "\n";
			}
		}

		unsigned int count;

		std::vector< CallInst* > calls;
		std::vector< Value* > arguments;
		std::vector< Value* > parents;

		std::vector< StoreInst* > stores;
	};

	virtual void countCalls( Value* root, Leafs* data, Value* parent = 0, unsigned int depth = 0 )
	{
		for( Value::use_iterator i = root->use_begin(), e = root->use_end(); i != e; ++i )
		{
			indent( depth );
			if( CallInst *call = dyn_cast< CallInst >( *i ) )
			{
				data->count += 1;
				data->calls.push_back( call );
				data->arguments.push_back( root );
				data->parents.push_back( parent != 0 ? parent : root );
			}
			else if( GetElementPtrInst *gep = dyn_cast< GetElementPtrInst >(*i) )
			{
				Value* p = parent != 0 ? parent : gep;
				countCalls( gep, data, p, depth + 1 );
			}
			else if( LoadInst *load = dyn_cast< LoadInst >(*i) )
			{
				Value* p = parent != 0 ? parent : load;
				countCalls( load, data, p, depth + 1 );
			}
			else if( StoreInst *store = dyn_cast< StoreInst >(*i) )
			{
				Value* target = store->getPointerOperand();
				Value* source = store->getValueOperand();
				Value* p = parent != 0 ? parent : store;

				if( source == root )
					countCalls( target, data, p, depth + 1 );
				else
					data->stores.push_back( store );
			}
			else
			{
				Instruction* inst = dyn_cast< Instruction >(*i);
				UNUSED( inst );
			}
		}
	}

#endif /* CLAMPPOINTERCALLS_H */
