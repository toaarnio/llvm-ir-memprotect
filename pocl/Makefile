build-all: library.bc library-fakecl.bc

CLANGFLAGS=-DBUILDING_RUNKERNEL=1

builtin-implementations.bc: builtin-implementations.cl ../tests/spir_kernel.h

builtin-implementations-fakecl.bc: builtin-implementations.cl ../tests/spir_kernel.h
	clang $(CLANGFLAGS) -I ../tests -DFAKECL=1 -x cl -target spir -Dcles_khr_int64 -Dcl_khr_fp16 -Dcl_khr_fp64 -emit-llvm -c $< -o $@

safe-implementations-fakecl.bc: safe-implementations.cl ../tests/spir_kernel.h
	clang $(CLANGFLAGS) -I ../tests -DFAKECL=1 -x cl -target spir -Dcles_khr_int64 -Dcl_khr_fp16 -Dcl_khr_fp64 -emit-llvm -c $< -o $@

%.clamped.bc: %.bc
	opt -load $$CLAMP_PLUGIN -clamp-pointers $< -o $@

%.ll: %.bc
	llvm-dis $< -o $@

%.ll: %.cl
	clang $(CLANGFLAGS) -I ../tests -x cl -target spir -S -Dcles_khr_int64 -Dcl_khr_fp16 -Dcl_khr_fp64 -emit-llvm -c $< -o $@

%.bc: %.cl
	clang $(CLANGFLAGS) -I ../tests -x cl -target spir -Dcles_khr_int64 -Dcl_khr_fp16 -Dcl_khr_fp64 -emit-llvm -c $< -o $@

builtin-declarations.ll: safe-implementations.clamped.ll
	grep '^define.*\*.*\*.*\*' $< | sed 's/^define /declare /;s/ {$$//' > $@

library.bc: builtin-implementations.bc safe-implementations.clamped.bc
	llvm-link $^ -o $@

library-fakecl.bc: builtin-implementations-fakecl.bc
	llvm-link $^ -o $@

#%.o: %.bc
#	clang -c $< -o $@

.PRECIOUS: %.s

clean:
	rm -f *.bc *.ll *.o *.s core *~
