build-all: library.bc library-fakecl.o builtin-declarations.ll

builtin-implementations.bc: builtin-implementations.cl ../tests/pocl_kernel.h

builtin-implementations-fakecl.bc: builtin-implementations.cl ../tests/pocl_kernel.h
	clang -I ../tests -DFAKECL=1 -x cl -Dcles_khr_int64 -Dcl_khr_fp16 -Dcl_khr_fp64 -emit-llvm -c $< -o $@

safe-implementations-fakecl.bc: safe-implementations.cl ../tests/pocl_kernel.h
	clang -I ../tests -DFAKECL=1 -x cl -Dcles_khr_int64 -Dcl_khr_fp16 -Dcl_khr_fp64 -emit-llvm -c $< -o $@

%.clamped.bc: %.bc
	opt -load $$CLAMP_PLUGIN -clamp-pointers $< -o $@

%.ll: %.bc
	llvm-dis $< -o $@

%.ll: %.cl
	clang -I ../tests -x cl -S -Dcles_khr_int64 -Dcl_khr_fp16 -Dcl_khr_fp64 -emit-llvm -c $< -o $@

%.bc: %.cl
	clang -I ../tests -x cl -Dcles_khr_int64 -Dcl_khr_fp16 -Dcl_khr_fp64 -emit-llvm -c $< -o $@

builtin-declarations.ll: safe-implementations.clamped.ll
	grep '^define.*\*.*\*.*\*' $< | sed 's/^define /declare /;s/ {$$//' > $@

library.bc: builtin-implementations.bc safe-implementations.clamped.bc
	llvm-link $^ -o $@

library-fakecl.bc: builtin-implementations-fakecl.bc safe-implementations-fakecl.clamped.bc
	llvm-link $^ -o $@

%.s: %.bc
	llc $< -o $@

%.o: %.s
	as -o $@ $<

.PRECIOUS: %.s

clean:
	rm -f *.bc *.ll *.o *.s core *~
