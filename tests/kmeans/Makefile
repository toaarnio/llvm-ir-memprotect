CXX = g++
DEBUGFLAGS = -g
CC_FLAGS = -I.. -g -O0
CXXFLAGS = $(CC_FLAGS)
LDFLAGS = -lm -lpthread
CL_LIBRARY = ../../pocl/library-fakecl.o

ifdef SAFE
CLAMPED=.clamped

%.ll: %.cl
	clang -O0 -g -x cl -fno-builtin -DFAKECL=1 -DBUILDING_RUNKERNEL=1 \
		-include ../pocl_kernel.h \
		-Dcles_khr_int64 -Dcl_khr_fp16 -Dcl_khr_fp64 \
		-c $< -emit-llvm -o $@

%.clamped.ll: %.ll
	opt -load $(CLAMP_PLUGIN) -clamp-pointers -S -o $@ $<

%.s: %.ll
	llc $< -o $@

%.o: %.s
	as -o $@ $<

.PRECIOUS: %.ll %.clamped.ll
else

%.o: %.cl
	clang -O0 -g -x cl -fno-builtin -DFAKECL=1 -DBUILDING_RUNKERNEL=1 \
		-include ../pocl_kernel.h \
		-Dcles_khr_int64 -Dcl_khr_fp16 -Dcl_khr_fp64 \
		-c $< -o $@
endif

KERNEL=kmeans-cl$(CLAMPED).o

kmeans: ../FakeCL.o $(CL_LIBRARY) $(KERNEL) cluster.o read_input.o kmeans.o getopt.o kmeans_clustering.o rmse.o
	$(CXX) $(LDFLAGS) $^ -o $@

clean:
	rm -f *.o *~ kmeans *.linkinfo
