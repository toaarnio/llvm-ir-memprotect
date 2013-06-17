CXX = g++
DEBUGFLAGS = -g
# You will need to modify the following 2 lines
# by inserting the correct path to the OpenCL
# lib and inc directories.
CXXFLAGS = -I.. -g -O0
LDFLAGS = -lm -lpthread

ifndef USE_FAKECL
LDFLAGS += -lOpenCL
endif

ifdef USE_FAKECL
CXXFLAGS += -DUSE_FAKECL
endif

CL_LIBRARY = ../../pocl/library-fakecl.o

ifdef SAFE
KERNEL=kernels.clamped.o

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
KERNEL=kernels.o

%.o: %.cl
	clang -O0 -g -x cl -fno-builtin -DFAKECL=1 -DBUILDING_RUNKERNEL=1 \
		-include ../pocl_kernel.h \
		-Dcles_khr_int64 -Dcl_khr_fp16 -Dcl_khr_fp64 \
		-c $< -o $@
endif

ifdef USE_FAKECL
pathfinder: main.o ../FakeCL.o OpenCL.o $(CL_LIBRARY) $(KERNEL)
else
pathfinder: main.o OpenCL.o
endif
	$(CXX) $(CXXFLAGS) $(LDFLAGS) $^ -o $@

clean:
	rm -f pathfinder *.o *.out *~ *\# *.ll *.s
