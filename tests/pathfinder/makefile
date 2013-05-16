CXX = g++
DEBUGFLAGS = -g
# You will need to modify the following 2 lines
# by inserting the correct path to the OpenCL
# lib and inc directories.
CXXFLAGS = -I.. -g -O0
LDFLAGS = -lm -lpthread

CL_LIBRARY = ../../pocl/library.o

%.o: %.cl
	clang -O0 -g -x cl -fno-builtin -DBUILDING_RUNKERNEL=1 -include ../pocl_kernel.h -Dcles_khr_int64 -Dcl_khr_fp16 -Dcl_khr_fp64 -c $< -o $@

pathfinder: main.o ../FakeCL.o OpenCL.o $(CL_LIBRARY) kernels.o
	$(CXX) $(CXXFLAGS) $(LDFLAGS) $^ -o $@

clean:
	rm -f pathfinder *.o *.out *~ *\#
