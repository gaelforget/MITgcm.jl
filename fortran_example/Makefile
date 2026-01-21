# Makefile for Fortran library

FC = gfortran
FFLAGS = -fPIC

# Detect OS for shared library extension
UNAME := $(shell uname)
ifeq ($(UNAME), Darwin)
    SHLIB = libmylib.dylib
    SHFLAGS = -dynamiclib
else
    SHLIB = libmylib.so
    SHFLAGS = -shared
endif

all: libmylib.a $(SHLIB)

mylib.o: mylib.f
	$(FC) $(FFLAGS) -c mylib.f -o mylib.o

# Static library
libmylib.a: mylib.o
	ar rcs libmylib.a mylib.o

# Shared library (needed for Julia)
$(SHLIB): mylib.o
	$(FC) $(SHFLAGS) -o $(SHLIB) mylib.o

clean:
	rm -f *.o *.a *.dylib *.so

test: $(SHLIB)
	julia test_fortran.jl

.PHONY: all clean test
