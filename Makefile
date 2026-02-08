executables = fcalc.exe
FC     = gfortran
FFLAGS = -O0 -Wall -Werror=unused-parameter -Werror=unused-variable -Werror=unused-function -Wno-maybe-uninitialized -Wno-surprising -Wno-intrinsic-shadow -fbounds-check -static -g -fmodule-private
obj    = kind.o util.o gnuplot.o constants.o qsort.o random.o stats.o interpret.o xinterpret.o

all: $(executables)

# Compile .f90 to .o
%.o: %.f90
	$(FC) $(FFLAGS) -c $<

fcalc.exe: kind.o util.o gnuplot.o constants.o qsort.o random.o stats.o interpret.o xinterpret.o
	$(FC) -o fcalc.exe kind.o util.o gnuplot.o constants.o qsort.o random.o stats.o interpret.o xinterpret.o $(FFLAGS)

run: $(executables)
	./fcalc.exe

clean:
	rm -f $(executables) $(obj)

