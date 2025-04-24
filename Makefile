executables = xcalc_gfort.exe
FC     = gfortran
FFLAGS = -O0 -Wall -Werror=unused-parameter -Werror=unused-variable -Werror=unused-function -Wno-maybe-uninitialized -Wno-surprising -fbounds-check -static -g -fmodule-private
obj    = calc.o xcalc.o

all: $(executables)

# Compile .f90 to .o
%.o: %.f90
	$(FC) $(FFLAGS) -c $<

xcalc_gfort.exe: calc.o xcalc.o
	$(FC) -o xcalc_gfort.exe calc.o xcalc.o $(FFLAGS)

run: $(executables)
	./xcalc_gfort.exe

clean:
	rm -f $(executables) $(obj)

