executables = xinterpret_gfort.exe xcalc_gfort.exe
FC     = gfortran
FFLAGS = -O0 -Wall -Werror=unused-parameter -Werror=unused-variable -Werror=unused-function -Wno-maybe-uninitialized -Wno-surprising -fbounds-check -static -g -fmodule-private
obj    = interpret.o xinterpret.o calc.o xcalc.o

all: $(executables)

# Compile .f90 to .o
%.o: %.f90
	$(FC) $(FFLAGS) -c $<

xinterpret_gfort.exe: interpret.o xinterpret.o
	$(FC) -o xinterpret_gfort.exe interpret.o xinterpret.o $(FFLAGS)

xcalc_gfort.exe: calc.o xcalc.o
	$(FC) -o xcalc_gfort.exe calc.o xcalc.o $(FFLAGS)

run: $(executables)
	./xinterpret_gfort.exe
	./xcalc_gfort.exe

clean:
	rm -f $(executables) $(obj)

