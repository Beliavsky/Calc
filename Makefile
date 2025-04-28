executables = xinterpret_gfort.exe
FC     = gfortran
FFLAGS = -O0 -Wall -Werror=unused-parameter -Werror=unused-variable -Werror=unused-function -Wno-maybe-uninitialized -Wno-surprising -fbounds-check -static -g -fmodule-private
obj    = kind.o util.o constants.o qsort.o random.o stats.o interpret.o xinterpret.o

all: $(executables)

# Compile .f90 to .o
%.o: %.f90
	$(FC) $(FFLAGS) -c $<

xinterpret_gfort.exe: kind.o util.o constants.o qsort.o random.o stats.o interpret.o xinterpret.o
	$(FC) -o xinterpret_gfort.exe kind.o util.o constants.o qsort.o random.o stats.o interpret.o xinterpret.o $(FFLAGS)

run: $(executables)
	./xinterpret_gfort.exe

clean:
	rm -f $(executables) $(obj)

