# Calc
Interpreter in Fortran that can handle scalars and 1D arrays. The output of `xinterpret.f90` is
```
> x = 3
3.000000

> y = [1, 2, 3]
[1.000000, 2.000000, 3.000000]

> z = y * x
[3.000000, 6.000000, 9.000000]

> w = [10 20 30] + y
[11.000000, 22.000000, 33.000000]

> z ^ 2
[9.000000, 36.000000, 81.000000]
```
