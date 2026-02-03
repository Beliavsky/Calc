# Calc

Calc is a Fortran-based interactive statistics interpreter with a session-to-Fortran transpiler.

## What the interpreter supports

- Scalars and 1D real arrays.
- Array literals, slicing, arithmetic, reductions, and basic control flow.
- Random simulation, fitting, and properties of many [probability distributions](distributions.md).
- Time-series helpers including ACF/PACF and AR/MA/ARMA/ARFIMA utilities.
- Plotting via gnuplot.

Some interpreter code examples:

```text
! General
run("code.txt")
calc code.txt

! Scalars and arithmetic
n = 10
r = n / 2
r^3
const m = 10^3

! Vectors
y = [1, 2, 3, 4, 5]
v = 10 * arange(10)
t = grid(11, 0.0, 0.1)

! Element-wise operations
z = n * y
w = y + v(1:size(y))
z ^ 2

! Slicing
v([2 4 6 8])
v(3:9:2)
reverse(v)

! Random numbers and time series
x = runif(10)
x0 = runif()
rn = rnorm(5)
arsim(1000, [0.5, -0.4])
acf(x, 10)
acf(x, 10, plot=.true.)
pacf(x, 10)
pacf(x, 10, plot=.true.)
acfpacf(x, 10)
acfpacf(x, 10, plot=.true.)
acfpacfar(x, 10)
acfpacfar(x, 10, plot=.true.)
fiacf(0.25, 10)
arfimaacf([0.4], [0.2], 0.25, 10)
fracdiff(x, 0.3)
arfimafit(x, 1, 1)
arfimasim(1000, 0.25, phi=[0.4], theta=[0.2])

! Stats
sum(x)
mean(x)
geomean(x)
harmean(x)
sd(x)
[mean(x) sd(x) minval(x) maxval(x)]
median(x)
rank(x)
stdz(x)

! Cumulative and differencing
cumsum(y)
cumprod(y)
diff(y)

! Sorting and ordering
sort(x)
indexx(x)
y(indexx(y))

! Head/tail
head(v)
tail(v)

! Comparisons
x > 0.5
x <= maxval(x)
y == [1 2 3 4 5]
y /= 3
y >= 4

! Two-vector functions
cor(x, sort(x))
cov(x, sort(x))
cor
cor(x, y, z)
dot(y, v(1:size(y)))
min(y, v(1:size(y)))
max(x, 0.5)

! Workspace
?vars
read prices.csv
clear
```

`acf`/`pacf` return lags `1..n` and plotting is optional (`plot=.false.` by default). `acfpacf`/`acfpacfar` can also optionally plot.

## Sample session

```text
> n = 10
10.000000

> y = [1, 2, 3]
[1.000000 2.000000 3.000000]

> z = n * y
[10.000000 20.000000 30.000000]

> w = [10 20 30] + y
[11.000000 22.000000 33.000000]

> z ^ 2
[100.000000 400.000000 900.000000]

> x = runif(n)
[.704414 .208529 .550907 .377847 .189410 .223593 .262786 .870069 .568524 .569907]

> sum(x)
4.525986

> [sum(x) minval(x) maxval(x)]
[4.525986 .189410 .870069]

> q
```

The semicolon `;` can be used as a continuation separator and suppresses output when it appears at the end of the line. Iteration is supported with `*n` at the start of a line, meaning execute the rest of the line `n` times.

Example (Newton iterations for `sqrt(3)`):

```text
a = 3
x = 1
*5  x = (x + a/x)/2
```

Typical output:

```text
1.750000
1.732143
1.732051
1.732051
1.732051
```

## Build and run

- Build interpreter:

```bat
make
```

- Run interpreter executable (name depends on your build target/toolchain).

## Transpiler (`transpile_session.py`)

Transpiles a `.fi` session script to a standalone Fortran program.

```bat
python transpile_session.py code.fi -o tests.f90
make -f Makefile_tests
```

## Transpiler behavior (current)

- Emits minimal `use ..., only:` imports.
- Infers scalar/array/integer declarations and converts `const` assignments to `parameter` declarations.
- Rewrites selected names to module procedures:
  - `rnorm` -> `random_normal`
  - `sort` -> `sorted`
  - `stdz` -> `standardize`
  - `dot` -> `dot_product`
- Supports interpreter shorthand defaults for several distribution helpers by expanding omitted optional arguments (for compile-safe Fortran calls), e.g. `rexp(n)`, `rlnorm(n)`, `rlogis(n)`, `rgamma(n,a)`, and corresponding `mssk_*` calls.
- Handles `read("file", col)` in assignments by emitting `call read_vec(...)` and then applying any remaining expression.
  - Example: `x = log(read("spy.csv", 2))` transpiles to
    - `call read_vec("spy.csv", x, 2)`
    - `x = log(x)`
- Supports top-level `acf(..., plot=...)` / `pacf(..., plot=...)` by generating explicit plotting blocks in Fortran.
- Rewrites legacy ARFIMA simulation call form when possible:
  - `arfimasim(n, [phi], [theta], d)` -> `arfimasim(n, d, phi=[phi], theta=[theta])`

## Interpreter-only commands

Some REPL/workspace commands are not mapped to standalone Fortran, e.g. `cor` (workspace correlation matrix mode), `?vars`, `clear`, `del ...`, and statement-form `read ...`.

## Notes

- Session logs and examples in this repository use `.fi` scripts.
- `Makefile_tests` builds `tests.f90` together with project modules into `tests.exe`.
