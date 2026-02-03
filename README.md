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
x = runif(10)                                ! 10 iid Uniform(0,1) draws
x0 = runif()                                 ! one Uniform(0,1) draw
rn = rnorm(5)                                ! 5 iid standard Normal draws
arsim(1000, [0.5, -0.4])                     ! simulate AR(2) series
acf(x, 10)                                   ! sample ACF for lags 1..10
acf(x, 10, plot=.true.)                      ! sample ACF and plot
pacf(x, 10)                                  ! sample PACF for lags 1..10
pacf(x, 10, plot=.true.)                     ! sample PACF and plot
acfpacf(x, 10)                               ! print aligned ACF/PACF table
acfpacf(x, 10, plot=.true.)                  ! table + joint ACF/PACF plot
acfpacfar(x, 10)                             ! print ACF/PACF/AR-coefficient table
acfpacfar(x, 10, plot=.true.)                ! table + joint ACF/PACF/AR plot
fiacf(0.25, 10)                              ! theoretical ACF of ARFIMA(0,d,0)
arfimaacf([0.4], [0.2], 0.25, 10)            ! theoretical ACF of ARFIMA(1,d,1)
fracdiff(x, 0.3)                             ! fractional differencing (1-L)^0.3 x
arfimafit(x, 1, 1)                           ! fit ARFIMA(1,d,1)
arfimasim(1000, 0.25, phi=[0.4], theta=[0.2]) ! simulate ARFIMA(1,d,1)

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
cor(x, y)                         ! Pearson correlation between x and y
cov(x, y)                         ! sample covariance between x and y
cor                               ! labeled correlation matrix for all same-length vectors in workspace
cor(x, y, z)                      ! labeled correlation matrix for the listed vectors
dot(x, y)                         ! dot product of x and y
min(x, y)                         ! element-wise minimum of x and y
max(x, 0.5)                       ! element-wise maximum of x and scalar 0.5

! Workspace
?vars
read prices.csv
clear
```

`acf`/`pacf` return lags `1..n` and plotting is optional (`plot=.false.` by default). `acfpacf`/`acfpacfar` can also optionally plot.

## Regression and model fitting

```text
! Simple linear regression
x = runif(200)
y = 1.0 + 2.0*x + 0.2*rnorm(200)
regress(x, y)                     ! with intercept by default
regress(x, y, intcp=0)            ! no-intercept regression

! Multiple regression
z = x^2
regress(y, x, z)                  ! multiple predictors via regress(...)

! AR/MA/ARMA fitting helpers
arfit(y, 1, 5)                    ! fit AR orders 1..5 and report fit metrics
mafit(y, 1, 5)                    ! fit MA orders 1..5 and report fit metrics
armafit(y, 1, 1)                  ! fit one ARMA(1,1) model
armafitgrid(y, 0, 3, 0, 3)        ! grid search over ARMA(p,q), p=0..3 and q=0..3
armafitaic(y, 0, 5, 0, 5)         ! choose ARMA order by information criterion over p,q ranges
```

`arfimafit(x, p, q)` prints a fit table including `npar` (number of estimated parameters), RMSE/AIC/BIC, and parameter estimates.

## Resampling

```text
x = rnorm(100)
resample(x)                       ! bootstrap sample, same size, with replacement
resample(x, n=20)                 ! sample size 20
resample(x, n=20, replace=0)      ! sample without replacement
```

## Distribution helpers

- Most distributions expose `r*`/`d*`/`p*`/`q*` helpers plus `fit_*` and often `mssk_*`.
- Uniform helpers now include `runif`, `dunif`, `punif`, and `qunif`.
- `fit_t(x)` now returns three parameters: `[mu, sigma, df]`.

See [distributions.md](distributions.md) for interpreter-name to statistical-name mapping.

## Data input modes

```text
read prices.csv                   ! REPL command: load named columns into workspace variables
x = read("spy.csv", 2)            ! function form: read numeric column 2 as a vector
ret = diff(log(read("spy.csv", 2)))
```

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
