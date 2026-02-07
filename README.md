# Calc

Calc is a Fortran-based interactive statistics interpreter with a session-to-Fortran transpiler.

## What the interpreter supports

- Scalars and 1D real arrays.
- Array literals, slicing, arithmetic, reductions, and basic control flow.
- Random simulation, fitting, and properties of many [probability distributions](distributions.md).
- Time-series helpers including ACF/PACF and AR/MA/ARMA/ARFIMA utilities.
- Robust summaries, common statistical tests, and AIC-based distribution scanning.
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
tn = rnct(5, 8.0, 1.5)                       ! 5 iid noncentral t draws (df=8, ncp=1.5)
mx = rmixnorm(5, [0.7,0.3], [0.0,2.0], [1.0,0.5]) ! 5 draws from a 2-component normal mixture
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
polyroots([-6, -1, 1])                       ! roots of x^2 - x - 6 -> packed as [Re1, Im1, Re2, Im2]
armastab([0.6, 0.2], [0.3])                  ! [is_stationary, is_invertible, min_mod_ar, min_mod_ma]
armastab(ma=[0.3])                           ! MA-only invertibility check

! Stats
sum(x)
product(x)
mean(x)
trimmean(x, 0.1)      ! 10% trimmed mean (drop lowest/highest 10%)
winsor_mean(x, 0.1)   ! 10% winsorized mean (cap tails at 10% quantiles)
mad(x)                ! median absolute deviation (robust scale)
iqr(x)                ! interquartile range
iqr_scale(x)          ! robust sd estimate: IQR / 1.349
geomean(x)
harmean(x)
sd(x)
[mean(x) sd(x) minval(x) maxval(x)]
mssk(x)                             ! mean, sd, skew, kurtosis
mssk_norm(0, 1)                     ! theoretical Normal moments [mean, sd, skew, kurtosis]
median(x)
rank(x)
stdz(x)

! Cumulative and differencing
cumsum(y)
cumprod(y)
diff(y)

! Sorting and ordering
sort(x)
p = indexx(y)                     ! permutation that sorts y
y(p)                              ! y reordered by that permutation (same values as sort(y))

! Head/tail
head(v)                            ! first 5 values (default)
head(v, 3)                         ! first 3 values
tail(v)                            ! last 5 values (default)
tail(v, 3)                         ! last 3 values

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
ttest2(x, y)                      ! Welch two-sample t test -> [t, df, p]
ks2_test(x, y)                    ! two-sample KS test -> [D, p]

! Workspace
?vars                              ! list currently defined variables and their values
read prices.csv                    ! read columns from prices.csv into workspace variables
clear                              ! remove user-defined variables and user-defined functions

! Control flow
if (mean(x) > 0.5) then
  "high"
else
  "low"
end if
do i=1,5
  i, i^2
end do
do i=1,5 i,i^2                     ! one-line do loop
for z in [0.1, 0.2, 0.3]
  z, sqrt(z)
end for
for z in [0.1, 0.2, 0.3] z,sqrt(z) ! one-line for loop

! User-defined functions (arguments are read-only / intent(in)-style)
function center(a)
  center = a - mean(a)
end function
xc = center(x)

function xnorm(a, power=2)         ! default argument in user function
  xnorm = sum(abs(a)^power)^(1/power)
end function
xnorm(x)                            ! uses power=2
xnorm(x, 1)                         ! overrides default
xnorm(x= x, power=1)                ! named arguments

! User-defined subroutines (default argument intent is inout)
subroutine bump(a, b)
  a = a + 1
  b = 2*b
end subroutine
call bump(x, y)

subroutine sum2(a, b, c)
  intent(in) :: a, b
  intent(out) :: c
  c = a + b
end subroutine
call sum2(2, 3, z)

subroutine shift_scale(y, shift=0, scale=1)
  intent(in out) :: y
  intent(in) :: shift, scale        ! defaults allowed for intent(in)
  y = scale*y + shift
end subroutine
call shift_scale(x)                 ! uses shift=0, scale=1
call shift_scale(x, 1.5, 0.5)       ! overrides defaults
call shift_scale(y=x, shift=1.5)    ! named arguments
```

`acf`/`pacf` return lags `1..n` and plotting is optional (`plot=.false.` by default). `acfpacf`/`acfpacfar` can also optionally plot.
`head`/`tail` accept an optional second argument for the number of elements to return.
One-line `do`/`for` loop bodies must be a single statement (you can still use `;` within that statement).
Defaults in function/subroutine headers must be trailing. In subroutines, defaults are allowed only for arguments declared `intent(in)`.
Named arguments are supported in user-defined function and subroutine calls; positional arguments cannot appear after a named argument.

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
poly1reg(y, x, 3)                 ! polynomial regression in one predictor up to degree 3

! AR/MA/ARMA fitting helpers
arfit(y, 1, 5)                    ! fit AR orders 1..5 and report fit metrics
mafit(y, 1, 5)                    ! fit MA orders 1..5 and report fit metrics
armafit(y, 1, 1)                  ! fit one ARMA(1,1) model
arsimfit(10^4, [0.6, 0.4])        ! simulate AR and fit default order=size(phi)
masimfit(10^4, [0.6, 0.4])        ! simulate MA and fit default order=size(theta)
armasimfit(10^4, [0.6], [0.3])    ! simulate ARMA and fit default p=size(ar), q=size(ma)
armafitgrid(y, 0, 3, 0, 3)        ! grid search over ARMA(p,q), p=0..3 and q=0..3
armafitaic(y, 0, 5, 0, 5)         ! choose ARMA order by information criterion over p,q ranges
fit_mixnorm(y, 2)                 ! fit a 2-component normal mixture [wgt, mean, sd]
fit_mixnorm_aic(y, 1, 5, nstart=5, verbose=.true., plot=.true.) ! choose mixture size by AIC

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

```text
x = rlnorm(5000, 0.0, 0.5)             ! r: random draws from Lognormal(meanlog, sdlog)
dlnorm(x, 0.0, 0.5)                    ! d: lognormal density
plnorm(x, 0.0, 0.5)                    ! p: lognormal CDF
qlnorm(0.95, 0.0, 0.5)                 ! q: lognormal quantile
skew_lnorm(0.5)                        ! theoretical skewness
kurt_lnorm(0.5)                        ! theoretical excess kurtosis
mssk_lnorm(0.0, 0.5)                   ! theoretical [mean, sd, skew, excess kurtosis]
fit_lnorm(x)                           ! fit [meanlog, sdlog] from data
distaicscan(abs(x), 1)             ! fit compatible distributions and rank by AIC
```

See [distributions.md](distributions.md) for interpreter-name to statistical-name mapping.

## Data input modes

```text
read prices.csv                   ! REPL command: load named columns into workspace variables
x = read("spy.csv", 2)            ! function form: read numeric column 2 as a vector
ret = diff(log(read("spy.csv", 2)))
```

## Robust stats and tests

```text
x = rnorm(300)
y = rnorm(250)

trimmean(x)                        ! default 10% trimmed mean
trimmean(x, 0.2)                   ! 20% trimmed mean
winsor_mean(x, 0.1)                ! 10% winsorized mean
mad(x)                             ! median absolute deviation
iqr_scale(x)                       ! robust scale = IQR/1.349

jb_test(x)                         ! Jarque-Bera normality test -> [JB, p]
ttest1(x, 0.0)                     ! one-sample t test -> [t, df, p]
ttest2(x, y)                       ! Welch two-sample t test -> [t, df, p]
ttest2(x, y, 1)                    ! pooled-variance two-sample t test
ks2_test(x, y)                     ! two-sample KS test -> [D, p]

! Kernel regression (1 predictor)
xg = arange(300)/300
yg = sin(6*xg) + 0.2*rnorm(300)
yh = kernelreg(yg, xg)             ! Nadaraya-Watson estimate with default bandwidth
yh = kernelreg(yg, xg, 0.06)       ! scalar bandwidth
yh = kernelreg(yg, xg, [0.03,0.06,0.12]) ! vector bandwidth: plots one curve per bandwidth
yh = kernelreg(yg, xg, 0.06, 1)    ! local linear fit (order=1)
yh = kernelreg(yg, xg, 0.06, order=[0,1,2]) ! one bandwidth, multiple orders (plotted together)
yh = kernelreg(yg, xg, [0.03,0.06], order=[0,1,2]) ! tensor product of bw and order curves
yh = kernelreg(yg, xg, 0.06, points=.true.) ! overlay sample points on the fit plot
yh = lowess(yg, xg)                         ! LOWESS with default span=0.3 and robust iterations
yh = lowess(yg, xg, 0.2, it=1)              ! smaller span, one robust iteration
yh = lowess(yg, xg, [0.15,0.3,0.5], plot=.true.) ! compare multiple spans on one plot
yh = lowesscv(yg, xg)                       ! choose span by LOOCV over a default span grid
yh = lowesscv(yg, xg, [0.1,0.2,0.3,0.4])    ! choose span by LOOCV over user grid
yh = knnreg(yg, xg, 25)                     ! k-nearest-neighbors smoother (order=0 default)
yh = knnreg(yg, xg, 25, order=1)            ! local linear fit on k-neighborhood
yh = knnreg(yg, xg, [15,30,60], plot=.true.) ! compare multiple k values on one plot
yh = knnregcv(yg, xg)                       ! choose k by LOOCV over a default k grid
yh = knnregcv(yg, xg, [8,12,20,30], order=1) ! choose k by LOOCV over user k grid

! Spline regression (plots fitted curve by default)
yh = splinereg(yg, xg, 4)                    ! cubic spline with 4 interior knots
yh = splinereg(yg, xg, 4, degree=1)          ! linear spline
yh = splinereg(yg, xg, 4, degree=[1,2,3])    ! plots one fitted curve per degree
yh = splinereg(yg, xg, 4, points=.true.)     ! overlay sample points on the fit plot
yh = splinereg(yg, xg, 4, degree=3, plot=.false.) ! compute only (no plot)

! Natural cubic spline regression (plots by default)
yh = naturalspline(yg, xg)                   ! choose k by cross validation (plot title shows selected k)
yh = naturalspline(yg, xg, 4)                ! natural cubic with 4 interior knots
yh = naturalspline(yg, xg, [2,4,8])          ! plots one fitted curve per k
yh = naturalspline(yg, xg, 4, points=.true.) ! overlay sample points on the fit plot
yh = naturalspline(yg, xg, 4, plot=.false.)  ! compute only (no plot)

! Plot output mode for whole session
set plotout png                              ! save plots to PNG files
set plotout pdf                              ! save plots to PDF files
set plotout svg                              ! save plots to SVG files
set plotout eps                              ! save plots to EPS files
set plotout screen                           ! back to interactive windows
set plotout                                  ! show current mode

! Saved plots use title-based names, e.g. acf_0001.png, cpfit_0002.pdf

! Changepoint simulation/fitting
xcp = cpsim(400, [150,300])                  ! defaults: mu=0, sd=1, plot=.false.
xcp = cpsim(400, [150,300], mu=[0,2,-1], sd=0.7, plot=.true.) ! plot data and true means
xcp = cpsim(400, [150,300], verbose=.true.)  ! print true segment table (default verbose=.false.)
fitcp = cpfit(xcp)                           ! defaults: mode=mean, max_cp=1, plot=.true.
fitcp2 = cpfit(xcp, max_cp=3, minseg=20, plot=.false.)
fitcp3 = cpfit(xcp, verbose=.false.)         ! suppress estimated segment table
fitcp_sd = cpfit(xcp, mode=sd, max_cp=2, plot=.false.)   ! variance changepoints
fitcp_both = cpfit(xcp, mode=both, max_cp=2, plot=.false.) ! mean+variance changepoints
fitcp_best = cpfitaic(xcp, max_cp=5, criterion=aic, plot=.true.) ! choose best by AIC (default)
fitcp_best2 = cpfitaic(xcp, max_cp=5, criterion=bic, plot=.false., plot_ic=.true.) ! IC-vs-max_cp plot
fitcp_best3 = cpfitaic(xcp, max_cp=5, verbose=.true., plot=.false.) ! print per-model segment tables
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

- Run interpreter executable:

```bat
fcalc.exe
```

- Run interpreter with plotting disabled:

```bat
fcalc.exe --noplot
```

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
- Supports `for ... in ...` / `end for` and one-line `for`/`do` loop forms.
- Supports the newer analysis helpers used above (`poly1reg`, `distaicscan`, robust stats, and tests).
- Rewrites legacy ARFIMA simulation call form when possible:
  - `arfimasim(n, [phi], [theta], d)` -> `arfimasim(n, d, phi=[phi], theta=[theta])`
- Rewrites `iter=` to `niter=` where required by Fortran procedures (for relevant time-series helpers).
- Rewrites integer order ranges to compile-safe forms for simulation-fit helpers:
  - `arsimfit(..., arange(...))` / `masimfit(..., arange(...))` -> `... irange(...)`
  - `armasimfit(..., pvec=..., qvec=...)` order arguments are emitted as real vectors as required by the procedure interface.
- Rewrites method-name shorthands to quoted strings where required (for example `method=burg` -> `method="burg"`).
- Treats `run("...fi")` in scripts as non-transpilable runtime control and comments it out in generated Fortran.

## Batch transpile runner (`xpytr_all.py`)

Runs `transpile_session.py` across many `.fi` scripts, compiles with `Makefile_tests`, and optionally runs interpreter scripts and generated executables.

```bat
python xpytr_all.py --include-re *simfit*.fi
```

Common options:

- `--include-re REGEX_OR_GLOB` include only matching files (supports regex, and glob-like patterns such as `*simfit*.fi`).
- `--exclude-re REGEX_OR_GLOB` exclude matching files.
- `--files "a.fi b.fi"` or `--files a.fi,b.fi` explicit file list.
- `--limit N` process only first `N` selected files.
- `--run-script` run `fcalc` on each script before Fortran compile.
- `--run-exe` run compiled `tests.exe`.
- `--hide-output` suppress runtime stdout/stderr from interpreter/exe runs.
- `--noplot` propagate `--noplot` to interpreter runs.
- `--time` print timing DataFrame and summary stats.
- `--fail-fast` stop on first failure.
- `--resume` / `--restart` resume or reset checkpoint (`xpytr_all_state.json` by default).

When failures occur, it writes `xpytr_all_errors.txt` containing, for each failed script:

- original `.fi` script
- transpiled `tests.f90` (when available)
- tool/compiler/runtime output

## Interpreter-only commands

Some REPL/workspace commands are not mapped to standalone Fortran, e.g. `cor` (workspace correlation matrix mode), `?vars`, `clear`, `del ...`, and statement-form `read ...`.

## Notes

- Session logs and examples in this repository use `.fi` scripts.
- `Makefile_tests` builds `tests.f90` together with project modules into `tests.exe`.
