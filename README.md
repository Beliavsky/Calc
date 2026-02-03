# Calc

Calc is a Fortran-based interactive statistics interpreter with a session-to-Fortran transpiler.

## What the interpreter supports

- Scalars and 1D real arrays.
- Array literals, slicing, arithmetic, reductions, and basic control flow.
- Random simulation and distribution helpers.
- Time-series helpers including ACF/PACF and AR/MA/ARMA/ARFIMA utilities.
- Plotting via gnuplot.

Examples:

```text
n = 10^4
x = rnorm(n)
acf(x, 10)
acf(x, 10, plot=.true.)
pacf(x, 10, plot=.true.)
acfpacf(x, 10, plot=.true.)
acfpacfar(x, 10, plot=.true.)

phi = [0.3]
theta = [0.5]
d = 0.2
x = arfimasim(n, d, phi, theta)
fiacf(d, 10)
arfimaacf(phi, theta, d, 10)
arfimafit(x, 1, 1)
```

`acf`/`pacf` return lags `1..n` and plotting is optional (`plot=.false.` by default).

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

Helper batch files:

- `pytr.bat <file.fi>`: transpile one `.fi` file to `tests.f90` and compile.
- `pytr_all.bat`: loop over all `*.fi` files in the current directory, transpile each, and compile each.

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
