# Distributions in Calc

This page maps interpreter distribution names to statistical names and parameters, and shows which helper families are available.

Calc naming follows R-style prefixes:
- `r*`: random generation
- `d*`: density / pdf
- `p*`: CDF
- `q*`: quantile / inverse CDF
- `fit_*`: parameter fitting
- `mssk_*`: theoretical mean, sd, skewness, excess kurtosis
- `skew_*`: theoretical skewness (shape-driven families)
- `kurt_*`: theoretical excess kurtosis (shape-driven families)

## Name Mapping

| Interpreter name family | Statistical name (parameters) |
|---|---|
| `runif`, `dunif`, `punif`, `qunif`, `mssk_unif` | Uniform `(a, b)` |
| `rnorm`, `dnorm`, `pnorm`, `qnorm`, `fit_norm`, `mssk_norm` | Normal `(mean, sd)` |
| `rexp`, `dexp`, `pexp`, `qexp`, `fit_exp`, `mssk_exp` | Exponential `(rate)` |
| `rgamma`, `dgamma`, `pgamma`, `qgamma`, `fit_gamma`, `mssk_gamma`, `skew_gamma`, `kurt_gamma` | Gamma `(shape, scale)` |
| `rlnorm`, `dlnorm`, `plnorm`, `qlnorm`, `fit_lnorm`, `mssk_lnorm`, `skew_lnorm`, `kurt_lnorm` | Lognormal `(meanlog, sdlog)` |
| `rt`, `dt`, `pt`, `qt`, `fit_t`, `mssk_t`, `kurt_t` | Student t `(df)` |
| `rnct`, `dnct`, `pnct`, `qnct`, `fit_nct`, `mssk_nct`, `skew_nct`, `kurt_nct` | Noncentral Student t `(df, ncp)` |
| `rmixnorm`, `dmixnorm`, `pmixnorm`, `qmixnorm`, `fit_mixnorm`, `mssk_mixnorm` | Finite normal mixture `(wgt, mu, sig)` |
| `rchisq`, `dchisq`, `pchisq`, `qchisq`, `fit_chisq`, `mssk_chisq`, `skew_chisq`, `kurt_chisq` | Chi-square `(df)` |
| `rf`, `df`, `pf`, `qf`, `fit_f`, `mssk_f`, `skew_f`, `kurt_f` | F `(df1, df2)` |
| `rbeta`, `dbeta`, `pbeta`, `qbeta`, `fit_beta`, `mssk_beta`, `skew_beta`, `kurt_beta` | Beta `(a, b)` |
| `rlogis`, `dlogis`, `plogis`, `qlogis`, `fit_logis`, `mssk_logis` | Logistic `(loc, scale)` |
| `rsech`, `dsech`, `psech`, `qsech`, `fit_sech`, `mssk_sech` | Hyperbolic secant `(loc, scale)` |
| `rlaplace`, `dlaplace`, `plaplace`, `qlaplace`, `fit_laplace`, `mssk_laplace` | Laplace `(loc, scale)` |
| `rcauchy`, `dcauchy`, `pcauchy`, `qcauchy`, `fit_cauchy`, `mssk_cauchy` | Cauchy `(loc, scale)` |
| `rged`, `dged`, `pged`, `qged`, `fit_ged`, `mssk_ged`, `kurt_ged` | GED `(loc, scale, beta)` |
| `rhyperb`, `dhyperb`, `phyperb`, `qhyperb`, `fit_hyperb`, `mssk_hyperb`, `kurt_hyperb` | Hyperbolic `(loc, scale, alpha)` |

## Helper Coverage

| Distribution | `r` | `d` | `p` | `q` | `mssk` | `skew` | `kurt` | `fit` |
|---|---|---|---|---|---|---|---|---|
| Uniform | yes | yes | yes | yes | yes (`mssk_unif`) | no | no | no |
| Normal | yes | yes | yes | yes | yes (`mssk_norm`) | no | no | yes (`fit_norm`) |
| Exponential | yes | yes | yes | yes | yes (`mssk_exp`) | no | no | yes (`fit_exp`) |
| Gamma | yes | yes | yes | yes | yes (`mssk_gamma`) | yes (`skew_gamma`) | yes (`kurt_gamma`) | yes (`fit_gamma`) |
| Lognormal | yes | yes | yes | yes | yes (`mssk_lnorm`) | yes (`skew_lnorm`) | yes (`kurt_lnorm`) | yes (`fit_lnorm`) |
| Student t | yes | yes | yes | yes | yes (`mssk_t`) | no | yes (`kurt_t`) | yes (`fit_t`) |
| Noncentral t | yes | yes | yes | yes | yes (`mssk_nct`) | yes (`skew_nct`) | yes (`kurt_nct`) | yes (`fit_nct`) |
| Normal mixture | yes | yes | yes | yes | yes (`mssk_mixnorm`) | no | no | yes (`fit_mixnorm`) |
| Chi-square | yes | yes | yes | yes | yes (`mssk_chisq`) | yes (`skew_chisq`) | yes (`kurt_chisq`) | yes (`fit_chisq`) |
| F | yes | yes | yes | yes | yes (`mssk_f`) | yes (`skew_f`) | yes (`kurt_f`) | yes (`fit_f`) |
| Beta | yes | yes | yes | yes | yes (`mssk_beta`) | yes (`skew_beta`) | yes (`kurt_beta`) | yes (`fit_beta`) |
| Logistic | yes | yes | yes | yes | yes (`mssk_logis`) | no | no | yes (`fit_logis`) |
| Hyperbolic secant | yes | yes | yes | yes | yes (`mssk_sech`) | no | no | yes (`fit_sech`) |
| Laplace | yes | yes | yes | yes | yes (`mssk_laplace`) | no | no | yes (`fit_laplace`) |
| Cauchy | yes | yes | yes | yes | yes (`mssk_cauchy`) | no | no | yes (`fit_cauchy`) |
| GED | yes | yes | yes | yes | yes (`mssk_ged`) | no | yes (`kurt_ged`) | yes (`fit_ged`) |
| Hyperbolic | yes | yes | yes | yes | yes (`mssk_hyperb`) | no | yes (`kurt_hyperb`) | yes (`fit_hyperb`) |

## Notes

- `mssk(x)` computes empirical moments from a sample, while `mssk_*` returns theoretical moments.
- `mssk_cauchy(loc, scale)` returns `NaN` moments because Cauchy moments are undefined.
- `mssk_nct(df)` defaults to central-t moments (`ncp=0`); `mssk_nct(df, ncp)` uses the noncentral case.
- `fit_mixnorm(x, k)` returns length `3k`: `[wgt(1:k), mean(1:k), sd(1:k)]`.
