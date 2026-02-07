# Distributions in Calc

This page maps interpreter distribution names to common names in the statistical literature.

Calc naming follows R-style prefixes in many places:
- `r*`: random generation
- `d*`: density / pdf
- `p*`: CDF
- `q*`: quantile / inverse CDF
- `fit_*`: parameter fitting
- `mssk_*`: theoretical mean, sd, skewness, excess kurtosis

## Name mapping

| Interpreter name family | Statistical name |
|---|---|
| `runif`, `dunif`, `punif`, `qunif` | Uniform distribution (default: Uniform(0,1)) |
| `rnorm`, `dnorm`, `pnorm`, `qnorm`, `fit_norm` | Normal (Gaussian) distribution |
| `rexp`, `dexp`, `pexp`, `qexp`, `fit_exp`, `mssk_exp` | Exponential distribution |
| `rgamma`, `dgamma`, `pgamma`, `qgamma`, `fit_gamma`, `mssk_gamma` | Gamma distribution |
| `rlnorm`, `dlnorm`, `plnorm`, `qlnorm`, `fit_lnorm`, `mssk_lnorm` | Lognormal distribution |
| `rt`, `dt`, `pt`, `qt`, `fit_t`, `mssk_t` | Student's t distribution |
| `rnct`, `dnct`, `pnct`, `qnct`, `fit_nct`, `mssk_nct` | Noncentral Student's t distribution |
| `rmixnorm`, `dmixnorm`, `pmixnorm`, `qmixnorm`, `fit_mixnorm`, `mssk_mixnorm` | Finite mixture of normal distributions |
| `rchisq`, `dchisq`, `pchisq`, `qchisq`, `fit_chisq`, `mssk_chisq` | Chi-square distribution |
| `rf`, `df`, `pf`, `qf`, `fit_f`, `mssk_f` | F distribution (Fisher-Snedecor) |
| `rbeta`, `dbeta`, `pbeta`, `qbeta`, `fit_beta`, `mssk_beta` | Beta distribution |
| `rlogis`, `dlogis`, `plogis`, `qlogis`, `fit_logis`, `mssk_logis` | Logistic distribution |
| `rsech`, `dsech`, `psech`, `qsech`, `fit_sech`, `mssk_sech` | Hyperbolic secant distribution |
| `rlaplace`, `dlaplace`, `plaplace`, `qlaplace`, `fit_laplace`, `mssk_laplace` | Laplace (double exponential) distribution |
| `rcauchy`, `dcauchy`, `pcauchy`, `qcauchy`, `fit_cauchy` | Cauchy distribution |
| `rged`, `dged`, `pged`, `qged`, `fit_ged` | Generalized Error Distribution (GED, generalized normal / exponential power) |
| `rhyperb`, `dhyperb`, `phyperb`, `qhyperb`, `fit_hyperb` | Hyperbolic distribution |

## Notes

- Uniform helper defaults use the standard support `[0,1]` when bounds are omitted.
- `mssk(x)` computes empirical moments from a sample, while `mssk_*` routines return theoretical moments for named distributions.
- `mssk_nct(df)` defaults to central t moments (`ncp=0`); `mssk_nct(df, ncp)` uses the noncentral case.
- `fit_mixnorm(x, k)` returns a length `3k` vector as `[wgt(1:k), mean(1:k), sd(1:k)]`.
