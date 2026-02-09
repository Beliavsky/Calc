program xgarch_sim_nonnormal
! simulate a garch(1,1) process with non-normal noise and fit parameters
! trying several noise distributions
use kind_mod, only: dp
use stats_mod, only: mean, sd, kurtosis, kurt_dist
use garch_mod, only: garch11_sim, garch11_fit, garch11_uncond_var, &
   garch11_uncond_exkurt, print_garch_est_true, garch11_sim_from_z
use random_mod, only: rsech, random_normal, noise_stdz, str_normal, &
   str_logistic, str_sech, str_laplace, str_student_t, str_ged
implicit none
integer, parameter :: n = 10**3, nfit = n, nprint_eps = 0, burnin = 2000, &
   iseed = 12345   
real(kind=dp), parameter :: mu = 0.0_dp, alpha = 0.10_dp, beta = 0.80_dp, &
   omega = 1.0_dp - alpha - beta, df_true = 6.0_dp, df_guess = 5.0_dp, &
   beta_ged_guess = 1.5_dp
integer :: i, idist, ndist
real(kind=dp) :: m, s, v_theory, mh, exkurt_samp, exkurt_theory, muhat, &
   omegahat, alphahat, betahat, llhat, var_eps, eps(n), h(n), zz(n), &
   exkurt_z, df, beta_ged, ucvar
character (len=*), parameter :: fmt_cr = "(a20, *(f15.6))", &
   fmt_cc = "(a20,*(1x,a))", fmt_cl ="(a20, *(1x,l))", dist_vec(*) = &
   [character (len=20) :: str_normal, str_logistic, str_sech, str_laplace, &
    str_student_t, str_ged], &
   noise_dist = str_student_t
logical, parameter :: fit_df = .true., fit_beta_ged = .false.
real(kind=dp), allocatable :: results(:,:)
ndist = size(dist_vec)
print fmt_cc, "noise distribution", trim(noise_dist)
print fmt_cl, "fit_df", fit_df
print fmt_cr, "df_guess", df_guess
print fmt_cl, "fit_beta_ged", fit_beta_ged
print fmt_cr, "beta_ged_guess", beta_ged_guess
df = df_guess
beta_ged = beta_ged_guess
zz = noise_stdz(n, noise_dist, df=df_true)
exkurt_z = kurt_dist(noise_dist, df=df_true)
call garch11_sim_from_z(omega, alpha, beta, zz, eps, h)
m = mean(eps)
s = sd(eps)
var_eps = s**2
mh = mean(h)
v_theory = garch11_uncond_var(omega, alpha, beta)
exkurt_samp = kurtosis(eps)
exkurt_theory = garch11_uncond_exkurt(omega, alpha, beta, exkurt_z)

print "('garch(1,1) sim')"
print "('n=',i0,' burnin=',i0,' seed=',i0)", n, burnin, iseed
print fmt_cr, "beta_ged_guess", beta_ged_guess
print "(/,3a12,/,3f12.6)", "omega", "alpha", "beta", omega, alpha, beta
print "(/,*(a12))", "", "theory", "sample", "diff"
print fmt_cr, "mean(eps)", 0.0_dp, m, -m
print fmt_cr, "mean(h)", v_theory, mh, v_theory - mh
print fmt_cr, "var(eps)", v_theory, var_eps, v_theory - var_eps
print fmt_cr, "kurtosis" ,exkurt_theory, exkurt_samp, exkurt_theory - exkurt_samp

print "(/,a, *(f10.6))", "mean, sd, kurt of noise:", mean(zz), sd(zz), kurtosis(zz)
if (nprint_eps > 0) then
   print "('first ', i0, ' eps and h:')", nprint_eps
   do i=1,min(nprint_eps, size(eps))
      print "(i4,2(1x,f12.6))", i, eps(i), h(i)
   end do
end if

allocate (results(ndist, 6))
do idist = 1, ndist
   call garch11_fit(eps(1:nfit), muhat, omegahat, alphahat, betahat, llhat, &
      dist=dist_vec(idist), df=df, fit_df=fit_df, beta_ged=beta_ged, &
      fit_beta_ged=fit_beta_ged)

   print "(/,'garch(1,1) fit by mle (', a, '), nelder-mead')", &
      trim(dist_vec(idist))
   ucvar = garch11_uncond_var(omegahat, alphahat, betahat)
   print "('var_hat_uncond:',f12.6)", ucvar
   print "('loglik_hat:',f16.6)", llhat
   results(idist, :) = [llhat, ucvar, muhat, omegahat, alphahat, betahat]
   call print_garch_est_true(muhat, omegahat, alphahat, betahat, mu, omega, &
      alpha, beta)
   if (dist_vec(idist) == str_student_t) print fmt_cr,"df:", df
end do
print "(/,a10, *(a15))", "dist", "loglik", "ucvar", "mu", "omega", "alpha", "beta"
do idist=1,ndist
   print "(a10, *(f15.6))", trim(dist_vec(idist)), results(idist, :)
end do
print*,"(9) done"
end program xgarch_sim_nonnormal
