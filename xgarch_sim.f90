program xgarch_sim
! simulate a garch(1,1) process with normal errors and verify that mle recovers 
! the parameters, trying several error distributions
use kind_mod, only: dp
use stats_mod, only: mean, sd, kurtosis
use garch_mod, only: garch11_sim, garch11_fit, garch11_uncond_var, &
   garch11_uncond_exkurt, print_garch_est_true
implicit none
integer, parameter :: n = 25000, nfit = n, nprint_eps = 0, burnin = 2000, &
   iseed = 12345   
real(kind=dp), parameter :: mu = 0.0_dp, alpha = 0.15_dp, beta = 0.80_dp, &
   omega = 1.0_dp - alpha - beta
integer :: i, idist
real(kind=dp) :: m, s, v_theory, mh, exkurt_samp, exkurt_theory, muhat, &
   omegahat, alphahat, betahat, llhat, var_eps, eps(n), h(n)
character (len=*), parameter :: fmt_cr = "(a12, *(f12.6))", dist_vec(*) = &
   [character (len=20) :: "normal", "sech"]

call garch11_sim(n, omega, alpha, beta, eps, h, burnin=burnin, iseed=iseed)

m = mean(eps)
s = sd(eps)
var_eps = s**2
mh = mean(h)
v_theory = garch11_uncond_var(omega, alpha, beta)
exkurt_samp = kurtosis(eps)
exkurt_theory = garch11_uncond_exkurt(omega, alpha, beta)

print "('garch(1,1) sim with normal innovations')"
print "('n=',i0,' burnin=',i0,' seed=',i0)", n, burnin, iseed
print "(/,3a12,/,3f12.6)", "omega", "alpha", "beta", omega, alpha, beta
print "(/,*(a12))", "", "theory", "sample", "diff"
print fmt_cr, "mean(eps)", 0.0_dp, m, -m
print fmt_cr, "mean(h)", v_theory, mh, v_theory - mh
print fmt_cr, "var(eps)", v_theory, var_eps, v_theory - var_eps
print fmt_cr, "kurtosis" ,exkurt_theory, exkurt_samp, exkurt_theory - exkurt_samp

if (nprint_eps > 0) then
   print "('first ', i0, ' eps and h:')", nprint_eps
   do i=1,min(nprint_eps, size(eps))
      print "(i4,2(1x,f12.6))", i, eps(i), h(i)
   end do
end if

do idist = 1, size(dist_vec)
   call garch11_fit(eps(1:nfit), muhat, omegahat, alphahat, betahat, llhat, &
      dist=dist_vec(idist))

   print "(/,'garch(1,1) fit by mle (', a, '), nelder-mead')", &
      trim(dist_vec(idist))
   print "('var_hat_uncond:',f12.6)", garch11_uncond_var(omegahat, alphahat, &
      betahat)
   print "('loglik_hat:',f16.6)", llhat

   call print_garch_est_true(muhat, omegahat, alphahat, betahat, mu, omega, &
      alpha, beta)
end do
end program xgarch_sim
