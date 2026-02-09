module garch_mod
use kind_mod, only: dp
use constants_mod, only: log_two_pi, log_two, pi
use random_mod, only: random_normal, random_seed_init
use stats_mod, only: nelder_mead, mean, sd
use util_mod, only: lowercase
use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
implicit none
private
public :: garch11_sim, garch11_fit, garch11_uncond_var, &
   garch11_uncond_exkurt, print_garch_est_true, garch11_sim_from_z
character (len=*), parameter :: fmt_p="(a12, *(f12.6))"

contains

pure function garch11_uncond_var(omega, alpha, beta) result(v)
! compute the unconditional variance of a garch(1,1) process (if it exists).
real(kind=dp), intent(in) :: omega ! variance intercept (>0).
real(kind=dp), intent(in) :: alpha ! arch coefficient (>=0).
real(kind=dp), intent(in) :: beta ! garch coefficient (>=0).
real(kind=dp) :: v
real(kind=dp) :: denom

denom = 1.0_dp - alpha - beta
if (denom > 0.0_dp) then
   v = omega / denom
else
   v = ieee_value(0.0_dp, ieee_quiet_nan)
end if
end function garch11_uncond_var

pure function garch11_uncond_exkurt(omega, alpha, beta, exkurt_z) result(exkurt)
! compute unconditional excess kurtosis for garch(1,1) with standardized z_t having given excess kurtosis (default 0 => normal).
real(kind=dp), intent(in) :: omega    ! variance intercept (>0).
real(kind=dp), intent(in) :: alpha    ! arch coefficient (>=0).
real(kind=dp), intent(in) :: beta     ! garch coefficient (>=0).
real(kind=dp), intent(in), optional :: exkurt_z ! innovation excess kurtosis; default 0.0 (normal).
real(kind=dp) :: exkurt
real(kind=dp) :: exkurt_z_loc
real(kind=dp) :: kurt_z
real(kind=dp) :: eh
real(kind=dp) :: eh2
real(kind=dp) :: denom1
real(kind=dp) :: denom2

exkurt_z_loc = 0.0_dp
if (present(exkurt_z)) exkurt_z_loc = exkurt_z

kurt_z = 3.0_dp + exkurt_z_loc
if (kurt_z <= 0.0_dp) then
   exkurt = ieee_value(0.0_dp, ieee_quiet_nan)
   return
end if

denom1 = 1.0_dp - alpha - beta
if (denom1 <= 0.0_dp) then
   exkurt = ieee_value(0.0_dp, ieee_quiet_nan)
   return
end if

eh = omega / denom1

! fourth-moment existence requires: alpha^2*kurt_z + 2*alpha*beta + beta^2 < 1
denom2 = 1.0_dp - (kurt_z*alpha*alpha + 2.0_dp*alpha*beta + beta*beta)
if (denom2 <= 0.0_dp) then
   exkurt = ieee_value(0.0_dp, ieee_quiet_nan)
   return
end if

eh2 = (omega*omega + 2.0_dp*omega*(alpha + beta)*eh) / denom2
exkurt = kurt_z*eh2/(eh*eh) - 3.0_dp
end function garch11_uncond_exkurt

subroutine garch11_sim(n, omega, alpha, beta, eps, h, z, burnin, iseed, h0)
! simulate eps_t from garch(1,1) with normal innovations and optional burn-in.
integer, intent(in) :: n ! number of observations to return.
real(kind=dp), intent(in) :: omega ! variance intercept (>0).
real(kind=dp), intent(in) :: alpha ! arch coefficient (>=0).
real(kind=dp), intent(in) :: beta ! garch coefficient (>=0).
real(kind=dp), intent(out) :: eps(n) ! simulated series eps(1:n).
real(kind=dp), intent(out), optional :: h(n) ! conditional variances h(1:n).
real(kind=dp), intent(out), optional :: z(n) ! iid n(0,1) draws z(1:n).
integer, intent(in), optional :: burnin ! number of burn-in steps (discarded).
integer, intent(in), optional :: iseed ! integer seed for random_seed_init.
real(kind=dp), intent(in), optional :: h0 ! initial variance h_0 for recursion.

integer :: i
integer :: b
real(kind=dp) :: hprev
real(kind=dp) :: eprev
real(kind=dp) :: hcur
real(kind=dp) :: zcur
real(kind=dp) :: denom

b = 0
if (present(burnin)) b = max(0, burnin)

if (present(iseed)) call random_seed_init(iseed)

denom = 1.0_dp - alpha - beta
if (present(h0)) then
   hprev = h0
else
   if (denom > 0.0_dp) then
      hprev = omega / denom
   else
      hprev = max(omega, 1.0_dp)
   end if
end if

eprev = 0.0_dp

if (b > 0) then
   do i=1,b
      zcur = random_normal()
      hcur = omega + alpha*(eprev*eprev) + beta*hprev
      if (hcur < 0.0_dp) hcur = 0.0_dp
      eprev = sqrt(hcur) * zcur
      hprev = hcur
   end do
end if

do i=1,n
   zcur = random_normal()
   hcur = omega + alpha*(eprev*eprev) + beta*hprev
   if (hcur < 0.0_dp) hcur = 0.0_dp
   eps(i) = sqrt(hcur) * zcur
   if (present(h)) h(i) = hcur
   if (present(z)) z(i) = zcur
   eprev = eps(i)
   hprev = hcur
end do
end subroutine garch11_sim

pure subroutine garch11_u_to_par(u, mu, omega, alpha, beta)
! map an unconstrained vector u to constrained garch(1,1) parameters.
real(kind=dp), intent(in) :: u(:) ! unconstrained parameters (size 4).
real(kind=dp), intent(out) :: mu ! conditional mean parameter.
real(kind=dp), intent(out) :: omega ! variance intercept (>0).
real(kind=dp), intent(out) :: alpha ! arch coefficient in [0,1).
real(kind=dp), intent(out) :: beta ! garch coefficient in [0,1) with alpha+beta<1.
real(kind=dp) :: a
real(kind=dp) :: b
real(kind=dp) :: den

mu = u(1)
omega = exp(u(2))

a = exp(u(3))
b = exp(u(4))
den = 1.0_dp + a + b
alpha = a / den
beta = b / den
end subroutine garch11_u_to_par

pure subroutine garch11_u_to_par_df(u, mu, omega, alpha, beta, df)
! map an unconstrained vector u to constrained garch(1,1) parameters and student-t df.
real(kind=dp), intent(in) :: u(:) ! unconstrained parameters (size 5).
real(kind=dp), intent(out) :: mu ! conditional mean parameter.
real(kind=dp), intent(out) :: omega ! variance intercept (>0).
real(kind=dp), intent(out) :: alpha ! arch coefficient in [0,1).
real(kind=dp), intent(out) :: beta ! garch coefficient in [0,1) with alpha+beta<1.
real(kind=dp), intent(out) :: df ! student-t degrees of freedom (>2).
real(kind=dp) :: a
real(kind=dp) :: b
real(kind=dp) :: den
real(kind=dp), parameter :: df_eps = 1.0e-6_dp

mu = u(1)
omega = exp(u(2))

a = exp(u(3))
b = exp(u(4))
den = 1.0_dp + a + b
alpha = a / den
beta = b / den

df = 2.0_dp + exp(u(5)) + df_eps
end subroutine garch11_u_to_par_df


pure subroutine garch11_u_to_par_ged(u, mu, omega, alpha, beta, beta_ged)
! map an unconstrained vector u to constrained garch(1,1) parameters and ged shape beta_ged.
real(kind=dp), intent(in) :: u(:) ! unconstrained parameters (size 5).
real(kind=dp), intent(out) :: mu ! conditional mean parameter.
real(kind=dp), intent(out) :: omega ! variance intercept (>0).
real(kind=dp), intent(out) :: alpha ! arch coefficient in [0,1).
real(kind=dp), intent(out) :: beta ! garch coefficient in [0,1) with alpha+beta<1.
real(kind=dp), intent(out) :: beta_ged ! ged shape parameter (>beta_min).
real(kind=dp) :: a
real(kind=dp) :: b
real(kind=dp) :: den
real(kind=dp), parameter :: beta_min = 0.100001_dp

mu = u(1)
omega = exp(u(2))

a = exp(u(3))
b = exp(u(4))
den = 1.0_dp + a + b
alpha = a / den
beta = b / den

beta_ged = beta_min + exp(u(5))
end subroutine garch11_u_to_par_ged

pure function logcosh(x) result(v)
! compute log(cosh(x)) in a numerically stable way.
real(kind=dp), intent(in) :: x ! function argument.
real(kind=dp) :: v
real(kind=dp) :: ax

ax = abs(x)
v = ax + log(1.0_dp + exp(-2.0_dp*ax)) - log_two
end function logcosh

pure function garch11_loglik_normal(y, mu, omega, alpha, beta, h0) result(ll)
! compute the gaussian loglikelihood for garch(1,1) given data y.
real(kind=dp), intent(in) :: y(:) ! observed series (data).
real(kind=dp), intent(in) :: mu ! conditional mean parameter.
real(kind=dp), intent(in) :: omega ! variance intercept (>0).
real(kind=dp), intent(in) :: alpha ! arch coefficient (>=0).
real(kind=dp), intent(in) :: beta ! garch coefficient (>=0).
real(kind=dp), intent(in), optional :: h0 ! initial variance h_0 for recursion.
real(kind=dp) :: ll
integer :: t
integer :: n
real(kind=dp) :: eprev
real(kind=dp) :: ecur
real(kind=dp) :: hprev
real(kind=dp) :: hcur
real(kind=dp) :: denom
real(kind=dp), parameter :: hmin = 1.0e-12_dp

n = size(y)
ll = 0.0_dp

denom = 1.0_dp - alpha - beta
if (present(h0)) then
   hprev = h0
else
   if (denom > 1.0e-10_dp) then
      hprev = omega / denom
   else
      hprev = max(omega, hmin)
   end if
end if

if (hprev < hmin) hprev = hmin
eprev = 0.0_dp

do t=1,n
   hcur = omega + alpha*(eprev*eprev) + beta*hprev
   if (hcur <= hmin) then
      ll = -huge(1.0_dp)
      return
   end if
   ecur = y(t) - mu
   ll = ll - 0.5_dp*(log_two_pi + log(hcur) + (ecur*ecur)/hcur)
   eprev = ecur
   hprev = hcur
end do
end function garch11_loglik_normal

pure function garch11_loglik_sech(y, mu, omega, alpha, beta, h0) result(ll)
! compute the hyperbolic-secant loglikelihood for garch(1,1) given data y.
real(kind=dp), intent(in) :: y(:) ! observed series (data).
real(kind=dp), intent(in) :: mu ! conditional mean parameter.
real(kind=dp), intent(in) :: omega ! variance intercept (>0).
real(kind=dp), intent(in) :: alpha ! arch coefficient (>=0).
real(kind=dp), intent(in) :: beta ! garch coefficient (>=0).
real(kind=dp), intent(in), optional :: h0 ! initial variance h_0 for recursion.
real(kind=dp) :: ll
integer :: t
integer :: n
real(kind=dp) :: eprev
real(kind=dp) :: ecur
real(kind=dp) :: hprev
real(kind=dp) :: hcur
real(kind=dp) :: denom
real(kind=dp) :: sqh
real(kind=dp) :: x
real(kind=dp), parameter :: hmin = 1.0e-12_dp
real(kind=dp), parameter :: c = 0.5_dp*pi

n = size(y)
ll = 0.0_dp

denom = 1.0_dp - alpha - beta
if (present(h0)) then
   hprev = h0
else
   if (denom > 1.0e-10_dp) then
      hprev = omega / denom
   else
      hprev = max(omega, hmin)
   end if
end if

if (hprev < hmin) hprev = hmin
eprev = 0.0_dp

do t=1,n
   hcur = omega + alpha*(eprev*eprev) + beta*hprev
   if (hcur <= hmin) then
      ll = -huge(1.0_dp)
      return
   end if
   ecur = y(t) - mu
   sqh = sqrt(hcur)
   x = c*(ecur/sqh)
   ll = ll - log_two - logcosh(x) - 0.5_dp*log(hcur)
   eprev = ecur
   hprev = hcur
end do
end function garch11_loglik_sech

pure function garch11_loglik_logis(y, mu, omega, alpha, beta, h0) result(ll)
! compute the (standardized) logistic loglikelihood for garch(1,1) given data y.
real(kind=dp), intent(in) :: y(:) ! observed series (data).
real(kind=dp), intent(in) :: mu ! conditional mean parameter.
real(kind=dp), intent(in) :: omega ! variance intercept (>0).
real(kind=dp), intent(in) :: alpha ! arch coefficient (>=0).
real(kind=dp), intent(in) :: beta ! garch coefficient (>=0).
real(kind=dp), intent(in), optional :: h0 ! initial variance h_0 for recursion.
real(kind=dp) :: ll
integer :: t
integer :: n
real(kind=dp) :: eprev
real(kind=dp) :: ecur
real(kind=dp) :: hprev
real(kind=dp) :: hcur
real(kind=dp) :: denom
real(kind=dp) :: sqh
real(kind=dp) :: z
real(kind=dp) :: x
real(kind=dp) :: logk
real(kind=dp), parameter :: hmin = 1.0e-12_dp
real(kind=dp), parameter :: c = 0.5_dp*pi/sqrt(3.0_dp)

n = size(y)
ll = 0.0_dp

logk = log(pi/sqrt(3.0_dp))

denom = 1.0_dp - alpha - beta
if (present(h0)) then
   hprev = h0
else
   if (denom > 1.0e-10_dp) then
      hprev = omega / denom
   else
      hprev = max(omega, hmin)
   end if
end if

if (hprev < hmin) hprev = hmin
eprev = 0.0_dp

do t=1,n
   hcur = omega + alpha*(eprev*eprev) + beta*hprev
   if (hcur <= hmin) then
      ll = -huge(1.0_dp)
      return
   end if
   ecur = y(t) - mu
   sqh = sqrt(hcur)
   z = ecur/sqh
   x = c*z
   ! logistic(0,1) has pdf 1/(4*cosh^2(u/2)); standardize to var 1 via u=(pi/sqrt(3))*z.
   ll = ll + logk - 2.0_dp*log_two - 2.0_dp*logcosh(x) - 0.5_dp*log(hcur)
   eprev = ecur
   hprev = hcur
end do
end function garch11_loglik_logis


pure function garch11_loglik_laplace(y, mu, omega, alpha, beta, h0) result(ll)
! compute the (standardized) laplace loglikelihood for garch(1,1) given data y.
real(kind=dp), intent(in) :: y(:) ! observed series (data).
real(kind=dp), intent(in) :: mu ! conditional mean parameter.
real(kind=dp), intent(in) :: omega ! variance intercept (>0).
real(kind=dp), intent(in) :: alpha ! arch coefficient (>=0).
real(kind=dp), intent(in) :: beta ! garch coefficient (>=0).
real(kind=dp), intent(in), optional :: h0 ! initial variance h_0 for recursion.
real(kind=dp) :: ll
integer :: t
integer :: n
real(kind=dp) :: eprev
real(kind=dp) :: ecur
real(kind=dp) :: hprev
real(kind=dp) :: hcur
real(kind=dp) :: denom
real(kind=dp) :: sqh
real(kind=dp) :: z
real(kind=dp) :: rt2
real(kind=dp) :: logk
real(kind=dp), parameter :: hmin = 1.0e-12_dp

n = size(y)
ll = 0.0_dp

! laplace(0,b) with b=1/sqrt(2) has var 1 => logpdf(z) = -0.5*log(2) - sqrt(2)*|z|
rt2 = sqrt(2.0_dp)
logk = -0.5_dp*log_two

denom = 1.0_dp - alpha - beta
if (present(h0)) then
   hprev = h0
else
   if (denom > 1.0e-10_dp) then
      hprev = omega / denom
   else
      hprev = max(omega, hmin)
   end if
end if

if (hprev < hmin) hprev = hmin
eprev = 0.0_dp

do t=1,n
   hcur = omega + alpha*(eprev*eprev) + beta*hprev
   if (hcur <= hmin) then
      ll = -huge(1.0_dp)
      return
   end if
   ecur = y(t) - mu
   sqh = sqrt(hcur)
   z = ecur/sqh
   ll = ll + logk - rt2*abs(z) - 0.5_dp*log(hcur)
   eprev = ecur
   hprev = hcur
end do
end function garch11_loglik_laplace

pure function garch11_loglik_t(y, mu, omega, alpha, beta, df, h0) result(ll)
! compute the (standardized) student-t loglikelihood for garch(1,1) given data y.
real(kind=dp), intent(in) :: y(:) ! observed series (data).
real(kind=dp), intent(in) :: mu ! conditional mean parameter.
real(kind=dp), intent(in) :: omega ! variance intercept (>0).
real(kind=dp), intent(in) :: alpha ! arch coefficient (>=0).
real(kind=dp), intent(in) :: beta ! garch coefficient (>=0).
real(kind=dp), intent(in) :: df ! student-t degrees of freedom (>2).
real(kind=dp), intent(in), optional :: h0 ! initial variance h_0 for recursion.
real(kind=dp) :: ll
integer :: t
integer :: n
real(kind=dp) :: eprev
real(kind=dp) :: ecur
real(kind=dp) :: hprev
real(kind=dp) :: hcur
real(kind=dp) :: denom
real(kind=dp) :: sqh
real(kind=dp) :: z
real(kind=dp) :: dfm2
real(kind=dp) :: logc
real(kind=dp) :: half_dfp1
real(kind=dp) :: q
real(kind=dp), parameter :: hmin = 1.0e-12_dp

n = size(y)
ll = 0.0_dp

dfm2 = df - 2.0_dp
if (dfm2 <= 0.0_dp) then
   ll = -huge(1.0_dp)
   return
end if

! standardized t: logc = log gamma((df+1)/2) - log gamma(df/2) - 0.5*log(pi*(df-2))
logc = log_gamma(0.5_dp*(df + 1.0_dp)) - log_gamma(0.5_dp*df) - 0.5_dp*log(pi*dfm2)
half_dfp1 = 0.5_dp*(df + 1.0_dp)

denom = 1.0_dp - alpha - beta
if (present(h0)) then
   hprev = h0
else
   if (denom > 1.0e-10_dp) then
      hprev = omega / denom
   else
      hprev = max(omega, hmin)
   end if
end if

if (hprev < hmin) hprev = hmin
eprev = 0.0_dp

do t=1,n
   hcur = omega + alpha*(eprev*eprev) + beta*hprev
   if (hcur <= hmin) then
      ll = -huge(1.0_dp)
      return
   end if
   ecur = y(t) - mu
   sqh = sqrt(hcur)
   z = ecur/sqh
   q = (z*z)/dfm2
   ll = ll + logc - half_dfp1*log(1.0_dp + q) - 0.5_dp*log(hcur)
   eprev = ecur
   hprev = hcur
end do
end function garch11_loglik_t


pure function garch11_loglik_ged(y, mu, omega, alpha, beta, beta_ged, h0) result(ll)
! compute the (standardized) generalized error distribution (ged) loglikelihood for garch(1,1) given data y.
real(kind=dp), intent(in) :: y(:) ! observed series (data).
real(kind=dp), intent(in) :: mu ! conditional mean parameter.
real(kind=dp), intent(in) :: omega ! variance intercept (>0).
real(kind=dp), intent(in) :: alpha ! arch coefficient (>=0).
real(kind=dp), intent(in) :: beta ! garch coefficient (>=0).
real(kind=dp), intent(in) :: beta_ged ! ged shape parameter (>0).
real(kind=dp), intent(in), optional :: h0 ! initial variance h_0 for recursion.
real(kind=dp) :: ll
integer :: t
integer :: n
real(kind=dp) :: eprev
real(kind=dp) :: ecur
real(kind=dp) :: hprev
real(kind=dp) :: hcur
real(kind=dp) :: denom
real(kind=dp) :: sqh
real(kind=dp) :: z
real(kind=dp) :: psh
real(kind=dp) :: invp
real(kind=dp) :: lg1
real(kind=dp) :: lg3
real(kind=dp) :: a
real(kind=dp) :: logc
real(kind=dp) :: w
real(kind=dp) :: lwp
real(kind=dp) :: pow
real(kind=dp), parameter :: hmin = 1.0e-12_dp
real(kind=dp), parameter :: beta_min = 0.100001_dp
real(kind=dp), parameter :: log_max = 700.0_dp

n = size(y)
ll = 0.0_dp

psh = beta_ged
if (psh <= beta_min) then
   ll = -huge(1.0_dp)
   return
end if

invp = 1.0_dp/psh
lg1 = log_gamma(invp)
lg3 = log_gamma(3.0_dp*invp)

! scale a so that var(z)=1: a = sqrt(gamma(1/p)/gamma(3/p))
a = exp(0.5_dp*(lg1 - lg3))

! log normalizing constant: log(p) - log(2) - log(a) - log_gamma(1/p)
logc = log(psh) - log_two - log(a) - lg1

denom = 1.0_dp - alpha - beta
if (present(h0)) then
   hprev = h0
else
   if (denom > 1.0e-10_dp) then
      hprev = omega / denom
   else
      hprev = max(omega, hmin)
   end if
end if

if (hprev < hmin) hprev = hmin
eprev = 0.0_dp

do t=1,n
   hcur = omega + alpha*(eprev*eprev) + beta*hprev
   if (hcur <= hmin) then
      ll = -huge(1.0_dp)
      return
   end if
   ecur = y(t) - mu
   sqh = sqrt(hcur)
   z = ecur/sqh

   w = abs(z)/a
   if (w == 0.0_dp) then
      pow = 0.0_dp
   else
      lwp = psh*log(w)
      if (lwp > log_max) then
         ll = -huge(1.0_dp)
         return
      end if
      pow = exp(lwp)
   end if

   ll = ll + logc - pow - 0.5_dp*log(hcur)

   eprev = ecur
   hprev = hcur
end do
end function garch11_loglik_ged

subroutine print_garch_est_true(muhat, omegahat, alphahat, betahat, &
                                mu   , omega   , alpha   , beta)
! print estimated and true GARCH parameters and their differences
real(kind=dp), intent(in) :: muhat, omegahat, alphahat, betahat, mu, omega, &
   alpha, beta
print "(/,*(a12))", "", "estim", "true", "diff"
print fmt_p, "mu", muhat, mu, muhat - mu
print fmt_p, "omega", omegahat, omega, omegahat - omega
print fmt_p, "alpha", alphahat, alpha, alphahat - alpha
print fmt_p, "beta", betahat, beta, betahat - beta
end subroutine print_garch_est_true

subroutine garch11_sim_from_z(omega, alpha, beta, z, eps, h, h0, eps0)
! simulate a garch(1,1) series using supplied standardized innovations z (no burn-in).
real(kind=dp), intent(in) :: omega ! variance intercept (>0).
real(kind=dp), intent(in) :: alpha ! arch coefficient (>=0).
real(kind=dp), intent(in) :: beta ! garch coefficient (>=0).
real(kind=dp), intent(in) :: z(:) ! iid innovations (mean 0, var 1); length determines n.
real(kind=dp), intent(out) :: eps(:) ! simulated eps(1:n); must have size(z).
real(kind=dp), intent(out), optional :: h(:) ! conditional variances h(1:n); must have size(z).
real(kind=dp), intent(in), optional :: h0 ! initial variance h_0 for recursion.
real(kind=dp), intent(in), optional :: eps0 ! initial shock eps_0 for recursion.

integer :: i
integer :: n
real(kind=dp) :: hprev
real(kind=dp) :: eprev
real(kind=dp) :: hcur
real(kind=dp) :: denom
real(kind=dp) :: nanv

nanv = ieee_value(0.0_dp, ieee_quiet_nan)
n = size(z)

if (size(eps) /= n) then
   eps = nanv
   if (present(h)) h = nanv
   return
end if

if (present(h)) then
   if (size(h) /= n) then
      eps = nanv
      h = nanv
      return
   end if
end if

denom = 1.0_dp - alpha - beta
if (present(h0)) then
   hprev = h0
else
   if (denom > 0.0_dp) then
      hprev = omega / denom
   else
      hprev = max(omega, 1.0_dp)
   end if
end if

eprev = 0.0_dp
if (present(eps0)) eprev = eps0

do i=1,n
   hcur = omega + alpha*(eprev*eprev) + beta*hprev
   if (hcur < 0.0_dp) hcur = 0.0_dp
   eps(i) = sqrt(hcur) * z(i)
   if (present(h)) h(i) = hcur
   eprev = eps(i)
   hprev = hcur
end do
end subroutine garch11_sim_from_z

subroutine garch11_fit(y, mu, omega, alpha, beta, loglik, u0, ubest, step,&
 max_iter, tol, dist, df, fit_df, beta_ged, fit_beta_ged)
! fit a garch(1,1) model by maximizing the loglikelihood using nelder-mead.
real(kind=dp), intent(in) :: y(:) ! observed series (data).
real(kind=dp), intent(out) :: mu ! fitted conditional mean.
real(kind=dp), intent(out) :: omega ! fitted variance intercept.
real(kind=dp), intent(out) :: alpha ! fitted arch coefficient.
real(kind=dp), intent(out) :: beta ! fitted garch coefficient.
real(kind=dp), intent(out) :: loglik ! maximized loglikelihood value.
real(kind=dp), intent(in), optional :: u0(4) ! optional starting point in unconstrained space.
real(kind=dp), intent(out), optional :: ubest(4) ! optional optimizer argmax in unconstrained space.
real(kind=dp), intent(in), optional :: step ! nelder-mead initial simplex step size.
integer, intent(in), optional :: max_iter ! nelder-mead iteration limit.
real(kind=dp), intent(in), optional :: tol ! nelder-mead termination tolerance.
character(len=*), intent(in), optional :: dist ! 'normal'(default),'sech','logis','laplace','t','ged'.
real(kind=dp), intent(inout), optional :: df ! student-t df (fixed or fitted; see fit_df).
logical, intent(in), optional :: fit_df ! if true and dist='t', estimate df; else fixed df.
real(kind=dp), intent(inout), optional :: beta_ged ! ged shape (fixed or fitted; see fit_beta_ged).
logical, intent(in), optional :: fit_beta_ged ! if true and dist='ged', estimate beta_ged; else fixed.

real(kind=dp) :: ustart(4)
real(kind=dp) :: ub(4)

real(kind=dp) :: ustart5(5)
real(kind=dp) :: ub5(5)

real(kind=dp) :: ustart5_ged(5)
real(kind=dp) :: ub5_ged(5)

real(kind=dp) :: step0
real(kind=dp) :: tol0
integer :: it0
real(kind=dp) :: m
real(kind=dp) :: s
real(kind=dp) :: v
real(kind=dp) :: omega0
real(kind=dp) :: alpha0
real(kind=dp) :: beta0
real(kind=dp) :: omden
real(kind=dp) :: a0
real(kind=dp) :: b0
real(kind=dp) :: eps
integer :: dist_id
character(len=:), allocatable :: d
real(kind=dp) :: nanv

logical :: fit_df0
real(kind=dp) :: df0
real(kind=dp) :: df_fixed
real(kind=dp), parameter :: df_min = 2.000001_dp

logical :: fit_beta_ged0
real(kind=dp) :: beta_ged0
real(kind=dp) :: beta_ged_fixed
real(kind=dp), parameter :: beta_ged_min = 0.100001_dp

eps = 1.0e-8_dp
nanv = ieee_value(0.0_dp, ieee_quiet_nan)

fit_df0 = .false.
if (present(fit_df)) fit_df0 = fit_df

fit_beta_ged0 = .false.
if (present(fit_beta_ged)) fit_beta_ged0 = fit_beta_ged

dist_id = 1
if (present(dist)) then
   d = lowercase(trim(adjustl(dist)))
   if (d == 'normal' .or. d == 'gaussian') then
      dist_id = 1
   else if (d == 'sech') then
      dist_id = 2
   else if (d == 'logis' .or. d == 'logistic') then
      dist_id = 3
   else if (d == 'laplace') then
      dist_id = 5
   else if (d == 't' .or. d == 'student' .or. d == 'studentt') then
      dist_id = 4
   else if (d == 'ged' .or. d == 'generalizederror' .or. d == 'generalized_error') then
      dist_id = 6
   else
      mu = nanv
      omega = nanv
      alpha = nanv
      beta = nanv
      loglik = nanv
      if (present(df)) df = nanv
      if (present(beta_ged)) beta_ged = nanv
      if (present(ubest)) ubest = 0.0_dp
      return
   end if
end if

if (dist_id /= 4) fit_df0 = .false.
if (dist_id /= 6) fit_beta_ged0 = .false.

if (dist_id == 4) then
   if (.not. fit_df0) then
      if (.not. present(df)) then
         mu = nanv
         omega = nanv
         alpha = nanv
         beta = nanv
         loglik = nanv
         if (present(ubest)) ubest = 0.0_dp
         return
      end if
      df_fixed = df
      if (df_fixed <= df_min) df_fixed = df_min
   else
      df0 = 8.0_dp
      if (present(df)) df0 = df
      if (df0 <= df_min) df0 = df_min
   end if
end if

if (dist_id == 6) then
   if (.not. fit_beta_ged0) then
      if (.not. present(beta_ged)) then
         mu = nanv
         omega = nanv
         alpha = nanv
         beta = nanv
         loglik = nanv
         if (present(ubest)) ubest = 0.0_dp
         return
      end if
      beta_ged_fixed = beta_ged
      if (beta_ged_fixed <= beta_ged_min) beta_ged_fixed = beta_ged_min
   else
      beta_ged0 = 2.0_dp
      if (present(beta_ged)) beta_ged0 = beta_ged
      if (beta_ged0 <= beta_ged_min) beta_ged0 = beta_ged_min
   end if
end if

step0 = 0.1_dp
if (present(step)) step0 = step

it0 = 500
if (present(max_iter)) it0 = max_iter

tol0 = 1.0e-6_dp
if (present(tol)) tol0 = tol

if (present(u0)) then
   ustart = u0
else
   m = mean(y)
   s = sd(y)
   v = s*s

   alpha0 = 0.05_dp
   beta0 = 0.90_dp
   omden = 1.0_dp - alpha0 - beta0

   omega0 = v*omden
   if (omega0 <= 0.0_dp) omega0 = max(v*0.01_dp, eps)

   a0 = max(alpha0/omden, eps)
   b0 = max(beta0/omden, eps)

   ustart(1) = m
   ustart(2) = log(omega0)
   ustart(3) = log(a0)
   ustart(4) = log(b0)
end if

if (dist_id == 4 .and. fit_df0) then
   ustart5(1:4) = ustart
   ustart5(5) = log(max(df0 - 2.0_dp, eps))
end if

if (dist_id == 6 .and. fit_beta_ged0) then
   ustart5_ged(1:4) = ustart
   ustart5_ged(5) = log(max(beta_ged0 - beta_ged_min, eps))
end if

select case (dist_id)
case (1)
   ub = nelder_mead(loglik_u_normal, ustart, step0, it0, tol0)
case (2)
   ub = nelder_mead(loglik_u_sech, ustart, step0, it0, tol0)
case (3)
   ub = nelder_mead(loglik_u_logis, ustart, step0, it0, tol0)
case (5)
   ub = nelder_mead(loglik_u_laplace, ustart, step0, it0, tol0)
case (4)
   if (fit_df0) then
      ub5 = nelder_mead(loglik_u_t, ustart5, step0, it0, tol0)
   else
      ub = nelder_mead(loglik_u_t_fixed, ustart, step0, it0, tol0)
   end if
case (6)
   if (fit_beta_ged0) then
      ub5_ged = nelder_mead(loglik_u_ged, ustart5_ged, step0, it0, tol0)
   else
      ub = nelder_mead(loglik_u_ged_fixed, ustart, step0, it0, tol0)
   end if
end select

if (dist_id == 4 .and. fit_df0) then
   call garch11_u_to_par_df(ub5, mu, omega, alpha, beta, df0)
   loglik = garch11_loglik_t(y, mu, omega, alpha, beta, df0)
   if (present(df)) df = df0
   if (present(ubest)) ubest = ub5(1:4)
else if (dist_id == 6 .and. fit_beta_ged0) then
   call garch11_u_to_par_ged(ub5_ged, mu, omega, alpha, beta, beta_ged0)
   loglik = garch11_loglik_ged(y, mu, omega, alpha, beta, beta_ged0)
   if (present(beta_ged)) beta_ged = beta_ged0
   if (present(ubest)) ubest = ub5_ged(1:4)
else
   call garch11_u_to_par(ub, mu, omega, alpha, beta)
   select case (dist_id)
   case (1)
      loglik = garch11_loglik_normal(y, mu, omega, alpha, beta)
   case (2)
      loglik = garch11_loglik_sech(y, mu, omega, alpha, beta)
   case (3)
      loglik = garch11_loglik_logis(y, mu, omega, alpha, beta)
   case (5)
      loglik = garch11_loglik_laplace(y, mu, omega, alpha, beta)
   case (4)
      loglik = garch11_loglik_t(y, mu, omega, alpha, beta, df_fixed)
      if (present(df)) df = df_fixed
   case (6)
      loglik = garch11_loglik_ged(y, mu, omega, alpha, beta, beta_ged_fixed)
      if (present(beta_ged)) beta_ged = beta_ged_fixed
   end select
   if (present(ubest)) ubest = ub
end if

contains

function loglik_u_normal(u) result(f)
real(kind=dp), intent(in) :: u(:)
real(kind=dp) :: f
real(kind=dp) :: mu1, omega1, alpha1, beta1
if (size(u) /= 4) then
   f = -huge(1.0_dp)
   return
end if
call garch11_u_to_par(u, mu1, omega1, alpha1, beta1)
f = garch11_loglik_normal(y, mu1, omega1, alpha1, beta1)
end function loglik_u_normal

function loglik_u_sech(u) result(f)
real(kind=dp), intent(in) :: u(:)
real(kind=dp) :: f
real(kind=dp) :: mu1, omega1, alpha1, beta1
if (size(u) /= 4) then
   f = -huge(1.0_dp)
   return
end if
call garch11_u_to_par(u, mu1, omega1, alpha1, beta1)
f = garch11_loglik_sech(y, mu1, omega1, alpha1, beta1)
end function loglik_u_sech

function loglik_u_logis(u) result(f)
real(kind=dp), intent(in) :: u(:)
real(kind=dp) :: f
real(kind=dp) :: mu1, omega1, alpha1, beta1
if (size(u) /= 4) then
   f = -huge(1.0_dp)
   return
end if
call garch11_u_to_par(u, mu1, omega1, alpha1, beta1)
f = garch11_loglik_logis(y, mu1, omega1, alpha1, beta1)
end function loglik_u_logis

function loglik_u_laplace(u) result(f)
real(kind=dp), intent(in) :: u(:)
real(kind=dp) :: f
real(kind=dp) :: mu1, omega1, alpha1, beta1
if (size(u) /= 4) then
   f = -huge(1.0_dp)
   return
end if
call garch11_u_to_par(u, mu1, omega1, alpha1, beta1)
f = garch11_loglik_laplace(y, mu1, omega1, alpha1, beta1)
end function loglik_u_laplace

function loglik_u_t(u) result(f)
real(kind=dp), intent(in) :: u(:)
real(kind=dp) :: f
real(kind=dp) :: mu1, omega1, alpha1, beta1, df1
if (size(u) /= 5) then
   f = -huge(1.0_dp)
   return
end if
call garch11_u_to_par_df(u, mu1, omega1, alpha1, beta1, df1)
f = garch11_loglik_t(y, mu1, omega1, alpha1, beta1, df1)
end function loglik_u_t

function loglik_u_t_fixed(u) result(f)
real(kind=dp), intent(in) :: u(:)
real(kind=dp) :: f
real(kind=dp) :: mu1, omega1, alpha1, beta1
if (size(u) /= 4) then
   f = -huge(1.0_dp)
   return
end if
call garch11_u_to_par(u, mu1, omega1, alpha1, beta1)
f = garch11_loglik_t(y, mu1, omega1, alpha1, beta1, df_fixed)
end function loglik_u_t_fixed

function loglik_u_ged(u) result(f)
real(kind=dp), intent(in) :: u(:)
real(kind=dp) :: f
real(kind=dp) :: mu1, omega1, alpha1, beta1, beta_ged1
if (size(u) /= 5) then
   f = -huge(1.0_dp)
   return
end if
call garch11_u_to_par_ged(u, mu1, omega1, alpha1, beta1, beta_ged1)
f = garch11_loglik_ged(y, mu1, omega1, alpha1, beta1, beta_ged1)
end function loglik_u_ged

function loglik_u_ged_fixed(u) result(f)
real(kind=dp), intent(in) :: u(:)
real(kind=dp) :: f
real(kind=dp) :: mu1, omega1, alpha1, beta1
if (size(u) /= 4) then
   f = -huge(1.0_dp)
   return
end if
call garch11_u_to_par(u, mu1, omega1, alpha1, beta1)
f = garch11_loglik_ged(y, mu1, omega1, alpha1, beta1, beta_ged_fixed)
end function loglik_u_ged_fixed

end subroutine garch11_fit


end module garch_mod
