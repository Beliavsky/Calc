module stats_mod
use kind_mod, only: dp
use constants_mod, only: pi
use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
use random_mod, only: random_normal
use qsort_mod, only: median
implicit none
private
public :: mean, sd, cor, cov, cumsum, cumprod, diff, standardize, &
          print_stats, skew, kurtosis, cummin, cummax, cummean, &
          geomean, harmean, acf, pacf, acfpacf, acfpacfar, fiacf, fracdiff, arcoef, arsim, masim, armasim, arfimasim, resample, regress, regress_multi, arfit, mafit, armafit, armafitgrid, armafitaic, arfimafit, aracf, maacf, arpacf, mapacf, &
          armaacf, arfimaacf, armapacf, mssk, mssk_exp, mssk_gamma, mssk_lnorm, mssk_t, mssk_chisq, mssk_f, mssk_beta, mssk_logis, mssk_sech, mssk_laplace, &
          dunif, dexp, dgamma, dlnorm, dnorm, dt, dchisq, df, dbeta, dlogis, dsech, dlaplace, dcauchy, dged, dhyperb, &
          punif, pexp, pgamma, plnorm, pnorm, pt, pchisq, pf, pbeta, plogis, psech, plaplace, pcauchy, pged, phyperb, &
          qunif, qexp, qgamma, qlnorm, qnorm, qt, qchisq, qf, qbeta, qlogis, qsech, qlaplace, qcauchy, qged, qhyperb, &
          rhyperb, fit_norm, fit_exp, fit_gamma, fit_lnorm, fit_t, fit_chisq, fit_f, fit_beta, fit_logis, fit_sech, fit_laplace, fit_cauchy, fit_ged, fit_hyperb

abstract interface
   function obj_fun(x) result(f)
      import dp
      real(kind=dp), intent(in) :: x(:)
      real(kind=dp) :: f
   end function obj_fun
end interface

contains

pure function nanv() result(x)
! Quiet NaN value.
real(kind=dp) :: x
x = ieee_value(0.0_dp, ieee_quiet_nan)
end function nanv

pure function standardize(x) result(y)
! shift and scale x so it has mean 0 and variance 1
real(kind=dp), intent(in) :: x(:)
real(kind=dp)             :: y(size(x))
real(kind=dp)             :: sumsq
integer                   :: n
n = size(x)
if (n == 1) y = 0.0_dp
if (n < 1) return
y = x - mean(x)
sumsq = sum(y**2)
if (sumsq > 0) y = y / sqrt(sumsq/n)
end function standardize

pure function mean(x) result(mean_val)
! return the mean of x
real(kind=dp), intent(in) :: x(:)
real(kind=dp) :: mean_val
mean_val = sum(x) / (max(1, size(x)))
end function mean

pure function geomean(x) result(geomean_val)
! return the geometric mean of x
real(kind=dp), intent(in) :: x(:)
real(kind=dp) :: geomean_val
geomean_val = exp(mean(log(x)))
end function geomean

pure function harmean(x) result(harmean_val)
! return the harmonic mean of x
real(kind=dp), intent(in) :: x(:)
real(kind=dp) :: harmean_val
integer :: n
n = size(x)
if (n > 0) then
   harmean_val = n/sum(1/x)
else
   harmean_val = 0.0_dp
end if
end function harmean

pure function sd(x) result(sd_val)
! return the standard deviation of x
real(kind=dp), intent(in) :: x(:)
real(kind=dp) :: sd_val
real(kind=dp) :: mean_x
mean_x = mean(x)
sd_val = sqrt(sum((x - mean_x)**2) / (size(x) - 1))
end function sd

pure function cor(x, y) result(corr_xy)
! Returns the linear Pearson correlation of x(:) and y(:)
! Returns a correlation < -1.0_dp to signal an error
real(kind=dp), intent(in) :: x(:), y(:)
real(kind=dp) :: corr_xy
real(kind=dp) :: x_mean, y_mean, cov_xy, var_x, var_y
integer :: n
n = size(x)
if (n /= size(y) .or. n == 0) then
   corr_xy = -2.0_dp
   return
end if
x_mean = sum(x) / n
y_mean = sum(y) / n
cov_xy = sum((x - x_mean) * (y - y_mean))
var_x  = sum((x - x_mean)**2)
var_y  = sum((y - y_mean)**2)
if (var_x <= 0.0_dp .or. var_y <= 0.0_dp) then
   corr_xy = -3.0_dp
else
   corr_xy = cov_xy / sqrt(var_x * var_y)
end if
end function cor

pure function cov(x, y) result(cov_xy)
! Returns the covariance of two 1D arrays
real(kind=dp), intent(in) :: x(:), y(:)
real(kind=dp) :: cov_xy
real(kind=dp) :: x_mean, y_mean
integer :: n
n = size(x)
if (n /= size(y) .or. n == 0) then
   error stop "x and y must have same size > 0 in cov"
end if
x_mean = sum(x) / n
y_mean = sum(y) / n
cov_xy = sum((x - x_mean) * (y - y_mean)) / (n - 1)
end function cov

pure function acf(x, k) result(r)
! return the first k autocorrelations (lags 1..k) of x
real(kind=dp), intent(in) :: x(:)
integer, intent(in) :: k
real(kind=dp), allocatable :: r(:)
real(kind=dp) :: mean_x, denom
integer :: n, lag, k_eff
n = size(x)
if (k < 1 .or. n < 2) then
   allocate (r(0))
   return
end if
k_eff = min(k, n - 1)
allocate (r(k_eff))
mean_x = mean(x)
denom = sum((x - mean_x)**2)
if (denom <= 0.0_dp) then
   r = -3.0_dp
   return
end if
do lag = 1, k_eff
   r(lag) = sum((x(1:n - lag) - mean_x) * (x(1 + lag:n) - mean_x)) / denom
end do
end function acf

pure function pacf(x, k) result(p)
! return the first k partial autocorrelations (lags 1..k) of x
real(kind=dp), intent(in) :: x(:)
integer, intent(in) :: k
real(kind=dp), allocatable :: p(:)
real(kind=dp), allocatable :: r(:), phi_dl(:,:), v(:)
integer :: n, k_eff, j, m
real(kind=dp), parameter :: eps = 1.0e-12_dp

n = size(x)
if (k < 1 .or. n < 2) then
   allocate (p(0))
   return
end if
k_eff = min(k, n - 1)
allocate (p(k_eff))
r = acf(x, k_eff)
if (all(r == -3.0_dp)) then
   p = -3.0_dp
   return
end if
allocate (phi_dl(k_eff, k_eff), v(k_eff))
phi_dl = 0.0_dp
v = 0.0_dp
p = 0.0_dp
phi_dl(1, 1) = r(1)
p(1) = r(1)
v(1) = 1.0_dp - r(1) * r(1)
if (k_eff == 1) then
   deallocate (r, phi_dl, v)
   return
end if
do m = 2, k_eff
   if (abs(v(m - 1)) <= eps) exit
   phi_dl(m, m) = (r(m) - sum(phi_dl(1:m - 1, m - 1) * r(m - 1:1:-1))) / v(m - 1)
   do j = 1, m - 1
      phi_dl(j, m) = phi_dl(j, m - 1) - phi_dl(m, m) * phi_dl(m - j, m - 1)
   end do
   p(m) = phi_dl(m, m)
   v(m) = v(m - 1) * (1.0_dp - phi_dl(m, m) * phi_dl(m, m))
end do
deallocate (r, phi_dl, v)
end function pacf

subroutine acfpacf(x, k, plot, title)
! print ACF/PACF table and optionally plot both series
use plot_mod, only: gplot => plot
real(kind=dp), intent(in) :: x(:)
integer, intent(in) :: k
logical, intent(in), optional :: plot
character(len=*), intent(in), optional :: title
real(kind=dp), allocatable :: ac(:), pc(:), lags(:), y2(:,:)
character(len=4) :: legends(2)
character(len=:), allocatable :: ttl
logical :: do_plot
integer :: j, k_eff

if (size(x) < 2) then
   print *, "Error: acfpacf() needs size(x) >= 2"
   return
end if
if (k < 1 .or. k > size(x) - 1) then
   print *, "Error: acfpacf() lag count must be between 1 and ", size(x) - 1
   return
end if

k_eff = min(k, size(x) - 1)
ac = acf(x, k_eff)
pc = pacf(x, k_eff)

print *
print "(a6,2a14)", "lag", "ACF", "PACF"
do j = 1, size(ac)
   print "(i6,2f14.6)", j, ac(j), pc(j)
end do

do_plot = .false.
if (present(plot)) do_plot = plot
if (.not. do_plot) return

legends = [character(len=4) :: "ACF", "PACF"]
allocate (lags(size(ac)), y2(size(ac), 2))
do j = 1, size(ac)
   lags(j) = real(j, dp)
end do
y2(:, 1) = ac
y2(:, 2) = pc
if (present(title)) then
   ttl = title
else
   ttl = "acfpacf"
end if
call gplot(lags, y2, title=ttl, xlabel="lag", legend_labels=legends)
end subroutine acfpacf

subroutine acfpacfar(x, k, plot, title)
! print ACF/PACF/AR table and optionally plot all three series
use plot_mod, only: gplot => plot
real(kind=dp), intent(in) :: x(:)
integer, intent(in) :: k
logical, intent(in), optional :: plot
character(len=*), intent(in), optional :: title
real(kind=dp), allocatable :: ac(:), pc(:), ar(:), lags(:), y3(:,:)
character(len=4) :: legends(3)
character(len=:), allocatable :: ttl
logical :: do_plot
integer :: j, k_eff

if (size(x) < 2) then
   print *, "Error: acfpacfar() needs size(x) >= 2"
   return
end if
if (k < 1 .or. k > size(x) - 1) then
   print *, "Error: acfpacfar() lag count must be between 1 and ", size(x) - 1
   return
end if

k_eff = min(k, size(x) - 1)
ac = acf(x, k_eff)
pc = pacf(x, k_eff)
ar = arcoef(x, k_eff)

print *
print "(a6,3a14)", "lag", "ACF", "PACF", "AR"
do j = 1, size(ac)
   print "(i6,3f14.6)", j, ac(j), pc(j), ar(j)
end do

do_plot = .false.
if (present(plot)) do_plot = plot
if (.not. do_plot) return

legends = [character(len=4) :: "ACF", "PACF", "AR"]
allocate (lags(size(ac)), y3(size(ac), 3))
do j = 1, size(ac)
   lags(j) = real(j, dp)
end do
y3(:, 1) = ac
y3(:, 2) = pc
y3(:, 3) = ar
if (present(title)) then
   ttl = title
else
   ttl = "acfpacfar"
end if
call gplot(lags, y3, title=ttl, xlabel="lag", legend_labels=legends)
end subroutine acfpacfar

pure function fiacf(d, k) result(r)
! theoretical ACF of ARFIMA(0,d,0) for lags 1..k
real(kind=dp), intent(in) :: d
integer, intent(in) :: k
real(kind=dp), allocatable :: r(:)
integer :: j
real(kind=dp), parameter :: eps = 1.0e-12_dp

if (k < 1) then
   allocate (r(0))
   return
end if
allocate (r(k))
if (abs(d) >= 0.5_dp) then
   r = -3.0_dp
   return
end if
if (abs(1.0_dp - d) <= eps) then
   r = -3.0_dp
   return
end if
r(1) = d / (1.0_dp - d)
do j = 2, k
   if (abs(real(j, dp) - d) <= eps) then
      r(j:k) = -3.0_dp
      return
   end if
   r(j) = r(j - 1) * (real(j - 1, dp) + d) / (real(j, dp) - d)
end do
end function fiacf

pure function fracdiff(x, d, m) result(y)
! fractional differencing: y = (1-L)^d x, truncated at lag m (default n-1)
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in) :: d
integer, intent(in), optional :: m
real(kind=dp), allocatable :: y(:)
real(kind=dp), allocatable :: w(:)
integer :: n, m_eff, t, k

n = size(x)
allocate (y(n))
if (n == 0) return
if (present(m)) then
   m_eff = min(max(0, m), n - 1)
else
   m_eff = n - 1
end if
allocate (w(0:m_eff))
w(0) = 1.0_dp
do k = 1, m_eff
   w(k) = w(k - 1) * (real(k - 1, dp) - d) / real(k, dp)
end do
do t = 1, n
   y(t) = 0.0_dp
   do k = 0, min(m_eff, t - 1)
      y(t) = y(t) + w(k) * x(t - k)
   end do
end do
end function fracdiff

function arcoef(x, k) result(phi)
! fit AR(k) coefficients by least squares
real(kind=dp), intent(in) :: x(:)
integer, intent(in) :: k
real(kind=dp), allocatable :: phi(:)
real(kind=dp), allocatable :: y(:), xmat(:,:), xtx(:,:), xty(:)
logical :: ok
integer :: n, n_eff, j

n = size(x)
if (k < 1 .or. n < 2 .or. k > n - 1) then
   allocate (phi(0))
   return
end if

n_eff = n - k
allocate (phi(k))
allocate (y(n_eff), xmat(n_eff, k))
y = x(k + 1:n)
do j = 1, k
   xmat(:, j) = x(k + 1 - j:n - j)
end do
xtx = matmul(transpose(xmat), xmat)
xty = matmul(transpose(xmat), y)
call solve_linear(xtx, xty, phi, ok)
if (.not. ok) phi = -3.0_dp
deallocate (y, xmat)
end function arcoef

subroutine arfimafit(x, p, q, niter)
! fit ARFIMA(p,d,q) by approximate Whittle likelihood
real(kind=dp), intent(in) :: x(:)
integer, intent(in) :: p, q
integer, intent(in), optional :: niter
integer :: n, m, i, t, j, maxpq, n_eff, it, k_params
real(kind=dp) :: d, rmse, aic, bic, sse, sigma2
real(kind=dp), allocatable :: xd(:), resid(:), phi(:), theta(:), ar0(:), u0(:), ubest(:), lam(:), per(:)
real(kind=dp) :: xr, xi, ang
character(len=18) :: s_rmse, s_aic, s_bic, s_val

if (p < 0 .or. q < 0) then
   print *, "Error: arfimafit() orders must be >= 0"
   return
end if
n = size(x)
if (n < 16) then
   print *, "Error: arfimafit() requires size(x) >= 16"
   return
end if
if (present(niter)) then
   it = niter
else
   it = 300
end if
if (it < 1) it = 1

m = (n - 1) / 2
if (m < 2) then
   print *, "Error: arfimafit() needs at least two Fourier frequencies"
   return
end if
allocate (lam(m), per(m))
xd = x - mean(x)
do j = 1, m
   lam(j) = 2.0_dp * pi * real(j, dp) / real(n, dp)
   xr = 0.0_dp
   xi = 0.0_dp
   do t = 1, n
      ang = lam(j) * real(t, dp)
      xr = xr + xd(t) * cos(ang)
      xi = xi - xd(t) * sin(ang)
   end do
   per(j) = (xr * xr + xi * xi) / (2.0_dp * pi * real(n, dp))
end do

allocate (phi(max(1, p)), theta(max(1, q)))
phi = 0.0_dp
theta = 0.0_dp
allocate (u0(p + q + 1), ubest(p + q + 1))
u0 = 0.0_dp
if (p > 0) then
   ar0 = arcoef(x, p)
   if (size(ar0) == p .and. .not. all(ar0 == -3.0_dp)) then
      do i = 1, p
         u0(i) = atanh(max(-0.999_dp, min(0.999_dp, ar0(i) / 0.98_dp)))
      end do
   end if
end if
ubest = nelder_mead(loglik, u0, 0.1_dp, it, 1.0e-5_dp)
call unpack_params(ubest, phi, theta, d)

xd = fracdiff(x - mean(x), d)
allocate (resid(n))
call arma_resid(xd, phi, theta, resid)
maxpq = max(p, q)
n_eff = n - maxpq
if (n_eff <= 0) then
   print *, "Error: arfimafit() invalid effective sample size"
   return
end if
sse = sum(resid(maxpq + 1:n)**2)
rmse = sqrt(max(0.0_dp, sse / real(n_eff, dp)))
if (sse > 0.0_dp) then
   sigma2 = sse / real(n_eff, dp)
   k_params = max(1, p + q + 1)
   aic = real(n_eff, dp) * log(sigma2) + 2.0_dp * real(k_params, dp)
   bic = real(n_eff, dp) * log(sigma2) + log(real(n_eff, dp)) * real(k_params, dp)
else
   aic = huge(1.0_dp)
   bic = huge(1.0_dp)
end if

print "(a6,a18,a18,a18,a12)", "npar", "RMSE", "AIC", "BIC", "d"
write (s_rmse, "(g18.6)") rmse
write (s_aic, "(g18.6)") aic
write (s_bic, "(g18.6)") bic
print "(i6,a18,a18,a18,f12.6)", p + q + 1, s_rmse, s_aic, s_bic, d
if (p > 0) then
   print *
   print "(a6)", "AR"
   write (*, "(6x)", advance="no")
   do i = 1, p
      write (s_val, "(a,i0)") "AR", i
      write (*, "(1x,a12)", advance="no") trim(s_val)
   end do
   print *
   write (*, "(i6)", advance="no") p
   do i = 1, p
      write (*, "(1x,f12.6)", advance="no") phi(i)
   end do
   print *
end if
if (q > 0) then
   print *
   print "(a6)", "MA"
   write (*, "(6x)", advance="no")
   do i = 1, q
      write (s_val, "(a,i0)") "MA", i
      write (*, "(1x,a12)", advance="no") trim(s_val)
   end do
   print *
   write (*, "(i6)", advance="no") q
   do i = 1, q
      write (*, "(1x,f12.6)", advance="no") theta(i)
   end do
   print *
end if

contains
   subroutine unpack_params(u, phi_p, theta_q, d_out)
      real(kind=dp), intent(in) :: u(:)
      real(kind=dp), intent(out) :: phi_p(:), theta_q(:), d_out
      integer :: k
      if (p > 0) then
         do k = 1, p
            phi_p(k) = 0.98_dp * tanh(u(k))
         end do
      end if
      if (q > 0) then
         do k = 1, q
            theta_q(k) = 0.98_dp * tanh(u(p + k))
         end do
      end if
      d_out = 0.49_dp * tanh(u(p + q + 1))
   end subroutine unpack_params

   function loglik(u) result(f)
      real(kind=dp), intent(in) :: u(:)
      real(kind=dp) :: f
      real(kind=dp), allocatable :: phi_l(:), theta_l(:), g(:)
      real(kind=dp) :: d_l, num_re, num_im, den_re, den_im, denom2, frac_term
      real(kind=dp) :: sig2, obj
      integer :: jj, kk
      real(kind=dp), parameter :: eps = 1.0e-12_dp

      allocate (phi_l(max(1, p)), theta_l(max(1, q)), g(m))
      phi_l = 0.0_dp
      theta_l = 0.0_dp
      call unpack_params(u, phi_l, theta_l, d_l)
      do jj = 1, m
         num_re = 1.0_dp
         num_im = 0.0_dp
         do kk = 1, q
            num_re = num_re + theta_l(kk) * cos(lam(jj) * real(kk, dp))
            num_im = num_im - theta_l(kk) * sin(lam(jj) * real(kk, dp))
         end do
         den_re = 1.0_dp
         den_im = 0.0_dp
         do kk = 1, p
            den_re = den_re - phi_l(kk) * cos(lam(jj) * real(kk, dp))
            den_im = den_im + phi_l(kk) * sin(lam(jj) * real(kk, dp))
         end do
         denom2 = den_re * den_re + den_im * den_im
         if (denom2 <= eps) then
            f = -huge(1.0_dp)
            return
         end if
         frac_term = max(eps, 2.0_dp * sin(0.5_dp * lam(jj)))
         frac_term = frac_term**(-2.0_dp * d_l)
         g(jj) = frac_term * (num_re * num_re + num_im * num_im) / denom2
         if (g(jj) <= eps) then
            f = -huge(1.0_dp)
            return
         end if
      end do
      sig2 = sum(per / g) / real(m, dp)
      if (sig2 <= eps) then
         f = -huge(1.0_dp)
         return
      end if
      obj = real(m, dp) * log(sig2) + sum(log(g))
      f = -obj
   end function loglik
end subroutine arfimafit

function arsim(n, phi) result(x)
! simulate n observations from an AR(p) with coefficients phi(:)
integer, intent(in) :: n
real(kind=dp), intent(in) :: phi(:)
real(kind=dp), allocatable :: x(:)
real(kind=dp), allocatable :: eps(:)
integer :: p, t, j, m
p = size(phi)
if (n < 1 .or. p < 1) then
   allocate (x(0))
   return
end if
allocate (x(n))
eps = random_normal(n)
do t = 1, n
   x(t) = eps(t)
   m = min(p, t - 1)
   if (m > 0) then
      do j = 1, m
         x(t) = x(t) + phi(j) * x(t - j)
      end do
   end if
end do
end function arsim

function masim(n, theta) result(x)
! simulate n observations from an MA(q) with coefficients theta(:)
integer, intent(in) :: n
real(kind=dp), intent(in) :: theta(:)
real(kind=dp), allocatable :: x(:)
real(kind=dp), allocatable :: eps(:)
integer :: q, t, j, m
q = size(theta)
if (n < 1 .or. q < 1) then
   allocate (x(0))
   return
end if
allocate (x(n))
eps = random_normal(n)
do t = 1, n
   x(t) = eps(t)
   m = min(q, t - 1)
   if (m > 0) then
      do j = 1, m
         x(t) = x(t) + theta(j) * eps(t - j)
      end do
   end if
end do
end function masim

function armasim(n, phi, theta) result(x)
! simulate n observations from an ARMA(p,q)
integer, intent(in) :: n
real(kind=dp), intent(in) :: phi(:)
real(kind=dp), intent(in) :: theta(:)
real(kind=dp), allocatable :: x(:)
real(kind=dp), allocatable :: eps(:)
integer :: p, q, t, j, m

p = size(phi)
q = size(theta)
if (n < 1) then
   allocate (x(0))
   return
end if
allocate (x(n))
eps = random_normal(n)
do t = 1, n
   x(t) = eps(t)
   m = min(p, t - 1)
   if (m > 0) then
      do j = 1, m
         x(t) = x(t) + phi(j) * x(t - j)
      end do
   end if
   m = min(q, t - 1)
   if (m > 0) then
      do j = 1, m
         x(t) = x(t) + theta(j) * eps(t - j)
      end do
   end if
end do
end function armasim

function arfimasim(n, d, phi, theta, burn, m) result(x)
! simulate n observations from ARFIMA(p,d,q)
integer, intent(in) :: n
real(kind=dp), intent(in) :: d
real(kind=dp), intent(in), optional :: phi(:)
real(kind=dp), intent(in), optional :: theta(:)
integer, intent(in), optional :: burn
integer, intent(in), optional :: m
real(kind=dp), allocatable :: x(:)
real(kind=dp), allocatable :: y(:), eps(:), xfd(:)
integer :: p, q, t, j, ar_m, ma_m, burn_eff, n_all, m_eff

if (present(phi)) then
   p = size(phi)
else
   p = 0
end if
if (present(theta)) then
   q = size(theta)
else
   q = 0
end if
if (n < 1 .or. abs(d) >= 0.5_dp) then
   allocate (x(0))
   return
end if
if (present(burn)) then
   burn_eff = max(0, burn)
else
   burn_eff = max(100, 10 * max(1, p + q))
end if
n_all = n + burn_eff
allocate (y(n_all), eps(n_all))
eps = random_normal(n_all)
y = 0.0_dp
do t = 1, n_all
   y(t) = eps(t)
   ar_m = min(p, t - 1)
   if (ar_m > 0 .and. present(phi)) then
      do j = 1, ar_m
         y(t) = y(t) + phi(j) * y(t - j)
      end do
   end if
   ma_m = min(q, t - 1)
   if (ma_m > 0 .and. present(theta)) then
      do j = 1, ma_m
         y(t) = y(t) + theta(j) * eps(t - j)
      end do
   end if
end do
if (present(m)) then
   m_eff = max(0, m)
   xfd = fracdiff(y, -d, m_eff)
else
   xfd = fracdiff(y, -d)
end if
allocate (x(n))
x = xfd(burn_eff + 1:burn_eff + n)
end function arfimasim

pure function mssk_exp(rate) result(v)
! Mean, standard deviation, skew and excess kurtosis of exponential distribution.
real(kind=dp), intent(in) :: rate
real(kind=dp) :: v(4)
if (rate <= 0.0_dp) then
   v = nanv()
else
   v(1) = 1.0_dp / rate
   v(2) = 1.0_dp / rate
   v(3) = 2.0_dp
   v(4) = 6.0_dp
end if
end function mssk_exp

pure function mssk_gamma(shape, scale) result(v)
! Mean, standard deviation, skew and excess kurtosis of gamma distribution.
real(kind=dp), intent(in) :: shape, scale
real(kind=dp) :: v(4)
if (shape <= 0.0_dp .or. scale <= 0.0_dp) then
   v = nanv()
else
   v(1) = shape * scale
   v(2) = sqrt(shape) * scale
   v(3) = 2.0_dp / sqrt(shape)
   v(4) = 6.0_dp / shape
end if
end function mssk_gamma

pure function mssk_lnorm(meanlog, sdlog) result(v)
! Mean, standard deviation, skew and excess kurtosis of lognormal distribution.
real(kind=dp), intent(in) :: meanlog, sdlog
real(kind=dp) :: v(4)
real(kind=dp) :: s2, m, var
if (sdlog <= 0.0_dp) then
   v = nanv()
else
   s2 = sdlog * sdlog
   m = exp(meanlog + 0.5_dp * s2)
   var = (exp(s2) - 1.0_dp) * exp(2.0_dp * meanlog + s2)
   v(1) = m
   v(2) = sqrt(var)
   v(3) = (exp(s2) + 2.0_dp) * sqrt(exp(s2) - 1.0_dp)
   v(4) = exp(4.0_dp * s2) + 2.0_dp * exp(3.0_dp * s2) + 3.0_dp * exp(2.0_dp * s2) - 6.0_dp
end if
end function mssk_lnorm

pure function mssk_t(df) result(v)
! Mean, standard deviation, skew and excess kurtosis of Student t distribution.
real(kind=dp), intent(in) :: df
real(kind=dp) :: v(4)
if (df <= 0.0_dp) then
   v = nanv()
   return
end if
if (df > 1.0_dp) then
   v(1) = 0.0_dp
else
   v(1) = nanv()
end if
if (df > 2.0_dp) then
   v(2) = sqrt(df / (df - 2.0_dp))
else
   v(2) = nanv()
end if
if (df > 3.0_dp) then
   v(3) = 0.0_dp
else
   v(3) = nanv()
end if
if (df > 4.0_dp) then
   v(4) = 6.0_dp / (df - 4.0_dp)
else
   v(4) = nanv()
end if
end function mssk_t

pure function mssk_chisq(df) result(v)
! Mean, standard deviation, skew and excess kurtosis of chi-square distribution.
real(kind=dp), intent(in) :: df
real(kind=dp) :: v(4)
if (df <= 0.0_dp) then
   v = nanv()
else
   v(1) = df
   v(2) = sqrt(2.0_dp * df)
   v(3) = sqrt(8.0_dp / df)
   v(4) = 12.0_dp / df
end if
end function mssk_chisq

pure function mssk_f(df1, df2) result(v)
! Mean, standard deviation, skew and excess kurtosis of F distribution.
real(kind=dp), intent(in) :: df1, df2
real(kind=dp) :: v(4)
real(kind=dp) :: var
if (df1 <= 0.0_dp .or. df2 <= 0.0_dp) then
   v = nanv()
   return
end if
if (df2 > 2.0_dp) then
   v(1) = df2 / (df2 - 2.0_dp)
else
   v(1) = nanv()
end if
if (df2 > 4.0_dp) then
   var = 2.0_dp * df2 * df2 * (df1 + df2 - 2.0_dp) / (df1 * (df2 - 2.0_dp)**2 * (df2 - 4.0_dp))
   v(2) = sqrt(var)
else
   v(2) = nanv()
end if
if (df2 > 6.0_dp) then
   v(3) = ((2.0_dp * df1 + df2 - 2.0_dp) * sqrt(8.0_dp * (df2 - 4.0_dp))) / &
          ((df2 - 6.0_dp) * sqrt(df1 * (df1 + df2 - 2.0_dp)))
else
   v(3) = nanv()
end if
if (df2 > 8.0_dp) then
   v(4) = (12.0_dp * df1 * (5.0_dp * df2 - 22.0_dp) * (df1 + df2 - 2.0_dp) + &
           (df2 - 4.0_dp) * (df2 - 2.0_dp)**2 * (df1 - 2.0_dp)**2) / &
          (df1 * (df2 - 6.0_dp) * (df2 - 8.0_dp) * (df1 + df2 - 2.0_dp)) - 3.0_dp
else
   v(4) = nanv()
end if
end function mssk_f

pure function mssk_beta(a, b) result(v)
! Mean, standard deviation, skew and excess kurtosis of beta distribution.
real(kind=dp), intent(in) :: a, b
real(kind=dp) :: v(4)
real(kind=dp) :: denom, var
if (a <= 0.0_dp .or. b <= 0.0_dp) then
   v = nanv()
   return
end if
denom = (a + b)
v(1) = a / denom
var = a * b / (denom * denom * (denom + 1.0_dp))
v(2) = sqrt(var)
v(3) = 2.0_dp * (b - a) * sqrt(denom + 1.0_dp) / ((denom + 2.0_dp) * sqrt(a * b))
v(4) = 6.0_dp * ((a - b)**2 * (denom + 1.0_dp) - a * b * (denom + 2.0_dp)) / &
       (a * b * (denom + 2.0_dp) * (denom + 3.0_dp))
end function mssk_beta

pure function mssk_logis(loc, scale) result(v)
! Mean, standard deviation, skew and excess kurtosis of logistic distribution.
real(kind=dp), intent(in) :: loc, scale
real(kind=dp) :: v(4)
if (scale <= 0.0_dp) then
   v = nanv()
else
   v(1) = loc
   v(2) = pi * scale / sqrt(3.0_dp)
   v(3) = 0.0_dp
   v(4) = 6.0_dp / 5.0_dp
end if
end function mssk_logis

pure function mssk_sech() result(v)
! Mean, standard deviation, skew and excess kurtosis of hyperbolic secant distribution.
real(kind=dp) :: v(4)
v(1) = 0.0_dp
v(2) = 1.0_dp
v(3) = 0.0_dp
v(4) = 2.0_dp
end function mssk_sech

pure function mssk_laplace(loc, scale) result(v)
! Mean, standard deviation, skew and excess kurtosis of Laplace distribution.
real(kind=dp), intent(in) :: loc, scale
real(kind=dp) :: v(4)
if (scale <= 0.0_dp) then
   v = nanv()
else
   v(1) = loc
   v(2) = scale * sqrt(2.0_dp)
   v(3) = 0.0_dp
   v(4) = 3.0_dp
end if
end function mssk_laplace

pure function mssk(x) result(v)
! Mean, standard deviation, skew and excess kurtosis of data array.
real(kind=dp), intent(in) :: x(:)
real(kind=dp) :: v(4)
v(1) = mean(x)
v(2) = sd(x)
v(3) = skew(x)
v(4) = kurtosis(x)
end function mssk

pure function dunif(x, a, b) result(y)
! Uniform density on [a,b], defaults a=0 and b=1.
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in), optional :: a, b
real(kind=dp) :: y(size(x))
real(kind=dp) :: lo, hi
if (present(a)) then
   lo = a
else
   lo = 0.0_dp
end if
if (present(b)) then
   hi = b
else
   hi = 1.0_dp
end if
if (hi <= lo) then
   y = nanv()
   return
end if
y = 0.0_dp
where (x >= lo .and. x <= hi)
   y = 1.0_dp/(hi - lo)
end where
end function dunif

pure function dexp(x, rate) result(y)
! Exponential density.
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in) :: rate
real(kind=dp) :: y(size(x))
integer :: i
if (rate <= 0.0_dp) then
   y = nanv()
   return
end if
do i = 1, size(x)
   if (x(i) < 0.0_dp) then
      y(i) = 0.0_dp
   else
      y(i) = rate * exp(-rate * x(i))
   end if
end do
end function dexp

pure function dgamma(x, shape, scale) result(y)
! Gamma density.
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in) :: shape, scale
real(kind=dp) :: y(size(x))
integer :: i
real(kind=dp) :: logc
if (shape <= 0.0_dp .or. scale <= 0.0_dp) then
   y = nanv()
   return
end if
logc = -log_gamma(shape) - shape * log(scale)
do i = 1, size(x)
   if (x(i) < 0.0_dp) then
      y(i) = 0.0_dp
   else
      y(i) = exp(logc + (shape - 1.0_dp) * log(x(i)) - x(i) / scale)
   end if
end do
end function dgamma

pure function dlnorm(x, meanlog, sdlog) result(y)
! Lognormal density.
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in) :: meanlog, sdlog
real(kind=dp) :: y(size(x))
integer :: i
real(kind=dp) :: logc
if (sdlog <= 0.0_dp) then
   y = nanv()
   return
end if
logc = -log(sdlog) - 0.5_dp * log(2.0_dp * pi)
do i = 1, size(x)
   if (x(i) <= 0.0_dp) then
      y(i) = 0.0_dp
   else
      y(i) = exp(logc - log(x(i)) - 0.5_dp * ((log(x(i)) - meanlog) / sdlog)**2)
   end if
end do
end function dlnorm

pure function dnorm(x, mean, sd) result(y)
! Normal density.
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in) :: mean, sd
real(kind=dp) :: y(size(x))
integer :: i
real(kind=dp) :: logc, z
if (sd <= 0.0_dp) then
   y = nanv()
   return
end if
logc = -log(sd) - 0.5_dp * log(2.0_dp * pi)
do i = 1, size(x)
   z = (x(i) - mean) / sd
   y(i) = exp(logc - 0.5_dp * z * z)
end do
end function dnorm

pure function dt(x, df) result(y)
! Student t density.
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in) :: df
real(kind=dp) :: y(size(x))
integer :: i
real(kind=dp) :: logc
if (df <= 0.0_dp) then
   y = nanv()
   return
end if
logc = log_gamma(0.5_dp * (df + 1.0_dp)) - log_gamma(0.5_dp * df) - 0.5_dp * (log(df) + log(pi))
do i = 1, size(x)
   y(i) = exp(logc - 0.5_dp * (df + 1.0_dp) * log(1.0_dp + (x(i) * x(i)) / df))
end do
end function dt

pure function dchisq(x, df) result(y)
! Chi-square density.
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in) :: df
real(kind=dp) :: y(size(x))
integer :: i
real(kind=dp) :: logc, shape, scale
if (df <= 0.0_dp) then
   y = nanv()
   return
end if
shape = 0.5_dp * df
scale = 2.0_dp
logc = -log_gamma(shape) - shape * log(scale)
do i = 1, size(x)
   if (x(i) < 0.0_dp) then
      y(i) = 0.0_dp
   else
      y(i) = exp(logc + (shape - 1.0_dp) * log(x(i)) - x(i) / scale)
   end if
end do
end function dchisq

pure function df(x, df1, df2) result(y)
! F density.
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in) :: df1, df2
real(kind=dp) :: y(size(x))
integer :: i
real(kind=dp) :: a, b, logc
if (df1 <= 0.0_dp .or. df2 <= 0.0_dp) then
   y = nanv()
   return
end if
a = 0.5_dp * df1
b = 0.5_dp * df2
logc = a * log(df1 / df2) - (log_gamma(a) + log_gamma(b) - log_gamma(a + b))
do i = 1, size(x)
   if (x(i) <= 0.0_dp) then
      y(i) = 0.0_dp
   else
      y(i) = exp(logc + (a - 1.0_dp) * log(x(i)) - (a + b) * log(1.0_dp + (df1 / df2) * x(i)))
   end if
end do
end function df

pure function dbeta(x, a, b) result(y)
! Beta density.
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in) :: a, b
real(kind=dp) :: y(size(x))
integer :: i
real(kind=dp) :: logc
if (a <= 0.0_dp .or. b <= 0.0_dp) then
   y = nanv()
   return
end if
logc = - (log_gamma(a) + log_gamma(b) - log_gamma(a + b))
do i = 1, size(x)
   if (x(i) <= 0.0_dp .or. x(i) >= 1.0_dp) then
      y(i) = 0.0_dp
   else
      y(i) = exp(logc + (a - 1.0_dp) * log(x(i)) + (b - 1.0_dp) * log(1.0_dp - x(i)))
   end if
end do
end function dbeta

pure function dlogis(x, loc, scale) result(y)
! Logistic density.
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in) :: loc, scale
real(kind=dp) :: y(size(x))
integer :: i
real(kind=dp) :: z, ez
if (scale <= 0.0_dp) then
   y = nanv()
   return
end if
do i = 1, size(x)
   z = (x(i) - loc) / scale
   ez = exp(-z)
   y(i) = ez / (scale * (1.0_dp + ez)**2)
end do
end function dlogis

pure function dlaplace(x, loc, scale) result(y)
! Laplace density.
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in) :: loc, scale
real(kind=dp) :: y(size(x))
integer :: i
if (scale <= 0.0_dp) then
   y = nanv()
   return
end if
do i = 1, size(x)
   y(i) = 0.5_dp / scale * exp(-abs(x(i) - loc) / scale)
end do
end function dlaplace

pure function dcauchy(x, loc, scale) result(y)
! Cauchy density.
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in) :: loc, scale
real(kind=dp) :: y(size(x))
integer :: i
real(kind=dp) :: z
if (scale <= 0.0_dp) then
   y = nanv()
   return
end if
do i = 1, size(x)
   z = (x(i) - loc) / scale
   y(i) = 1.0_dp / (pi * scale * (1.0_dp + z * z))
end do
end function dcauchy

pure function dged(x, loc, scale, beta) result(y)
! Generalized error density.
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in) :: loc, scale, beta
real(kind=dp) :: y(size(x))
integer :: i
real(kind=dp) :: logc
if (scale <= 0.0_dp .or. beta <= 0.0_dp) then
   y = nanv()
   return
end if
logc = log(beta) - log(2.0_dp * scale) - log_gamma(1.0_dp / beta)
do i = 1, size(x)
   y(i) = exp(logc - (abs(x(i) - loc) / scale)**beta)
end do
end function dged

pure function dhyperb(x, loc, scale, alpha) result(y)
! Symmetric hyperbolic density.
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in) :: loc, scale, alpha
real(kind=dp) :: y(size(x))
integer :: i
real(kind=dp) :: c, k1, r
if (scale <= 0.0_dp .or. alpha <= 0.0_dp) then
   y = nanv()
   return
end if
k1 = besselk1(alpha * scale)
if (k1 <= 0.0_dp) then
   y = nanv()
   return
end if
c = alpha / (2.0_dp * scale * k1)
do i = 1, size(x)
   r = sqrt(scale * scale + (x(i) - loc)**2)
   y(i) = c * exp(-alpha * r)
end do
end function dhyperb

pure function dsech(x) result(y)
! Hyperbolic secant density.
real(kind=dp), intent(in) :: x(:)
real(kind=dp) :: y(size(x))
integer :: i
do i = 1, size(x)
   y(i) = 0.5_dp / cosh(0.5_dp * pi * x(i))
end do
end function dsech

pure function punif(x, a, b) result(y)
! Uniform CDF on [a,b], defaults a=0 and b=1.
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in), optional :: a, b
real(kind=dp) :: y(size(x))
real(kind=dp) :: lo, hi
integer :: i
if (present(a)) then
   lo = a
else
   lo = 0.0_dp
end if
if (present(b)) then
   hi = b
else
   hi = 1.0_dp
end if
if (hi <= lo) then
   y = nanv()
   return
end if
do i = 1, size(x)
   if (x(i) <= lo) then
      y(i) = 0.0_dp
   else if (x(i) >= hi) then
      y(i) = 1.0_dp
   else
      y(i) = (x(i) - lo)/(hi - lo)
   end if
end do
end function punif

pure function pexp(x, rate) result(y)
! Exponential CDF.
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in) :: rate
real(kind=dp) :: y(size(x))
integer :: i
if (rate <= 0.0_dp) then
   y = nanv()
   return
end if
do i = 1, size(x)
   if (x(i) < 0.0_dp) then
      y(i) = 0.0_dp
   else
      y(i) = 1.0_dp - exp(-rate * x(i))
   end if
end do
end function pexp

pure function pgamma(x, shape, scale) result(y)
! Gamma CDF.
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in) :: shape, scale
real(kind=dp) :: y(size(x))
integer :: i
if (shape <= 0.0_dp .or. scale <= 0.0_dp) then
   y = nanv()
   return
end if
do i = 1, size(x)
   if (x(i) < 0.0_dp) then
      y(i) = 0.0_dp
   else
      y(i) = gammp(shape, x(i) / scale)
   end if
end do
end function pgamma

pure function plnorm(x, meanlog, sdlog) result(y)
! Lognormal CDF.
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in) :: meanlog, sdlog
real(kind=dp) :: y(size(x))
integer :: i
real(kind=dp) :: z
if (sdlog <= 0.0_dp) then
   y = nanv()
   return
end if
do i = 1, size(x)
   if (x(i) <= 0.0_dp) then
      y(i) = 0.0_dp
   else
      z = (log(x(i)) - meanlog) / (sdlog * sqrt(2.0_dp))
      y(i) = 0.5_dp * (1.0_dp + erf(z))
   end if
end do
end function plnorm

pure function pt(x, df) result(y)
! Student t CDF.
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in) :: df
real(kind=dp) :: y(size(x))
integer :: i
if (df <= 0.0_dp) then
   y = nanv()
   return
end if
do i = 1, size(x)
   y(i) = tcdf(x(i), nint(df))
end do
end function pt

pure function pchisq(x, df) result(y)
! Chi-square CDF.
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in) :: df
real(kind=dp) :: y(size(x))
integer :: i
if (df <= 0.0_dp) then
   y = nanv()
   return
end if
do i = 1, size(x)
   if (x(i) < 0.0_dp) then
      y(i) = 0.0_dp
   else
      y(i) = gammp(0.5_dp * df, 0.5_dp * x(i))
   end if
end do
end function pchisq

pure function pf(x, df1, df2) result(y)
! F CDF.
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in) :: df1, df2
real(kind=dp) :: y(size(x))
integer :: i
real(kind=dp) :: a, b, z
if (df1 <= 0.0_dp .or. df2 <= 0.0_dp) then
   y = nanv()
   return
end if
a = 0.5_dp * df1
b = 0.5_dp * df2
do i = 1, size(x)
   if (x(i) <= 0.0_dp) then
      y(i) = 0.0_dp
   else
      z = (df1 * x(i)) / (df1 * x(i) + df2)
      y(i) = betai(a, b, z)
   end if
end do
end function pf

pure function pbeta(x, a, b) result(y)
! Beta CDF.
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in) :: a, b
real(kind=dp) :: y(size(x))
integer :: i
if (a <= 0.0_dp .or. b <= 0.0_dp) then
   y = nanv()
   return
end if
do i = 1, size(x)
   if (x(i) <= 0.0_dp) then
      y(i) = 0.0_dp
   else if (x(i) >= 1.0_dp) then
      y(i) = 1.0_dp
   else
      y(i) = betai(a, b, x(i))
   end if
end do
end function pbeta

pure function plogis(x, loc, scale) result(y)
! Logistic CDF.
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in) :: loc, scale
real(kind=dp) :: y(size(x))
integer :: i
real(kind=dp) :: z
if (scale <= 0.0_dp) then
   y = nanv()
   return
end if
do i = 1, size(x)
   z = (x(i) - loc) / scale
   y(i) = 1.0_dp / (1.0_dp + exp(-z))
end do
end function plogis

pure function plaplace(x, loc, scale) result(y)
! Laplace CDF.
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in) :: loc, scale
real(kind=dp) :: y(size(x))
integer :: i
if (scale <= 0.0_dp) then
   y = nanv()
   return
end if
do i = 1, size(x)
   if (x(i) < loc) then
      y(i) = 0.5_dp * exp((x(i) - loc) / scale)
   else
      y(i) = 1.0_dp - 0.5_dp * exp(-(x(i) - loc) / scale)
   end if
end do
end function plaplace

pure function pcauchy(x, loc, scale) result(y)
! Cauchy CDF.
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in) :: loc, scale
real(kind=dp) :: y(size(x))
integer :: i
real(kind=dp) :: z
if (scale <= 0.0_dp) then
   y = nanv()
   return
end if
do i = 1, size(x)
   z = (x(i) - loc) / scale
   y(i) = 0.5_dp + atan(z) / pi
end do
end function pcauchy

pure function pged(x, loc, scale, beta) result(y)
! Generalized error CDF.
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in) :: loc, scale, beta
real(kind=dp) :: y(size(x))
integer :: i
real(kind=dp) :: a, t, g
if (scale <= 0.0_dp .or. beta <= 0.0_dp) then
   y = nanv()
   return
end if
a = 1.0_dp / beta
do i = 1, size(x)
   t = abs(x(i) - loc) / scale
   g = gammp(a, t**beta)
   if (x(i) < loc) then
      y(i) = 0.5_dp - 0.5_dp * g
   else
      y(i) = 0.5_dp + 0.5_dp * g
   end if
end do
end function pged

pure function phyperb(x, loc, scale, alpha) result(y)
! Symmetric hyperbolic CDF.
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in) :: loc, scale, alpha
real(kind=dp) :: y(size(x))
integer :: i
real(kind=dp) :: t, area
if (scale <= 0.0_dp .or. alpha <= 0.0_dp) then
   y = nanv()
   return
end if
do i = 1, size(x)
   if (x(i) == loc) then
      y(i) = 0.5_dp
   else
      t = abs(x(i) - loc)
      area = hyperb_int(t, loc, scale, alpha)
      if (x(i) < loc) then
         y(i) = 0.5_dp - area
      else
         y(i) = 0.5_dp + area
      end if
      if (y(i) < 0.0_dp) y(i) = 0.0_dp
      if (y(i) > 1.0_dp) y(i) = 1.0_dp
   end if
end do
end function phyperb

pure function psech(x) result(y)
! Hyperbolic secant CDF.
real(kind=dp), intent(in) :: x(:)
real(kind=dp) :: y(size(x))
integer :: i
do i = 1, size(x)
   y(i) = (2.0_dp / pi) * atan(exp(0.5_dp * pi * x(i)))
end do
end function psech

pure function pnorm(x, mean, sd) result(y)
! Normal CDF.
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in) :: mean, sd
real(kind=dp) :: y(size(x))
integer :: i
real(kind=dp) :: z
if (sd <= 0.0_dp) then
   y = nanv()
   return
end if
do i = 1, size(x)
   z = (x(i) - mean) / (sd * sqrt(2.0_dp))
   y(i) = 0.5_dp * (1.0_dp + erf(z))
end do
end function pnorm

pure function hyperb_pdf_scalar(x, loc, scale, alpha) result(y)
! Symmetric hyperbolic density (scalar).
real(kind=dp), intent(in) :: x, loc, scale, alpha
real(kind=dp) :: y
real(kind=dp) :: c, k1, r
if (scale <= 0.0_dp .or. alpha <= 0.0_dp) then
   y = nanv()
   return
end if
k1 = besselk1(alpha * scale)
if (k1 <= 0.0_dp) then
   y = nanv()
   return
end if
c = alpha / (2.0_dp * scale * k1)
r = sqrt(scale * scale + (x - loc)**2)
y = c * exp(-alpha * r)
end function hyperb_pdf_scalar

pure function hyperb_int(t, loc, scale, alpha) result(area)
! Integral of symmetric hyperbolic density from 0 to t.
real(kind=dp), intent(in) :: t, loc, scale, alpha
real(kind=dp) :: area
integer :: n, i
real(kind=dp) :: h, s, s2, f0, fn, x, prev
integer, parameter :: nmax = 4096
real(kind=dp), parameter :: tol = 1.0e-8_dp

if (t <= 0.0_dp) then
   area = 0.0_dp
   return
end if

n = 64
prev = -1.0_dp
do
   h = t / real(n, dp)
   f0 = hyperb_pdf_scalar(loc, loc, scale, alpha)
   fn = hyperb_pdf_scalar(loc + t, loc, scale, alpha)
   s = 0.0_dp
   s2 = 0.0_dp
   do i = 1, n - 1, 2
      x = real(i, dp) * h
      s = s + hyperb_pdf_scalar(loc + x, loc, scale, alpha)
   end do
   do i = 2, n - 2, 2
      x = real(i, dp) * h
      s2 = s2 + hyperb_pdf_scalar(loc + x, loc, scale, alpha)
   end do
   area = (h / 3.0_dp) * (f0 + fn + 4.0_dp * s + 2.0_dp * s2)
   if (prev >= 0.0_dp) then
      if (abs(area - prev) < tol * max(1.0_dp, abs(area))) exit
   end if
   if (n >= nmax) exit
   prev = area
   n = n * 2
end do
end function hyperb_int

pure function inv_norm(p) result(x)
! Inverse standard normal CDF.
real(kind=dp), intent(in) :: p
real(kind=dp) :: x, q, r
real(kind=dp), parameter :: a1 = -3.969683028665376e+01_dp
real(kind=dp), parameter :: a2 =  2.209460984245205e+02_dp
real(kind=dp), parameter :: a3 = -2.759285104469687e+02_dp
real(kind=dp), parameter :: a4 =  1.383577518672690e+02_dp
real(kind=dp), parameter :: a5 = -3.066479806614716e+01_dp
real(kind=dp), parameter :: a6 =  2.506628277459239e+00_dp
real(kind=dp), parameter :: b1 = -5.447609879822406e+01_dp
real(kind=dp), parameter :: b2 =  1.615858368580409e+02_dp
real(kind=dp), parameter :: b3 = -1.556989798598866e+02_dp
real(kind=dp), parameter :: b4 =  6.680131188771972e+01_dp
real(kind=dp), parameter :: b5 = -1.328068155288572e+01_dp
real(kind=dp), parameter :: c1 = -7.784894002430293e-03_dp
real(kind=dp), parameter :: c2 = -3.223964580411365e-01_dp
real(kind=dp), parameter :: c3 = -2.400758277161838e+00_dp
real(kind=dp), parameter :: c4 = -2.549732539343734e+00_dp
real(kind=dp), parameter :: c5 =  4.374664141464968e+00_dp
real(kind=dp), parameter :: c6 =  2.938163982698783e+00_dp
real(kind=dp), parameter :: d1 =  7.784695709041462e-03_dp
real(kind=dp), parameter :: d2 =  3.224671290700398e-01_dp
real(kind=dp), parameter :: d3 =  2.445134137142996e+00_dp
real(kind=dp), parameter :: d4 =  3.754408661907416e+00_dp
real(kind=dp), parameter :: plow = 0.02425_dp
real(kind=dp), parameter :: phigh = 1.0_dp - plow

if (p <= 0.0_dp) then
   x = -huge(1.0_dp)
   return
else if (p >= 1.0_dp) then
   x = huge(1.0_dp)
   return
end if
if (p < plow) then
   q = sqrt(-2.0_dp * log(p))
   x = (((((c1*q + c2)*q + c3)*q + c4)*q + c5)*q + c6) / &
       ((((d1*q + d2)*q + d3)*q + d4)*q + 1.0_dp)
else if (p > phigh) then
   q = sqrt(-2.0_dp * log(1.0_dp - p))
   x = (((((c1*q + c2)*q + c3)*q + c4)*q + c5)*q + c6) / &
       ((((d1*q + d2)*q + d3)*q + d4)*q + 1.0_dp)
else
   q = p - 0.5_dp
   r = q * q
   x = (((((a1*r + a2)*r + a3)*r + a4)*r + a5)*r + a6) * q / &
       (((((b1*r + b2)*r + b3)*r + b4)*r + b5)*r + 1.0_dp)
end if
end function inv_norm

function nelder_mead(f, x0, step, max_iter, tol) result(xbest)
! Nelder-Mead optimizer (maximization).
procedure(obj_fun) :: f
real(kind=dp), intent(in) :: x0(:)
real(kind=dp), intent(in) :: step
integer, intent(in) :: max_iter
real(kind=dp), intent(in) :: tol
real(kind=dp) :: xbest(size(x0))
integer :: n, i, j, iter, best, worst, second
real(kind=dp), allocatable :: simplex(:,:), fval(:), centroid(:), xr(:), xe(:), xc(:)
real(kind=dp) :: fr, fe, fc
real(kind=dp), parameter :: alpha = 1.0_dp, gamma = 2.0_dp, rho = 0.5_dp, sigma = 0.5_dp

n = size(x0)
allocate (simplex(n, n + 1), fval(n + 1), centroid(n), xr(n), xe(n), xc(n))
simplex(:, 1) = x0
do i = 1, n
   simplex(:, i + 1) = x0
   simplex(i, i + 1) = x0(i) + step
end do
do i = 1, n + 1
   fval(i) = f(simplex(:, i))
end do

do iter = 1, max_iter
   best = 1; worst = 1
   do i = 2, n + 1
      if (fval(i) > fval(best)) best = i
      if (fval(i) < fval(worst)) worst = i
   end do
   second = best
   do i = 1, n + 1
      if (i == worst) cycle
      if (second == worst .or. fval(i) < fval(second)) second = i
   end do
   if (maxval(abs(simplex(:, best) - simplex(:, worst))) < tol) exit

   centroid = 0.0_dp
   do j = 1, n + 1
      if (j == worst) cycle
      centroid = centroid + simplex(:, j)
   end do
   centroid = centroid / real(n, dp)

   xr = centroid + alpha * (centroid - simplex(:, worst))
   fr = f(xr)
   if (fr > fval(best)) then
      xe = centroid + gamma * (xr - centroid)
      fe = f(xe)
      if (fe > fr) then
         simplex(:, worst) = xe
         fval(worst) = fe
      else
         simplex(:, worst) = xr
         fval(worst) = fr
      end if
   else if (fr > fval(second)) then
      simplex(:, worst) = xr
      fval(worst) = fr
   else
      xc = centroid + rho * (simplex(:, worst) - centroid)
      fc = f(xc)
      if (fc > fval(worst)) then
         simplex(:, worst) = xc
         fval(worst) = fc
      else
         do j = 1, n + 1
            if (j == best) cycle
            simplex(:, j) = simplex(:, best) + sigma * (simplex(:, j) - simplex(:, best))
            fval(j) = f(simplex(:, j))
         end do
      end if
   end if
end do

best = 1
do i = 2, n + 1
   if (fval(i) > fval(best)) best = i
end do
xbest = simplex(:, best)
end function nelder_mead

pure function qunif(p, a, b) result(x)
! Uniform quantile on [a,b], defaults a=0 and b=1.
real(kind=dp), intent(in) :: p(:)
real(kind=dp), intent(in), optional :: a, b
real(kind=dp) :: x(size(p))
real(kind=dp) :: lo, hi
integer :: i
if (present(a)) then
   lo = a
else
   lo = 0.0_dp
end if
if (present(b)) then
   hi = b
else
   hi = 1.0_dp
end if
if (hi <= lo) then
   x = nanv()
   return
end if
do i = 1, size(p)
   if (p(i) <= 0.0_dp) then
      x(i) = lo
   else if (p(i) >= 1.0_dp) then
      x(i) = hi
   else
      x(i) = lo + p(i)*(hi - lo)
   end if
end do
end function qunif

pure function qexp(p, rate) result(x)
! Exponential quantile.
real(kind=dp), intent(in) :: p(:)
real(kind=dp), intent(in) :: rate
real(kind=dp) :: x(size(p))
integer :: i
if (rate <= 0.0_dp) then
   x = nanv()
   return
end if
do i = 1, size(p)
   if (p(i) <= 0.0_dp) then
      x(i) = 0.0_dp
   else if (p(i) >= 1.0_dp) then
      x(i) = huge(1.0_dp)
   else
      x(i) = -log(1.0_dp - p(i)) / rate
   end if
end do
end function qexp

pure function qgamma(p, shape, scale) result(x)
! Gamma quantile.
real(kind=dp), intent(in) :: p(:)
real(kind=dp), intent(in) :: shape, scale
real(kind=dp) :: x(size(p))
integer :: i, it
real(kind=dp) :: lo, hi, mid, pm
if (shape <= 0.0_dp .or. scale <= 0.0_dp) then
   x = nanv()
   return
end if
do i = 1, size(p)
   if (p(i) <= 0.0_dp) then
      x(i) = 0.0_dp
   else if (p(i) >= 1.0_dp) then
      x(i) = huge(1.0_dp)
   else
      lo = 0.0_dp
      hi = max(1.0_dp, shape * scale * 10.0_dp)
      do while (gammp(shape, hi / scale) < p(i))
         hi = hi * 2.0_dp
         if (hi > huge(1.0_dp) / 4.0_dp) exit
      end do
      do it = 1, 60
         mid = 0.5_dp * (lo + hi)
         pm = gammp(shape, mid / scale)
         if (pm < p(i)) then
            lo = mid
         else
            hi = mid
         end if
      end do
      x(i) = 0.5_dp * (lo + hi)
   end if
end do
end function qgamma

pure function qlnorm(p, meanlog, sdlog) result(x)
! Lognormal quantile.
real(kind=dp), intent(in) :: p(:)
real(kind=dp), intent(in) :: meanlog, sdlog
real(kind=dp) :: x(size(p))
integer :: i
if (sdlog <= 0.0_dp) then
   x = nanv()
   return
end if
do i = 1, size(p)
   if (p(i) <= 0.0_dp) then
      x(i) = 0.0_dp
   else if (p(i) >= 1.0_dp) then
      x(i) = huge(1.0_dp)
   else
      x(i) = exp(meanlog + sdlog * inv_norm(p(i)))
   end if
end do
end function qlnorm

pure function qt(p, df) result(x)
! Student t quantile.
real(kind=dp), intent(in) :: p(:)
real(kind=dp), intent(in) :: df
real(kind=dp) :: x(size(p))
integer :: i, it
real(kind=dp) :: lo, hi, mid, pm
if (df <= 0.0_dp) then
   x = nanv()
   return
end if
do i = 1, size(p)
   if (p(i) <= 0.0_dp) then
      x(i) = -huge(1.0_dp)
   else if (p(i) >= 1.0_dp) then
      x(i) = huge(1.0_dp)
   else
      lo = -10.0_dp
      hi = 10.0_dp
      do while (tcdf(hi, nint(df)) < p(i))
         lo = hi
         hi = hi * 2.0_dp
         if (hi > huge(1.0_dp) / 4.0_dp) exit
      end do
      do while (tcdf(lo, nint(df)) > p(i))
         hi = lo
         lo = lo * 2.0_dp
         if (lo < -huge(1.0_dp) / 4.0_dp) exit
      end do
      do it = 1, 60
         mid = 0.5_dp * (lo + hi)
         pm = tcdf(mid, nint(df))
         if (pm < p(i)) then
            lo = mid
         else
            hi = mid
         end if
      end do
      x(i) = 0.5_dp * (lo + hi)
   end if
end do
end function qt

pure function qchisq(p, df) result(x)
! Chi-square quantile.
real(kind=dp), intent(in) :: p(:)
real(kind=dp), intent(in) :: df
real(kind=dp) :: x(size(p))
integer :: i, it
real(kind=dp) :: lo, hi, mid, pm, shape
if (df <= 0.0_dp) then
   x = nanv()
   return
end if
shape = 0.5_dp * df
do i = 1, size(p)
   if (p(i) <= 0.0_dp) then
      x(i) = 0.0_dp
   else if (p(i) >= 1.0_dp) then
      x(i) = huge(1.0_dp)
   else
      lo = 0.0_dp
      hi = max(1.0_dp, df * 10.0_dp)
      do while (gammp(shape, 0.5_dp * hi) < p(i))
         hi = hi * 2.0_dp
         if (hi > huge(1.0_dp) / 4.0_dp) exit
      end do
      do it = 1, 60
         mid = 0.5_dp * (lo + hi)
         pm = gammp(shape, 0.5_dp * mid)
         if (pm < p(i)) then
            lo = mid
         else
            hi = mid
         end if
      end do
      x(i) = 0.5_dp * (lo + hi)
   end if
end do
end function qchisq

pure function qf(p, df1, df2) result(x)
! F quantile.
real(kind=dp), intent(in) :: p(:)
real(kind=dp), intent(in) :: df1, df2
real(kind=dp) :: x(size(p))
integer :: i, it
real(kind=dp) :: lo, hi, mid, pm, a, b, z
if (df1 <= 0.0_dp .or. df2 <= 0.0_dp) then
   x = nanv()
   return
end if
a = 0.5_dp * df1
b = 0.5_dp * df2
do i = 1, size(p)
   if (p(i) <= 0.0_dp) then
      x(i) = 0.0_dp
   else if (p(i) >= 1.0_dp) then
      x(i) = huge(1.0_dp)
   else
      lo = 0.0_dp
      hi = 1.0_dp
      do
         z = (df1 * hi) / (df1 * hi + df2)
         pm = betai(a, b, z)
         if (pm >= p(i)) exit
         hi = hi * 2.0_dp
         if (hi > huge(1.0_dp) / 4.0_dp) exit
      end do
      do it = 1, 60
         mid = 0.5_dp * (lo + hi)
         z = (df1 * mid) / (df1 * mid + df2)
         pm = betai(a, b, z)
         if (pm < p(i)) then
            lo = mid
         else
            hi = mid
         end if
      end do
      x(i) = 0.5_dp * (lo + hi)
   end if
end do
end function qf

pure function qbeta(p, a, b) result(x)
! Beta quantile.
real(kind=dp), intent(in) :: p(:)
real(kind=dp), intent(in) :: a, b
real(kind=dp) :: x(size(p))
integer :: i, it
real(kind=dp) :: lo, hi, mid, pm
if (a <= 0.0_dp .or. b <= 0.0_dp) then
   x = nanv()
   return
end if
do i = 1, size(p)
   if (p(i) <= 0.0_dp) then
      x(i) = 0.0_dp
   else if (p(i) >= 1.0_dp) then
      x(i) = 1.0_dp
   else
      lo = 0.0_dp
      hi = 1.0_dp
      do it = 1, 60
         mid = 0.5_dp * (lo + hi)
         pm = betai(a, b, mid)
         if (pm < p(i)) then
            lo = mid
         else
            hi = mid
         end if
      end do
      x(i) = 0.5_dp * (lo + hi)
   end if
end do
end function qbeta

pure function qlogis(p, loc, scale) result(x)
! Logistic quantile.
real(kind=dp), intent(in) :: p(:)
real(kind=dp), intent(in) :: loc, scale
real(kind=dp) :: x(size(p))
integer :: i
if (scale <= 0.0_dp) then
   x = nanv()
   return
end if
do i = 1, size(p)
   if (p(i) <= 0.0_dp) then
      x(i) = -huge(1.0_dp)
   else if (p(i) >= 1.0_dp) then
      x(i) = huge(1.0_dp)
   else
      x(i) = loc + scale * log(p(i) / (1.0_dp - p(i)))
   end if
end do
end function qlogis

pure function qlaplace(p, loc, scale) result(x)
! Laplace quantile.
real(kind=dp), intent(in) :: p(:)
real(kind=dp), intent(in) :: loc, scale
real(kind=dp) :: x(size(p))
integer :: i
if (scale <= 0.0_dp) then
   x = nanv()
   return
end if
do i = 1, size(p)
   if (p(i) <= 0.0_dp) then
      x(i) = -huge(1.0_dp)
   else if (p(i) >= 1.0_dp) then
      x(i) = huge(1.0_dp)
   else if (p(i) < 0.5_dp) then
      x(i) = loc + scale * log(2.0_dp * p(i))
   else
      x(i) = loc - scale * log(2.0_dp * (1.0_dp - p(i)))
   end if
end do
end function qlaplace

pure function qcauchy(p, loc, scale) result(x)
! Cauchy quantile.
real(kind=dp), intent(in) :: p(:)
real(kind=dp), intent(in) :: loc, scale
real(kind=dp) :: x(size(p))
integer :: i
if (scale <= 0.0_dp) then
   x = nanv()
   return
end if
do i = 1, size(p)
   if (p(i) <= 0.0_dp) then
      x(i) = -huge(1.0_dp)
   else if (p(i) >= 1.0_dp) then
      x(i) = huge(1.0_dp)
   else
      x(i) = loc + scale * tan(pi * (p(i) - 0.5_dp))
   end if
end do
end function qcauchy

pure function qged(p, loc, scale, beta) result(x)
! Generalized error quantile.
real(kind=dp), intent(in) :: p(:)
real(kind=dp), intent(in) :: loc, scale, beta
real(kind=dp) :: x(size(p))
integer :: i, it
real(kind=dp) :: a, u, lo, hi, mid, g
if (scale <= 0.0_dp .or. beta <= 0.0_dp) then
   x = nanv()
   return
end if
a = 1.0_dp / beta
do i = 1, size(p)
   if (p(i) <= 0.0_dp) then
      x(i) = -huge(1.0_dp)
   else if (p(i) >= 1.0_dp) then
      x(i) = huge(1.0_dp)
   else if (p(i) == 0.5_dp) then
      x(i) = loc
   else
      u = 2.0_dp * abs(p(i) - 0.5_dp)
      lo = 0.0_dp
      hi = 1.0_dp
      do
         g = gammp(a, hi**beta)
         if (g >= u) exit
         hi = hi * 2.0_dp
      end do
      do it = 1, 100
         mid = 0.5_dp * (lo + hi)
         g = gammp(a, mid**beta)
         if (g < u) then
            lo = mid
         else
            hi = mid
         end if
      end do
      if (p(i) < 0.5_dp) then
         x(i) = loc - scale * 0.5_dp * (lo + hi)
      else
         x(i) = loc + scale * 0.5_dp * (lo + hi)
      end if
   end if
end do
end function qged

pure function qhyperb(p, loc, scale, alpha) result(x)
! Symmetric hyperbolic quantile.
real(kind=dp), intent(in) :: p(:)
real(kind=dp), intent(in) :: loc, scale, alpha
real(kind=dp) :: x(size(p))
integer :: i, it
real(kind=dp) :: u, lo, hi, mid, area
if (scale <= 0.0_dp .or. alpha <= 0.0_dp) then
   x = nanv()
   return
end if
do i = 1, size(p)
   if (p(i) <= 0.0_dp) then
      x(i) = -huge(1.0_dp)
   else if (p(i) >= 1.0_dp) then
      x(i) = huge(1.0_dp)
   else if (p(i) == 0.5_dp) then
      x(i) = loc
   else
      u = abs(p(i) - 0.5_dp)
      lo = 0.0_dp
      hi = scale
      do
         area = hyperb_int(hi, loc, scale, alpha)
         if (area >= u) exit
         hi = hi * 2.0_dp
      end do
      do it = 1, 100
         mid = 0.5_dp * (lo + hi)
         area = hyperb_int(mid, loc, scale, alpha)
         if (area < u) then
            lo = mid
         else
            hi = mid
         end if
      end do
      if (p(i) < 0.5_dp) then
         x(i) = loc - 0.5_dp * (lo + hi)
      else
         x(i) = loc + 0.5_dp * (lo + hi)
      end if
   end if
end do
end function qhyperb

pure function qsech(p) result(x)
! Hyperbolic secant quantile.
real(kind=dp), intent(in) :: p(:)
real(kind=dp) :: x(size(p))
integer :: i
do i = 1, size(p)
   if (p(i) <= 0.0_dp) then
      x(i) = -huge(1.0_dp)
   else if (p(i) >= 1.0_dp) then
      x(i) = huge(1.0_dp)
   else
      x(i) = (2.0_dp / pi) * log(tan(0.5_dp * pi * p(i)))
   end if
end do
end function qsech

pure function qnorm(p, mean, sd) result(x)
! Normal quantile.
real(kind=dp), intent(in) :: p(:)
real(kind=dp), intent(in) :: mean, sd
real(kind=dp) :: x(size(p))
integer :: i
if (sd <= 0.0_dp) then
   x = nanv()
   return
end if
do i = 1, size(p)
   x(i) = mean + sd * inv_norm(p(i))
end do
end function qnorm

function rhyperb(n, loc, scale, alpha) result(r)
! Symmetric hyperbolic random variates via rejection sampling.
integer, intent(in) :: n
real(kind=dp), intent(in) :: loc, scale, alpha
real(kind=dp), allocatable :: r(:)
integer :: i
real(kind=dp) :: u, v, b, x, d, acc
if (n < 1) then
   allocate(r(0))
   return
end if
if (scale <= 0.0_dp .or. alpha <= 0.0_dp) then
   allocate(r(n))
   r = nanv()
   return
end if
allocate(r(n))
b = 1.0_dp / alpha
do i = 1, n
   do
      call random_number(u)
      if (u <= 0.0_dp) u = 1.0e-12_dp
      if (u >= 1.0_dp) u = 1.0_dp - 1.0e-12_dp
      if (u < 0.5_dp) then
         x = loc + b * log(2.0_dp * u)
      else
         x = loc - b * log(2.0_dp * (1.0_dp - u))
      end if
      d = abs(x - loc)
      acc = exp(-alpha * (sqrt(scale * scale + d * d) - d)) / &
            max(1.0_dp, scale * besselk1(alpha * scale))
      call random_number(v)
      if (v <= acc) exit
   end do
   r(i) = x
end do
end function rhyperb

pure function besseli0(x) result(y)
! Modified Bessel function I0.
real(kind=dp), intent(in) :: x
real(kind=dp) :: y, ax, y2
ax = abs(x)
if (ax < 3.75_dp) then
   y2 = (x / 3.75_dp)**2
   y = 1.0_dp + y2 * (3.5156229_dp + y2 * (3.0899424_dp + y2 * (1.2067492_dp + &
       y2 * (0.2659732_dp + y2 * (0.0360768_dp + y2 * 0.0045813_dp)))))
else
   y2 = 3.75_dp / ax
   y = (exp(ax) / sqrt(ax)) * (0.39894228_dp + y2 * (0.01328592_dp + y2 * (0.00225319_dp + &
       y2 * (-0.00157565_dp + y2 * (0.00916281_dp + y2 * (-0.02057706_dp + &
       y2 * (0.02635537_dp + y2 * (-0.01647633_dp + y2 * 0.00392377_dp))))))))
end if
end function besseli0

pure function besseli1(x) result(y)
! Modified Bessel function I1.
real(kind=dp), intent(in) :: x
real(kind=dp) :: y, ax, y2
ax = abs(x)
if (ax < 3.75_dp) then
   y2 = (x / 3.75_dp)**2
   y = x * (0.5_dp + y2 * (0.87890594_dp + y2 * (0.51498869_dp + y2 * (0.15084934_dp + &
       y2 * (0.02658733_dp + y2 * (0.00301532_dp + y2 * 0.00032411_dp))))))
else
   y2 = 3.75_dp / ax
   y = (exp(ax) / sqrt(ax)) * (0.39894228_dp + y2 * (-0.03988024_dp + y2 * (-0.00362018_dp + &
       y2 * (0.00163801_dp + y2 * (-0.01031555_dp + y2 * (0.02282967_dp + &
       y2 * (-0.02895312_dp + y2 * (0.01787654_dp + y2 * (-0.00420059_dp)))))))))
   if (x < 0.0_dp) y = -y
end if
end function besseli1

pure function besselk0(x) result(y)
! Modified Bessel function K0.
real(kind=dp), intent(in) :: x
real(kind=dp) :: y, y2
if (x <= 0.0_dp) then
   y = huge(1.0_dp)
   return
end if
if (x <= 2.0_dp) then
   y2 = x * x / 4.0_dp
   y = -log(x / 2.0_dp) * besseli0(x) + (-0.57721566_dp + y2 * (0.42278420_dp + &
       y2 * (0.23069756_dp + y2 * (0.03488590_dp + y2 * (0.00262698_dp + &
       y2 * (0.00010750_dp + y2 * 0.00000740_dp))))))
else
   y2 = 2.0_dp / x
   y = (exp(-x) / sqrt(x)) * (1.25331414_dp + y2 * (-0.07832358_dp + y2 * (0.02189568_dp + &
       y2 * (-0.01062446_dp + y2 * (0.00587872_dp + y2 * (-0.00251540_dp + &
       y2 * 0.00053208_dp))))))
end if
end function besselk0

pure function besselk1(x) result(y)
! Modified Bessel function K1.
real(kind=dp), intent(in) :: x
real(kind=dp) :: y, y2
if (x <= 0.0_dp) then
   y = huge(1.0_dp)
   return
end if
if (x <= 2.0_dp) then
   y2 = x * x / 4.0_dp
   y = log(x / 2.0_dp) * besseli1(x) + (1.0_dp / x) * (1.0_dp + y2 * (0.15443144_dp + &
       y2 * (-0.67278579_dp + y2 * (-0.18156897_dp + y2 * (-0.01919402_dp + &
       y2 * (-0.00110404_dp + y2 * (-0.00004686_dp)))))))
else
   y2 = 2.0_dp / x
   y = (exp(-x) / sqrt(x)) * (1.25331414_dp + y2 * (0.23498619_dp + y2 * (-0.03655620_dp + &
       y2 * (0.01504268_dp + y2 * (-0.00780353_dp + y2 * (0.00325614_dp + &
       y2 * (-0.00068245_dp)))))))
end if
end function besselk1

pure function log1pexp(t) result(y)
! Stable log(1+exp(t)).
real(kind=dp), intent(in) :: t
real(kind=dp) :: y
if (t > 0.0_dp) then
   y = t + log(1.0_dp + exp(-t))
else
   y = log(1.0_dp + exp(t))
end if
end function log1pexp

pure function log_beta(a, b) result(y)
! Log beta function.
real(kind=dp), intent(in) :: a, b
real(kind=dp) :: y
y = log_gamma(a) + log_gamma(b) - log_gamma(a + b)
end function log_beta

function fit_norm(x) result(pars)
! Method-of-moments then MLE for normal distribution.
real(kind=dp), intent(in) :: x(:)
real(kind=dp) :: pars(2)
real(kind=dp) :: mu0, sd0
real(kind=dp) :: u0(2), ubest(2)
integer :: n
real(kind=dp) :: tol

n = size(x)
mu0 = mean(x)
sd0 = sd(x)
if (sd0 <= 0.0_dp) then
   pars = nanv(); return
end if
u0 = [mu0, log(sd0)]
tol = 1.0e-6_dp
ubest = nelder_mead(loglik, u0, 0.1_dp, 200, tol)
pars(1) = ubest(1)
pars(2) = exp(ubest(2))

contains
   function loglik(u) result(f)
      real(kind=dp), intent(in) :: u(:)
      real(kind=dp) :: f, mu, sd
      mu = u(1)
      sd = exp(u(2))
      if (sd <= 0.0_dp) then
         f = -huge(1.0_dp)
      else
         f = -real(n, dp) * (log(sd) + 0.5_dp * log(2.0_dp * pi)) - 0.5_dp * sum(((x - mu) / sd)**2)
      end if
   end function loglik
end function fit_norm
function fit_exp(x) result(pars)
! Method-of-moments then MLE for exponential distribution.
real(kind=dp), intent(in) :: x(:)
real(kind=dp) :: pars(1)
real(kind=dp) :: rate0, u0(1), ubest(1)
real(kind=dp) :: tol

if (any(x < 0.0_dp)) then
   pars = nanv(); return
end if
rate0 = 1.0_dp / mean(x)
if (rate0 <= 0.0_dp) then
   pars = nanv(); return
end if
u0 = [log(rate0)]
tol = 1.0e-6_dp
ubest = nelder_mead(loglik, u0, 0.1_dp, 200, tol)
pars(1) = exp(ubest(1))

contains
   function loglik(u) result(f)
      real(kind=dp), intent(in) :: u(:)
      real(kind=dp) :: f, rate
      rate = exp(u(1))
      if (rate <= 0.0_dp) then
         f = -huge(1.0_dp)
      else
         f = real(size(x), dp) * log(rate) - rate * sum(x)
      end if
   end function loglik
end function fit_exp
function fit_gamma(x) result(pars)
! Method-of-moments then MLE for gamma distribution.
real(kind=dp), intent(in) :: x(:)
real(kind=dp) :: pars(2)
real(kind=dp) :: m, v, shape0, scale0
real(kind=dp) :: u0(2), ubest(2)
real(kind=dp) :: tol

if (any(x < 0.0_dp)) then
   pars = nanv(); return
end if
m = mean(x)
v = sd(x)**2
if (m <= 0.0_dp .or. v <= 0.0_dp) then
   pars = nanv(); return
end if
shape0 = (m * m) / v
scale0 = v / m
u0 = [log(shape0), log(scale0)]
tol = 1.0e-6_dp
ubest = nelder_mead(loglik, u0, 0.1_dp, 300, tol)
pars(1) = exp(ubest(1))
pars(2) = exp(ubest(2))

contains
   function loglik(u) result(f)
      real(kind=dp), intent(in) :: u(:)
      real(kind=dp) :: f, shape, scale
      shape = exp(u(1))
      scale = exp(u(2))
      if (shape <= 0.0_dp .or. scale <= 0.0_dp) then
         f = -huge(1.0_dp)
      else
         f = (shape - 1.0_dp) * sum(log(x)) - sum(x) / scale - real(size(x), dp) * (shape * log(scale) + log_gamma(shape))
      end if
   end function loglik
end function fit_gamma

function fit_lnorm(x) result(pars)
! Method-of-moments then MLE for lognormal distribution.
real(kind=dp), intent(in) :: x(:)
real(kind=dp) :: pars(2)
real(kind=dp), allocatable :: lx(:)
real(kind=dp) :: mu0, sd0

if (any(x <= 0.0_dp)) then
   pars = nanv(); return
end if
allocate (lx(size(x)))
lx = log(x)
mu0 = mean(lx)
sd0 = sd(lx)
pars(1) = mu0
pars(2) = sd0
deallocate (lx)
end function fit_lnorm

function fit_t(x) result(pars)
! MLE for location-scale Student t distribution.
! Returns [mu, sigma, df].
real(kind=dp), intent(in) :: x(:)
real(kind=dp) :: pars(3)
real(kind=dp) :: kex, df0, sd0, mu0
real(kind=dp) :: u0(3), ubest(3)
real(kind=dp), allocatable :: z(:)
real(kind=dp) :: tol

mu0 = mean(x)
sd0 = sd(x)
if (sd0 <= 0.0_dp) then
   pars = nanv(); return
end if
allocate (z(size(x)))
z = (x - mu0) / sd0
kex = kurtosis(z)
if (kex > 0.0_dp) then
   df0 = 6.0_dp / kex + 4.0_dp
else
   df0 = 30.0_dp
end if
df0 = max(df0, 2.01_dp)
u0 = [0.0_dp, 0.0_dp, log(df0 - 2.0_dp)]
tol = 1.0e-6_dp
ubest = nelder_mead(loglik, u0, 0.2_dp, 600, tol)
pars(1) = mu0 + sd0 * ubest(1)
pars(2) = sd0 * exp(ubest(2))
pars(3) = 2.0_dp + exp(ubest(3))
deallocate (z)

contains
   function loglik(u) result(f)
      real(kind=dp), intent(in) :: u(:)
      real(kind=dp) :: f, mu, sigma, df
      mu = mu0 + sd0 * u(1)
      sigma = sd0 * exp(u(2))
      df = 2.0_dp + exp(u(3))
      if (sigma <= 0.0_dp .or. df <= 2.0_dp) then
         f = -huge(1.0_dp)
      else
         f = sum(log_gamma(0.5_dp * (df + 1.0_dp)) - log_gamma(0.5_dp * df) - 0.5_dp * log(df * pi) &
             - log(sigma) - 0.5_dp * (df + 1.0_dp) * log(1.0_dp + ((x - mu) / sigma)**2 / df))
      end if
   end function loglik
end function fit_t
function fit_chisq(x) result(pars)
! Method-of-moments then MLE for chi-square distribution.
real(kind=dp), intent(in) :: x(:)
real(kind=dp) :: pars(1)
real(kind=dp) :: df0, u0(1), ubest(1)
real(kind=dp) :: tol

if (any(x < 0.0_dp)) then
   pars = nanv(); return
end if
df0 = mean(x)
if (df0 <= 0.0_dp) then
   pars = nanv(); return
end if
u0 = [log(df0)]
tol = 1.0e-6_dp
ubest = nelder_mead(loglik, u0, 0.1_dp, 300, tol)
pars(1) = exp(ubest(1))

contains
   function loglik(u) result(f)
      real(kind=dp), intent(in) :: u(:)
      real(kind=dp) :: f, df
      df = exp(u(1))
      if (df <= 0.0_dp) then
         f = -huge(1.0_dp)
      else
         f = (0.5_dp * df - 1.0_dp) * sum(log(x)) - 0.5_dp * sum(x) - real(size(x), dp) * &
             (0.5_dp * df * log(2.0_dp) + log_gamma(0.5_dp * df))
      end if
   end function loglik
end function fit_chisq
function fit_f(x) result(pars)
! Method-of-moments then MLE for F distribution.
real(kind=dp), intent(in) :: x(:)
real(kind=dp) :: pars(2)
real(kind=dp) :: m, v, df2, df1, K, tmp
real(kind=dp) :: u0(2), ubest(2)
real(kind=dp) :: tol

if (any(x <= 0.0_dp)) then
   pars = nanv(); return
end if
m = mean(x)
v = sd(x)**2
if (m <= 1.0_dp .or. v <= 0.0_dp) then
   pars = nanv(); return
end if
df2 = 2.0_dp * m / (m - 1.0_dp)
K = 2.0_dp * df2 * df2 / ((df2 - 2.0_dp)**2 * (df2 - 4.0_dp))
tmp = v / K - 1.0_dp
if (df2 <= 4.0_dp .or. tmp <= 0.0_dp) then
   df1 = 10.0_dp
else
   df1 = (df2 - 2.0_dp) / tmp
end if
u0 = [log(df1), log(df2)]
tol = 1.0e-6_dp
ubest = nelder_mead(loglik, u0, 0.1_dp, 400, tol)
pars(1) = exp(ubest(1))
pars(2) = exp(ubest(2))

contains
   function loglik(u) result(f)
      real(kind=dp), intent(in) :: u(:)
      real(kind=dp) :: f, df1, df2, a, b
      df1 = exp(u(1))
      df2 = exp(u(2))
      if (df1 <= 0.0_dp .or. df2 <= 0.0_dp) then
         f = -huge(1.0_dp)
      else
         a = 0.5_dp * df1
         b = 0.5_dp * df2
         f = real(size(x), dp) * (a * log(df1 / df2) - log_beta(a, b)) + &
             (a - 1.0_dp) * sum(log(x)) - (a + b) * sum(log(1.0_dp + (df1 / df2) * x))
      end if
   end function loglik
end function fit_f
function fit_beta(x) result(pars)
! Method-of-moments then MLE for beta distribution.
real(kind=dp), intent(in) :: x(:)
real(kind=dp) :: pars(2)
real(kind=dp) :: m, v, t, a0, b0
real(kind=dp) :: u0(2), ubest(2)
real(kind=dp) :: tol

if (any(x <= 0.0_dp) .or. any(x >= 1.0_dp)) then
   pars = nanv(); return
end if
m = mean(x)
v = sd(x)**2
t = m * (1.0_dp - m) / v - 1.0_dp
if (t <= 0.0_dp) then
   pars = nanv(); return
end if
a0 = m * t
b0 = (1.0_dp - m) * t
u0 = [log(a0), log(b0)]
tol = 1.0e-6_dp
ubest = nelder_mead(loglik, u0, 0.1_dp, 300, tol)
pars(1) = exp(ubest(1))
pars(2) = exp(ubest(2))

contains
   function loglik(u) result(f)
      real(kind=dp), intent(in) :: u(:)
      real(kind=dp) :: f, a, b
      a = exp(u(1)); b = exp(u(2))
      if (a <= 0.0_dp .or. b <= 0.0_dp) then
         f = -huge(1.0_dp)
      else
         f = (a - 1.0_dp) * sum(log(x)) + (b - 1.0_dp) * sum(log(1.0_dp - x)) - &
             real(size(x), dp) * log_beta(a, b)
      end if
   end function loglik
end function fit_beta
function fit_logis(x) result(pars)
! Method-of-moments then MLE for logistic distribution.
real(kind=dp), intent(in) :: x(:)
real(kind=dp) :: pars(2)
real(kind=dp) :: mu0, s0
real(kind=dp) :: u0(2), ubest(2)
real(kind=dp) :: tol

mu0 = mean(x)
s0 = sd(x) * sqrt(3.0_dp) / pi
if (s0 <= 0.0_dp) then
   pars = nanv(); return
end if
u0 = [mu0, log(s0)]
tol = 1.0e-6_dp
ubest = nelder_mead(loglik, u0, 0.1_dp, 300, tol)
pars(1) = ubest(1)
pars(2) = exp(ubest(2))

contains
   function loglik(u) result(f)
      real(kind=dp), intent(in) :: u(:)
      real(kind=dp) :: f, loc, scale
      integer :: i
      loc = u(1)
      scale = exp(u(2))
      if (scale <= 0.0_dp) then
         f = -huge(1.0_dp)
      else
         f = 0.0_dp
         do i = 1, size(x)
            f = f - log(scale) - (x(i) - loc) / scale - 2.0_dp * log1pexp(-(x(i) - loc) / scale)
         end do
      end if
   end function loglik
end function fit_logis

function fit_laplace(x) result(pars)
! Method-of-moments fit for Laplace distribution.
real(kind=dp), intent(in) :: x(:)
real(kind=dp) :: pars(2)
real(kind=dp) :: loc0, scale0

loc0 = median(x)
scale0 = mean(abs(x - loc0))
if (scale0 <= 0.0_dp) then
   pars = nanv(); return
end if
pars(1) = loc0
pars(2) = scale0
end function fit_laplace

function fit_cauchy(x) result(pars)
! MLE for Cauchy distribution (location, scale).
real(kind=dp), intent(in) :: x(:)
real(kind=dp) :: pars(2)
real(kind=dp) :: loc0, scale0
real(kind=dp) :: u0(2), ubest(2)
real(kind=dp) :: tol

loc0 = median(x)
scale0 = median(abs(x - loc0))
if (scale0 <= 0.0_dp) then
   pars = nanv(); return
end if
u0 = [loc0, log(scale0)]
tol = 1.0e-6_dp
ubest = nelder_mead(loglik, u0, 0.1_dp, 300, tol)
pars(1) = ubest(1)
pars(2) = exp(ubest(2))

contains
   function loglik(u) result(f)
      real(kind=dp), intent(in) :: u(:)
      real(kind=dp) :: f, loc, scale, z
      integer :: i
      loc = u(1)
      scale = exp(u(2))
      if (scale <= 0.0_dp) then
         f = -huge(1.0_dp)
      else
         f = 0.0_dp
         do i = 1, size(x)
            z = (x(i) - loc) / scale
            f = f - log(pi * scale * (1.0_dp + z * z))
         end do
      end if
   end function loglik
end function fit_cauchy

function fit_ged(x) result(pars)
! MLE for generalized error distribution (location, scale, beta).
real(kind=dp), intent(in) :: x(:)
real(kind=dp) :: pars(3)
real(kind=dp) :: loc0, scale0, beta0
real(kind=dp) :: u0(3), ubest(3)
real(kind=dp) :: tol

loc0 = median(x)
scale0 = sd(x)
if (scale0 <= 0.0_dp) then
   pars = nanv(); return
end if
beta0 = 2.0_dp
u0 = [loc0, log(scale0), log(beta0)]
tol = 1.0e-6_dp
ubest = nelder_mead(loglik, u0, 0.1_dp, 400, tol)
pars(1) = ubest(1)
pars(2) = exp(ubest(2))
pars(3) = exp(ubest(3))

contains
   function loglik(u) result(f)
      real(kind=dp), intent(in) :: u(:)
      real(kind=dp) :: f, loc, scale, beta
      loc = u(1)
      scale = exp(u(2))
      beta = exp(u(3))
      if (scale <= 0.0_dp .or. beta <= 0.0_dp) then
         f = -huge(1.0_dp)
      else
         f = real(size(x), dp) * (log(beta) - log(2.0_dp * scale) - log_gamma(1.0_dp / beta)) - &
             sum((abs(x - loc) / scale)**beta)
      end if
   end function loglik
end function fit_ged

function fit_hyperb(x) result(pars)
! MLE for symmetric hyperbolic distribution (location, scale, alpha).
real(kind=dp), intent(in) :: x(:)
real(kind=dp) :: pars(3)
real(kind=dp) :: loc0, scale0, alpha0
real(kind=dp) :: u0(3), ubest(3), sd0, mad
real(kind=dp) :: tol

loc0 = median(x)
sd0 = sd(x)
if (sd0 <= 0.0_dp) then
   pars = nanv(); return
end if
mad = median(abs(x - loc0))
if (mad > 0.0_dp) then
   alpha0 = log(2.0_dp) / mad
else
   alpha0 = 1.0_dp
end if
alpha0 = max(alpha0, 1.0e-6_dp)
scale0 = hyperb_scale_from_sd(alpha0, sd0)
u0 = [loc0, log(scale0), log(alpha0)]
tol = 1.0e-6_dp
ubest = nelder_mead(loglik, u0, 0.1_dp, 400, tol)
pars(1) = ubest(1)
pars(2) = exp(ubest(2))
pars(3) = exp(ubest(3))

contains
   function hyperb_scale_from_sd(alpha, sd) result(scale)
      real(kind=dp), intent(in) :: alpha, sd
      real(kind=dp) :: scale, lo, hi, mid, v
      integer :: it
      if (sd <= 0.0_dp) then
         scale = 1.0_dp
         return
      end if
      lo = 1.0e-6_dp
      hi = max(1.0_dp, sd * 5.0_dp)
      do it = 1, 60
         mid = 0.5_dp * (lo + hi)
         v = hyperb_var(mid, alpha)
         if (v > sd * sd) then
            hi = mid
         else
            lo = mid
         end if
      end do
      scale = 0.5_dp * (lo + hi)
   end function hyperb_scale_from_sd

   function hyperb_var(scale, alpha) result(v)
      real(kind=dp), intent(in) :: scale, alpha
      real(kind=dp) :: v, k0, k1, k2, x
      if (scale <= 0.0_dp .or. alpha <= 0.0_dp) then
         v = huge(1.0_dp)
         return
      end if
      x = alpha * scale
      k1 = besselk1(x)
      k0 = besselk0(x)
      if (k1 <= 0.0_dp .or. k0 <= 0.0_dp) then
         v = huge(1.0_dp)
         return
      end if
      k2 = k0 + 2.0_dp * k1 / x
      v = (scale / alpha) * (k2 / k1)
   end function hyperb_var

   function loglik(u) result(f)
      real(kind=dp), intent(in) :: u(:)
      real(kind=dp) :: f, loc, scale, alpha, k1
      loc = u(1)
      scale = exp(u(2))
      alpha = exp(u(3))
      if (scale <= 0.0_dp .or. alpha <= 0.0_dp) then
         f = -huge(1.0_dp)
      else
         k1 = besselk1(alpha * scale)
         if (k1 <= 0.0_dp) then
            f = -huge(1.0_dp)
         else
            f = real(size(x), dp) * (log(alpha) - log(2.0_dp * scale) - log(k1)) - &
                alpha * sum(sqrt(scale * scale + (x - loc)**2))
         end if
      end if
   end function loglik
end function fit_hyperb

function fit_sech(x) result(pars)
! Method-of-moments then MLE for hyperbolic secant distribution.
real(kind=dp), intent(in) :: x(:)
real(kind=dp) :: pars(2)
real(kind=dp) :: mu0, s0
real(kind=dp) :: u0(2), ubest(2)
real(kind=dp) :: tol

mu0 = mean(x)
s0 = sd(x)
if (s0 <= 0.0_dp) then
   pars = nanv(); return
end if
u0 = [mu0, log(s0)]
tol = 1.0e-6_dp
ubest = nelder_mead(loglik, u0, 0.1_dp, 300, tol)
pars(1) = ubest(1)
pars(2) = exp(ubest(2))

contains
   function loglik(u) result(f)
      real(kind=dp), intent(in) :: u(:)
      real(kind=dp) :: f, loc, scale
      integer :: i
      loc = u(1)
      scale = exp(u(2))
      if (scale <= 0.0_dp) then
         f = -huge(1.0_dp)
      else
         f = 0.0_dp
         do i = 1, size(x)
            f = f - log(2.0_dp * scale) - log(cosh(0.5_dp * pi * (x(i) - loc) / scale))
         end do
      end if
   end function loglik
end function fit_sech

pure subroutine arma_resid(x, phi, theta, resid)
! compute ARMA residuals for given coefficients
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in) :: phi(:)
real(kind=dp), intent(in) :: theta(:)
real(kind=dp), intent(out) :: resid(:)
integer :: n, p, q, t, j, m

n = size(x)
p = size(phi)
q = size(theta)
resid = 0.0_dp
do t = 1, n
   resid(t) = x(t)
   m = min(p, t - 1)
   if (m > 0) then
      do j = 1, m
         resid(t) = resid(t) - phi(j) * x(t - j)
      end do
   end if
   m = min(q, t - 1)
   if (m > 0) then
      do j = 1, m
         resid(t) = resid(t) - theta(j) * resid(t - j)
      end do
   end if
end do
end subroutine arma_resid

pure subroutine armafit_metrics(x, p, q, n_iter, rmse, aic, bic, phi, theta, ok)
! ARMA fit metrics.
real(kind=dp), intent(in) :: x(:)
integer, intent(in) :: p, q, n_iter
real(kind=dp), intent(out) :: rmse, aic, bic
real(kind=dp), intent(out) :: phi(:), theta(:)
logical, intent(out) :: ok
integer :: n, n_eff, t, j, maxpq, k_params
real(kind=dp), allocatable :: y(:), xmat(:,:), beta(:), xtx(:,:), xty(:), resid(:)
real(kind=dp) :: sse, sigma2, ridge

n = size(x)
maxpq = max(p, q)
n_eff = n - maxpq
ok = .true.
if (n_eff <= 0) then
   rmse = 0.0_dp; aic = huge(1.0_dp); bic = huge(1.0_dp)
   ok = .false.; return
end if
if (p + q == 0) then
   allocate (resid(n))
   resid = x - mean(x)
   sse = sum(resid**2)
   rmse = sqrt(sse / real(n_eff, dp))
   sigma2 = sse / real(n_eff, dp)
   k_params = 1
   aic = real(n_eff, dp) * log(sigma2) + 2.0_dp * real(k_params, dp)
   bic = real(n_eff, dp) * log(sigma2) + log(real(n_eff, dp)) * real(k_params, dp)
   deallocate (resid)
   return
end if
allocate (resid(n))
resid = x
do t = 1, n_iter
   allocate (y(n_eff), xmat(n_eff, p + q))
   y = x(maxpq + 1:n)
   do j = 1, p
      xmat(:, j) = x(maxpq + 1 - j:n - j)
   end do
   do j = 1, q
      xmat(:, p + j) = resid(maxpq + 1 - j:n - j)
   end do
   xtx = matmul(transpose(xmat), xmat)
   xty = matmul(transpose(xmat), y)
   call solve_linear(xtx, xty, beta, ok)
   if (.not. ok) then
      ridge = 1.0e-6_dp
      do j = 1, p + q
         xtx(j, j) = xtx(j, j) + ridge
      end do
      call solve_linear(xtx, xty, beta, ok)
   end if
   deallocate (y, xmat)
   if (.not. ok) exit
   if (p > 0) phi(1:p) = beta(1:p)
   if (q > 0) theta(1:q) = beta(p + 1:p + q)
   call arma_resid(x, phi, theta, resid)
end do
if (.not. ok) then
   rmse = 0.0_dp; aic = huge(1.0_dp); bic = huge(1.0_dp)
   deallocate (resid)
   return
end if
sse = sum(resid(maxpq + 1:n)**2)
rmse = sqrt(sse / real(n_eff, dp))
if (p + q < 1) then
   k_params = 1
else
   k_params = p + q
end if
if (sse > 0.0_dp) then
   sigma2 = sse / real(n_eff, dp)
   aic = real(n_eff, dp) * log(sigma2) + 2.0_dp * real(k_params, dp)
   bic = real(n_eff, dp) * log(sigma2) + log(real(n_eff, dp)) * real(k_params, dp)
else
   aic = huge(1.0_dp)
   bic = huge(1.0_dp)
end if
deallocate (resid)
end subroutine armafit_metrics

subroutine armafit(x, p, q, niter)
! fit ARMA(p,q) and report RMSE/AIC/BIC and coefficients
real(kind=dp), intent(in) :: x(:)
integer, intent(in) :: p, q
integer, intent(in), optional :: niter
real(kind=dp), allocatable :: phi(:), theta(:)
real(kind=dp) :: rmse, aic, bic
logical :: ok
integer :: it
character(len=18) :: s_rmse, s_aic, s_bic
character(len=18) :: s_val

if (p < 0 .or. q < 0) then
   print *, "Error: armafit() orders must be >= 0"
   return
end if
if (size(x) < 2) then
   print *, "Error: armafit() requires size(x) > 1"
   return
end if
if (present(niter)) then
   it = niter
else
   it = 5
end if
if (it < 1) it = 1
allocate (phi(max(1, p)), theta(max(1, q)))
phi = 0.0_dp; theta = 0.0_dp
call armafit_metrics(x, p, q, it, rmse, aic, bic, phi, theta, ok)
if (.not. ok) then
   print *, "Error: armafit() failed"
   return
end if
print "(a6,a18,a18,a18)", "lag", "RMSE", "AIC", "BIC"
write (s_rmse, "(g18.6)") rmse
write (s_aic, "(g18.6)") aic
write (s_bic, "(g18.6)") bic
print "(i6,a18,a18,a18)", p + q, s_rmse, s_aic, s_bic

print *
if (p > 0) then
   print "(a6)", "AR"
   write (*, "(6x)", advance="no")
   do it = 1, p
      write (s_val, "(a,i0)") "AR", it
      write (*, "(1x,a12)", advance="no") trim(s_val)
   end do
   print *
   write (*, "(i6)", advance="no") p
   do it = 1, p
      write (*, "(1x,f12.6)", advance="no") phi(it)
   end do
   print *
end if
if (q > 0) then
   print *
   print "(a6)", "MA"
   write (*, "(6x)", advance="no")
   do it = 1, q
      write (s_val, "(a,i0)") "MA", it
      write (*, "(1x,a12)", advance="no") trim(s_val)
   end do
   print *
   write (*, "(i6)", advance="no") q
   do it = 1, q
      write (*, "(1x,f12.6)", advance="no") theta(it)
   end do
   print *
end if
end subroutine armafit

subroutine armafitgrid(x, p1, p2, q1, q2, niter)
! fit ARMA over a grid of orders
real(kind=dp), intent(in) :: x(:)
integer, intent(in) :: p1, p2, q1, q2
integer, intent(in), optional :: niter
integer :: p, q, it, best_aic_p, best_aic_q, best_bic_p, best_bic_q
real(kind=dp) :: rmse, aic, bic, best_aic, best_bic
real(kind=dp), allocatable :: phi(:), theta(:)
logical :: ok
character(len=18) :: s_rmse, s_aic, s_bic

if (p1 < 0 .or. q1 < 0 .or. p2 < p1 .or. q2 < q1) then
   print *, "Error: invalid order range in armafitgrid()"
   return
end if
if (present(niter)) then
   it = niter
else
   it = 5
end if
if (it < 1) it = 1
best_aic = huge(1.0_dp)
best_bic = huge(1.0_dp)
best_aic_p = p1; best_aic_q = q1
best_bic_p = p1; best_bic_q = q1
print "(a6,a6,a18,a18,a18)", "p", "q", "RMSE", "AIC", "BIC"
do p = p1, p2
   do q = q1, q2
      allocate (phi(max(1, p)), theta(max(1, q)))
      phi = 0.0_dp; theta = 0.0_dp
      call armafit_metrics(x, p, q, it, rmse, aic, bic, phi, theta, ok)
      if (.not. ok) then
         print "(i6,i6,3a18)", p, q, "NaN", "NaN", "NaN"
      else
         write (s_rmse, "(g18.6)") rmse
         write (s_aic, "(g18.6)") aic
         write (s_bic, "(g18.6)") bic
         print "(i6,i6,a18,a18,a18)", p, q, s_rmse, s_aic, s_bic
         if (aic < best_aic) then
            best_aic = aic; best_aic_p = p; best_aic_q = q
         end if
         if (bic < best_bic) then
            best_bic = bic; best_bic_p = p; best_bic_q = q
         end if
      end if
      deallocate (phi, theta)
   end do
end do
print *
print "(a,i0,a,i0)", "AIC chooses p=", best_aic_p, " q=", best_aic_q
print "(a,i0,a,i0)", "BIC chooses p=", best_bic_p, " q=", best_bic_q
end subroutine armafitgrid

subroutine armafitaic(x, nar_max, nma_max, niter)
! fit ARMA models up to max orders with early-stop by AIC
real(kind=dp), intent(in) :: x(:)
integer, intent(in), optional :: nar_max, nma_max
integer, intent(in), optional :: niter
integer :: pmax, qmax, r, p, q, no_improve, it
real(kind=dp) :: best_aic_prev
integer :: best_aic_p, best_aic_q, best_bic_p, best_bic_q
real(kind=dp) :: rmse, aic, bic, best_aic, best_bic
real(kind=dp), allocatable :: phi(:), theta(:)
logical :: ok
character(len=18) :: s_rmse, s_aic, s_bic

if (present(nar_max)) then
   pmax = nar_max
else
   pmax = 5
end if
if (present(nma_max)) then
   qmax = nma_max
else
   qmax = 5
end if
if (present(niter)) then
   it = niter
else
   it = 5
end if
if (it < 1) it = 1
if (pmax < 0 .or. qmax < 0) then
   print *, "Error: armafitaic() max orders must be >= 0"
   return
end if

best_aic = huge(1.0_dp)
best_bic = huge(1.0_dp)
best_aic_p = 0; best_aic_q = 0
best_bic_p = 0; best_bic_q = 0
no_improve = 0

print "(a6,a6,a18,a18,a18)", "p", "q", "RMSE", "AIC", "BIC"
do r = 0, max(pmax, qmax)
   best_aic_prev = best_aic
   do p = 0, min(r, pmax)
      q = r
      if (q <= qmax .and. max(p, q) == r) then
         allocate (phi(max(1, p)), theta(max(1, q)))
         phi = 0.0_dp; theta = 0.0_dp
         call armafit_metrics(x, p, q, it, rmse, aic, bic, phi, theta, ok)
         if (.not. ok) then
            print "(i6,i6,3a18)", p, q, "NaN", "NaN", "NaN"
         else
            write (s_rmse, "(g18.6)") rmse
            write (s_aic, "(g18.6)") aic
            write (s_bic, "(g18.6)") bic
            print "(i6,i6,a18,a18,a18)", p, q, s_rmse, s_aic, s_bic
            if (aic < best_aic) then
               best_aic = aic; best_aic_p = p; best_aic_q = q
            end if
            if (bic < best_bic) then
               best_bic = bic; best_bic_p = p; best_bic_q = q
            end if
         end if
         deallocate (phi, theta)
      end if
   end do
   do q = 0, min(r, qmax)
      p = r
      if (p <= pmax .and. max(p, q) == r) then
         if (q == r) cycle
         allocate (phi(max(1, p)), theta(max(1, q)))
         phi = 0.0_dp; theta = 0.0_dp
         call armafit_metrics(x, p, q, it, rmse, aic, bic, phi, theta, ok)
         if (.not. ok) then
            print "(i6,i6,3a18)", p, q, "NaN", "NaN", "NaN"
         else
            write (s_rmse, "(g18.6)") rmse
            write (s_aic, "(g18.6)") aic
            write (s_bic, "(g18.6)") bic
            print "(i6,i6,a18,a18,a18)", p, q, s_rmse, s_aic, s_bic
            if (aic < best_aic) then
               best_aic = aic; best_aic_p = p; best_aic_q = q
            end if
            if (bic < best_bic) then
               best_bic = bic; best_bic_p = p; best_bic_q = q
            end if
         end if
         deallocate (phi, theta)
      end if
   end do
   if (best_aic == huge(1.0_dp)) cycle
   if (best_aic < best_aic_prev) then
      no_improve = 0
   else
      no_improve = no_improve + 1
   end if
   if (no_improve >= 2) exit
end do

print *
print "(a,i0,a,i0)", "AIC chooses p=", best_aic_p, " q=", best_aic_q
print "(a,i0,a,i0)", "BIC chooses p=", best_bic_p, " q=", best_bic_q
end subroutine armafitaic

pure function aracf(phi, k) result(r)
! theoretical ACF for AR(p) up to lag k via Yule-Walker
real(kind=dp), intent(in) :: phi(:)
integer, intent(in) :: k
real(kind=dp), allocatable :: r(:)
integer :: p, i, j, n_lag
real(kind=dp), allocatable :: A(:,:), b(:), gamma(:), gamma_sol(:)
logical :: ok

p = size(phi)
if (k < 1 .or. p < 1) then
   allocate (r(0))
   return
end if
n_lag = k
allocate (gamma(0:p), r(n_lag), A(p, p), b(p), gamma_sol(p))
A = 0.0_dp
b = 0.0_dp
do i = 1, p
   do j = 1, p
      A(i, j) = 0.0_dp
      if (i == j) A(i, j) = 1.0_dp
      if (i - j >= 1 .and. i - j <= p) A(i, j) = A(i, j) - phi(i - j)
      if (i + j >= 1 .and. i + j <= p) A(i, j) = A(i, j) - phi(i + j)
   end do
   b(i) = phi(i)
end do
call solve_linear(A, b, gamma_sol, ok)
if (.not. ok) then
   r = -3.0_dp
   deallocate (gamma, A, b, gamma_sol)
   return
end if
gamma(1:p) = gamma_sol
gamma(0) = 1.0_dp
do i = 1, n_lag
   if (i <= p) then
      r(i) = gamma(i)
   else
      r(i) = 0.0_dp
      do j = 1, p
         r(i) = r(i) + phi(j) * r(i - j)
      end do
   end if
end do
if (gamma(0) /= 0.0_dp) then
   r = r / gamma(0)
end if
deallocate (gamma, A, b, gamma_sol)
end function aracf

pure function maacf(theta, k) result(r)
! theoretical ACF for MA(q) up to lag k
real(kind=dp), intent(in) :: theta(:)
integer, intent(in) :: k
real(kind=dp), allocatable :: r(:)
integer :: q, h, j, n_lag
real(kind=dp) :: gamma0, gammah

q = size(theta)
if (k < 1 .or. q < 1) then
   allocate (r(0))
   return
end if
n_lag = k
allocate (r(n_lag))
gamma0 = 1.0_dp
do j = 1, q
   gamma0 = gamma0 + theta(j) * theta(j)
end do
do h = 1, n_lag
   if (h > q) then
      r(h) = 0.0_dp
   else
      gammah = 0.0_dp
      do j = 0, q - h
         if (j == 0) then
            gammah = gammah + theta(h)
         else
            gammah = gammah + theta(j) * theta(j + h)
         end if
      end do
      if (gamma0 /= 0.0_dp) then
         r(h) = gammah / gamma0
      else
         r(h) = 0.0_dp
      end if
   end if
end do
end function maacf

pure function armaacf(phi, theta, k) result(r)
! theoretical ACF for ARMA(p,q) up to lag k via psi-weights
real(kind=dp), intent(in) :: phi(:)
real(kind=dp), intent(in) :: theta(:)
integer, intent(in) :: k
real(kind=dp), allocatable :: r(:)
integer :: p, q, h, j, m, L
real(kind=dp), allocatable :: psi(:)
real(kind=dp) :: gamma0, gammah, theta_j

p = size(phi)
q = size(theta)
if (k < 1) then
   allocate (r(0))
   return
end if
if (p < 1 .and. q < 1) then
   allocate (r(k))
   r = 0.0_dp
   return
end if
L = max(50, k + q + 50)
allocate (psi(0:L), r(k))
psi = 0.0_dp
psi(0) = 1.0_dp
do j = 1, L
   if (j <= q) then
      theta_j = theta(j)
   else
      theta_j = 0.0_dp
   end if
   psi(j) = theta_j
   m = min(p, j)
   if (m > 0) then
      psi(j) = psi(j) + sum(phi(1:m) * psi(j - 1:j - m:-1))
   end if
end do
gamma0 = sum(psi(0:L) * psi(0:L))
do h = 1, k
   gammah = 0.0_dp
   do j = 0, L - h
      gammah = gammah + psi(j) * psi(j + h)
   end do
   if (gamma0 /= 0.0_dp) then
      r(h) = gammah / gamma0
   else
      r(h) = 0.0_dp
   end if
end do
deallocate (psi)
end function armaacf

pure function arfimaacf(phi, theta, d, k) result(r)
! theoretical ACF for ARFIMA(p,d,q) up to lag k via spectral integration
real(kind=dp), intent(in) :: phi(:)
real(kind=dp), intent(in) :: theta(:)
real(kind=dp), intent(in) :: d
integer, intent(in) :: k
real(kind=dp), allocatable :: r(:)
integer :: p, q, h, j, m, kk
real(kind=dp) :: lam, frac_term, num_re, num_im, den_re, den_im, den2, f_lam, g0, gh
real(kind=dp), parameter :: eps = 1.0e-12_dp

p = size(phi)
q = size(theta)
if (k < 1) then
   allocate (r(0))
   return
end if
allocate (r(k))
if (abs(d) >= 0.5_dp) then
   r = -3.0_dp
   return
end if
if ((p == 0 .or. all(abs(phi) <= eps)) .and. (q == 0 .or. all(abs(theta) <= eps))) then
   r = fiacf(d, k)
   return
end if

m = max(2048, 64 * (k + max(p, q) + 1))
g0 = 0.0_dp
do j = 1, m
   lam = pi * (real(j, dp) - 0.5_dp) / real(m, dp)
   frac_term = max(eps, 2.0_dp * sin(0.5_dp * lam))
   frac_term = frac_term**(-2.0_dp * d)
   num_re = 1.0_dp
   num_im = 0.0_dp
   do kk = 1, q
      num_re = num_re + theta(kk) * cos(lam * real(kk, dp))
      num_im = num_im - theta(kk) * sin(lam * real(kk, dp))
   end do
   den_re = 1.0_dp
   den_im = 0.0_dp
   do kk = 1, p
      den_re = den_re - phi(kk) * cos(lam * real(kk, dp))
      den_im = den_im + phi(kk) * sin(lam * real(kk, dp))
   end do
   den2 = den_re * den_re + den_im * den_im
   if (den2 <= eps) then
      r = -3.0_dp
      return
   end if
   f_lam = frac_term * (num_re * num_re + num_im * num_im) / den2
   g0 = g0 + f_lam
end do
g0 = g0 / real(m, dp)
if (g0 <= eps) then
   r = -3.0_dp
   return
end if

do h = 1, k
   gh = 0.0_dp
   do j = 1, m
      lam = pi * (real(j, dp) - 0.5_dp) / real(m, dp)
      frac_term = max(eps, 2.0_dp * sin(0.5_dp * lam))
      frac_term = frac_term**(-2.0_dp * d)
      num_re = 1.0_dp
      num_im = 0.0_dp
      do kk = 1, q
         num_re = num_re + theta(kk) * cos(lam * real(kk, dp))
         num_im = num_im - theta(kk) * sin(lam * real(kk, dp))
      end do
      den_re = 1.0_dp
      den_im = 0.0_dp
      do kk = 1, p
         den_re = den_re - phi(kk) * cos(lam * real(kk, dp))
         den_im = den_im + phi(kk) * sin(lam * real(kk, dp))
      end do
      den2 = den_re * den_re + den_im * den_im
      if (den2 <= eps) then
         r = -3.0_dp
         return
      end if
      f_lam = frac_term * (num_re * num_re + num_im * num_im) / den2
      gh = gh + f_lam * cos(real(h, dp) * lam)
   end do
   gh = gh / real(m, dp)
   r(h) = gh / g0
end do
end function arfimaacf

pure function armapacf(phi, theta, k) result(pacf)
! theoretical PACF for ARMA(p,q) up to lag k using Durbin-Levinson
real(kind=dp), intent(in) :: phi(:)
real(kind=dp), intent(in) :: theta(:)
integer, intent(in) :: k
real(kind=dp), allocatable :: pacf(:)
real(kind=dp), allocatable :: r(:), phi_dl(:,:), v(:)
integer :: j, m

if (k < 1) then
   allocate (pacf(0))
   return
end if
if (size(phi) < 1 .and. size(theta) < 1) then
   allocate (pacf(k))
   pacf = 0.0_dp
   return
end if
r = armaacf(phi, theta, k)
allocate (pacf(k))
allocate (phi_dl(k, k), v(k))
phi_dl = 0.0_dp
v = 0.0_dp
pacf = 0.0_dp
if (abs(1.0_dp - r(1) * r(1)) < 1.0e-12_dp) then
   pacf(1) = r(1)
   deallocate (r, phi_dl, v)
   return
end if
phi_dl(1, 1) = r(1)
pacf(1) = r(1)
v(1) = 1.0_dp - r(1) * r(1)
do m = 2, k
   phi_dl(m, m) = (r(m) - sum(phi_dl(1:m - 1, m - 1) * r(m - 1:1:-1))) / v(m - 1)
   do j = 1, m - 1
      phi_dl(j, m) = phi_dl(j, m - 1) - phi_dl(m, m) * phi_dl(m - j, m - 1)
   end do
   pacf(m) = phi_dl(m, m)
   v(m) = v(m - 1) * (1.0_dp - phi_dl(m, m) * phi_dl(m, m))
end do
deallocate (r, phi_dl, v)
end function armapacf

pure function arpacf(phi, k) result(pacf)
! theoretical PACF for AR(p) up to lag k using Durbin-Levinson
real(kind=dp), intent(in) :: phi(:)
integer, intent(in) :: k
real(kind=dp), allocatable :: pacf(:)
real(kind=dp), allocatable :: r(:), phi_dl(:,:), v(:)
integer :: j, m

if (k < 1 .or. size(phi) < 1) then
   allocate (pacf(0))
   return
end if
r = aracf(phi, k)
allocate (pacf(k))
allocate (phi_dl(k, k), v(k))
phi_dl = 0.0_dp
v = 0.0_dp
pacf = 0.0_dp
if (abs(1.0_dp - r(1) * r(1)) < 1.0e-12_dp) then
   pacf(1) = r(1)
   deallocate (r, phi_dl, v)
   return
end if
phi_dl(1, 1) = r(1)
pacf(1) = r(1)
v(1) = 1.0_dp - r(1) * r(1)
do m = 2, k
   phi_dl(m, m) = (r(m) - sum(phi_dl(1:m - 1, m - 1) * r(m - 1:1:-1))) / v(m - 1)
   do j = 1, m - 1
      phi_dl(j, m) = phi_dl(j, m - 1) - phi_dl(m, m) * phi_dl(m - j, m - 1)
   end do
   pacf(m) = phi_dl(m, m)
   v(m) = v(m - 1) * (1.0_dp - phi_dl(m, m) * phi_dl(m, m))
end do
deallocate (r, phi_dl, v)
end function arpacf

pure function mapacf(theta, k) result(pacf)
! theoretical PACF for MA(q) up to lag k using Durbin-Levinson
real(kind=dp), intent(in) :: theta(:)
integer, intent(in) :: k
real(kind=dp), allocatable :: pacf(:)
real(kind=dp), allocatable :: r(:), phi_dl(:,:), v(:)
integer :: j, m

if (k < 1 .or. size(theta) < 1) then
   allocate (pacf(0))
   return
end if
r = maacf(theta, k)
allocate (pacf(k))
allocate (phi_dl(k, k), v(k))
phi_dl = 0.0_dp
v = 0.0_dp
pacf = 0.0_dp
if (abs(1.0_dp - r(1) * r(1)) < 1.0e-12_dp) then
   pacf(1) = r(1)
   deallocate (r, phi_dl, v)
   return
end if
phi_dl(1, 1) = r(1)
pacf(1) = r(1)
v(1) = 1.0_dp - r(1) * r(1)
do m = 2, k
   phi_dl(m, m) = (r(m) - sum(phi_dl(1:m - 1, m - 1) * r(m - 1:1:-1))) / v(m - 1)
   do j = 1, m - 1
      phi_dl(j, m) = phi_dl(j, m - 1) - phi_dl(m, m) * phi_dl(m - j, m - 1)
   end do
   pacf(m) = phi_dl(m, m)
   v(m) = v(m - 1) * (1.0_dp - phi_dl(m, m) * phi_dl(m, m))
end do
deallocate (r, phi_dl, v)
end function mapacf

pure subroutine ma_resid(x, theta, resid)
! compute MA residuals for given coefficients
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in) :: theta(:)
real(kind=dp), intent(out) :: resid(:)
integer :: n, k, t, j

n = size(x)
k = size(theta)
if (k <= 0) then
   resid(1:n) = x
   return
end if
resid(1:k) = x(1:k)
do t = k + 1, n
   resid(t) = x(t)
   do j = 1, k
      resid(t) = resid(t) - theta(j) * resid(t - j)
   end do
end do
end subroutine ma_resid

pure subroutine ma_refine(x, theta, n_iter)
! simple gradient descent refinement using MA residual recursion
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(inout) :: theta(:)
integer, intent(in), optional :: n_iter
integer :: n, k, iter, j, max_iter
real(kind=dp), parameter :: delta = 1.0e-6_dp
real(kind=dp) :: step, sse0, sse1
real(kind=dp), allocatable :: resid(:), grad(:), theta_try(:)

n = size(x)
k = size(theta)
if (k <= 0) return
allocate (resid(n), grad(k), theta_try(k))
if (present(n_iter)) then
   max_iter = n_iter
else
   max_iter = 20
end if
if (max_iter < 1) max_iter = 1
step = 1.0e-2_dp
do iter = 1, max_iter
   call ma_resid(x, theta, resid)
   sse0 = sum(resid(k + 1:n)**2)
   do j = 1, k
      theta_try = theta
      theta_try(j) = theta_try(j) + delta
      call ma_resid(x, theta_try, resid)
      sse1 = sum(resid(k + 1:n)**2)
      grad(j) = (sse1 - sse0) / delta
   end do
   theta_try = theta - step * grad
   call ma_resid(x, theta_try, resid)
   sse1 = sum(resid(k + 1:n)**2)
   if (sse1 < sse0 .and. sse1 == sse1) then
      theta = theta_try
   else
      step = step * 0.5_dp
      if (step < 1.0e-6_dp) exit
   end if
end do
deallocate (resid, grad, theta_try)
end subroutine ma_refine

function resample(x, n, replace) result(y)
! resample elements of x with (default) or without replacement
real(kind=dp), intent(in) :: x(:)
integer, intent(in), optional :: n
logical, intent(in), optional :: replace
real(kind=dp), allocatable :: y(:)
integer, allocatable :: idx(:)
integer :: n0, ny, i, j, tmp
real(kind=dp) :: u
logical :: repl

n0 = size(x)
if (present(n)) then
   ny = n
else
   ny = n0
end if
if (ny < 0 .or. n0 < 0) then
   allocate (y(0))
   return
end if
if (present(replace)) then
   repl = replace
else
   repl = .true.
end if
if (.not. repl .and. ny > n0) then
   allocate (y(0))
   return
end if
allocate (y(ny))
if (ny == 0) return
if (repl) then
   do i = 1, ny
      call random_number(u)
      j = 1 + int(u * n0)
      if (j < 1) j = 1
      if (j > n0) j = n0
      y(i) = x(j)
   end do
else
   allocate (idx(n0))
   do i = 1, n0
      idx(i) = i
   end do
   do i = n0, 2, -1
      call random_number(u)
      j = 1 + int(u * i)
      if (j < 1) j = 1
      if (j > i) j = i
      tmp = idx(i)
      idx(i) = idx(j)
      idx(j) = tmp
   end do
   do i = 1, ny
      y(i) = x(idx(i))
   end do
end if
end function resample

subroutine regress(y, x, intcp)
! simple linear regression y = a*x + b with diagnostics
real(kind=dp), intent(in) :: y(:), x(:)
logical, intent(in), optional :: intcp
real(kind=dp) :: a, b
real(kind=dp) :: x_mean, y_mean, sxx, sxy, sst, sse, mse, r2, se_a, se_b
real(kind=dp) :: t_a, t_b, p_a, p_b, f_stat, adj_r2, aic, bic
integer :: n, df, k
character(len=10) :: lbl
logical :: use_intcp

n = size(x)
if (n /= size(y) .or. n < 2) then
   print *, "Error: regress() requires equal-size arrays with size > 1"
   return
end if
x_mean = mean(x)
y_mean = mean(y)
sxx = sum((x - x_mean)**2)
if (sxx == 0.0_dp) then
   print *, "Error: regress() requires non-constant x"
   return
end if
if (present(intcp)) then
   use_intcp = intcp
else
   use_intcp = .true.
end if
if (use_intcp) then
   sxy = sum((x - x_mean) * (y - y_mean))
   a = sxy / sxx
   b = y_mean - a * x_mean
else
   sxy = sum(x * y)
   a = sxy / sum(x**2)
   b = 0.0_dp
end if
sst = sum((y - y_mean)**2)
sse = sum((y - (a * x + b))**2)
if (use_intcp) then
   df = n - 2
else
   df = n - 1
   sst = sum(y**2)
end if
if (df > 0) then
   mse = sse / df
else
   mse = 0.0_dp
end if
if (sst > 0.0_dp) then
   r2 = 1.0_dp - sse / sst
else
   r2 = 0.0_dp
end if
if (df > 0) then
   se_a = sqrt(mse / sxx)
   se_b = sqrt(mse * (1.0_dp / n + x_mean**2 / sxx))
else
   se_a = 0.0_dp
   se_b = 0.0_dp
end if
if (se_a > 0.0_dp) then
   t_a = a / se_a
   p_a = 2.0_dp * (1.0_dp - tcdf(abs(t_a), df))
else
   t_a = 0.0_dp
   p_a = 0.0_dp
end if
if (se_b > 0.0_dp) then
   t_b = b / se_b
   p_b = 2.0_dp * (1.0_dp - tcdf(abs(t_b), df))
else
   t_b = 0.0_dp
   p_b = 0.0_dp
end if
if (df > 0) then
   f_stat = ((sst - sse) / 1.0_dp) / (sse / df)
else
   f_stat = 0.0_dp
end if
if (df > 0) then
   adj_r2 = 1.0_dp - (1.0_dp - r2) * real(n - 1, dp) / real(df, dp)
else
   adj_r2 = 0.0_dp
end if
if (use_intcp) then
   k = 2
else
   k = 1
end if
if (n > 0 .and. sse > 0.0_dp) then
   aic = real(n, dp) * log(sse / real(n, dp)) + 2.0_dp * real(k, dp)
   bic = real(n, dp) * log(sse / real(n, dp)) + log(real(n, dp)) * real(k, dp)
else
   aic = 0.0_dp
   bic = 0.0_dp
end if
print "(a12,a12,a12,a12,a12,a12,a12,a12)", "n", "r2", "adj_r2", "aic", "bic", "sse", "mse", "f"
print "(i12,7f12.6)", n, r2, adj_r2, aic, bic, sse, mse, f_stat
print *
print "(a10,4a13)", "coef", "estimate", "std_err", "t", "p"
lbl = "slope"
print "(a10,4f13.6)", lbl, a, se_a, t_a, p_a
if (use_intcp) then
   lbl = "intcp"
   print "(a10,4f13.6)", lbl, b, se_b, t_b, p_b
end if
end subroutine regress

subroutine regress_multi(y, x, labels, intcp)
! multiple linear regression y = b0 + b1*x1 + ...
real(kind=dp), intent(in) :: y(:)
real(kind=dp), intent(in) :: x(:,:)
character(len=*), intent(in) :: labels(:)
logical, intent(in), optional :: intcp
real(kind=dp), allocatable :: z(:,:), xtx(:,:), xty(:), beta(:), yhat(:), v(:)
real(kind=dp) :: sst, sse, mse, r2, f_stat, adj_r2, aic, bic
real(kind=dp), allocatable :: se(:), tval(:), pval(:)
integer :: n, p, i, df, k
logical :: ok
logical :: use_intcp

n = size(y)
p = size(x, 2)
if (size(x, 1) /= n .or. n < 2) then
   print *, "Error: regress() requires equal-size arrays with size > 1"
   return
end if
if (p < 1) then
   print *, "Error: regress() needs at least one predictor"
   return
end if

if (present(intcp)) then
   use_intcp = intcp
else
   use_intcp = .true.
end if
if (use_intcp) then
   allocate (z(n, p + 1))
   z(:, 1) = 1.0_dp
   z(:, 2:) = x
else
   allocate (z(n, p))
   z(:, :) = x
end if

xtx = matmul(transpose(z), z)
xty = matmul(transpose(z), y)
call solve_linear(xtx, xty, beta, ok)
if (.not. ok) then
   print *, "Error: regress() singular design matrix"
   return
end if

sse = sum((y - matmul(z, beta))**2)
if (use_intcp) then
   yhat = matmul(z, beta)
   sst = sum((y - mean(y))**2)
else
   yhat = matmul(z, beta)
   sst = sum(y**2)
end if
if (use_intcp) then
   df = n - (p + 1)
else
   df = n - p
end if
if (df > 0) then
   mse = sse / df
else
   mse = 0.0_dp
end if
if (sst > 0.0_dp) then
   r2 = 1.0_dp - sse / sst
else
   r2 = 0.0_dp
end if
if (df > 0) then
   f_stat = ((sst - sse) / real(p, dp)) / (sse / df)
else
   f_stat = 0.0_dp
end if

if (use_intcp) then
   allocate (se(p + 1), tval(p + 1), pval(p + 1))
   do i = 1, p + 1
      call solve_linear(xtx, unit_vec(p + 1, i), v, ok)
      if (.not. ok .or. df <= 0) then
         se(i) = 0.0_dp
         tval(i) = 0.0_dp
         pval(i) = 0.0_dp
      else
         se(i) = sqrt(max(0.0_dp, mse * v(i)))
         if (se(i) > 0.0_dp) then
            tval(i) = beta(i) / se(i)
            pval(i) = 2.0_dp * (1.0_dp - tcdf(abs(tval(i)), df))
         else
            tval(i) = 0.0_dp
            pval(i) = 0.0_dp
         end if
      end if
   end do
else
   allocate (se(p), tval(p), pval(p))
   do i = 1, p
      call solve_linear(xtx, unit_vec(p, i), v, ok)
      if (.not. ok .or. df <= 0) then
         se(i) = 0.0_dp
         tval(i) = 0.0_dp
         pval(i) = 0.0_dp
      else
         se(i) = sqrt(max(0.0_dp, mse * v(i)))
         if (se(i) > 0.0_dp) then
            tval(i) = beta(i) / se(i)
            pval(i) = 2.0_dp * (1.0_dp - tcdf(abs(tval(i)), df))
         else
            tval(i) = 0.0_dp
            pval(i) = 0.0_dp
         end if
      end if
   end do
end if

if (df > 0) then
   adj_r2 = 1.0_dp - (1.0_dp - r2) * real(n - 1, dp) / real(df, dp)
else
   adj_r2 = 0.0_dp
end if
if (use_intcp) then
   k = p + 1
else
   k = p
end if
if (n > 0 .and. sse > 0.0_dp) then
   aic = real(n, dp) * log(sse / real(n, dp)) + 2.0_dp * real(k, dp)
   bic = real(n, dp) * log(sse / real(n, dp)) + log(real(n, dp)) * real(k, dp)
else
   aic = 0.0_dp
   bic = 0.0_dp
end if
print "(a12,a12,a12,a12,a12,a12,a12,a12)", "n", "r2", "adj_r2", "aic", "bic", "sse", "mse", "f"
print "(i12,7f12.6)", n, r2, adj_r2, aic, bic, sse, mse, f_stat
print *
print "(a10,4a13)", "coef", "estimate", "std_err", "t", "p"
if (use_intcp) then
   print "(a10,4f13.6)", "intcp", beta(1), se(1), tval(1), pval(1)
   do i = 1, p
      print "(a10,4f13.6)", trim(labels(i)), beta(i + 1), se(i + 1), tval(i + 1), pval(i + 1)
   end do
else
   do i = 1, p
      print "(a10,4f13.6)", trim(labels(i)), beta(i), se(i), tval(i), pval(i)
   end do
end if
end subroutine regress_multi

subroutine arfit(x, k1, k2, nacf, nlb)
! fit AR models and report RMSE/AIC/BIC and coefficients
real(kind=dp), intent(in) :: x(:)
integer, intent(in) :: k1
integer, intent(in), optional :: k2
integer, intent(in), optional :: nacf
integer, intent(in), optional :: nlb
integer :: n, k, k_start, k_end, n_eff, j, best_aic_k, best_bic_k, k_params
real(kind=dp) :: aic, bic, best_aic, best_bic, sse, sigma2
real(kind=dp), allocatable :: y(:), xmat(:,:), beta(:), xtx(:,:), xty(:)
real(kind=dp), allocatable :: aicv(:), bicv(:), rmsev(:), coeffs(:,:)
logical, allocatable :: okv(:)
logical :: ok
character(len=18) :: s_rmse, s_aic, s_bic, s_q, s_p
integer :: acf_lags, lb_lags, df_lb
real(kind=dp) :: qstat, pval
real(kind=dp), allocatable :: resid(:), resacf(:)

n = size(x)
if (n < 2) then
   print *, "Error: arfit() requires size(x) > 1"
   return
end if
if (present(k2)) then
   k_start = k1
   k_end = k2
else
   k_start = k1
   k_end = k1
end if
if (k_start < 0 .or. k_end < k_start) then
   print *, "Error: invalid lag order range in arfit()"
   return
end if
if (k_end >= n) then
   print *, "Error: lag order must be < size(x)"
   return
end if
acf_lags = 0
if (present(nacf)) acf_lags = nacf
lb_lags = 0
if (present(nlb)) lb_lags = nlb
if (acf_lags < 0) then
   print *, "Error: acf lag count must be >= 0"
   return
end if
if (lb_lags < 0) then
   print *, "Error: lb lag count must be >= 0"
   return
end if

best_aic = huge(1.0_dp)
best_bic = huge(1.0_dp)
best_aic_k = k_start
best_bic_k = k_start

allocate (aicv(0:k_end), bicv(0:k_end), rmsev(0:k_end), &
          coeffs(0:k_end, 1:max(1, k_end)), okv(0:k_end))
aicv = 0.0_dp; bicv = 0.0_dp; rmsev = 0.0_dp; coeffs = 0.0_dp; okv = .false.

do k = k_start, k_end
   if (k == 0) then
      n_eff = n
      allocate (y(n_eff))
      y = x
      sse = sum((y - mean(y))**2)
   else
      n_eff = n - k
      allocate (y(n_eff), xmat(n_eff, k))
      y = x(k + 1:n)
      do j = 1, k
         xmat(:, j) = x(k + 1 - j:n - j)
      end do

      xtx = matmul(transpose(xmat), xmat)
      xty = matmul(transpose(xmat), y)
      call solve_linear(xtx, xty, beta, ok)
      if (.not. ok) then
         okv(k) = .false.
         deallocate (y, xmat)
         cycle
      end if
      sse = sum((y - matmul(xmat, beta))**2)
   end if
   if (n_eff > 0 .and. sse > 0.0_dp) then
      sigma2 = sse / real(n_eff, dp)
      if (k == 0) then
         k_params = 1
      else
         k_params = k
      end if
      aic = real(n_eff, dp) * log(sigma2) + 2.0_dp * real(k_params, dp)
      bic = real(n_eff, dp) * log(sigma2) + log(real(n_eff, dp)) * real(k_params, dp)
   else
      aic = huge(1.0_dp)
      bic = huge(1.0_dp)
   end if

   okv(k) = .true.
   aicv(k) = aic
   bicv(k) = bic
   if (n_eff > 0) then
      rmsev(k) = sqrt(sse / real(n_eff, dp))
   else
      rmsev(k) = 0.0_dp
   end if
   if (k > 0) then
      do j = 1, k
         coeffs(k, j) = beta(j)
      end do
   end if
   if (aic < best_aic) then
      best_aic = aic
      best_aic_k = k
   end if
   if (bic < best_bic) then
      best_bic = bic
      best_bic_k = k
   end if
   if (k == 0) then
      deallocate (y)
   else
      deallocate (y, xmat)
   end if
end do

print "(a6,a18,a18,a18)", "lag", "RMSE", "AIC", "BIC"
do k = k_start, k_end
   if (.not. okv(k)) then
      print "(i6,3a18)", k, "NaN", "NaN", "NaN"
   else
      write (s_rmse, "(g18.6)") rmsev(k)
      write (s_aic, "(g18.6)") aicv(k)
      write (s_bic, "(g18.6)") bicv(k)
      print "(i6,a18,a18,a18)", k, s_rmse, s_aic, s_bic
   end if
end do

print *
print "(a6)", "lag"
write (*, "(6x)", advance="no")
do j = 1, k_end
   write (s_rmse, "(a,i0)") "AR", j
   write (*, "(1x,a12)", advance="no") trim(s_rmse)
end do
print *
do k = k_start, k_end
   if (.not. okv(k)) then
      print "(i6,1x,a)", k, "NaN"
   else
      write (*, "(i6)", advance="no") k
      do j = 1, k_end
         if (j <= k) then
            write (*, "(1x,f12.6)", advance="no") coeffs(k, j)
         else
            write (*, "(1x,a12)", advance="no") ""
         end if
      end do
      print *
   end if
end do

if (k_end > k_start) then
   print *
   print "(a,i0)", "AIC chooses lag ", best_aic_k
   print "(a,i0)", "BIC chooses lag ", best_bic_k
end if

if (acf_lags > 0) then
   print *
   print "(a)", "resid acf"
   print "(a6)", "lag"
   write (*, "(6x)", advance="no")
   do j = 1, acf_lags
      write (s_rmse, "(a,i0)") "AC", j
      write (*, "(1x,a12)", advance="no") trim(s_rmse)
   end do
   print *
   do k = k_start, k_end
      if (.not. okv(k)) then
         print "(i6,1x,a)", k, "NaN"
      else
         if (k == 0) then
            allocate (resid(n))
            resid = x - mean(x)
         else
            n_eff = n - k
            allocate (y(n_eff), xmat(n_eff, k), resid(n_eff))
            y = x(k + 1:n)
            do j = 1, k
               xmat(:, j) = x(k + 1 - j:n - j)
            end do
            resid = y - matmul(xmat, coeffs(k, 1:k))
            deallocate (y, xmat)
         end if
         resacf = acf(resid, acf_lags)
         write (*, "(i6)", advance="no") k
         do j = 1, acf_lags
            write (*, "(1x,f12.6)", advance="no") resacf(j)
         end do
         print *
         deallocate (resid, resacf)
      end if
   end do
end if

if (lb_lags > 0) then
   print *
   print "(a)", "ljung-box"
   print "(a6,a18,a8,a18)", "lag", "Q", "df", "p"
   do k = k_start, k_end
      if (.not. okv(k)) then
         print "(i6,3a18)", k, "NaN", "NaN", "NaN"
      else
         if (k == 0) then
            allocate (resid(n))
            resid = x - mean(x)
            n_eff = n
         else
            n_eff = n - k
            allocate (y(n_eff), xmat(n_eff, k), resid(n_eff))
            y = x(k + 1:n)
            do j = 1, k
               xmat(:, j) = x(k + 1 - j:n - j)
            end do
            resid = y - matmul(xmat, coeffs(k, 1:k))
            deallocate (y, xmat)
         end if
         resacf = acf(resid, lb_lags)
         qstat = 0.0_dp
         do j = 1, lb_lags
            qstat = qstat + resacf(j) * resacf(j) / real(n_eff - j, dp)
         end do
         qstat = real(n_eff, dp) * (real(n_eff, dp) + 2.0_dp) * qstat
         df_lb = lb_lags - k
         if (df_lb < 1) df_lb = 1
         pval = 1.0_dp - chisq_cdf(qstat, df_lb)
         write (s_q, "(g18.6)") qstat
         write (s_p, "(g18.6)") pval
         print "(i6,a18,i8,a18)", k, s_q, df_lb, s_p
         deallocate (resid, resacf)
      end if
   end do
end if
end subroutine arfit

subroutine mafit(x, k1, k2, nacf, nlb, niter)
! fit MA models and report RMSE/AIC/BIC and coefficients
real(kind=dp), intent(in) :: x(:)
integer, intent(in) :: k1
integer, intent(in), optional :: k2
integer, intent(in), optional :: nacf
integer, intent(in), optional :: nlb
integer, intent(in), optional :: niter
integer :: n, k, k_start, k_end, n_eff, j, best_aic_k, best_bic_k, k_params
integer :: iter, n_iter, df_lb
real(kind=dp) :: aic, bic, best_aic, best_bic, sse, sigma2
real(kind=dp) :: qstat, pval
real(kind=dp), allocatable :: y(:), xmat(:,:), beta(:), xtx(:,:), xty(:)
real(kind=dp), allocatable :: aicv(:), bicv(:), rmsev(:), coeffs(:,:)
real(kind=dp), allocatable :: eps(:), resid(:), resacf(:)
logical, allocatable :: okv(:)
logical :: ok
character(len=18) :: s_rmse, s_aic, s_bic, s_q, s_p
integer :: acf_lags, lb_lags

n = size(x)
if (n < 2) then
   print *, "Error: mafit() requires size(x) > 1"
   return
end if
if (present(k2)) then
   k_start = k1
   k_end = k2
else
   k_start = k1
   k_end = k1
end if
if (k_start < 0 .or. k_end < k_start) then
   print *, "Error: invalid lag order range in mafit()"
   return
end if
if (k_end >= n) then
   print *, "Error: lag order must be < size(x)"
   return
end if
acf_lags = 0
if (present(nacf)) acf_lags = nacf
lb_lags = 0
if (present(nlb)) lb_lags = nlb
if (acf_lags < 0) then
   print *, "Error: acf lag count must be >= 0"
   return
end if
if (lb_lags < 0) then
   print *, "Error: lb lag count must be >= 0"
   return
end if

best_aic = huge(1.0_dp)
best_bic = huge(1.0_dp)
best_aic_k = k_start
best_bic_k = k_start

allocate (aicv(0:k_end), bicv(0:k_end), rmsev(0:k_end), &
          coeffs(0:k_end, 1:max(1, k_end)), okv(0:k_end))
aicv = 0.0_dp; bicv = 0.0_dp; rmsev = 0.0_dp; coeffs = 0.0_dp; okv = .false.

if (present(niter)) then
   n_iter = niter
else
   n_iter = 5
end if
if (n_iter < 1) n_iter = 1
do k = k_start, k_end
   if (k == 0) then
      n_eff = n
      sse = sum((x - mean(x))**2)
      okv(k) = .true.
   else
      n_eff = n - k
      allocate (eps(n))
      eps = x
      ok = .true.
      do iter = 1, n_iter
         allocate (y(n_eff), xmat(n_eff, k))
         y = x(k + 1:n)
         do j = 1, k
            xmat(:, j) = eps(k + 1 - j:n - j)
         end do
         xtx = matmul(transpose(xmat), xmat)
         xty = matmul(transpose(xmat), y)
         call solve_linear(xtx, xty, beta, ok)
         deallocate (y, xmat)
         if (.not. ok) exit
         eps(1:k) = x(1:k)
         do j = k + 1, n
            eps(j) = x(j) - sum(beta(1:k) * eps(j - 1:j - k:-1))
         end do
      end do
      if (.not. ok) then
         okv(k) = .false.
         deallocate (eps)
         cycle
      end if
      do j = 1, k
         coeffs(k, j) = beta(j)
      end do
      call ma_refine(x, coeffs(k, 1:k), n_iter)
      allocate (resid(n))
      call ma_resid(x, coeffs(k, 1:k), resid)
      sse = sum(resid(k + 1:n)**2)
      okv(k) = .true.
      deallocate (eps, resid)
   end if

   if (n_eff > 0 .and. sse > 0.0_dp) then
      sigma2 = sse / real(n_eff, dp)
      if (k == 0) then
         k_params = 1
      else
         k_params = k
      end if
      aic = real(n_eff, dp) * log(sigma2) + 2.0_dp * real(k_params, dp)
      bic = real(n_eff, dp) * log(sigma2) + log(real(n_eff, dp)) * real(k_params, dp)
   else
      aic = huge(1.0_dp)
      bic = huge(1.0_dp)
   end if
   aicv(k) = aic
   bicv(k) = bic
   if (n_eff > 0) then
      rmsev(k) = sqrt(sse / real(n_eff, dp))
   else
      rmsev(k) = 0.0_dp
   end if
   if (aic < best_aic) then
      best_aic = aic
      best_aic_k = k
   end if
   if (bic < best_bic) then
      best_bic = bic
      best_bic_k = k
   end if
end do

print "(a6,a18,a18,a18)", "lag", "RMSE", "AIC", "BIC"
do k = k_start, k_end
   if (.not. okv(k)) then
      print "(i6,3a18)", k, "NaN", "NaN", "NaN"
   else
      write (s_rmse, "(f18.6)") rmsev(k)
      write (s_aic, "(f18.6)") aicv(k)
      write (s_bic, "(f18.6)") bicv(k)
      print "(i6,a18,a18,a18)", k, s_rmse, s_aic, s_bic
   end if
end do

print *
print "(a6)", "lag"
write (*, "(6x)", advance="no")
do j = 1, k_end
   write (s_rmse, "(a,i0)") "MA", j
   write (*, "(1x,a12)", advance="no") trim(s_rmse)
end do
print *
do k = k_start, k_end
   if (.not. okv(k)) then
      print "(i6,1x,a)", k, "NaN"
   else
      write (*, "(i6)", advance="no") k
      do j = 1, k_end
         if (j <= k) then
            write (*, "(1x,f12.6)", advance="no") coeffs(k, j)
         else
            write (*, "(1x,a12)", advance="no") ""
         end if
      end do
      print *
   end if
end do

if (k_end > k_start) then
   print *
   print "(a,i0)", "AIC chooses lag ", best_aic_k
   print "(a,i0)", "BIC chooses lag ", best_bic_k
end if

if (acf_lags > 0) then
   print *
   print "(a)", "resid acf"
   print "(a6)", "lag"
   write (*, "(6x)", advance="no")
   do j = 1, acf_lags
      write (s_rmse, "(a,i0)") "AC", j
      write (*, "(1x,a12)", advance="no") trim(s_rmse)
   end do
   print *
   do k = k_start, k_end
      if (.not. okv(k)) then
         print "(i6,1x,a)", k, "NaN"
      else
         if (k == 0) then
            allocate (resid(n))
            resid = x - mean(x)
            resacf = acf(resid, acf_lags)
         else
            n_eff = n - k
            allocate (resid(n))
            call ma_resid(x, coeffs(k, 1:k), resid)
            resacf = acf(resid(k + 1:n), acf_lags)
         end if
         write (*, "(i6)", advance="no") k
         do j = 1, acf_lags
            write (*, "(1x,f12.6)", advance="no") resacf(j)
         end do
         print *
         deallocate (resid, resacf)
      end if
   end do
end if

if (lb_lags > 0) then
   print *
   print "(a)", "ljung-box"
   print "(a6,a18,a8,a18)", "lag", "Q", "df", "p"
   do k = k_start, k_end
      if (.not. okv(k)) then
         print "(i6,3a18)", k, "NaN", "NaN", "NaN"
      else
         if (k == 0) then
            allocate (resid(n))
            resid = x - mean(x)
            n_eff = n
            resacf = acf(resid, lb_lags)
         else
            n_eff = n - k
            allocate (resid(n))
            call ma_resid(x, coeffs(k, 1:k), resid)
            resacf = acf(resid(k + 1:n), lb_lags)
         end if
         qstat = 0.0_dp
         do j = 1, lb_lags
            qstat = qstat + resacf(j) * resacf(j) / real(n_eff - j, dp)
         end do
         qstat = real(n_eff, dp) * (real(n_eff, dp) + 2.0_dp) * qstat
         df_lb = lb_lags - k
         if (df_lb < 1) df_lb = 1
         pval = 1.0_dp - chisq_cdf(qstat, df_lb)
         write (s_q, "(f18.6)") qstat
         write (s_p, "(f18.6)") pval
         print "(i6,a18,i8,a18)", k, s_q, df_lb, s_p
         deallocate (resid, resacf)
      end if
   end do
end if
end subroutine mafit

pure function tcdf(t, df) result(p)
! Student t CDF using incomplete beta
real(kind=dp), intent(in) :: t
integer, intent(in) :: df
real(kind=dp) :: p
real(kind=dp) :: x, a, b

if (df <= 0) then
   p = 0.0_dp
   return
end if
if (t == 0.0_dp) then
   p = 0.5_dp
   return
end if
a = 0.5_dp * df
b = 0.5_dp
x = df / (df + t * t)
if (t > 0.0_dp) then
   p = 1.0_dp - 0.5_dp * betai(a, b, x)
else
   p = 0.5_dp * betai(a, b, x)
end if
end function tcdf

pure function betai(a, b, x) result(bt)
! Regularized incomplete beta function.
real(kind=dp), intent(in) :: a, b, x
real(kind=dp) :: bt, front
if (x <= 0.0_dp) then
   bt = 0.0_dp
   return
else if (x >= 1.0_dp) then
   bt = 1.0_dp
   return
end if
front = exp(log_gamma(a + b) - log_gamma(a) - log_gamma(b) + a * log(x) + b * log(1.0_dp - x))
if (x < (a + 1.0_dp) / (a + b + 2.0_dp)) then
   bt = front * betacf(a, b, x) / a
else
   bt = 1.0_dp - front * betacf(b, a, 1.0_dp - x) / b
end if
end function betai

pure function chisq_cdf(x, df) result(p)
! Chi-square CDF.
real(kind=dp), intent(in) :: x
integer, intent(in) :: df
real(kind=dp) :: p
if (x <= 0.0_dp) then
   p = 0.0_dp
else
   p = gammp(0.5_dp * real(df, dp), 0.5_dp * x)
end if
end function chisq_cdf

pure function gammp(a, x) result(gp)
! Regularized lower incomplete gamma.
real(kind=dp), intent(in) :: a, x
real(kind=dp) :: gp
real(kind=dp) :: gln
if (x < 0.0_dp .or. a <= 0.0_dp) then
   gp = 0.0_dp
   return
end if
gln = log_gamma(a)
if (x < a + 1.0_dp) then
   gp = gser(a, x, gln)
else
   gp = 1.0_dp - gcf(a, x, gln)
end if
end function gammp

pure function gser(a, x, gln) result(gser_out)
! Series for incomplete gamma.
real(kind=dp), intent(in) :: a, x, gln
real(kind=dp) :: gser_out
integer, parameter :: itmax = 1000
real(kind=dp), parameter :: eps = 1.0e-12_dp
integer :: n
real(kind=dp) :: ap, del, sum

if (x <= 0.0_dp) then
   gser_out = 0.0_dp
   return
end if
ap = a
del = 1.0_dp / a
sum = del
do n = 1, itmax
   ap = ap + 1.0_dp
   del = del * x / ap
   sum = sum + del
   if (abs(del) < abs(sum) * eps) exit
end do
gser_out = sum * exp(-x + a * log(x) - gln)
end function gser

pure function gcf(a, x, gln) result(gcf_out)
! Continued fraction for incomplete gamma.
real(kind=dp), intent(in) :: a, x, gln
real(kind=dp) :: gcf_out
integer, parameter :: itmax = 1000
real(kind=dp), parameter :: eps = 1.0e-12_dp
real(kind=dp), parameter :: fpmin = 1.0e-30_dp
integer :: i
real(kind=dp) :: an, b, c, d, del, h

b = x + 1.0_dp - a
c = 1.0_dp / fpmin
d = 1.0_dp / b
h = d
do i = 1, itmax
   an = -real(i, dp) * (real(i, dp) - a)
   b = b + 2.0_dp
   d = an * d + b
   if (abs(d) < fpmin) d = fpmin
   c = b + an / c
   if (abs(c) < fpmin) c = fpmin
   d = 1.0_dp / d
   del = d * c
   h = h * del
   if (abs(del - 1.0_dp) < eps) exit
end do
gcf_out = exp(-x + a * log(x) - gln) * h
end function gcf

pure function betacf(a, b, x) result(cf)
! Continued fraction for incomplete beta.
real(kind=dp), intent(in) :: a, b, x
real(kind=dp) :: cf
integer, parameter :: maxit = 200
real(kind=dp), parameter :: eps = 3.0e-12_dp, fpmin = 1.0e-30_dp
integer :: m, m2
real(kind=dp) :: aa, c, d, del, h, qab, qap, qam

qab = a + b
qap = a + 1.0_dp
qam = a - 1.0_dp
c = 1.0_dp
d = 1.0_dp - qab * x / qap
if (abs(d) < fpmin) d = fpmin
d = 1.0_dp / d
h = d
do m = 1, maxit
   m2 = 2 * m
   aa = m * (b - m) * x / ((qam + m2) * (a + m2))
   d = 1.0_dp + aa * d
   if (abs(d) < fpmin) d = fpmin
   c = 1.0_dp + aa / c
   if (abs(c) < fpmin) c = fpmin
   d = 1.0_dp / d
   h = h * d * c
   aa = -(a + m) * (qab + m) * x / ((a + m2) * (qap + m2))
   d = 1.0_dp + aa * d
   if (abs(d) < fpmin) d = fpmin
   c = 1.0_dp + aa / c
   if (abs(c) < fpmin) c = fpmin
   d = 1.0_dp / d
   del = d * c
   h = h * del
   if (abs(del - 1.0_dp) <= eps) exit
end do
cf = h
end function betacf

pure subroutine solve_linear(a_in, b_in, x, ok)
! Compute solve linear.
real(kind=dp), intent(in) :: a_in(:,:), b_in(:)
real(kind=dp), allocatable, intent(out) :: x(:)
logical, intent(out) :: ok
real(kind=dp), allocatable :: a(:,:), b(:)
real(kind=dp) :: piv, tmp, factor, maxv
integer :: n, i, j, k, pivrow

n = size(b_in)
allocate (a(n, n), b(n), x(n))
a = a_in
b = b_in
ok = .true.

do k = 1, n
   pivrow = k
   maxv = abs(a(k, k))
   do i = k + 1, n
      if (abs(a(i, k)) > maxv) then
         maxv = abs(a(i, k))
         pivrow = i
      end if
   end do
   if (maxv == 0.0_dp) then
      ok = .false.
      return
   end if
   if (pivrow /= k) then
      a([k, pivrow], :) = a([pivrow, k], :)
      tmp = b(k)
      b(k) = b(pivrow)
      b(pivrow) = tmp
   end if
   piv = a(k, k)
   do i = k + 1, n
      factor = a(i, k) / piv
      a(i, k:n) = a(i, k:n) - factor * a(k, k:n)
      b(i) = b(i) - factor * b(k)
   end do
end do

do i = n, 1, -1
   tmp = b(i)
   do j = i + 1, n
      tmp = tmp - a(i, j) * x(j)
   end do
   if (a(i, i) == 0.0_dp) then
      ok = .false.
      return
   end if
   x(i) = tmp / a(i, i)
end do
end subroutine solve_linear

pure function unit_vec(n, idx) result(v)
! Compute unit vec.
integer, intent(in) :: n, idx
real(kind=dp) :: v(n)
v = 0.0_dp
if (idx >= 1 .and. idx <= n) v(idx) = 1.0_dp
end function unit_vec

pure function cumsum(x) result(y)
! return the cumulative sum of x
real(kind=dp), intent(in) :: x(:)
real(kind=dp), allocatable :: y(:)
integer :: i, n, ierr
n = size(x)
allocate (y(n), stat=ierr)
if (n < 1 .or. ierr /= 0) return
y(1) = x(1)
do i=2,n
   y(i) = y(i-1) + x(i)
end do
end function cumsum

pure function cummean(x) result(y)
! return the cumulative mean of x
real(kind=dp), intent(in) :: x(:)
real(kind=dp), allocatable :: y(:)
integer :: i, n, ierr
n = size(x)
allocate (y(n), stat=ierr)
if (n < 1 .or. ierr /= 0) return
y = cumsum(x)
do i=2,n
   y(i) = y(i)/i
end do
end function cummean

pure function cummin(x) result(y)
! return the cumulative minimum of x
real(kind=dp), intent(in) :: x(:)
real(kind=dp), allocatable :: y(:)
integer :: i, n, ierr
n = size(x)
allocate (y(n), stat=ierr)
if (n < 1 .or. ierr /= 0) return
y(1) = x(1)
do i=2,n
   y(i) = min(y(i-1), x(i))
end do
end function cummin

pure function cummax(x) result(y)
! return the cumulative maximum of x
real(kind=dp), intent(in) :: x(:)
real(kind=dp), allocatable :: y(:)
integer :: i, n, ierr
n = size(x)
allocate (y(n), stat=ierr)
if (n < 1 .or. ierr /= 0) return
y(1) = x(1)
do i=2,n
   y(i) = max(y(i-1), x(i))
end do
end function cummax

pure function cumprod(x) result(y)
! return the cumulative sum of x
real(kind=dp), intent(in) :: x(:)
real(kind=dp), allocatable :: y(:)
integer :: i, n, ierr
n = size(x)
allocate (y(n), stat=ierr)
if (n < 1 .or. ierr /= 0) return
y(1) = x(1)
do i=2,n
   y(i) = y(i-1) * x(i)
end do
end function cumprod

pure function diff(x) result(y)
! return the consecutive differences of x
real(kind=dp), intent(in) :: x(:)
real(kind=dp) :: y(size(x)-1)
integer :: n
n = size(x)
if (n < 2) return
y = x(2:) - x(:n-1)
end function diff

subroutine print_stats(x)
! Print summary statistics for array.
real(kind=dp), intent(in) :: x(:)
integer :: n, ierr
n = size(x)
print "(*(a10))", "size", "mean", "sd", "skew", "kurt", "min", "max", "first", "last"
if (n > 0) then
   write (*, "(i10, *(f10.4))", iostat=ierr) n, mean(x), sd(x), &
      skew(x), kurtosis(x), minval(x), maxval(x), x(1), x(n)
else
   print "(i10)", n
end if
end subroutine print_stats

pure function skew(x) result(skew_val)
! return the skewness of x
real(kind=dp), intent(in) :: x(:)
real(kind=dp) :: skew_val
real(kind=dp) :: mean_x, sd_x
integer :: n
n = size(x)
mean_x = mean(x)
sd_x = sd(x)
skew_val = sum(((x - mean_x) / sd_x)**3) / n
end function skew

pure function kurtosis(x) result(kurtosis_val)
! return the kurtosis of x
real(kind=dp), intent(in) :: x(:)
real(kind=dp) :: kurtosis_val
real(kind=dp) :: mean_x, sd_x
integer :: n
n = size(x)
mean_x = mean(x)
sd_x = sd(x)
kurtosis_val = sum(((x - mean_x) / sd_x)**4) / n - 3.0_dp
end function kurtosis

end module stats_mod
