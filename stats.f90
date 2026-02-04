module stats_mod
use kind_mod, only: dp
use constants_mod, only: pi
use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
use random_mod, only: random_normal, random_seed_init
use qsort_mod, only: median, quantile, sorted
implicit none
private
public :: mean, sd, cor, cov, cumsum, cumprod, diff, standardize, &
          print_stats, skew, kurtosis, cummin, cummax, cummean, &
          geomean, harmean, trimmean, winsor_mean, mad, iqr_scale, jb_test, ttest1, ttest2, ks2_test, kernelreg, kde, &
          acf, pacf, acfpacf, acfpacfar, fiacf, fracdiff, arcoef, arsim, masim, armasim, arfimasim, cpsim, cpfit, cpfitaic, cpfit_aic, resample, regress, regress_multi, poly1reg, splinereg, naturalspline, distaicscan, arfit, mafit, armafit, armafitgrid, armafitaic, arfimafit, aracf, maacf, arpacf, mapacf, &
          armaacf, arfimaacf, armapacf, mssk, mssk_exp, mssk_gamma, mssk_lnorm, mssk_t, mssk_nct, mssk_mixnorm, mssk_chisq, mssk_f, mssk_beta, mssk_logis, mssk_sech, mssk_laplace, &
          dunif, dexp, dgamma, dlnorm, dnorm, dmixnorm, dt, dnct, dchisq, df, dbeta, dlogis, dsech, dlaplace, dcauchy, dged, dhyperb, &
          punif, pexp, pgamma, plnorm, pnorm, pmixnorm, pt, pnct, pchisq, pf, pbeta, plogis, psech, plaplace, pcauchy, pged, phyperb, &
          qunif, qexp, qgamma, qlnorm, qnorm, qmixnorm, qt, qnct, qchisq, qf, qbeta, qlogis, qsech, qlaplace, qcauchy, qged, qhyperb, &
          rhyperb, fit_norm, fit_exp, fit_gamma, fit_lnorm, fit_t, fit_nct, fit_mixnorm, fit_mixnorm_aic, fix_mixnorm_aic, &
          fit_chisq, fit_f, fit_beta, fit_logis, fit_sech, fit_laplace, fit_cauchy, fit_ged, fit_hyperb

interface kernelreg
   module procedure kernelreg_scalar
   module procedure kernelreg_vec
   module procedure kernelreg_scalar_ordvec
   module procedure kernelreg_vec_ordvec
end interface kernelreg

interface splinereg
   module procedure splinereg_scalar
   module procedure splinereg_degvec
end interface splinereg

interface naturalspline
   module procedure naturalspline_scalar
   module procedure naturalspline_kvec
end interface naturalspline

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

pure logical function mixnorm_params_ok(wgt, mu, sig) result(ok)
! Validate finite normal-mixture parameters.
real(kind=dp), intent(in) :: wgt(:), mu(:), sig(:)
ok = .false.
if (size(wgt) < 1) return
if (size(mu) /= size(wgt) .or. size(sig) /= size(wgt)) return
if (any(wgt < 0.0_dp) .or. any(sig <= 0.0_dp)) return
if (sum(wgt) <= 0.0_dp) return
ok = .true.
end function mixnorm_params_ok

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

pure function trimmean(x, alpha) result(tm)
! Trimmed mean with symmetric trim fraction alpha in [0,0.5).
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in), optional :: alpha
real(kind=dp) :: tm
real(kind=dp), allocatable :: xs(:)
real(kind=dp) :: a
integer :: n, k, i1, i2
n = size(x)
if (n < 1) then
   tm = nanv(); return
end if
if (present(alpha)) then
   a = alpha
else
   a = 0.1_dp
end if
if (a < 0.0_dp .or. a >= 0.5_dp) then
   tm = nanv(); return
end if
xs = sorted(x)
k = int(floor(a * real(n, dp)))
i1 = 1 + k
i2 = n - k
if (i2 < i1) then
   tm = nanv(); return
end if
tm = sum(xs(i1:i2)) / real(i2 - i1 + 1, dp)
end function trimmean

pure function winsor_mean(x, alpha) result(wm)
! Winsorized mean with symmetric winsor fraction alpha in [0,0.5).
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in), optional :: alpha
real(kind=dp) :: wm
real(kind=dp), allocatable :: xs(:), y(:)
real(kind=dp) :: a
integer :: n, k
n = size(x)
if (n < 1) then
   wm = nanv(); return
end if
if (present(alpha)) then
   a = alpha
else
   a = 0.1_dp
end if
if (a < 0.0_dp .or. a >= 0.5_dp) then
   wm = nanv(); return
end if
xs = sorted(x)
y = xs
k = int(floor(a * real(n, dp)))
if (k > 0) then
   y(1:k) = xs(k + 1)
   y(n - k + 1:n) = xs(n - k)
end if
wm = mean(y)
end function winsor_mean

pure function mad(x, center) result(v)
! Median absolute deviation from median (or provided center).
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in), optional :: center
real(kind=dp) :: v
real(kind=dp), allocatable :: ad(:)
real(kind=dp) :: c
if (size(x) < 1) then
   v = nanv(); return
end if
if (present(center)) then
   c = center
else
   c = median(x)
end if
allocate (ad(size(x)))
ad = abs(x - c)
v = median(ad)
deallocate (ad)
end function mad

pure function iqr_scale(x) result(v)
! IQR-based robust scale estimate (IQR/1.349).
real(kind=dp), intent(in) :: x(:)
real(kind=dp) :: v
real(kind=dp), allocatable :: q(:)
if (size(x) < 1) then
   v = nanv(); return
end if
q = quantile(x, [0.25_dp, 0.75_dp])
v = (q(2) - q(1)) / 1.349_dp
end function iqr_scale

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

pure function arcoef(x, k) result(phi)
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
   pure subroutine unpack_params(u, phi_p, theta_q, d_out)
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

   pure function loglik(u) result(f)
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

pure function mssk_nct(df, ncp) result(v)
! Mean, standard deviation, skew and excess kurtosis of noncentral t distribution.
real(kind=dp), intent(in) :: df
real(kind=dp), intent(in), optional :: ncp
real(kind=dp) :: v(4)
real(kind=dp) :: delta
real(kind=dp) :: mu1r, mu2r, mu3r, mu4r
real(kind=dp) :: mu, var, sdv, cm3, cm4
real(kind=dp) :: g1, g2, g3, g4
real(kind=dp) :: log_half_df
if (present(ncp)) then
   delta = ncp
else
   delta = 0.0_dp
end if

if (df <= 0.0_dp) then
   v = nanv()
   return
end if

v = nanv()
log_half_df = log(0.5_dp * df)

if (df > 1.0_dp) then
   g1 = exp(0.5_dp * log_half_df + log_gamma(0.5_dp * (df - 1.0_dp)) - log_gamma(0.5_dp * df))
   mu1r = delta * g1
   v(1) = mu1r
else
   return
end if

if (df > 2.0_dp) then
   g2 = exp(log_half_df + log_gamma(0.5_dp * (df - 2.0_dp)) - log_gamma(0.5_dp * df))
   mu2r = (1.0_dp + delta*delta) * g2
   mu = v(1)
   var = mu2r - mu*mu
   if (var <= 0.0_dp) return
   sdv = sqrt(var)
   v(2) = sdv
else
   return
end if

if (df > 3.0_dp) then
   g3 = exp(1.5_dp * log_half_df + log_gamma(0.5_dp * (df - 3.0_dp)) - log_gamma(0.5_dp * df))
   mu3r = (3.0_dp*delta + delta**3) * g3
   cm3 = mu3r - 3.0_dp*mu*mu2r + 2.0_dp*mu**3
   v(3) = cm3 / (sdv**3)
else
   return
end if

if (df > 4.0_dp) then
   g4 = exp(2.0_dp * log_half_df + log_gamma(0.5_dp * (df - 4.0_dp)) - log_gamma(0.5_dp * df))
   mu4r = (3.0_dp + 6.0_dp*delta*delta + delta**4) * g4
   cm4 = mu4r - 4.0_dp*mu*mu3r + 6.0_dp*mu*mu*mu2r - 3.0_dp*mu**4
   v(4) = cm4/(var*var) - 3.0_dp
end if
end function mssk_nct

pure function mssk_mixnorm(wgt, mu, sig) result(v)
! Mean, standard deviation, skew and excess kurtosis of a finite normal mixture.
real(kind=dp), intent(in) :: wgt(:), mu(:), sig(:)
real(kind=dp) :: v(4)
real(kind=dp), allocatable :: wn(:)
real(kind=dp) :: sw, m1, m2, m3, m4, var, sdv, cm3, cm4
integer :: k
k = size(wgt)
if (.not. mixnorm_params_ok(wgt, mu, sig)) then
   v = nanv()
   return
end if
allocate (wn(k))
sw = sum(wgt)
wn = wgt / sw
m1 = sum(wn * mu)
m2 = sum(wn * (sig*sig + mu*mu))
m3 = sum(wn * (mu**3 + 3.0_dp*mu*(sig*sig)))
m4 = sum(wn * (mu**4 + 6.0_dp*(mu*mu)*(sig*sig) + 3.0_dp*sig**4))
var = m2 - m1*m1
if (var <= 0.0_dp) then
   v(1) = m1
   v(2) = 0.0_dp
   v(3) = nanv()
   v(4) = nanv()
   deallocate (wn)
   return
end if
sdv = sqrt(var)
cm3 = m3 - 3.0_dp*m1*m2 + 2.0_dp*m1**3
cm4 = m4 - 4.0_dp*m1*m3 + 6.0_dp*(m1*m1)*m2 - 3.0_dp*m1**4
v(1) = m1
v(2) = sdv
v(3) = cm3 / (sdv**3)
v(4) = cm4 / (var*var) - 3.0_dp
deallocate (wn)
end function mssk_mixnorm

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

function kde(x, ngrid) result(y)
! Gaussian-kernel density estimate; always plots estimate versus grid.
use plot_mod, only: gplot => plot
real(kind=dp), intent(in) :: x(:)
integer, intent(in), optional :: ngrid
real(kind=dp), allocatable :: y(:)
real(kind=dp), allocatable :: gx(:)
real(kind=dp) :: h, sx, xmin, xmax, z
integer :: n, m, i, j

n = size(x)
if (n < 2) then
   allocate (y(0))
   print *, "Error: kde() requires size(x) > 1"
   return
end if
if (present(ngrid)) then
   m = max(20, ngrid)
else
   m = 200
end if
sx = sd(x)
if (sx <= 0.0_dp) then
   allocate (y(0))
   print *, "Error: kde() requires non-constant x"
   return
end if
h = 1.06_dp * sx * real(n, dp)**(-0.2_dp)
h = max(h, 1.0e-8_dp)
xmin = minval(x) - 3.0_dp * h
xmax = maxval(x) + 3.0_dp * h
allocate (y(m), gx(m))
do i = 1, m
   gx(i) = xmin + (xmax - xmin) * real(i - 1, dp) / real(max(1, m - 1), dp)
   y(i) = 0.0_dp
   do j = 1, n
      z = (gx(i) - x(j)) / h
      y(i) = y(i) + exp(-0.5_dp * z * z)
   end do
   y(i) = y(i) / (real(n, dp) * h * sqrt(2.0_dp * pi))
end do
call gplot(gx, y, title="kde", xlabel="x")
deallocate (gx)
end function kde

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

pure function dmixnorm(x, wgt, mu, sig) result(y)
! Finite normal mixture density.
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in) :: wgt(:), mu(:), sig(:)
real(kind=dp) :: y(size(x))
real(kind=dp), allocatable :: wn(:)
real(kind=dp) :: sw
integer :: i, j, k
k = size(wgt)
if (.not. mixnorm_params_ok(wgt, mu, sig)) then
   y = nanv()
   return
end if
allocate (wn(k))
sw = sum(wgt)
wn = wgt / sw
do i = 1, size(x)
   y(i) = 0.0_dp
   do j = 1, k
      y(i) = y(i) + wn(j) * exp(-0.5_dp * ((x(i) - mu(j))/sig(j))**2) / (sig(j) * sqrt(2.0_dp*pi))
   end do
end do
deallocate (wn)
end function dmixnorm

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

pure function dnct(x, df, ncp) result(y)
! Noncentral Student t density.
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in) :: df, ncp
real(kind=dp) :: y(size(x))
integer :: i
if (df <= 0.0_dp) then
   y = nanv()
   return
end if
do i = 1, size(x)
   y(i) = nct_pdf_scalar(x(i), df, ncp)
end do
end function dnct

pure elemental function nct_pdf_scalar(x, df, ncp) result(val)
! Numerical integration over chi-square(df) mixing distribution.
real(kind=dp), intent(in) :: x, df, ncp
real(kind=dp) :: val
integer, parameter :: nseg = 240
integer :: j
real(kind=dp) :: plo(1), phi(1), qlo(1), qhi(1), vlo, vhi, h, vj, w, z, chi, lnchi
real(kind=dp), parameter :: eps = 1.0e-8_dp, invsqrt2pi = 0.39894228040143267794_dp

if (df <= 0.0_dp) then
   val = nanv()
   return
end if
plo(1) = eps
phi(1) = 1.0_dp - eps
qlo = qchisq(plo, df)
qhi = qchisq(phi, df)
vlo = max(0.0_dp, qlo(1))
vhi = qhi(1)
if (vhi <= vlo .or. .not. (vhi > 0.0_dp)) then
   val = 0.0_dp
   return
end if
h = (vhi - vlo) / real(nseg, dp)
val = 0.0_dp
do j = 0, nseg
   vj = vlo + h * real(j, dp)
   if (vj <= 0.0_dp) cycle
   lnchi = (0.5_dp * df - 1.0_dp) * log(vj) - 0.5_dp * vj - 0.5_dp * df * log(2.0_dp) - log_gamma(0.5_dp * df)
   chi = exp(lnchi)
   z = x * sqrt(vj / df) - ncp
   if (j == 0 .or. j == nseg) then
      w = 1.0_dp
   else if (mod(j, 2) == 1) then
      w = 4.0_dp
   else
      w = 2.0_dp
   end if
   val = val + w * (invsqrt2pi * exp(-0.5_dp * z * z) * sqrt(vj / df) * chi)
end do
val = max(0.0_dp, val * h / 3.0_dp)
end function nct_pdf_scalar

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

pure function pnct(x, df, ncp) result(y)
! Noncentral Student t CDF.
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in) :: df, ncp
real(kind=dp) :: y(size(x))
integer :: i
if (df <= 0.0_dp) then
   y = nanv()
   return
end if
do i = 1, size(x)
   y(i) = nct_cdf_scalar(x(i), df, ncp)
end do
end function pnct

pure elemental function nct_cdf_scalar(x, df, ncp) result(val)
! Numerical integration over chi-square(df) mixing distribution.
real(kind=dp), intent(in) :: x, df, ncp
real(kind=dp) :: val
integer, parameter :: nseg = 240
integer :: j
real(kind=dp) :: plo(1), phi(1), qlo(1), qhi(1), vlo, vhi, h, vj, w, z, chi, lnchi, cdfn
real(kind=dp), parameter :: eps = 1.0e-8_dp, invsqrt2 = 0.70710678118654752440_dp

if (df <= 0.0_dp) then
   val = nanv()
   return
end if
plo(1) = eps
phi(1) = 1.0_dp - eps
qlo = qchisq(plo, df)
qhi = qchisq(phi, df)
vlo = max(0.0_dp, qlo(1))
vhi = qhi(1)
if (vhi <= vlo .or. .not. (vhi > 0.0_dp)) then
   val = 0.5_dp
   return
end if
h = (vhi - vlo) / real(nseg, dp)
val = 0.0_dp
do j = 0, nseg
   vj = vlo + h * real(j, dp)
   if (vj <= 0.0_dp) cycle
   lnchi = (0.5_dp * df - 1.0_dp) * log(vj) - 0.5_dp * vj - 0.5_dp * df * log(2.0_dp) - log_gamma(0.5_dp * df)
   chi = exp(lnchi)
   z = x * sqrt(vj / df) - ncp
   cdfn = 0.5_dp * (1.0_dp + erf(z * invsqrt2))
   if (j == 0 .or. j == nseg) then
      w = 1.0_dp
   else if (mod(j, 2) == 1) then
      w = 4.0_dp
   else
      w = 2.0_dp
   end if
   val = val + w * cdfn * chi
end do
val = min(1.0_dp, max(0.0_dp, val * h / 3.0_dp))
end function nct_cdf_scalar

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

pure function pmixnorm(x, wgt, mu, sig) result(y)
! Finite normal mixture CDF.
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in) :: wgt(:), mu(:), sig(:)
real(kind=dp) :: y(size(x))
real(kind=dp), allocatable :: wn(:)
real(kind=dp) :: sw
integer :: i, j, k
k = size(wgt)
if (.not. mixnorm_params_ok(wgt, mu, sig)) then
   y = nanv()
   return
end if
allocate (wn(k))
sw = sum(wgt)
wn = wgt / sw
do i = 1, size(x)
   y(i) = 0.0_dp
   do j = 1, k
      y(i) = y(i) + wn(j) * 0.5_dp * (1.0_dp + erf((x(i) - mu(j)) / (sig(j) * sqrt(2.0_dp))))
   end do
   if (y(i) < 0.0_dp) y(i) = 0.0_dp
   if (y(i) > 1.0_dp) y(i) = 1.0_dp
end do
deallocate (wn)
end function pmixnorm

elemental function hyperb_pdf_scalar(x, loc, scale, alpha) result(y)
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

elemental function hyperb_int(t, loc, scale, alpha) result(area)
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

elemental function inv_norm(p) result(x)
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

pure function qnct(p, df, ncp) result(x)
! Noncentral Student t quantile.
real(kind=dp), intent(in) :: p(:)
real(kind=dp), intent(in) :: df, ncp
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
      lo = ncp - 10.0_dp
      hi = ncp + 10.0_dp
      do while (nct_cdf_scalar(hi, df, ncp) < p(i))
         lo = hi
         hi = hi + 10.0_dp
         if (hi > huge(1.0_dp) / 8.0_dp) exit
      end do
      do while (nct_cdf_scalar(lo, df, ncp) > p(i))
         hi = lo
         lo = lo - 10.0_dp
         if (lo < -huge(1.0_dp) / 8.0_dp) exit
      end do
      do it = 1, 70
         mid = 0.5_dp * (lo + hi)
         pm = nct_cdf_scalar(mid, df, ncp)
         if (pm < p(i)) then
            lo = mid
         else
            hi = mid
         end if
      end do
      x(i) = 0.5_dp * (lo + hi)
   end if
end do
end function qnct

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

pure function qmixnorm(p, wgt, mu, sig) result(x)
! Finite normal mixture quantile by bisection on pmixnorm.
real(kind=dp), intent(in) :: p(:)
real(kind=dp), intent(in) :: wgt(:), mu(:), sig(:)
real(kind=dp) :: x(size(p))
real(kind=dp), allocatable :: wn(:), cdfv(:)
real(kind=dp) :: sw, lo, hi, mid, mu_mix, sd_mix, mom4(4)
integer :: i, it, k
k = size(wgt)
if (.not. mixnorm_params_ok(wgt, mu, sig)) then
   x = nanv()
   return
end if
allocate (wn(k), cdfv(1))
sw = sum(wgt)
wn = wgt / sw
mu_mix = sum(wn * mu)
mom4 = mssk_mixnorm(wn, mu, sig)
sd_mix = max(1.0e-10_dp, mom4(2))
do i = 1, size(p)
   if (p(i) <= 0.0_dp) then
      x(i) = -huge(1.0_dp)
   else if (p(i) >= 1.0_dp) then
      x(i) = huge(1.0_dp)
   else
      lo = mu_mix - 10.0_dp * sd_mix
      hi = mu_mix + 10.0_dp * sd_mix
      cdfv = pmixnorm([lo], wn, mu, sig)
      do while (cdfv(1) > p(i))
         hi = lo
         lo = lo - 5.0_dp * sd_mix
         cdfv = pmixnorm([lo], wn, mu, sig)
         if (lo < -huge(1.0_dp) / 8.0_dp) exit
      end do
      cdfv = pmixnorm([hi], wn, mu, sig)
      do while (cdfv(1) < p(i))
         lo = hi
         hi = hi + 5.0_dp * sd_mix
         cdfv = pmixnorm([hi], wn, mu, sig)
         if (hi > huge(1.0_dp) / 8.0_dp) exit
      end do
      do it = 1, 80
         mid = 0.5_dp * (lo + hi)
         cdfv = pmixnorm([mid], wn, mu, sig)
         if (cdfv(1) < p(i)) then
            lo = mid
         else
            hi = mid
         end if
      end do
      x(i) = 0.5_dp * (lo + hi)
   end if
end do
deallocate (wn, cdfv)
end function qmixnorm

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

elemental function besseli0(x) result(y)
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

elemental function besseli1(x) result(y)
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

elemental function besselk0(x) result(y)
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

elemental function besselk1(x) result(y)
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

elemental function log1pexp(t) result(y)
! Stable log(1+exp(t)).
real(kind=dp), intent(in) :: t
real(kind=dp) :: y
if (t > 0.0_dp) then
   y = t + log(1.0_dp + exp(-t))
else
   y = log(1.0_dp + exp(t))
end if
end function log1pexp

elemental function log_beta(a, b) result(y)
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
   pure function loglik(u) result(f)
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
   pure function loglik(u) result(f)
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
   pure function loglik(u) result(f)
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

pure function fit_lnorm(x) result(pars)
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
   pure function loglik(u) result(f)
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

function fit_nct(x) result(pars)
! MLE for noncentral Student t distribution.
! Returns [df, ncp].
real(kind=dp), intent(in) :: x(:)
real(kind=dp) :: pars(2)
real(kind=dp) :: mu0, sd0, kex, df0, u0(2), ubest(2), tol
real(kind=dp), allocatable :: z(:)

if (size(x) < 2) then
   pars = nanv(); return
end if
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
df0 = max(1.1_dp, df0)
u0 = [mu0, log(df0)]
tol = 1.0e-6_dp
ubest = nelder_mead(loglik, u0, 0.2_dp, 500, tol)
pars(1) = exp(ubest(2))
pars(2) = ubest(1)
deallocate (z)

contains
   pure function loglik(u) result(f)
      real(kind=dp), intent(in) :: u(:)
      real(kind=dp) :: f, df, ncp
      real(kind=dp), allocatable :: fx(:)
      df = exp(u(2))
      ncp = u(1)
      if (df <= 0.0_dp) then
         f = -huge(1.0_dp)
         return
      end if
      fx = dnct(x, df, ncp)
      if (any(fx <= 0.0_dp) .or. any(fx /= fx)) then
         f = -huge(1.0_dp)
      else
         f = sum(log(fx))
      end if
   end function loglik
end function fit_nct

function fit_mixnorm(x, k, verbose) result(pars)
! EM fit for k-component finite normal mixture.
! Returns concatenated [wgt(1:k), mean(1:k), sd(1:k)].
real(kind=dp), intent(in) :: x(:)
integer, intent(in) :: k
logical, intent(in), optional :: verbose
real(kind=dp), allocatable :: pars(:)
real(kind=dp), allocatable :: w(:), mu(:), sig(:), nk(:), sumwx(:), varj(:)
real(kind=dp), allocatable :: resp(:,:), log_r(:), x_sorted(:)
real(kind=dp) :: xsd, xmin, xmax, ll, ll_old, denom, maxlog, sw, sig_floor
integer :: n, i, j, iter, m
logical :: swapped
logical :: do_verbose

n = size(x)
allocate (pars(0))
do_verbose = .false.
if (present(verbose)) do_verbose = verbose
if (k < 1 .or. n < max(2, k)) then
   return
end if

xsd = sd(x)
if (xsd <= 0.0_dp) then
   deallocate (pars)
   allocate (pars(3*k))
   pars = nanv()
   return
end if

allocate (w(k), mu(k), sig(k), nk(k), sumwx(k), varj(k), resp(n, k), log_r(k), x_sorted(n))
x_sorted = x
call sort_in_place(x_sorted)
xmin = x_sorted(1)
xmax = x_sorted(n)
if (xmax <= xmin) then
   deallocate (w, mu, sig, nk, sumwx, varj, resp, log_r, x_sorted)
   deallocate (pars)
   allocate (pars(3*k))
   pars = nanv()
   return
end if

w = 1.0_dp / real(k, dp)
do j = 1, k
   m = max(1, min(n, int((real(j, dp) - 0.5_dp) * real(n, dp) / real(k, dp))))
   mu(j) = x_sorted(m)
end do
sig = max(xsd / sqrt(real(k, dp)), 0.2_dp * xsd)
sig_floor = max(1.0e-6_dp * xsd, 1.0e-8_dp)
ll_old = -huge(1.0_dp)

do iter = 1, 300
   ll = 0.0_dp
   do i = 1, n
      do j = 1, k
         log_r(j) = log(max(w(j), 1.0e-300_dp)) - log(sig(j)) - 0.5_dp*log(2.0_dp*pi) - &
                    0.5_dp * ((x(i) - mu(j))/sig(j))**2
      end do
      maxlog = maxval(log_r)
      denom = sum(exp(log_r - maxlog))
      if (denom <= 0.0_dp) cycle
      resp(i, :) = exp(log_r - maxlog) / denom
      ll = ll + maxlog + log(denom)
   end do

   nk = sum(resp, dim=1)
   nk = max(nk, 1.0e-8_dp)
   w = nk / real(n, dp)
   sw = sum(w)
   if (sw > 0.0_dp) w = w / sw

   do j = 1, k
      sumwx(j) = sum(resp(:, j) * x)
      mu(j) = sumwx(j) / nk(j)
   end do
   do j = 1, k
      varj(j) = sum(resp(:, j) * (x - mu(j))**2) / nk(j)
      sig(j) = sqrt(max(varj(j), sig_floor*sig_floor))
   end do

   if (abs(ll - ll_old) < 1.0e-7_dp * (1.0_dp + abs(ll))) exit
   ll_old = ll
end do

! Sort components by descending weight for deterministic output.
do i = 1, k - 1
   swapped = .false.
   do j = 1, k - i
      if (w(j) < w(j + 1)) then
         call swap_vals(mu(j), mu(j + 1))
         call swap_vals(sig(j), sig(j + 1))
         call swap_vals(w(j), w(j + 1))
         swapped = .true.
      end if
   end do
   if (.not. swapped) exit
end do
w = w / sum(w)

deallocate (pars)
allocate (pars(3*k))
pars(1:k) = w
pars(k + 1:2*k) = mu
pars(2*k + 1:3*k) = sig

if (do_verbose) then
   print *
   print "(a8,2a14)", "weight", "mean", "sd"
   do j = 1, k
      print "(f8.4,2f14.6)", w(j), mu(j), sig(j)
   end do
end if

deallocate (w, mu, sig, nk, sumwx, varj, resp, log_r, x_sorted)

contains
   subroutine sort_in_place(a)
      real(kind=dp), intent(inout) :: a(:)
      integer :: ii, jj
      real(kind=dp) :: tmp
      do ii = 2, size(a)
         tmp = a(ii)
         jj = ii - 1
         do while (jj >= 1)
            if (a(jj) <= tmp) exit
            a(jj + 1) = a(jj)
            jj = jj - 1
         end do
         a(jj + 1) = tmp
      end do
   end subroutine sort_in_place

   subroutine swap_vals(a, b)
      real(kind=dp), intent(inout) :: a, b
      real(kind=dp) :: t
      t = a
      a = b
      b = t
   end subroutine swap_vals
end function fit_mixnorm

function fit_mixnorm_aic(x, kmin, kmax, nstart, verbose, plot) result(best_pars)
! AIC-based selection over k for finite normal mixture.
! Returns best concatenated [wgt(1:k), mean(1:k), sd(1:k)].
use plot_mod, only: gplot => plot
real(kind=dp), intent(in) :: x(:)
integer, intent(in) :: kmin, kmax
integer, intent(in), optional :: nstart
logical, intent(in), optional :: verbose, plot
real(kind=dp), allocatable :: best_pars(:)
real(kind=dp), allocatable :: pars(:), pars_k(:), fx(:), w(:), mu(:), sig(:), gx(:), gy(:)
real(kind=dp) :: ll, aic, best_aic, sdx, xmin, xmax
character(len=64) :: ttl
logical :: do_verbose, do_plot
integer :: k, ks, s, pcount, best_k, n, m

n = size(x)
allocate (best_pars(0))
if (n < 2 .or. kmin < 1 .or. kmax < kmin) return
if (present(nstart)) then
   ks = max(1, nstart)
else
   ks = 3
end if
do_verbose = .true.
if (present(verbose)) do_verbose = verbose
do_plot = .false.
if (present(plot)) do_plot = plot

best_aic = huge(1.0_dp)
best_k = -1
if (do_verbose) then
   print *
   print "(a6,a18,a18)", "k", "logLik", "AIC"
end if

do k = kmin, kmax
   ll = -huge(1.0_dp)
   if (allocated(pars_k)) deallocate (pars_k)
   allocate (pars_k(0))
   do s = 1, ks
      pars = fit_mixnorm(x, k)
      if (size(pars) /= 3*k) cycle
      w = pars(1:k)
      mu = pars(k + 1:2*k)
      sig = pars(2*k + 1:3*k)
      fx = dmixnorm(x, w, mu, sig)
      if (any(fx <= 0.0_dp) .or. any(fx /= fx)) cycle
      if (sum(log(fx)) > ll) then
         ll = sum(log(fx))
         if (allocated(pars_k)) deallocate (pars_k)
         allocate (pars_k(3*k))
         pars_k = pars
      end if
   end do
   if (ll <= -huge(1.0_dp) / 2.0_dp) cycle
   pcount = 3*k - 1
   aic = -2.0_dp * ll + 2.0_dp * real(pcount, dp)
   if (do_verbose) print "(i6,2f18.6)", k, ll, aic
   if (aic < best_aic) then
      best_aic = aic
      best_k = k
      if (allocated(best_pars)) deallocate (best_pars)
      allocate (best_pars(3*k))
      best_pars = pars_k
   end if
end do

if (best_k < 1 .or. size(best_pars) /= 3*best_k) then
   if (allocated(best_pars)) deallocate (best_pars)
   allocate (best_pars(0))
   return
end if

if (do_verbose) then
   print "(a,i0,a,f14.6)", "AIC chooses k=", best_k, "  AIC=", best_aic
   print "(a8,2a14)", "weight", "mean", "sd"
   do k = 1, best_k
      print "(f8.4,2f14.6)", best_pars(k), best_pars(best_k + k), best_pars(2*best_k + k)
   end do
end if

if (do_plot) then
   m = 200
   sdx = max(1.0e-8_dp, sd(x))
   xmin = minval(x) - 3.0_dp * sdx
   xmax = maxval(x) + 3.0_dp * sdx
   if (allocated(gx)) deallocate (gx)
   if (allocated(gy)) deallocate (gy)
   if (allocated(w)) deallocate (w)
   if (allocated(mu)) deallocate (mu)
   if (allocated(sig)) deallocate (sig)
   allocate (gx(m), gy(m), w(best_k), mu(best_k), sig(best_k))
   w = best_pars(1:best_k)
   mu = best_pars(best_k + 1:2*best_k)
   sig = best_pars(2*best_k + 1:3*best_k)
   do k = 1, m
      gx(k) = xmin + (xmax - xmin) * real(k - 1, dp) / real(max(1, m - 1), dp)
   end do
   gy = dmixnorm(gx, w, mu, sig)
   write (ttl, "(a,i0,a)") "mixnorm AIC fit (", best_k, " components)"
   call gplot(gx, gy, title=trim(ttl), xlabel="x")
   deallocate (gx, gy, w, mu, sig)
end if
end function fit_mixnorm_aic

function fix_mixnorm_aic(x, kmin, kmax, nstart, verbose, plot) result(best_pars)
! Backward-compatible alias.
real(kind=dp), intent(in) :: x(:)
integer, intent(in) :: kmin, kmax
integer, intent(in), optional :: nstart
logical, intent(in), optional :: verbose, plot
real(kind=dp), allocatable :: best_pars(:)
best_pars = fit_mixnorm_aic(x, kmin, kmax, nstart=nstart, verbose=verbose, plot=plot)
end function fix_mixnorm_aic

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
   pure function loglik(u) result(f)
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
   pure function loglik(u) result(f)
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
   pure function loglik(u) result(f)
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
   pure function loglik(u) result(f)
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

pure function fit_laplace(x) result(pars)
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
   pure function loglik(u) result(f)
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
   pure function loglik(u) result(f)
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
   elemental function hyperb_scale_from_sd(alpha, sd) result(scale)
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

   elemental function hyperb_var(scale, alpha) result(v)
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

   pure function loglik(u) result(f)
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
   pure function loglik(u) result(f)
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

pure function jb_test(x) result(v)
! Jarque-Bera normality test, returns [JB, p].
real(kind=dp), intent(in) :: x(:)
real(kind=dp) :: v(2)
real(kind=dp) :: n, s, k, jb
if (size(x) < 3) then
   v = nanv(); return
end if
n = real(size(x), dp)
s = skew(x)
k = kurtosis(x)
jb = (n / 6.0_dp) * (s*s + 0.25_dp * k*k)
v(1) = jb
v(2) = 1.0_dp - chisq_cdf(jb, 2)
end function jb_test

pure function ttest1(x, mu0) result(v)
! One-sample t-test for mean(x)=mu0, returns [t, df, p].
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in) :: mu0
real(kind=dp) :: v(3)
real(kind=dp) :: sx, t
integer :: n, df
n = size(x)
if (n < 2) then
   v = nanv(); return
end if
sx = sd(x)
if (sx <= 0.0_dp) then
   v = nanv(); return
end if
df = n - 1
t = (mean(x) - mu0) / (sx / sqrt(real(n, dp)))
v(1) = t
v(2) = real(df, dp)
v(3) = 2.0_dp * (1.0_dp - tcdf(abs(t), df))
end function ttest1

pure function ttest2(x, y, pooled) result(v)
! Two-sample t-test for mean(x)=mean(y), returns [t, df, p].
! pooled=.true. gives equal-variance test; default is Welch.
real(kind=dp), intent(in) :: x(:), y(:)
logical, intent(in), optional :: pooled
real(kind=dp) :: v(3)
real(kind=dp) :: mx, my, sx2, sy2, se, t, dfw
integer :: nx, ny, df
logical :: pool
nx = size(x); ny = size(y)
if (nx < 2 .or. ny < 2) then
   v = nanv(); return
end if
mx = mean(x); my = mean(y)
sx2 = sd(x)**2; sy2 = sd(y)**2
pool = .false.
if (present(pooled)) pool = pooled
if (pool) then
   df = nx + ny - 2
   if (df < 1) then
      v = nanv(); return
   end if
   se = sqrt((((real(nx - 1, dp)*sx2) + (real(ny - 1, dp)*sy2)) / real(df, dp)) * (1.0_dp/real(nx, dp) + 1.0_dp/real(ny, dp)))
   if (se <= 0.0_dp) then
      v = nanv(); return
   end if
   t = (mx - my) / se
   v(1) = t
   v(2) = real(df, dp)
   v(3) = 2.0_dp * (1.0_dp - tcdf(abs(t), df))
else
   se = sqrt(sx2/real(nx, dp) + sy2/real(ny, dp))
   if (se <= 0.0_dp) then
      v = nanv(); return
   end if
   t = (mx - my) / se
   dfw = (sx2/real(nx, dp) + sy2/real(ny, dp))**2 / &
         ((sx2/real(nx, dp))**2/real(max(1, nx - 1), dp) + (sy2/real(ny, dp))**2/real(max(1, ny - 1), dp))
   df = max(1, nint(dfw))
   v(1) = t
   v(2) = real(dfw, dp)
   v(3) = 2.0_dp * (1.0_dp - tcdf(abs(t), df))
end if
end function ttest2

pure function ks2_test(x, y) result(v)
! Two-sample Kolmogorov-Smirnov test, returns [D, p_approx].
real(kind=dp), intent(in) :: x(:), y(:)
real(kind=dp) :: v(2)
real(kind=dp), allocatable :: xs(:), ys(:)
real(kind=dp) :: d, fx, fy, ne, lam, p
integer :: nx, ny, i, j
nx = size(x); ny = size(y)
if (nx < 1 .or. ny < 1) then
   v = nanv(); return
end if
xs = sorted(x)
ys = sorted(y)
i = 1; j = 1
d = 0.0_dp
do while (i <= nx .and. j <= ny)
   if (xs(i) <= ys(j)) then
      i = i + 1
   else
      j = j + 1
   end if
   fx = real(i - 1, dp) / real(nx, dp)
   fy = real(j - 1, dp) / real(ny, dp)
   d = max(d, abs(fx - fy))
end do
do while (i <= nx)
   i = i + 1
   fx = real(i - 1, dp) / real(nx, dp)
   fy = real(j - 1, dp) / real(ny, dp)
   d = max(d, abs(fx - fy))
end do
do while (j <= ny)
   j = j + 1
   fx = real(i - 1, dp) / real(nx, dp)
   fy = real(j - 1, dp) / real(ny, dp)
   d = max(d, abs(fx - fy))
end do
ne = real(nx * ny, dp) / real(nx + ny, dp)
lam = (sqrt(ne) + 0.12_dp + 0.11_dp/sqrt(ne)) * d
p = 2.0_dp * exp(-2.0_dp * lam * lam)
if (p > 1.0_dp) p = 1.0_dp
if (p < 0.0_dp) p = 0.0_dp
v(1) = d
v(2) = p
end function ks2_test

function kernelreg_scalar(y, x, bw, order, points) result(yhat)
! Nadaraya-Watson Gaussian-kernel regression evaluated at x.
use plot_mod, only: gplot => plot
real(kind=dp), intent(in) :: y(:), x(:)
real(kind=dp), intent(in), optional :: bw
integer, intent(in), optional :: order
logical, intent(in), optional :: points
real(kind=dp), allocatable :: yhat(:)
real(kind=dp) :: h, sx
integer :: n, ord
logical :: do_points
character(len=48) :: ttl

n = size(x)
if (n < 2 .or. size(y) /= n) then
   allocate (yhat(0))
   return
end if
if (present(bw)) then
   h = bw
else
   sx = sd(x)
   h = 1.06_dp * sx * real(n, dp)**(-0.2_dp)
end if
ord = 0
if (present(order)) ord = order
yhat = kernelreg_core(y, x, h, ord)
do_points = .false.
if (present(points)) do_points = points
write (ttl, "(a,i0)") "kernelreg (#obs=", n
ttl = trim(ttl)//")"
if (do_points) call gplot(x, yhat, title=trim(ttl), xlabel="x", points_y=y)
end function kernelreg_scalar

function kernelreg_vec(y, x, bw, order, points) result(yhat)
! Kernel regression with multiple bandwidths; plots all estimates.
use plot_mod, only: gplot => plot
real(kind=dp), intent(in) :: y(:), x(:)
real(kind=dp), intent(in) :: bw(:)
integer, intent(in), optional :: order
logical, intent(in), optional :: points
real(kind=dp), allocatable :: yhat(:)
real(kind=dp), allocatable :: y2(:,:)
character(len=16), allocatable :: legends(:)
integer :: n, j, ord
logical :: do_points
character(len=48) :: ttl

n = size(x)
if (n < 2 .or. size(y) /= n .or. size(bw) < 1) then
   allocate (yhat(0))
   return
end if
ord = 0
if (present(order)) ord = order
allocate (y2(n, size(bw)), legends(size(bw)))
do j = 1, size(bw)
   y2(:, j) = kernelreg_core(y, x, bw(j), ord)
   write (legends(j), "(a,f7.4)") "bw=", bw(j)
end do
yhat = y2(:, 1)
do_points = .false.
if (present(points)) do_points = points
write (ttl, "(a,i0)") "kernelreg (#obs=", n
ttl = trim(ttl)//")"
if (do_points) then
   call gplot(x, y2, title=trim(ttl), xlabel="x", legend_labels=legends, points_y=y)
else
   call gplot(x, y2, title=trim(ttl), xlabel="x", legend_labels=legends)
end if
deallocate (y2, legends)
end function kernelreg_vec

function kernelreg_scalar_ordvec(y, x, bw, order, points) result(yhat)
! Kernel regression with one bandwidth and multiple orders; plots all estimates.
use plot_mod, only: gplot => plot
real(kind=dp), intent(in) :: y(:), x(:)
real(kind=dp), intent(in) :: bw
integer, intent(in) :: order(:)
logical, intent(in), optional :: points
real(kind=dp), allocatable :: yhat(:)
real(kind=dp), allocatable :: y2(:,:)
character(len=16), allocatable :: legends(:)
integer :: n, j
logical :: do_points
character(len=48) :: ttl

n = size(x)
if (n < 2 .or. size(y) /= n .or. size(order) < 1) then
   allocate (yhat(0))
   return
end if
allocate (y2(n, size(order)), legends(size(order)))
do j = 1, size(order)
   y2(:, j) = kernelreg_core(y, x, bw, order(j))
   write (legends(j), "(a,i0)") "ord=", order(j)
end do
yhat = y2(:, 1)
do_points = .false.
if (present(points)) do_points = points
write (ttl, "(a,i0)") "kernelreg (#obs=", n
ttl = trim(ttl)//")"
if (do_points) then
   call gplot(x, y2, title=trim(ttl), xlabel="x", legend_labels=legends, points_y=y)
else
   call gplot(x, y2, title=trim(ttl), xlabel="x", legend_labels=legends)
end if
deallocate (y2, legends)
end function kernelreg_scalar_ordvec

function kernelreg_vec_ordvec(y, x, bw, order, points) result(yhat)
! Kernel regression with multiple bandwidths and orders; plots tensor-product curves.
use plot_mod, only: gplot => plot
real(kind=dp), intent(in) :: y(:), x(:)
real(kind=dp), intent(in) :: bw(:)
integer, intent(in) :: order(:)
logical, intent(in), optional :: points
real(kind=dp), allocatable :: yhat(:)
real(kind=dp), allocatable :: y2(:,:)
character(len=24), allocatable :: legends(:)
integer :: n, j, k, idx, ncurves
logical :: do_points
character(len=48) :: ttl

n = size(x)
if (n < 2 .or. size(y) /= n .or. size(bw) < 1 .or. size(order) < 1) then
   allocate (yhat(0))
   return
end if
ncurves = size(bw)*size(order)
allocate (y2(n, ncurves), legends(ncurves))
idx = 0
do k = 1, size(order)
   do j = 1, size(bw)
      idx = idx + 1
      y2(:, idx) = kernelreg_core(y, x, bw(j), order(k))
      write (legends(idx), "(a,f7.4,a,i0)") "bw=", bw(j), ",o=", order(k)
   end do
end do
yhat = y2(:, 1)
do_points = .false.
if (present(points)) do_points = points
write (ttl, "(a,i0)") "kernelreg (#obs=", n
ttl = trim(ttl)//")"
if (do_points) then
   call gplot(x, y2, title=trim(ttl), xlabel="x", legend_labels=legends, points_y=y)
else
   call gplot(x, y2, title=trim(ttl), xlabel="x", legend_labels=legends)
end if
deallocate (y2, legends)
end function kernelreg_vec_ordvec

pure function kernelreg_core(y, x, h, order) result(yhat)
! Local polynomial kernel regression at design points x for scalar h and order>=0.
real(kind=dp), intent(in) :: y(:), x(:), h
integer, intent(in) :: order
real(kind=dp), allocatable :: yhat(:)
real(kind=dp) :: z, w, num, den, d
real(kind=dp), allocatable :: xtwx(:,:), xtwy(:), beta(:), powd(:)
integer :: n, i, j, a, b, ord, p, ord_try
logical :: ok
n = size(x)
allocate (yhat(n))
if (h <= 0.0_dp .or. order < 0) then
   yhat = nanv()
   return
end if
ord = min(order, n - 1)
if (ord <= 0) then
   ! order 0: Nadaraya-Watson
   do i = 1, n
      num = 0.0_dp
      den = 0.0_dp
      do j = 1, n
         z = (x(i) - x(j)) / h
         w = exp(-0.5_dp * z * z)
         num = num + w * y(j)
         den = den + w
      end do
      if (den > 0.0_dp) then
         yhat(i) = num / den
      else
         yhat(i) = nanv()
      end if
   end do
   return
end if

do i = 1, n
   yhat(i) = nanv()
   do ord_try = ord, 0, -1
      p = ord_try + 1
      allocate (xtwx(p, p), xtwy(p), beta(p), powd(0:ord_try))
      xtwx = 0.0_dp
      xtwy = 0.0_dp
      do j = 1, n
         d = x(j) - x(i)
         z = d / h
         w = exp(-0.5_dp * z * z)
         powd(0) = 1.0_dp
         do a = 1, ord_try
            powd(a) = powd(a - 1) * d
         end do
         do a = 0, ord_try
            xtwy(a + 1) = xtwy(a + 1) + w * powd(a) * y(j)
            do b = 0, ord_try
               xtwx(a + 1, b + 1) = xtwx(a + 1, b + 1) + w * powd(a) * powd(b)
            end do
         end do
      end do
      call solve_linear(xtwx, xtwy, beta, ok)
      if (ok) then
         yhat(i) = beta(1)
         deallocate (xtwx, xtwy, beta, powd)
         exit
      end if
      deallocate (xtwx, xtwy, beta, powd)
   end do
end do
end function kernelreg_core

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

subroutine poly1reg(y, x, deg, intcp)
! Polynomial regression in one predictor: y ~ 1 + x + x^2 + ... + x^deg.
real(kind=dp), intent(in) :: y(:), x(:)
integer, intent(in) :: deg
integer, intent(in), optional :: intcp
real(kind=dp), allocatable :: xmat(:,:)
character(len=16), allocatable :: labels(:)
character(len=16) :: nm
integer :: n, j
logical :: use_intcp
n = size(x)
if (n /= size(y) .or. n < 2) then
   print *, "Error: poly1reg() requires equal-size arrays with size > 1"
   return
end if
if (deg < 1) then
   print *, "Error: poly1reg() degree must be >= 1"
   return
end if
use_intcp = .true.
if (present(intcp)) use_intcp = (intcp /= 0)
allocate (xmat(n, deg), labels(deg))
do j = 1, deg
   xmat(:, j) = x**j
   write (nm, "(a,i0)") "x^", j
   labels(j) = trim(nm)
end do
call regress_multi(y, xmat, labels, intcp=use_intcp)
deallocate (xmat, labels)
end subroutine poly1reg

function splinereg_scalar(y, x, k, degree, intcp, plot, points) result(yhat)
! Truncated-power spline regression with one predictor.
! Basis: [x, x^2, ..., x^degree, (x-knot_j)_+^degree].
use plot_mod, only: gplot => plot
real(kind=dp), intent(in) :: y(:), x(:)
integer, intent(in) :: k
integer, intent(in), optional :: degree, intcp, plot
logical, intent(in), optional :: points
real(kind=dp), allocatable :: yhat(:)
integer :: deg
logical :: use_intcp, do_plot, do_points
character(len=48) :: ttl

deg = 3
if (present(degree)) deg = degree
use_intcp = .true.
if (present(intcp)) use_intcp = (intcp /= 0)
do_plot = .true.
if (present(plot)) do_plot = (plot /= 0)
do_points = .false.
if (present(points)) do_points = points
yhat = splinereg_core(y, x, k, deg, use_intcp)
write (ttl, "(a,i0)") "splinereg (#obs=", size(x)
ttl = trim(ttl)//")"
if (do_plot .and. size(yhat) > 0) then
   if (do_points) then
      call gplot(x, yhat, title=trim(ttl), xlabel="x", points_y=y)
   else
      call gplot(x, yhat, title=trim(ttl), xlabel="x")
   end if
end if
end function splinereg_scalar

function splinereg_degvec(y, x, k, degree, intcp, plot, points) result(yhat)
! Spline regression with multiple degrees; plots all estimates.
use plot_mod, only: gplot => plot
real(kind=dp), intent(in) :: y(:), x(:)
integer, intent(in) :: k
integer, intent(in) :: degree(:)
integer, intent(in), optional :: intcp, plot
logical, intent(in), optional :: points
real(kind=dp), allocatable :: yhat(:)
real(kind=dp), allocatable :: y2(:,:)
character(len=16), allocatable :: legends(:)
integer :: n, j
logical :: use_intcp, do_plot, do_points
character(len=48) :: ttl

n = size(x)
if (n /= size(y) .or. n < 2 .or. size(degree) < 1) then
   allocate (yhat(0))
   return
end if
use_intcp = .true.
if (present(intcp)) use_intcp = (intcp /= 0)
do_plot = .true.
if (present(plot)) do_plot = (plot /= 0)
do_points = .false.
if (present(points)) do_points = points
allocate (y2(n, size(degree)), legends(size(degree)))
do j = 1, size(degree)
   y2(:, j) = splinereg_core(y, x, k, degree(j), use_intcp)
   write (legends(j), "(a,i0)") "deg=", degree(j)
end do
yhat = y2(:, 1)
write (ttl, "(a,i0)") "splinereg (#obs=", n
ttl = trim(ttl)//")"
if (do_plot) then
   if (do_points) then
      call gplot(x, y2, title=trim(ttl), xlabel="x", legend_labels=legends, points_y=y)
   else
      call gplot(x, y2, title=trim(ttl), xlabel="x", legend_labels=legends)
   end if
end if
deallocate (y2, legends)
end function splinereg_degvec

function splinereg_core(y, x, k, degree, use_intcp) result(yhat)
! Core spline fit for a single degree, optionally with intercept.
real(kind=dp), intent(in) :: y(:), x(:)
integer, intent(in) :: k, degree
logical, intent(in) :: use_intcp
real(kind=dp), allocatable :: yhat(:)
real(kind=dp), allocatable :: xmat(:,:), xtx(:,:), xty(:), beta(:), q(:), knots(:)
integer :: n, deg, j, ncol, kk
logical :: ok

deg = degree
n = size(x)
if (n /= size(y) .or. n < 2) then
   print *, "Error: splinereg() requires equal-size arrays with size > 1"
   allocate (yhat(0))
   return
end if
if (k < 0) then
   print *, "Error: splinereg() requires k >= 0"
   allocate (yhat(0))
   return
end if
if (deg < 0) then
   print *, "Error: splinereg() requires degree >= 0"
   allocate (yhat(0))
   return
end if
kk = min(k, max(0, n - 1))
if (kk > 0) then
   allocate (q(kk), knots(kk))
   do j = 1, kk
      q(j) = real(j, dp)/real(kk + 1, dp)
   end do
   knots = quantile(x, q)
end if

ncol = deg + kk
if (ncol < 1) then
   print *, "Error: splinereg() needs degree > 0 or k > 0"
   allocate (yhat(0))
   if (allocated(q)) deallocate (q, knots)
   return
end if
allocate (xmat(n, ncol))
do j = 1, deg
   xmat(:, j) = x**j
end do
do j = 1, kk
   xmat(:, deg + j) = max(x - knots(j), 0.0_dp)**deg
end do

if (use_intcp) then
   allocate (xtx(ncol + 1, ncol + 1), xty(ncol))
   xtx = 0.0_dp
   xtx(1, 1) = real(n, dp)
   xtx(1, 2:) = sum(xmat, dim=1)
   xtx(2:, 1) = xtx(1, 2:)
   xtx(2:, 2:) = matmul(transpose(xmat), xmat)
   xty = matmul(transpose(xmat), y)
   allocate (beta(ncol + 1))
   call solve_linear(xtx, [sum(y), xty], beta, ok)
   if (.not. ok) then
      print *, "Error: splinereg() singular design matrix"
      allocate (yhat(0))
      if (allocated(q)) deallocate (q, knots)
      deallocate (xmat, xtx, xty, beta)
      return
   end if
   allocate (yhat(n))
   yhat = beta(1) + matmul(xmat, beta(2:))
else
   xtx = matmul(transpose(xmat), xmat)
   xty = matmul(transpose(xmat), y)
   call solve_linear(xtx, xty, beta, ok)
   if (.not. ok) then
      print *, "Error: splinereg() singular design matrix"
      allocate (yhat(0))
      if (allocated(q)) deallocate (q, knots)
      deallocate (xmat, xtx, xty)
      return
   end if
   allocate (yhat(n))
   yhat = matmul(xmat, beta)
end if

if (allocated(q)) deallocate (q, knots)
deallocate (xmat, xtx, xty, beta)
end function splinereg_core

pure function ns_dplus3(xv, knot) result(v)
real(kind=dp), intent(in) :: xv(:), knot
real(kind=dp) :: v(size(xv))
v = max(xv - knot, 0.0_dp)**3
end function ns_dplus3

pure function ns_hfun(xv, kj, k1, k2) result(v)
real(kind=dp), intent(in) :: xv(:), kj, k1, k2
real(kind=dp) :: v(size(xv))
v = ns_dplus3(xv, kj) - ns_dplus3(xv, k1)*(k2 - kj)/(k2 - k1) + ns_dplus3(xv, k2)*(k1 - kj)/(k2 - k1)
end function ns_hfun

function naturalspline_predict(y_train, x_train, k, x_out, use_intcp, ok) result(yhat_out)
! Fit natural spline on training data and predict at x_out.
real(kind=dp), intent(in) :: y_train(:), x_train(:), x_out(:)
integer, intent(in) :: k
logical, intent(in) :: use_intcp
logical, intent(out) :: ok
real(kind=dp), allocatable :: yhat_out(:)
real(kind=dp), allocatable :: xmat(:,:), xout_mat(:,:), xtx(:,:), xty(:), beta(:)
real(kind=dp), allocatable :: q(:), iknots(:), knots(:)
real(kind=dp) :: xmin, xmax, eps, ref1, ref2, den
integer :: n, nout, kk, j, ncol, nkn

ok = .false.
n = size(x_train)
nout = size(x_out)
if (n /= size(y_train) .or. n < 2 .or. nout < 1 .or. k < 0) then
   allocate (yhat_out(0))
   return
end if
xmin = minval(x_train)
xmax = maxval(x_train)
if (xmax <= xmin) then
   allocate (yhat_out(0))
   return
end if
kk = min(k, max(0, n - 2))
if (kk > 0) then
   allocate (q(kk), iknots(kk))
   do j = 1, kk
      q(j) = real(j, dp)/real(kk + 1, dp)
   end do
   iknots = quantile(x_train, q)
   eps = max(1.0e-12_dp, 1.0e-8_dp*(xmax - xmin))
   iknots = max(min(iknots, xmax - eps), xmin + eps)
end if

nkn = kk + 2
allocate (knots(nkn))
knots(1) = xmin
if (kk > 0) knots(2:kk + 1) = iknots
knots(nkn) = xmax

ncol = 1 + kk
allocate (xmat(n, ncol), xout_mat(nout, ncol))
xmat(:, 1) = x_train
xout_mat(:, 1) = x_out
if (kk > 0) then
   ref1 = knots(kk + 1)
   ref2 = knots(kk + 2)
   den = ref2 - ref1
   if (den <= 0.0_dp) then
      allocate (yhat_out(0))
      if (allocated(q)) deallocate (q, iknots)
      deallocate (knots, xmat, xout_mat)
      return
   end if
   do j = 1, kk
      xmat(:, 1 + j) = ns_hfun(x_train, knots(j + 1), ref1, ref2)
      xout_mat(:, 1 + j) = ns_hfun(x_out, knots(j + 1), ref1, ref2)
   end do
end if

if (use_intcp) then
   allocate (xtx(ncol + 1, ncol + 1), xty(ncol), beta(ncol + 1))
   xtx = 0.0_dp
   xtx(1, 1) = real(n, dp)
   xtx(1, 2:) = sum(xmat, dim=1)
   xtx(2:, 1) = xtx(1, 2:)
   xtx(2:, 2:) = matmul(transpose(xmat), xmat)
   xty = matmul(transpose(xmat), y_train)
   call solve_linear(xtx, [sum(y_train), xty], beta, ok)
   if (.not. ok) then
      allocate (yhat_out(0))
      if (allocated(q)) deallocate (q, iknots)
      deallocate (knots, xmat, xout_mat, xtx, xty, beta)
      return
   end if
   allocate (yhat_out(nout))
   yhat_out = beta(1) + matmul(xout_mat, beta(2:))
else
   xtx = matmul(transpose(xmat), xmat)
   xty = matmul(transpose(xmat), y_train)
   call solve_linear(xtx, xty, beta, ok)
   if (.not. ok) then
      allocate (yhat_out(0))
      if (allocated(q)) deallocate (q, iknots)
      deallocate (knots, xmat, xout_mat, xtx, xty)
      return
   end if
   allocate (yhat_out(nout))
   yhat_out = matmul(xout_mat, beta)
end if

if (allocated(q)) deallocate (q, iknots)
deallocate (knots, xmat, xout_mat, xtx, xty, beta)
end function naturalspline_predict

function naturalspline_scalar(y, x, k, intcp, plot, points) result(yhat)
use plot_mod, only: gplot => plot
real(kind=dp), intent(in) :: y(:), x(:)
integer, intent(in), optional :: k
integer, intent(in), optional :: intcp, plot
logical, intent(in), optional :: points
real(kind=dp), allocatable :: yhat(:), ytr(:), xtr(:), yva(:), xva(:), ypred(:)
real(kind=dp) :: cv, best_cv
integer :: n, i, f, folds, ntr, nva, ktry, best_k, kmax
logical :: use_intcp, do_plot, do_points, ok
character(len=64) :: ttl

n = size(x)
if (n /= size(y) .or. n < 2) then
   print *, "Error: naturalspline() requires equal-size arrays with size > 1"
   allocate (yhat(0))
   return
end if
use_intcp = .true.
if (present(intcp)) use_intcp = (intcp /= 0)
do_plot = .true.
if (present(plot)) do_plot = (plot /= 0)
do_points = .false.
if (present(points)) do_points = points

if (present(k)) then
   if (k < 0) then
      print *, "Error: naturalspline() requires k >= 0"
      allocate (yhat(0))
      return
   end if
   best_k = k
else
   kmax = min(12, max(0, n - 2))
   if (kmax <= 0) then
      best_k = 0
   else
      folds = min(5, n)
      best_cv = huge(1.0_dp)
      best_k = 0
      do ktry = 0, kmax
         cv = 0.0_dp
         do f = 1, folds
            nva = 0
            do i = 1, n
               if (mod(i - 1, folds) == (f - 1)) nva = nva + 1
            end do
            ntr = n - nva
            if (ntr < 3 .or. nva < 1) cycle
            allocate (xtr(ntr), ytr(ntr), xva(nva), yva(nva))
            ntr = 0; nva = 0
            do i = 1, n
               if (mod(i - 1, folds) == (f - 1)) then
                  nva = nva + 1
                  xva(nva) = x(i); yva(nva) = y(i)
               else
                  ntr = ntr + 1
                  xtr(ntr) = x(i); ytr(ntr) = y(i)
               end if
            end do
            ypred = naturalspline_predict(ytr, xtr, ktry, xva, use_intcp, ok)
            if (.not. ok .or. size(ypred) /= size(yva)) then
               cv = huge(1.0_dp)
               deallocate (xtr, ytr, xva, yva)
               exit
            end if
            cv = cv + sum((yva - ypred)**2)
            deallocate (xtr, ytr, xva, yva)
         end do
         if (cv < best_cv) then
            best_cv = cv
            best_k = ktry
         end if
      end do
   end if
end if

yhat = naturalspline_predict(y, x, best_k, x, use_intcp, ok)
if (.not. ok) then
   print *, "Error: naturalspline() singular design matrix"
   if (.not. allocated(yhat)) allocate (yhat(0))
   return
end if
if (do_plot .and. size(yhat) > 0) then
   if (present(k)) then
      write (ttl, "(a,i0,a)") "naturalspline (#obs=", n, ")"
   else
      write (ttl, "(a,i0,a,i0,a)") "naturalspline (cv k=", best_k, ", #obs=", n, ")"
   end if
   if (do_points) then
      call gplot(x, yhat, title=trim(ttl), xlabel="x", points_y=y)
   else
      call gplot(x, yhat, title=trim(ttl), xlabel="x")
   end if
end if
end function naturalspline_scalar

function naturalspline_kvec(y, x, k, intcp, plot, points) result(yhat)
! Natural cubic spline regression with multiple knot counts; plots all estimates.
use plot_mod, only: gplot => plot
real(kind=dp), intent(in) :: y(:), x(:)
integer, intent(in) :: k(:)
integer, intent(in), optional :: intcp, plot
logical, intent(in), optional :: points
real(kind=dp), allocatable :: yhat(:), tmp(:), y2(:,:)
character(len=12), allocatable :: legends(:)
integer :: n, j, intcp_i
logical :: do_plot, do_points
character(len=48) :: ttl

n = size(x)
if (n /= size(y) .or. n < 2 .or. size(k) < 1) then
   allocate (yhat(0))
   return
end if
if (any(k < 0)) then
   print *, "Error: naturalspline() requires k >= 0"
   allocate (yhat(0))
   return
end if
intcp_i = 1
if (present(intcp)) intcp_i = intcp
do_plot = .true.
if (present(plot)) do_plot = (plot /= 0)
do_points = .false.
if (present(points)) do_points = points
allocate (y2(n, size(k)), legends(size(k)))
do j = 1, size(k)
   tmp = naturalspline_scalar(y, x, k(j), intcp=intcp_i, plot=0)
   if (size(tmp) /= n) then
      allocate (yhat(0))
      deallocate (y2, legends)
      return
   end if
   y2(:, j) = tmp
   write (legends(j), "(a,i0)") "k=", k(j)
end do
yhat = y2(:, 1)
write (ttl, "(a,i0)") "naturalspline (#obs=", n
ttl = trim(ttl)//")"
if (do_plot) then
   if (do_points) then
      call gplot(x, y2, title=trim(ttl), xlabel="x", legend_labels=legends, points_y=y)
   else
      call gplot(x, y2, title=trim(ttl), xlabel="x", legend_labels=legends)
   end if
end if
deallocate (y2, legends)
end function naturalspline_kvec

function cpsim(n, cp, mu, sd, seed, plot, verbose) result(x)
! Simulate piecewise-normal data with changepoints.
! cp has length m and defines m+1 segments.
! mu/sd may be absent (defaults 0/1), scalar (broadcast), or length m+1.
use plot_mod, only: gplot => plot
integer, intent(in) :: n
real(kind=dp), intent(in) :: cp(:)
real(kind=dp), intent(in), optional :: mu(:), sd(:)
integer, intent(in), optional :: seed
integer, intent(in), optional :: plot, verbose
real(kind=dp), allocatable :: x(:), z(:), mu_seg(:), sd_seg(:), ytrue(:), tt(:)
integer, allocatable :: cpi(:)
integer :: m, i, j, lo, hi
logical :: do_plot, do_verbose
character(len=48) :: ttl

if (n < 1) then
   allocate (x(0))
   return
end if
if (present(seed)) call random_seed_init(seed)
do_plot = .false.
if (present(plot)) do_plot = (plot /= 0)
do_verbose = .false.
if (present(verbose)) do_verbose = (verbose /= 0)
allocate (x(n), z(n))
z = random_normal(n)
m = size(cp)
allocate (cpi(m))
if (m > 0) cpi = nint(cp)
if (m > 1) then
   do i = 2, m
      if (cpi(i) <= cpi(i - 1)) then
         print *, "Error: cpsim() cp must be strictly increasing"
         allocate (x(0))
         return
      end if
   end do
end if
if (m > 0) then
   if (minval(cpi) < 1 .or. maxval(cpi) >= n) then
      print *, "Error: cpsim() cp must be between 1 and n-1"
      allocate (x(0))
      return
   end if
end if

allocate (mu_seg(m + 1), sd_seg(m + 1))
mu_seg = 0.0_dp
sd_seg = 1.0_dp
if (present(mu)) then
   if (size(mu) == 1) then
      mu_seg = mu(1)
   else if (size(mu) == m + 1) then
      mu_seg = mu
   else
      print *, "Error: cpsim() mu must be scalar or length m+1"
      allocate (x(0))
      return
   end if
end if
if (present(sd)) then
   if (size(sd) == 1) then
      sd_seg = sd(1)
   else if (size(sd) == m + 1) then
      sd_seg = sd
   else
      print *, "Error: cpsim() sd must be scalar or length m+1"
      allocate (x(0))
      return
   end if
end if
if (any(sd_seg <= 0.0_dp)) then
   print *, "Error: cpsim() sd must be > 0"
   allocate (x(0))
   return
end if

if (do_verbose) then
   print *
   print "(a)", "cpsim segment parameters"
   if (m > 0) then
      write (*, "(a)", advance="no") "changepoints: "
      do i = 1, m
         if (i > 1) write (*, "(a)", advance="no") ", "
         write (*, "(i0)", advance="no") cpi(i)
      end do
      print *
   else
      print *, "changepoints: none"
   end if
   print "(a8,a8,a8,a14,a14)", "segment", "start", "end", "mean", "sd"
   lo = 1
   do j = 1, m + 1
      if (j <= m) then
         hi = cpi(j)
      else
         hi = n
      end if
      print "(i8,i8,i8,2f14.6)", j, lo, hi, mu_seg(j), sd_seg(j)
      lo = hi + 1
   end do
end if

lo = 1
do j = 1, m + 1
   if (j <= m) then
      hi = cpi(j)
   else
      hi = n
   end if
   if (hi >= lo) x(lo:hi) = mu_seg(j) + sd_seg(j)*z(lo:hi)
   lo = hi + 1
end do
if (do_plot) then
   allocate (ytrue(n), tt(n))
   lo = 1
   do j = 1, m + 1
      if (j <= m) then
         hi = cpi(j)
      else
         hi = n
      end if
      if (hi >= lo) ytrue(lo:hi) = mu_seg(j)
      lo = hi + 1
   end do
   do i = 1, n
      tt(i) = real(i, dp)
   end do
   write (ttl, "(a,i0)") "cpsim (#obs=", n
   ttl = trim(ttl)//")"
   call gplot(tt, ytrue, title=trim(ttl), xlabel="t", points_y=x)
end if
end function cpsim

function cpfit(x, mode, max_cp, minseg, plot, verbose) result(out)
! Fit changepoints by greedy binary segmentation.
! mode="mean": mean shifts, common sigma.
! mode="sd":   variance shifts, common mean.
! mode="both": mean and variance shifts.
use plot_mod, only: gplot => plot
real(kind=dp), intent(in) :: x(:)
character(len=*), intent(in), optional :: mode
integer, intent(in), optional :: max_cp, minseg, plot, verbose
real(kind=dp), allocatable :: out(:)
real(kind=dp), allocatable :: s1(:), s2(:), mu_seg(:), sd_seg(:), yhat(:), tt(:)
integer, allocatable :: lseg(:), rseg(:), cps(:)
integer :: n, mcp, mseg, nseg, s, best_s, best_t, t, c
real(kind=dp) :: best_gain, gain, sse_total, sigma2, ll, aic, bic, mu0, rss, eps
character(len=16) :: mode_
logical :: do_plot, do_verbose
character(len=64) :: ttl
integer :: kpar

n = size(x)
if (n < 2) then
   allocate (out(0))
   return
end if
mode_ = "mean"
if (present(mode)) mode_ = trim(mode)
do t = 1, len_trim(mode_)
   if (mode_(t:t) >= "A" .and. mode_(t:t) <= "Z") mode_(t:t) = achar(iachar(mode_(t:t)) + 32)
end do
if (trim(mode_) /= "mean" .and. trim(mode_) /= "sd" .and. trim(mode_) /= "both") then
   print *, "Error: cpfit() mode must be 'mean', 'sd', or 'both'"
   allocate (out(0))
   return
end if
mcp = 1
if (present(max_cp)) mcp = max(0, max_cp)
mseg = 10
if (present(minseg)) mseg = max(2, minseg)
do_plot = .true.
if (present(plot)) do_plot = (plot /= 0)
do_verbose = .true.
if (present(verbose)) do_verbose = (verbose /= 0)
if (2*mseg > n) mseg = max(2, n/2)
eps = 1.0e-12_dp

allocate (s1(0:n), s2(0:n))
s1(0) = 0.0_dp; s2(0) = 0.0_dp
do t = 1, n
   s1(t) = s1(t - 1) + x(t)
   s2(t) = s2(t - 1) + x(t)*x(t)
end do
mu0 = s1(n)/real(n, dp)

allocate (lseg(max(1, mcp + 1)), rseg(max(1, mcp + 1)))
nseg = 1
lseg(1) = 1; rseg(1) = n

do c = 1, mcp
   best_gain = 0.0_dp
   best_s = 0
   best_t = 0
   do s = 1, nseg
      do t = lseg(s) + mseg - 1, rseg(s) - mseg
         gain = seg_cost(lseg(s), rseg(s)) - seg_cost(lseg(s), t) - seg_cost(t + 1, rseg(s))
         if (gain > best_gain) then
            best_gain = gain
            best_s = s
            best_t = t
         end if
      end do
   end do
   if (best_s == 0 .or. best_gain <= 0.0_dp) exit
   do s = nseg, best_s + 1, -1
      lseg(s + 1) = lseg(s)
      rseg(s + 1) = rseg(s)
   end do
   lseg(best_s + 1) = best_t + 1
   rseg(best_s + 1) = rseg(best_s)
   rseg(best_s) = best_t
   nseg = nseg + 1
end do

allocate (cps(max(0, nseg - 1)), mu_seg(nseg), sd_seg(nseg))
do s = 1, nseg
   if (trim(mode_) == "sd") then
      mu_seg(s) = mu0
      rss = rss_fixedmu(lseg(s), rseg(s), mu0)
   else
      mu_seg(s) = seg_mean(lseg(s), rseg(s))
      rss = sse_int(lseg(s), rseg(s))
   end if
   sd_seg(s) = sqrt(max(eps, rss/real(rseg(s) - lseg(s) + 1, dp)))
   if (s < nseg) cps(s) = rseg(s)
end do

select case (trim(mode_))
case ("mean")
   sse_total = 0.0_dp
   do s = 1, nseg
      sse_total = sse_total + sse_int(lseg(s), rseg(s))
   end do
   sigma2 = max(eps, sse_total/real(n, dp))
   ll = -0.5_dp*real(n, dp)*(log(2.0_dp*pi*sigma2) + 1.0_dp)
   kpar = nseg + 1
   aic = -2.0_dp*ll + 2.0_dp*real(kpar, dp)
   bic = -2.0_dp*ll + log(real(n, dp))*real(kpar, dp)
   allocate (out(1 + (nseg - 1) + nseg + 3))
   out(1) = real(nseg - 1, dp)
   if (nseg > 1) out(2:nseg) = real(cps(1:nseg - 1), dp)
   out(nseg + 1:2*nseg) = mu_seg
   out(2*nseg + 1) = sqrt(sigma2)
   out(2*nseg + 2) = aic
   out(2*nseg + 3) = bic
case ("sd")
   ll = 0.0_dp
   do s = 1, nseg
      rss = rss_fixedmu(lseg(s), rseg(s), mu0)
      sigma2 = max(eps, rss/real(rseg(s) - lseg(s) + 1, dp))
      ll = ll - 0.5_dp*real(rseg(s) - lseg(s) + 1, dp)*(log(2.0_dp*pi*sigma2) + 1.0_dp)
   end do
   kpar = 1 + nseg
   aic = -2.0_dp*ll + 2.0_dp*real(kpar, dp)
   bic = -2.0_dp*ll + log(real(n, dp))*real(kpar, dp)
   allocate (out(1 + (nseg - 1) + 1 + nseg + 2))
   out(1) = real(nseg - 1, dp)
   if (nseg > 1) out(2:nseg) = real(cps(1:nseg - 1), dp)
   out(nseg + 1) = mu0
   out(nseg + 2:2*nseg + 1) = sd_seg
   out(2*nseg + 2) = aic
   out(2*nseg + 3) = bic
case ("both")
   ll = 0.0_dp
   do s = 1, nseg
      rss = sse_int(lseg(s), rseg(s))
      sigma2 = max(eps, rss/real(rseg(s) - lseg(s) + 1, dp))
      ll = ll - 0.5_dp*real(rseg(s) - lseg(s) + 1, dp)*(log(2.0_dp*pi*sigma2) + 1.0_dp)
   end do
   kpar = 2*nseg
   aic = -2.0_dp*ll + 2.0_dp*real(kpar, dp)
   bic = -2.0_dp*ll + log(real(n, dp))*real(kpar, dp)
   allocate (out(1 + (nseg - 1) + nseg + nseg + 2))
   out(1) = real(nseg - 1, dp)
   if (nseg > 1) out(2:nseg) = real(cps(1:nseg - 1), dp)
   out(nseg + 1:2*nseg) = mu_seg
   out(2*nseg + 1:3*nseg) = sd_seg
   out(3*nseg + 1) = aic
   out(3*nseg + 2) = bic
end select

if (do_verbose) then
   print *
   print "(a,a,a)", "cpfit mode=", trim(mode_), " : segment estimates"
   if (nseg > 1) then
      write (*, "(a)", advance="no") "changepoints: "
      do s = 1, nseg - 1
         if (s > 1) write (*, "(a)", advance="no") ", "
         write (*, "(i0)", advance="no") cps(s)
      end do
      print *
   else
      print *, "changepoints: none"
   end if
   print "(a8,a8,a8,a14,a14)", "segment", "start", "end", "mean", "sd"
   do s = 1, nseg
      print "(i8,i8,i8,2f14.6)", s, lseg(s), rseg(s), mu_seg(s), sd_seg(s)
   end do
end if

if (do_plot) then
   allocate (yhat(n), tt(n))
   do t = 1, n
      tt(t) = real(t, dp)
   end do
   do s = 1, nseg
      yhat(lseg(s):rseg(s)) = mu_seg(s)
   end do
   write (ttl, "(a,a,a,i0,a)") "cpfit (", trim(mode_), ", #obs=", n, ")"
   call gplot(tt, yhat, title=trim(ttl), xlabel="t", points_y=x)
end if

contains

   pure function sse_int(l, r) result(v)
      integer, intent(in) :: l, r
      real(kind=dp) :: v, sx, sx2
      integer :: len
      len = r - l + 1
      if (len <= 0) then
         v = 0.0_dp
         return
      end if
      sx = s1(r) - s1(l - 1)
      sx2 = s2(r) - s2(l - 1)
      v = max(0.0_dp, sx2 - sx*sx/real(len, dp))
   end function sse_int

   pure function seg_mean(l, r) result(v)
      integer, intent(in) :: l, r
      real(kind=dp) :: v
      v = (s1(r) - s1(l - 1))/real(r - l + 1, dp)
   end function seg_mean

   pure function rss_fixedmu(l, r, mu) result(v)
      integer, intent(in) :: l, r
      real(kind=dp), intent(in) :: mu
      real(kind=dp) :: v, sx, sx2
      integer :: len
      len = r - l + 1
      sx = s1(r) - s1(l - 1)
      sx2 = s2(r) - s2(l - 1)
      v = max(0.0_dp, sx2 - 2.0_dp*mu*sx + real(len, dp)*mu*mu)
   end function rss_fixedmu

   pure function seg_cost(l, r) result(v)
      integer, intent(in) :: l, r
      real(kind=dp) :: v, rss
      integer :: len
      len = r - l + 1
      select case (trim(mode_))
      case ("mean")
         v = sse_int(l, r)
      case ("sd")
         rss = max(eps, rss_fixedmu(l, r, mu0))
         v = real(len, dp)*log(rss/real(len, dp))
      case ("both")
         rss = max(eps, sse_int(l, r))
         v = real(len, dp)*log(rss/real(len, dp))
      end select
   end function seg_cost

end function cpfit

function cpfitaic(x, mode, max_cp, minseg, criterion, plot, plot_ic, verbose) result(best_out)
! Select changepoint model over max_cp=0..max_cp using AIC/BIC.
! Returns cpfit() output for the selected model.
use plot_mod, only: gplot => plot
real(kind=dp), intent(in) :: x(:)
character(len=*), intent(in), optional :: mode, criterion
integer, intent(in), optional :: max_cp, minseg, plot, plot_ic, verbose
real(kind=dp), allocatable :: best_out(:)
type fit_holder
   real(kind=dp), allocatable :: v(:)
end type fit_holder
type(fit_holder), allocatable :: fits(:)
real(kind=dp), allocatable :: aicv(:), bicv(:), yfit(:,:), tt(:), icm(:), icy(:,:)
integer, allocatable :: ncpv(:)
character(len=16), allocatable :: legends(:), legends_ic(:)
character(len=16) :: mode_, crit_
character(len=64) :: ttl
integer :: n, mmax, mseg, m, nm, best_i, ncp, s, lo, hi
real(kind=dp) :: best_val
logical :: do_plot, do_plot_ic, do_verbose

allocate (best_out(0))
n = size(x)
if (n < 2) return

mode_ = "mean"
if (present(mode)) mode_ = trim(mode)
do s = 1, len_trim(mode_)
   if (mode_(s:s) >= "A" .and. mode_(s:s) <= "Z") mode_(s:s) = achar(iachar(mode_(s:s)) + 32)
end do
if (trim(mode_) /= "mean" .and. trim(mode_) /= "sd" .and. trim(mode_) /= "both") then
   print *, "Error: cpfitaic() mode must be 'mean', 'sd', or 'both'"
   return
end if

crit_ = "aic"
if (present(criterion)) crit_ = trim(criterion)
do s = 1, len_trim(crit_)
   if (crit_(s:s) >= "A" .and. crit_(s:s) <= "Z") crit_(s:s) = achar(iachar(crit_(s:s)) + 32)
end do
if (trim(crit_) /= "aic" .and. trim(crit_) /= "bic") then
   print *, "Error: cpfitaic() criterion must be 'aic' or 'bic'"
   return
end if

mmax = 5
if (present(max_cp)) mmax = max(0, max_cp)
mseg = 10
if (present(minseg)) mseg = max(2, minseg)
do_plot = .true.
if (present(plot)) do_plot = (plot /= 0)
do_plot_ic = .false.
if (present(plot_ic)) do_plot_ic = (plot_ic /= 0)
do_verbose = .true.
if (present(verbose)) do_verbose = (verbose /= 0)

nm = mmax + 1
allocate (fits(nm), aicv(nm), bicv(nm), ncpv(nm))
allocate (yfit(n, nm), tt(n), legends(nm))
do m = 1, n
   tt(m) = real(m, dp)
end do

do m = 0, mmax
   fits(m + 1)%v = cpfit(x, mode=trim(mode_), max_cp=m, minseg=mseg, plot=0, verbose=0)
   if (size(fits(m + 1)%v) < 3) then
      print *, "Error: cpfitaic() failed for max_cp=", m
      return
   end if
   aicv(m + 1) = fits(m + 1)%v(size(fits(m + 1)%v) - 1)
   bicv(m + 1) = fits(m + 1)%v(size(fits(m + 1)%v))
   ncpv(m + 1) = nint(fits(m + 1)%v(1))
   call fitted_means_from_cpfit(fits(m + 1)%v, mode_, yfit(:, m + 1))
   write (legends(m + 1), "(a,i0)") "max_cp=", m
   if (do_verbose) call print_model_table(m, fits(m + 1)%v, mode_)
end do

best_i = 1
if (trim(crit_) == "aic") then
   best_val = aicv(1)
   do m = 2, nm
      if (aicv(m) < best_val) then
         best_val = aicv(m)
         best_i = m
      end if
   end do
else
   best_val = bicv(1)
   do m = 2, nm
      if (bicv(m) < best_val) then
         best_val = bicv(m)
         best_i = m
      end if
   end do
end if

ncp = ncpv(best_i)
best_out = fits(best_i)%v

if (do_verbose) then
   print *
   print "(a)", "cpfitaic model scan"
   print "(a8,a8,a18,a18)", "max_cp", "ncp", "AIC", "BIC"
   do m = 0, mmax
      print "(2i8,2f18.6)", m, ncpv(m + 1), aicv(m + 1), bicv(m + 1)
   end do
   print "(a,a,a,i0,a,i0,a)", "criterion=", trim(crit_), "  chooses max_cp=", best_i - 1, " (ncp=", ncp, ")"
end if

if (do_plot) then
   write (ttl, "(a,a,a,i0,a)") "cpfitaic fits (", trim(mode_), ", #obs=", n, ")"
   call gplot(tt, yfit, title=trim(ttl), xlabel="t", legend_labels=legends, points_y=x)
end if

if (do_plot_ic) then
   allocate (icm(nm), icy(nm, 2), legends_ic(2))
   do m = 1, nm
      icm(m) = real(m - 1, dp)
   end do
   icy(:, 1) = aicv
   icy(:, 2) = bicv
   legends_ic(1) = "AIC"
   legends_ic(2) = "BIC"
   write (ttl, "(a,a,a)") "cpfitaic information criteria (", trim(mode_), ")"
   call gplot(icm, icy, title=trim(ttl), xlabel="max_cp", legend_labels=legends_ic)
   deallocate (icm, icy, legends_ic)
end if

contains

   subroutine fitted_means_from_cpfit(v, mode_s, yhat)
      real(kind=dp), intent(in) :: v(:)
      character(len=*), intent(in) :: mode_s
      real(kind=dp), intent(out) :: yhat(:)
      integer :: nseg, ncp_l, j
      integer, allocatable :: cps_l(:)
      real(kind=dp), allocatable :: mu_l(:)

      ncp_l = max(0, nint(v(1)))
      nseg = ncp_l + 1
      allocate (cps_l(ncp_l), mu_l(nseg))
      if (ncp_l > 0) cps_l = nint(v(2:ncp_l + 1))
      if (trim(mode_s) == "sd") then
         mu_l = v(ncp_l + 2)
      else
         mu_l = v(ncp_l + 2:2*ncp_l + 2)
      end if

      lo = 1
      do j = 1, nseg
         if (j <= ncp_l) then
            hi = min(size(yhat), max(lo, cps_l(j)))
         else
            hi = size(yhat)
         end if
         if (hi >= lo) yhat(lo:hi) = mu_l(j)
         lo = hi + 1
      end do
      if (lo <= size(yhat)) yhat(lo:) = mu_l(nseg)
      deallocate (cps_l, mu_l)
   end subroutine fitted_means_from_cpfit

   subroutine print_model_table(maxcp_s, v, mode_s)
      integer, intent(in) :: maxcp_s
      real(kind=dp), intent(in) :: v(:)
      character(len=*), intent(in) :: mode_s
      integer :: ncp_l, nseg, j, lo_l, hi_l
      integer, allocatable :: cps_l(:)
      real(kind=dp), allocatable :: mu_l(:), sd_l(:)
      real(kind=dp) :: aic_l, bic_l

      ncp_l = max(0, nint(v(1)))
      nseg = ncp_l + 1
      allocate (cps_l(ncp_l), mu_l(nseg), sd_l(nseg))
      if (ncp_l > 0) cps_l = nint(v(2:ncp_l + 1))
      select case (trim(mode_s))
      case ("mean")
         mu_l = v(ncp_l + 2:2*ncp_l + 2)
         sd_l = v(2*ncp_l + 3)
      case ("sd")
         mu_l = v(ncp_l + 2)
         sd_l = v(ncp_l + 3:2*ncp_l + 2)
      case default
         mu_l = v(ncp_l + 2:2*ncp_l + 2)
         sd_l = v(2*ncp_l + 3:3*ncp_l + 3)
      end select
      aic_l = v(size(v) - 1)
      bic_l = v(size(v))

      print *
      print "(a,i0,a,i0,a,f12.4,a,f12.4)", "model max_cp=", maxcp_s, "  ncp=", ncp_l, "  AIC=", aic_l, "  BIC=", bic_l
      if (ncp_l > 0) then
         write (*, "(a)", advance="no") "changepoints: "
         do j = 1, ncp_l
            if (j > 1) write (*, "(a)", advance="no") ", "
            write (*, "(i0)", advance="no") cps_l(j)
         end do
         print *
      else
         print *, "changepoints: none"
      end if
      print "(a8,a8,a8,a14,a14)", "segment", "start", "end", "mean", "sd"
      lo_l = 1
      do j = 1, nseg
         if (j <= ncp_l) then
            hi_l = cps_l(j)
         else
            hi_l = n
         end if
         print "(i8,i8,i8,2f14.6)", j, lo_l, hi_l, mu_l(j), sd_l(j)
         lo_l = hi_l + 1
      end do

      deallocate (cps_l, mu_l, sd_l)
   end subroutine print_model_table

end function cpfitaic

function cpfit_aic(x, mode, max_cp, minseg, criterion, plot, plot_ic, verbose) result(best_out)
! Backward-compatible alias.
real(kind=dp), intent(in) :: x(:)
character(len=*), intent(in), optional :: mode, criterion
integer, intent(in), optional :: max_cp, minseg, plot, plot_ic, verbose
real(kind=dp), allocatable :: best_out(:)
best_out = cpfitaic(x, mode=mode, max_cp=max_cp, minseg=minseg, criterion=criterion, plot=plot, plot_ic=plot_ic, verbose=verbose)
end function cpfit_aic

subroutine distaicscan(x, verbose)
! Fit sensible distributions to x and print AIC ranking table.
real(kind=dp), intent(in) :: x(:)
integer, intent(in), optional :: verbose
integer, parameter :: mmax = 20
character(len=16) :: names(mmax), tmpn
real(kind=dp) :: aicv(mmax), llv(mmax)
real(kind=dp), allocatable :: p(:), fx(:)
logical :: do_verbose
integer :: m, i, j
real(kind=dp) :: ta, tl

if (size(x) < 2) then
   print *, "Error: distaicscan() requires size(x) > 1"
   return
end if
do_verbose = .true.
if (present(verbose)) do_verbose = (verbose /= 0)
m = 0

! normal
p = fit_norm(x); fx = dnorm(x, p(1), p(2)); call add_fit("norm", p, fx)
! t
p = fit_t(x); fx = dt((x - p(1)) / p(2), p(3)) / p(2); call add_fit("t", p, fx)
! nct
p = fit_nct(x); fx = dnct(x, p(1), p(2)); call add_fit("nct", p, fx)
! logistic
p = fit_logis(x); fx = dlogis(x, p(1), p(2)); call add_fit("logis", p, fx)
! sech
p = fit_sech(x); fx = dsech((x - p(1)) / p(2)) / p(2); call add_fit("sech", p, fx)
! laplace
p = fit_laplace(x); fx = dlaplace(x, p(1), p(2)); call add_fit("laplace", p, fx)
! cauchy
p = fit_cauchy(x); fx = dcauchy(x, p(1), p(2)); call add_fit("cauchy", p, fx)
! ged
p = fit_ged(x); fx = dged(x, p(1), p(2), p(3)); call add_fit("ged", p, fx)
! hyperbolic
p = fit_hyperb(x); fx = dhyperb(x, p(1), p(2), p(3)); call add_fit("hyperb", p, fx)

if (all(x >= 0.0_dp)) then
   p = fit_exp(x); fx = dexp(x, p(1)); call add_fit("exp", p, fx)
   p = fit_gamma(x); fx = dgamma(x, p(1), p(2)); call add_fit("gamma", p, fx)
   p = fit_chisq(x); fx = dchisq(x, p(1)); call add_fit("chisq", p, fx)
end if
if (all(x > 0.0_dp)) then
   p = fit_lnorm(x); fx = dlnorm(x, p(1), p(2)); call add_fit("lnorm", p, fx)
end if
if (all(x > 0.0_dp .and. x < 1.0_dp)) then
   p = fit_beta(x); fx = dbeta(x, p(1), p(2)); call add_fit("beta", p, fx)
end if
if (all(x > 0.0_dp)) then
   p = fit_f(x); fx = df(x, p(1), p(2)); call add_fit("f", p, fx)
end if

if (m < 1) then
   print *, "No compatible fitted distributions"
   return
end if

! sort by ascending AIC
do i = 1, m - 1
   do j = i + 1, m
      if (aicv(j) < aicv(i)) then
         ta = aicv(i); aicv(i) = aicv(j); aicv(j) = ta
         tl = llv(i); llv(i) = llv(j); llv(j) = tl
         tmpn = names(i); names(i) = names(j); names(j) = tmpn
      end if
   end do
end do

if (do_verbose) then
   print "(a16,a18,a18)", "distribution", "logLik", "AIC"
   do i = 1, m
      print "(a16,2f18.6)", trim(names(i)), llv(i), aicv(i)
   end do
else
   print "(a,a)", "AIC best: ", trim(names(1))
end if

contains
   subroutine add_fit(nm, pars, dens)
      character(len=*), intent(in) :: nm
      real(kind=dp), intent(in) :: pars(:)
      real(kind=dp), intent(in) :: dens(:)
      real(kind=dp) :: ll, aic
      if (size(dens) /= size(x)) return
      if (any(dens <= 0.0_dp) .or. any(dens /= dens)) return
      ll = sum(log(dens))
      aic = -2.0_dp * ll + 2.0_dp * real(size(pars), dp)
      if (m < mmax) then
         m = m + 1
         names(m) = nm
         aicv(m) = aic
         llv(m) = ll
      end if
   end subroutine add_fit
end subroutine distaicscan

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

elemental function tcdf(t, df) result(p)
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

elemental function betai(a, b, x) result(bt)
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

elemental function chisq_cdf(x, df) result(p)
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

elemental function gammp(a, x) result(gp)
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

elemental function gser(a, x, gln) result(gser_out)
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

elemental function gcf(a, x, gln) result(gcf_out)
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

elemental function betacf(a, b, x) result(cf)
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
