module stats_mod
use kind_mod, only: dp
implicit none
private
public :: mean, sd, cor, cov
contains
function mean(x) result(mean_val)
! return the mean of x
real(kind=dp), intent(in) :: x(:)
real(kind=dp) :: mean_val
mean_val = sum(x) / size(x)
end function mean

function sd(x) result(sd_val)
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
end module stats_mod