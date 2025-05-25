module stats_mod
use kind_mod, only: dp
implicit none
private
public :: mean, sd, cor, cov, cumsum, cumprod, diff, standardize, &
          print_stats, skew, kurtosis, cummin, cummax, cummean
contains

function standardize(x) result(y)
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

function cumsum(x) result(y)
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

function cummean(x) result(y)
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

function cummin(x) result(y)
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

function cummax(x) result(y)
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

function cumprod(x) result(y)
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

function diff(x) result(y)
! return the consecutive differences of x
real(kind=dp), intent(in) :: x(:)
real(kind=dp) :: y(size(x)-1)
integer :: n
n = size(x)
if (n < 2) return
y = x(2:) - x(:n-1)
end function diff

subroutine print_stats(x)
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
