module stats_mod
use kind_mod, only: dp
implicit none
private
public :: mean, sd
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
end module stats_mod