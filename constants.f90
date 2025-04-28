module constants_mod
use kind_mod, only: dp
implicit none
private
public :: pi, log_two_pi, log_two, sqrt_two
real(kind=dp), parameter :: &
   pi         = 3.141592653589793238462643_dp, &
   log_two_pi = 1.837877066409345483560659_dp, &
   log_two    = 0.69314718055994529_dp, &
   sqrt_two   = 1.4142135623730951_dp, &
   euler      = 0.5772156649015328606065120900824024_dp, &
   polygamma_one_half = 4.93480220054468_dp, & ! polygamma(1, 0.5), &
   psi_half = -1.9635100260214235_dp ! psi(0.5) from SciPy
end module constants_mod
