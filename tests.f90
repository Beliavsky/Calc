program session
  use kind_mod, only: dp
  use stats_mod, only: mssk, mssk_nct
  use random_mod, only: rnct
  implicit none
  real(kind=dp), allocatable :: print(:), x(:)

   x = rnct(1000, 8.0_dp, 1.5_dp)
   print = mssk(x)
   print = mssk_nct(8.0_dp, 1.5_dp)
   print = mssk_nct(8.0_dp)
end program session
