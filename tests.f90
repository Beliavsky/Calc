program session
  use kind_mod, only: dp
  use stats_mod, only: cpfitaic, cpsim
  implicit none
  integer :: n
  real(kind=dp), allocatable :: r(:), r2(:), x(:)

   n = 100
   x = cpsim(n, (1.0_dp*([35.0_dp, 70.0_dp])), verbose=0)
   r = cpfitaic(x, max_cp=nint(1.0_dp*(3)), criterion="bic", plot=0)
   r2 = cpfitaic(x, max_cp=nint(1.0_dp*(3)), criterion="aic", plot=0, plot_ic=0)
end program session
