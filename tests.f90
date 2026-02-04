program session
  use kind_mod, only: dp
  use util_mod, only: arange
  use stats_mod, only: distaicscan, jb_test, kde, poly1reg, trimmean, ttest2
  use random_mod, only: random_normal
  implicit none
  real(kind=dp), allocatable :: x(:), y(:)

   x = random_normal(200)
   print *, trimmean(x, 0.1_dp)
   print *, jb_test(x)
   y = random_normal(150)
   print *, ttest2(x, y)
   call poly1reg(x, arange(200), 2)
   call distaicscan(abs(x), 1)
   print *, kde(x, 80)
end program session
