module random_mod
  use kind_mod, only: dp
  use constants_mod, only: pi, one_over_sqrt_two
  use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
  implicit none
  private
  public :: noise_stdz, random_normal, random_gamma, random_student_t, &
     random_seed_init, runif, rexp, rgamma, rlnorm, rt, rnct, rmixnorm, &
     mixnoise, rchisq, rf, rbeta, rlogis, rsech, rlaplace, rcauchy, rged, &
     str_normal, str_sech, str_logistic, str_laplace, str_student_t, str_ged, &
     str_beta, str_cauchy, str_chisq, str_exp, str_f, str_gamma, str_hyperb, &
     str_lognormal
  interface runif
    module procedure runif_scalar, runif_vec, runif_mat
  end interface runif
  interface random_normal
     module procedure random_normal_scalar, random_normal_vec, &
                      random_normal_mat
  end interface random_normal
  interface random_student_t
     module procedure random_student_t_scalar, random_student_t_vec
  end interface random_student_t
  ! For Box-Muller: storage for the second variate.
  logical, save :: have_saved = .false.
  real(kind=dp), save :: saved_value = 0.0_dp
  character (len=*), parameter :: str_normal="normal", str_sech="sech", &
     str_logistic="logis", str_laplace="laplace", str_student_t="t", &
     str_ged="ged", str_cauchy="cauchy", str_hyperb="hyperb", str_exp="exp", &
     str_gamma="gamma", str_chisq="chisq", str_lognormal="lnorm", &
     str_beta="beta", str_f="f"
contains

  function random_normal_scalar() result(z)
  ! Returns a standard normal variate using the Box–Muller method.
    real(kind=dp) :: z
    real(kind=dp) :: u1, u2
    if (have_saved) then
      z = saved_value
      have_saved = .false.
    else
      call random_number(u1)
      call random_number(u2)
      if (u1 == 0.0_dp) u1 = 1.0e-10_dp
      z = sqrt(-2.0_dp * log(u1)) * cos(2.0_dp * pi * u2)
      saved_value = sqrt(-2.0_dp * log(u1)) * sin(2.0_dp * pi * u2)
      have_saved = .true.
    end if
  end function random_normal_scalar

function random_normal_vec(n) result(z)
integer, intent(in) :: n
real(kind=dp)       :: z(n)
integer             :: i
do i=1,n
   z(i) = random_normal_scalar()
end do
end function random_normal_vec

function random_normal_mat(n1, n2) result(z)
integer, intent(in) :: n1, n2
real(kind=dp)       :: z(n1, n2)
integer             :: i2
do i2=1,n2
   z(:,i2) = random_normal_vec(n1)
end do
end function random_normal_mat

  !------------------------------------------------------------
  ! Function: random_gamma
  !
  ! Returns a Gamma random variate with shape parameter 'a'
  ! and scale parameter 'b' using the Marsaglia–Tsang algorithm.
  ! For a < 1 the algorithm uses the standard transformation.
  !------------------------------------------------------------
  recursive function random_gamma(a, b) result(x)
    real(kind=dp), intent(in) :: a, b
    real(kind=dp) :: x, d, c, u, v, z
    if (a < 1.0_dp) then
       ! Use the property Gamma(a) = Gamma(a+1)*U^(1/a)
       x = random_gamma(a + 1.0_dp, b)
       call random_number(u)
       x = x * u**(1.0_dp / a)
       return
    else
       d = a - 1.0_dp/3.0_dp
       c = 1.0_dp / sqrt(9.0_dp * d)
       do
         z = random_normal()
         v = (1.0_dp + c * z)**3
         if (v <= 0.0_dp) cycle
         call random_number(u)
         if (log(u) < 0.5_dp * z**2 + d - d*v + d * log(v)) then
            x = d * v * b
            return
         end if
       end do
    end if
  end function random_gamma

  !------------------------------------------------------------
  ! Function: random_student_t
  !
  ! Returns a Student t variate with degrees-of-freedom 'dof'
  ! (of type real(kind=dp)).  This is achieved by generating
  ! Z ~ N(0,1) and V ~ chi-square(dof) (via Gamma(dof/2,2)),
  ! then computing t = Z/sqrt(V/dof).
  !------------------------------------------------------------
  function random_student_t_scalar(dof) result(t)
    real(kind=dp), intent(in) :: dof
    real(kind=dp) :: t, z, v
    z = random_normal()
    v = random_gamma(dof/2.0_dp, 2.0_dp)
    t = z / sqrt(v / dof)
  end function random_student_t_scalar

  function random_student_t_vec(n, dof) result(t)
    integer, intent(in) :: n
    real(kind=dp), intent(in) :: dof
    real(kind=dp) :: t(n)
    integer :: i
    do i=1,n
       t(i) = random_student_t_scalar(dof)
    end do
  end function random_student_t_vec

subroutine random_seed_init(iseed, nburn, print_seed)
! Initializes the random number generator seed by setting each seed element 
! to a predefined value plus iseed.
integer, intent(in) :: iseed
integer, intent(in), optional :: nburn
logical, intent(in), optional :: print_seed
integer, allocatable :: seed(:)
integer :: seed_size, i
real(kind=dp) :: xran
call random_seed(size=seed_size)
allocate(seed(seed_size))
do i = 1, seed_size
  seed(i) = int(1.0e6_dp * real(i, kind=dp)) + iseed
end do
call random_seed(put=seed)
if (present(nburn)) then
   do i=1,nburn
      call random_number(xran)
   end do
end if
if (present(print_seed)) then
   if (print_seed) print "('seed vector:', *(1x,i0))", seed
end if
end subroutine random_seed_init

function runif_scalar() result(r)
real(kind=dp) :: r
call random_number(r)
end function runif_scalar

function runif_vec(n) result(r)
integer, intent(in)  :: n
real(kind=dp), allocatable :: r(:)
if (n < 1) then
  allocate(r(0))
else 
  allocate(r(n))
  call random_number(r)
end if
end function runif_vec

function runif_mat(n1,n2) result(r)
integer, intent(in)  :: n1,n2
real(kind=dp), allocatable :: r(:,:)
allocate (r(n1,n2))
call random_number(r)
end function runif_mat

function rexp(n, rate) result(r)
integer, intent(in) :: n
real(kind=dp), intent(in), optional :: rate
real(kind=dp), allocatable :: r(:)
integer :: i
real(kind=dp) :: u, rate_
if (n < 1) then
  allocate(r(0))
  return
end if
if (present(rate)) then
   rate_ = rate
else
   rate_ = 1.0_dp
end if
allocate(r(n))
do i=1,n
  call random_number(u)
  if (u <= 0.0_dp) u = 1.0e-12_dp
  r(i) = -log(u) / rate_
end do
end function rexp

function rgamma(n, shape, scale) result(r)
integer, intent(in) :: n
real(kind=dp), intent(in) :: shape, scale
real(kind=dp), allocatable :: r(:)
integer :: i
if (n < 1) then
  allocate(r(0))
  return
end if
allocate(r(n))
do i=1,n
  r(i) = random_gamma(shape, scale)
end do
end function rgamma

function rlnorm(n, meanlog, sdlog) result(r)
integer, intent(in) :: n
real(kind=dp), intent(in) :: meanlog, sdlog
real(kind=dp), allocatable :: r(:)
integer :: i
if (n < 1) then
  allocate(r(0))
  return
end if
allocate(r(n))
do i=1,n
  r(i) = exp(meanlog + sdlog * random_normal())
end do
end function rlnorm

function rt(n, df) result(r)
integer, intent(in) :: n
real(kind=dp), intent(in) :: df
real(kind=dp), allocatable :: r(:)
integer :: i
if (n < 1) then
  allocate(r(0))
  return
end if
allocate(r(n))
do i=1,n
  r(i) = random_student_t(df)
end do
end function rt

function rnct(n, df, ncp) result(r)
integer, intent(in) :: n
real(kind=dp), intent(in) :: df, ncp
real(kind=dp), allocatable :: r(:)
integer :: i
real(kind=dp) :: z, v
if (n < 1) then
  allocate(r(0))
  return
end if
allocate(r(n))
do i=1,n
  z = random_normal() + ncp
  v = random_gamma(df/2.0_dp, 2.0_dp)
  r(i) = z / sqrt(v / df)
end do
end function rnct

function rmixnorm(n, wgt, mean, sd) result(r)
integer, intent(in) :: n
real(kind=dp), intent(in) :: wgt(:), mean(:), sd(:)
real(kind=dp), allocatable :: r(:)
real(kind=dp), allocatable :: wn(:), cw(:)
real(kind=dp) :: sw, u
integer :: i, j, k, comp
if (n < 1) then
  allocate(r(0))
  return
end if
k = size(wgt)
allocate(r(n))
if (k < 1 .or. size(mean) /= k .or. size(sd) /= k) then
  r = ieee_value(0.0_dp, ieee_quiet_nan)
  return
end if
if (any(wgt < 0.0_dp) .or. any(sd <= 0.0_dp)) then
  r = ieee_value(0.0_dp, ieee_quiet_nan)
  return
end if
sw = sum(wgt)
if (sw <= 0.0_dp) then
  r = ieee_value(0.0_dp, ieee_quiet_nan)
  return
end if
allocate(wn(k), cw(k))
wn = wgt / sw
cw(1) = wn(1)
do j = 2, k
  cw(j) = cw(j - 1) + wn(j)
end do
cw(k) = 1.0_dp
do i = 1, n
  call random_number(u)
  comp = k
  do j = 1, k
    if (u <= cw(j)) then
      comp = j
      exit
    end if
  end do
  r(i) = mean(comp) + sd(comp) * random_normal()
end do
deallocate(wn, cw)
end function rmixnorm

function mixnoise(noise, wgt, mean, sd) result(r)
real(kind=dp), intent(in) :: noise(:)
real(kind=dp), intent(in) :: wgt(:), mean(:), sd(:)
real(kind=dp), allocatable :: r(:)
real(kind=dp), allocatable :: wn(:), cw(:)
real(kind=dp) :: sw, u
integer :: i, j, k, n, comp
n = size(noise)
if (n < 1) then
  allocate(r(0))
  return
end if
k = size(wgt)
allocate(r(n))
if (k < 1 .or. size(mean) /= k .or. size(sd) /= k) then
  r = ieee_value(0.0_dp, ieee_quiet_nan)
  return
end if
if (any(wgt < 0.0_dp) .or. any(sd <= 0.0_dp)) then
  r = ieee_value(0.0_dp, ieee_quiet_nan)
  return
end if
sw = sum(wgt)
if (sw <= 0.0_dp) then
  r = ieee_value(0.0_dp, ieee_quiet_nan)
  return
end if
allocate(wn(k), cw(k))
wn = wgt / sw
cw(1) = wn(1)
do j = 2, k
  cw(j) = cw(j - 1) + wn(j)
end do
cw(k) = 1.0_dp
do i = 1, n
  call random_number(u)
  comp = k
  do j = 1, k
    if (u <= cw(j)) then
      comp = j
      exit
    end if
  end do
  r(i) = mean(comp) + sd(comp) * noise(i)
end do
deallocate(wn, cw)
end function mixnoise

function rchisq(n, df) result(r)
integer, intent(in) :: n
real(kind=dp), intent(in) :: df
real(kind=dp), allocatable :: r(:)
integer :: i
if (n < 1) then
  allocate(r(0))
  return
end if
allocate(r(n))
do i=1,n
  r(i) = random_gamma(df/2.0_dp, 2.0_dp)
end do
end function rchisq

function rf(n, df1, df2) result(r)
integer, intent(in) :: n
real(kind=dp), intent(in) :: df1, df2
real(kind=dp), allocatable :: r(:)
integer :: i
real(kind=dp) :: x1, x2
if (n < 1) then
  allocate(r(0))
  return
end if
allocate(r(n))
do i=1,n
  x1 = random_gamma(df1/2.0_dp, 2.0_dp) / df1
  x2 = random_gamma(df2/2.0_dp, 2.0_dp) / df2
  r(i) = x1 / x2
end do
end function rf

function rbeta(n, a, b) result(r)
integer, intent(in) :: n
real(kind=dp), intent(in) :: a, b
real(kind=dp), allocatable :: r(:)
integer :: i
real(kind=dp) :: x1, x2
if (n < 1) then
  allocate(r(0))
  return
end if
allocate(r(n))
do i=1,n
  x1 = random_gamma(a, 1.0_dp)
  x2 = random_gamma(b, 1.0_dp)
  r(i) = x1 / (x1 + x2)
end do
end function rbeta

function rlogis(n, loc, scale) result(r)
integer, intent(in) :: n
real(kind=dp), intent(in) :: loc, scale
real(kind=dp), allocatable :: r(:)
integer :: i
real(kind=dp) :: u
if (n < 1) then
  allocate(r(0))
  return
end if
allocate(r(n))
do i=1,n
  call random_number(u)
  if (u <= 0.0_dp) u = 1.0e-12_dp
  if (u >= 1.0_dp) u = 1.0_dp - 1.0e-12_dp
  r(i) = loc + scale * log(u / (1.0_dp - u))
end do
end function rlogis

function rsech(n) result(r)
integer, intent(in) :: n
real(kind=dp), allocatable :: r(:)
integer :: i
real(kind=dp) :: u
if (n < 1) then
  allocate(r(0))
  return
end if
allocate(r(n))
do i=1,n
  call random_number(u)
  if (u <= 0.0_dp) u = 1.0e-12_dp
  if (u >= 1.0_dp) u = 1.0_dp - 1.0e-12_dp
  r(i) = (2.0_dp / pi) * log(tan(0.5_dp * pi * u))
end do
end function rsech

function rlaplace(n, loc, scale) result(r)
integer, intent(in) :: n
real(kind=dp), intent(in) :: loc, scale
real(kind=dp), allocatable :: r(:)
integer :: i
real(kind=dp) :: u
if (n < 1) then
  allocate(r(0))
  return
end if
allocate(r(n))
do i=1,n
  call random_number(u)
  if (u < 0.5_dp) then
     r(i) = loc + scale * log(2.0_dp * u)
  else
     r(i) = loc - scale * log(2.0_dp * (1.0_dp - u))
  end if
end do
end function rlaplace

function rcauchy(n, loc, scale) result(r)
integer, intent(in) :: n
real(kind=dp), intent(in) :: loc, scale
real(kind=dp), allocatable :: r(:)
integer :: i
real(kind=dp) :: u
if (n < 1) then
  allocate(r(0))
  return
end if
allocate(r(n))
do i=1,n
  call random_number(u)
  if (u <= 0.0_dp) u = 1.0e-12_dp
  if (u >= 1.0_dp) u = 1.0_dp - 1.0e-12_dp
  r(i) = loc + scale * tan(pi * (u - 0.5_dp))
end do
end function rcauchy

function rged(n, loc, scale, beta) result(r)
integer, intent(in) :: n
real(kind=dp), intent(in) :: loc, scale, beta
real(kind=dp), allocatable :: r(:)
integer :: i
real(kind=dp) :: u, g, sgn, a
if (n < 1) then
  allocate(r(0))
  return
end if
allocate(r(n))
a = 1.0_dp / beta
do i=1,n
  call random_number(u)
  if (u < 0.5_dp) then
     sgn = -1.0_dp
  else
     sgn = 1.0_dp
  end if
  g = random_gamma(a, 1.0_dp)
  r(i) = loc + scale * sgn * g**(1.0_dp / beta)
end do
end function rged

function noise_stdz(n, dist, df, beta) result(z)
! return n standard variates from specified distribution
integer, intent(in) :: n ! # of variates
character (len=*), intent(in) :: dist ! probability distribution
real(kind=dp), intent(in), optional :: df ! degrees-of-freedom of Student t
real(kind=dp), intent(in), optional :: beta ! shape parameter for generalized error distribution (GED)
real(kind=dp) :: z(n)
select case (dist)
   case (str_normal)   ; z = random_normal(n)
   case (str_sech)     ; z = rsech(n)
   case (str_logistic) ; z = rlogis(n, 0.0_dp, scale=0.55132889542179209_dp) ! constant is sqrt(3.0_dp/(pi**2)
   case (str_laplace)  ; z = rlaplace(n, 0.0_dp, one_over_sqrt_two)
   case (str_student_t)
      if (.not. present(df)) error stop "need df for t variates in noise_stdz"
      if (df > 2.0_dp) then 
         z = sqrt((df-2)/df) * rt(n, df)
      else
         z = rt(n, df)
         if (n > 1) z = z/sqrt(sum(z**2)/n)
      end if
   case (str_ged)
      z = rged(n, loc=0.0_dp, scale=sqrt(gamma(1.0_dp/beta)/gamma(3.0_dp/beta)), &
               beta=beta)      
end select
end function noise_stdz

end module random_mod
