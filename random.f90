module random_mod
  use kind_mod, only: dp
  use constants_mod, only: pi
  implicit none
  private
  public :: random_normal, random_gamma, random_student_t, &
            random_seed_init
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

end module random_mod