module util_mod
use kind_mod, only: dp
implicit none
private
public :: matched_parentheses, matched_brackets, arange, head, &
   tail, grid
contains

elemental logical function matched_parentheses(s) result(is_valid)
!> Returns .true. if parentheses in input string are balanced
  character(len=*), intent(in) :: s
  integer :: balance, i
  balance = 0
  is_valid = .false.
  do i = 1, len_trim(s)
    select case (s(i:i))
    case ("(")
      balance = balance + 1
    case (")")
      balance = balance - 1
      if (balance < 0) return
    end select
  end do
  is_valid = balance == 0
end function matched_parentheses

elemental logical function matched_brackets(s) result(is_valid)
!> Returns .true. if parentheses in input string are balanced
  character(len=*), intent(in) :: s
  integer :: balance, i
  balance = 0
  is_valid = .false.
  do i = 1, len_trim(s)
    select case (s(i:i))
    case ("[")
      balance = balance + 1
    case ("]")
      balance = balance - 1
      if (balance < 0) return
    end select
  end do
  is_valid = balance == 0
end function matched_brackets

function arange(n) result(vec)
integer, intent(in) :: n
real(kind=dp) :: vec(n)
integer :: i
do i=1,n
   vec(i) = real(i, kind=dp)
end do
end function arange

function grid(n, x0, xh) result(vec)
integer, intent(in) :: n
real(kind=dp) :: vec(n)
real(kind=dp), intent(in) :: x0, xh
integer :: i
if (n < 1) return
vec(1) = x0
do i=2,n
   vec(i) = vec(i-1) + xh
end do
end function grid

pure function head(x, n) result(y)
!=====================================================================
!  First n (default 5) elements of a real(kind=dp) vector
!---------------------------------------------------------------------
   real(kind=dp), intent(in)          :: x(:)
   integer,        intent(in), optional :: n
   real(kind=dp), allocatable         :: y(:)
   integer :: n_                       ! number of elements to return
   if (present(n)) then
      n_ = n
   else
      n_ = 5
   end if
   n_ = min(max(n_,0), size(x))         ! clamp to [0, size(x)]

   allocate(y(n_))
   if (n_ > 0) y = x(:n_)
end function head

pure function tail(x, n) result(y)
!=====================================================================
!  Last n (default 5) elements of a real(kind=dp) vector
!---------------------------------------------------------------------
   real(kind=dp), intent(in)          :: x(:)
   integer,        intent(in), optional :: n
   real(kind=dp), allocatable         :: y(:)
   integer :: n_, first                 ! number to return and first index
   if (present(n)) then
      n_ = n
   else
      n_ = 5
   end if

   n_ = min(max(n_,0), size(x))

   first = size(x) - n_ + 1
   allocate(y(n_))
   if (n_ > 0) y = x(first:)
end function tail

end module util_mod
