module util_mod
use kind_mod, only: dp
implicit none
private
public :: matched_parentheses, matched_brackets, arange, head, &
   tail, grid, print_real, replace, is_numeral, is_letter, &
   is_alphanumeric, zeros, ones, windows
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
! return an array of 1.0 through n inclusive
integer, intent(in) :: n
real(kind=dp) :: vec(n)
integer :: i
do i=1,n
   vec(i) = real(i, kind=dp)
end do
end function arange

function grid(n, x0, xh) result(vec)
! return a grid of n values starting at x0 with increment of xh
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
!  First n (default 5) elements of a real(kind=dp) vector
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
!  Last n (default 5) elements of a real(kind=dp) vector
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

impure elemental subroutine print_real(x)
! print real x with leading zero for abs(x) < 1, and use
! scientific notation for very large numbers
real(kind=dp), intent(in) :: x
if (abs(x) < 1.0_dp) then
   if (x >= 0) then
      print "(F8.6)", x
   else
      print "(F9.6)", x ! space for leading negative sign
   end if
else if (abs(x) > 1.0e22_dp) then ! use scientific notation
   if (x >= 0) then
      print "(ES12.6)", x
   else
      print "(ES13.6)", x ! space for leading negative sign
   end if
else
   print "(F0.6)", x
end if
end subroutine print_real

function replace(string, old, new) result(string_new)
! replace – return a copy of string with every occurrence of old replaced by new
character(len=*), intent(in) :: string, old, new
character(len=:), allocatable :: string_new
integer :: current, pos, len_old
len_old = len_trim(old)
! nothing to replace – return the original string
if (len_old == 0) then
   string_new = string
   return
end if
string_new = ""           ! start with an empty result
current    = 1
do
   pos = index(string(current:), old)
   if (pos == 0) exit
   pos = pos + current - 1
   string_new = string_new // string(current:pos-1) // new
   current    = pos + len_old
end do
string_new = string_new // string(current:)
end function replace

elemental function is_numeral(xchar) result(tf)
! return .true. if xchar is a numeral '0', '1', ..., '9'
character (len=1), intent(in) :: xchar
logical                       :: tf
tf = xchar >= '0' .and. xchar <= '9'
end function is_numeral

elemental function is_letter(xchar) result(tf)
! return .true. if xchar is a lower or upper case letter
character (len=1), intent(in) :: xchar
logical                       :: tf
tf = (xchar >= 'a' .and. xchar <= 'z') .or. &
     (xchar >= 'A' .and. xchar <= 'Z')
end function is_letter

elemental function is_alphanumeric(xchar) result(tf)
! return .true. if xchar is a numeral or letter
character (len=1), intent(in) :: xchar
logical                       :: tf
tf = is_letter(xchar) .or. is_numeral(xchar)
end function is_alphanumeric

pure function zeros(n) result(v)
! return a vector of n zeros
integer, intent(in) :: n
real(kind=dp), allocatable :: v(:)
allocate (v(n), source=0.0_dp)
end function zeros

pure function ones(n) result(v)
! return a vector of n ones
integer, intent(in) :: n
real(kind=dp), allocatable :: v(:)
allocate (v(n), source=1.0_dp)
end function ones

function windows() result(tf)
logical :: tf
character (len=1000) :: pathstring
call get_environment_variable("PATH", pathstring)
tf = pathstring(1:1) /= "/"
end function windows

end module util_mod
