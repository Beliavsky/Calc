module util_mod
use kind_mod, only: dp
implicit none
private
public :: matched_parentheses, matched_brackets, arange, head, &
   tail, grid, print_real, replace, is_numeral, is_letter, &
   is_alphanumeric, zeros, ones, windows, rep, matrix, read_vec

interface rep
   module procedure rep_vec
end interface rep

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
! test if the operating system is Windows by checking if the path starts with /
logical :: tf
character (len=1000) :: pathstring
call get_environment_variable("PATH", pathstring)
tf = pathstring(1:1) /= "/"
end function windows

function rep_vec(x, n) result(y)
! repeat a 1D array to get a new 1D array
real(kind=dp), intent(in)  :: x(:)  ! array to copy
integer      , intent(in)  :: n     ! number of copies
real(kind=dp), allocatable :: y(:)
integer :: i, j, nx, ny
nx = size(x)
ny = n*nx
if (ny < 1) then
   allocate (y(0))
   return
end if
allocate (y(ny))
j = 1
do i=1,n
   y(j:j+nx-1) = x
   j = j + nx
end do
end function rep_vec

function matrix(x) result(xmat)
! convert scalar to 1x1 matrix
real(kind=dp), intent(in) :: x
real(kind=dp)             :: xmat(1,1)
xmat = x
end function matrix

subroutine read_vec(file, x, icol)
!─────────────────────────────────────────────────────────────────────────────
!  Read the real-valued ICOL-th column of text file FILE into X(:).
!  Leading "header" lines that cannot be read as reals are ignored.
!  Reading stops when, after data have started, a record is encountered
!  from which the ICOL-th real value cannot be obtained.
!
   character(len=*),               intent(in)  :: file
   real   (kind=dp), allocatable,  intent(out) :: x(:)
   integer,               optional, intent(in) :: icol

   integer            :: u, ios, j, ic, n
   character(len=1000) :: line          ! complete input record
   real(dp)           :: val
   character (len=1)  :: dummy
   logical            :: found_data
   real(dp), allocatable :: tmp(:)
   integer            :: comment_pos

!–––– column choice ––––––––––––––––––––––––––––––––––––––––––––––––––––––––
   if (present(icol)) then
      ic = icol
   else
      ic = 1
   end if

!–––– open the file ––––––––––––––––––––––––––––––––––––––––––––––––––––––––
   open(newunit=u, file=trim(file), action='read', status='old', iostat=ios)
   if (ios /= 0) then
      write(*,'("Error: cannot open file ''",a,"'' (iostat=",i0,")")') trim(file), ios
      allocate(x(0));  return
   end if

!–––– initialise ––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
   allocate(x(0))
   n          = 0
   found_data = .false.

!–––– main loop ––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
   do
      read(u,'(A)', iostat=ios) line
      if (ios /= 0) exit                         ! EOF / read error → done
      if (line == '') cycle                      ! skip blank lines

      ! strip “!” comments
      comment_pos = index(line,'!')
      if (comment_pos > 0) line = line(:comment_pos-1)
      if (len_trim(line) == 0) cycle

      ! attempt to read ICOL‑th real value
      read(line,*, iostat=ios) (dummy, j=1,ic-1), val
      if (ios /= 0) then
         if (.not. found_data) then
            cycle                                ! still in the header part
         else
            exit                                 ! data had started → stop
         end if
      end if

      ! got a value – store it
      found_data = .true.
      n = n + 1
      if (allocated(tmp)) deallocate(tmp) 
      allocate(tmp(n))
      if (n > 1) tmp(1:n-1) = x
      tmp(n) = val
      call move_alloc(tmp, x)
   end do

   close(u)
end subroutine read_vec

end module util_mod
