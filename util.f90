module util_mod
use kind_mod, only: dp
implicit none
private
public :: matched_parentheses, matched_brackets, arange, irange, head, &
   tail, grid, print_real, replace, is_numeral, is_letter, &
   is_alphanumeric, zeros, ones, windows, rep, matrix, read_vec, &
   reverse, runif1, polyroots, lowercase

interface rep
   module procedure rep_vec
end interface rep

interface arange
   module procedure arange1
   module procedure arange2
   module procedure arange3
end interface arange

interface irange
   module procedure irange1
   module procedure irange2
   module procedure irange3
end interface irange

interface polyroots
   module procedure polyroots_real
   module procedure polyroots_int
end interface polyroots

contains

function runif1() result(y)
real(kind=dp) :: y
call random_number(y)
end function runif1

function polyroots_real(a) result(r)
! roots of polynomial a(1) + a(2)*x + ... + a(n)*x^(n-1)
! returned as [Re(z1), Im(z1), Re(z2), Im(z2), ...]
real(kind=dp), intent(in) :: a(:)
real(kind=dp), allocatable :: r(:)
complex(kind=dp), allocatable :: coef(:), z(:), znew(:)
complex(kind=dp) :: pval, denom, delta
real(kind=dp), parameter :: eps = 1.0e-12_dp, tol = 1.0e-10_dp
integer, parameter :: max_iter = 200
real(kind=dp) :: twopi, angle, maxc, radius, delta_max
integer :: i, j, iter, ilo, ihi, nred, nzero, ntotal, idx

if (size(a) < 2) then
   allocate (r(0))
   return
end if

ilo = 0
do i = 1, size(a)
   if (abs(a(i)) > eps) then
      ilo = i
      exit
   end if
end do
if (ilo == 0) then
   allocate (r(0))
   return
end if

ihi = 0
do i = size(a), 1, -1
   if (abs(a(i)) > eps) then
      ihi = i
      exit
   end if
end do
if (ihi <= 1) then
   allocate (r(0))
   return
end if

ntotal = ihi - 1
nzero = ilo - 1
nred = ihi - ilo
allocate (r(2*ntotal))
r = 0.0_dp
if (nred < 1) return

allocate (coef(0:nred))
do i = 0, nred
   coef(i) = cmplx(a(ilo + i), 0.0_dp, kind=dp)
end do
coef = coef / coef(nred)

maxc = 0.0_dp
do i = 0, nred - 1
   maxc = max(maxc, abs(coef(i)))
end do
radius = 1.0_dp + maxc
twopi = 2.0_dp*acos(-1.0_dp)

allocate (z(nred), znew(nred))
do i = 1, nred
   angle = twopi*real(i - 1, dp)/real(nred, dp)
   z(i) = cmplx(radius*cos(angle), radius*sin(angle), kind=dp)
end do

do iter = 1, max_iter
   delta_max = 0.0_dp
   do i = 1, nred
      pval = poly_eval_complex(coef, nred, z(i))
      denom = cmplx(1.0_dp, 0.0_dp, kind=dp)
      do j = 1, nred
         if (j /= i) denom = denom*(z(i) - z(j))
      end do
      if (abs(denom) < eps) denom = denom + cmplx(eps, eps, kind=dp)
      delta = pval/denom
      znew(i) = z(i) - delta
      delta_max = max(delta_max, abs(delta))
   end do
   z = znew
   if (delta_max < tol) exit
end do

idx = 1
do i = 1, nzero
   r(idx) = 0.0_dp
   r(idx + 1) = 0.0_dp
   idx = idx + 2
end do
do i = 1, nred
   r(idx) = real(z(i), kind=dp)
   r(idx + 1) = aimag(z(i))
   idx = idx + 2
end do
end function polyroots_real

function polyroots_int(a) result(r)
integer, intent(in) :: a(:)
real(kind=dp), allocatable :: r(:)
r = polyroots_real(real(a, kind=dp))
end function polyroots_int

pure function poly_eval_complex(c, n, z) result(v)
complex(kind=dp), intent(in) :: c(0:)
integer, intent(in) :: n
complex(kind=dp), intent(in) :: z
complex(kind=dp) :: v
integer :: i
v = c(n)
do i = n - 1, 0, -1
   v = v*z + c(i)
end do
end function poly_eval_complex

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

pure function arange1(n) result(vec)
! return an array of 1.0 through n inclusive
integer, intent(in) :: n
real(kind=dp) :: vec(n)
integer :: i
do i=1,n
   vec(i) = real(i, kind=dp)
end do
end function arange1

pure function arange2(start, stop) result(vec)
! return an array from start to stop inclusive with step 1.0
real(kind=dp), intent(in) :: start, stop
real(kind=dp), allocatable :: vec(:)
vec = arange3(start, stop, 1.0_dp)
end function arange2

pure function arange3(start, stop, step) result(vec)
! return an array from start to stop inclusive with given step
real(kind=dp), intent(in) :: start, stop, step
real(kind=dp), allocatable :: vec(:)
real(kind=dp) :: val, eps
integer :: n, i

eps = 1.0e-12_dp
if (step == 0.0_dp) then
   allocate (vec(0))
   return
end if
if ((step > 0.0_dp .and. stop < start) .or. (step < 0.0_dp .and. stop > start)) then
   allocate (vec(0))
   return
end if

n = 0
val = start
if (step > 0.0_dp) then
   do while (val <= stop + eps)
      n = n + 1
      val = val + step
   end do
else
   do while (val >= stop - eps)
      n = n + 1
      val = val + step
   end do
end if

allocate (vec(n))
val = start
do i = 1, n
   vec(i) = val
   val = val + step
end do
end function arange3

pure function irange1(n) result(vec)
! return an integer array of 1 through n inclusive
integer, intent(in) :: n
integer :: vec(n)
integer :: i
do i = 1, n
   vec(i) = i
end do
end function irange1

pure function irange2(start, stop) result(vec)
! return an integer array from start to stop inclusive with step 1
integer, intent(in) :: start, stop
integer, allocatable :: vec(:)
vec = irange3(start, stop, 1)
end function irange2

pure function irange3(start, stop, step) result(vec)
! return an integer array from start to stop inclusive with given step
integer, intent(in) :: start, stop, step
integer, allocatable :: vec(:)
integer :: val, n, i

if (step == 0) then
   allocate (vec(0))
   return
end if
if ((step > 0 .and. stop < start) .or. (step < 0 .and. stop > start)) then
   allocate (vec(0))
   return
end if

n = 0
val = start
if (step > 0) then
   do while (val <= stop)
      n = n + 1
      val = val + step
   end do
else
   do while (val >= stop)
      n = n + 1
      val = val + step
   end do
end if

allocate (vec(n))
val = start
do i = 1, n
   vec(i) = val
   val = val + step
end do
end function irange3


pure function grid(n, x0, xh) result(vec)
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

pure function replace(string, old, new) result(string_new)
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

pure function rep_vec(x, n) result(y)
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

pure function matrix(x) result(xmat)
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
   allocate(x(0))
!–––– open the file ––––––––––––––––––––––––––––––––––––––––––––––––––––––––
   open(newunit=u, file=trim(file), action='read', status='old', iostat=ios)
   if (ios /= 0) then
      write(*,'("Error: cannot open file ''",a,"'' (iostat=",i0,")")') trim(file), ios
      return
   end if

!–––– initialise ––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
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

      ! attempt to read ICOL-th real value
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
   if (size(x) == 0) then
      print "(a,i0,a)", "could not read real data from column ", ic, &
      " of file " // trim(file)
   end if
end subroutine read_vec

pure function reverse(arr) result(res)
    real(kind=dp), intent(in) :: arr(:)
    real(kind=dp), allocatable :: res(:)
    integer :: n
    n = size(arr)
    allocate(res(n))
    if (n > 0) res = arr(n:1:-1)
end function reverse

pure function lowercase(s) result(out)
! convert ASCII letters to lowercase
character(len=*), intent(in) :: s
character(len=len(s)) :: out
integer :: i, c
do i = 1, len(s)
   c = iachar(s(i:i))
   if (c >= iachar('A') .and. c <= iachar('Z')) then
      out(i:i) = achar(c + 32)
   else
      out(i:i) = s(i:i)
   end if
end do
end function lowercase

end module util_mod
