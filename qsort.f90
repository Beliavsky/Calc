module qsort_mod
   use kind_mod, only : dp
   implicit none
   private
   public :: indexx, quick_sort_in_place, quick_sort, &
             sorted, rank, median, unique

   !–––––––––––––––– generic interfaces –––––––––––––––––––––––––––––––––
   interface indexx
      module procedure indexx_real, indexx_int
   end interface indexx

   interface rank                           ! <── new
      module procedure rank_real, rank_int
   end interface rank

   interface median
      module procedure median_real, median_int
   end interface median
   !––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
contains
!=======================================================================
!  helper: return **sorted copy** of a real vector
!=======================================================================
   pure function sorted(x) result(y)
      real(kind=dp), intent(in) :: x(:)
      real(kind=dp)             :: y(size(x))
      y = x
      call quick_sort_in_place(y)
   end function sorted
!=======================================================================
!  indexx for INTEGER – thin wrapper around indexx_real
!=======================================================================
   pure function indexx_int(xx) result(iord)
      integer, intent(in) :: xx(:)
      integer             :: iord(size(xx))
      iord = indexx_real(1.0_dp*xx)
   end function indexx_int
!=======================================================================
!  indexx for REAL
!=======================================================================
   pure function indexx_real(xx) result(iord)
      real(kind=dp), intent(in) :: xx(:)
      integer                   :: iord(size(xx))
      integer :: i
      do i = 1, size(xx)
         iord(i) = i
      end do
      call quick_sort(xx, iord)
   end function indexx_real
!=======================================================================
!  in-place quick-sort
!=======================================================================
   pure subroutine quick_sort_in_place(list)
      real(kind=dp), intent(in out) :: list(:)
      integer :: order(size(list)), i
      do i = 1, size(order)
         order(i) = i
      end do
      call quick_sort(list, order)
      list = list(order)
   end subroutine quick_sort_in_place
!=======================================================================
!  quick_sort (returns ordering)
!=======================================================================
   pure subroutine quick_sort(list, order)
      real(kind=dp), intent(in)    :: list(:)
      integer, intent(in out)      :: order(:)
      integer                      :: temp_order(size(list))
      temp_order = order
      call quick_sort_1(list, temp_order, 1, size(list))
      order = temp_order
   contains
      pure recursive subroutine quick_sort_1(list, order, left_end, right_end)
         real(kind=dp), intent(in) :: list(:)
         integer, intent(in out)   :: order(:)
         integer, intent(in)       :: left_end, right_end
         integer                   :: i, j, itemp
         real(kind=dp)             :: reference
         integer, parameter        :: max_simple_sort_size = 6
         if (right_end < left_end + max_simple_sort_size) then
            call interchange_sort(list, order, left_end, right_end)
         else
            reference = list(order((left_end + right_end)/2))
            i = left_end - 1; j = right_end + 1
            do
               do
                  i = i + 1
                  if (list(order(i)) >= reference) exit
               end do
               do
                  j = j - 1
                  if (list(order(j)) <= reference) exit
               end do
               if (i < j) then
                  itemp = order(i); order(i) = order(j); order(j) = itemp
               else if (i == j) then
                  i = i + 1
                  exit
               else
                  exit
               end if
            end do
            if (left_end < j) call quick_sort_1(list, order, left_end, j)
            if (i < right_end) call quick_sort_1(list, order, i, right_end)
         end if
      end subroutine quick_sort_1

      pure subroutine interchange_sort(list, order, left_end, right_end)
         real(kind=dp), intent(in) :: list(:)
         integer, intent(in out)   :: order(:)
         integer, intent(in)       :: left_end, right_end
         integer                   :: i, j, itemp
         do i = left_end, right_end - 1
            do j = i + 1, right_end
               if (list(order(i)) > list(order(j))) then
                  itemp = order(i)
                  order(i) = order(j)
                  order(j) = itemp
               end if
            end do
         end do
      end subroutine interchange_sort
   end subroutine quick_sort

!=======================================================================
!  Integer ranking (competition style).  Safe for all Fortran compilers.
!=======================================================================
   pure function rank_real(x) result(rk)
      !! Return rank(1:n) for REAL(dp) vector x.
      !! Ascending order; ties share the lowest rank in the tie-block.
      real(kind=dp), intent(in) :: x(:)
      integer                   :: rk(size(x))

      integer, allocatable :: ord(:)
      integer              :: n, k, j

      n   = size(x)
      if (n == 0) return        ! nothing to do for empty vector

      ord = indexx_real(x)      ! sorting permutation

      k = 1                     ! first position in current tie-block
      do while (k <= n)
         j = k                  ! scan forward to find end of tie-block
         do
            if (j == n) exit
            if (x(ord(j)) /= x(ord(j+1))) exit
            j = j + 1
         end do

         rk(ord(k:j)) = k       ! assign same rank to the whole block
         k = j + 1              ! start of next block
      end do
   end function rank_real
!-----------------------------------------------------------------------
   pure function rank_int(x) result(rk)
      !! Integer rank for an INTEGER vector (delegates to rank_real)
      integer, intent(in) :: x(:)
      integer             :: rk(size(x))

      rk = rank_real(real(x,kind=dp))
   end function rank_int
!=======================================================================

!=======================================================================
!  Median for REAL(dp) and INTEGER vectors
!=======================================================================
   pure function median_real(x) result(med)
      !! Median of a REAL(dp) vector.
      !! For an even‐length vector the mean of the two middle values
      !! is returned.
      real(kind=dp), intent(in) :: x(:)
      real(kind=dp)             :: med

      integer :: n
      real(kind=dp), allocatable :: y(:)

      n = size(x)
      if (n == 0) then
         med = 0.0_dp
         return
      end if

      y  = sorted(x)                       ! already provided helper
      if (mod(n,2) == 1) then              ! odd length
         med = y((n+1)/2)
      else                                 ! even length
         med = 0.5_dp * ( y(n/2) + y(n/2 + 1) )
      end if
   end function median_real
!-----------------------------------------------------------------------
   pure function median_int(x) result(med)
      !! Median of an INTEGER vector (integer result).
      !! For an even‐length vector the *integer* mean of the two middle
      !! values (truncated toward zero) is returned.
      integer, intent(in) :: x(:)
      integer             :: med

      integer              :: n
      integer, allocatable :: ord(:)

      n = size(x)
      if (n == 0) then
         med = 0
         return
      end if

      ord = indexx_int(x)                  ! ordering permutation
      if (mod(n,2) == 1) then              ! odd
         med = x( ord((n+1)/2) )
      else                                 ! even
         med = ( x( ord(n/2) ) + x( ord(n/2 + 1) ) ) / 2
      end if
   end function median_int

pure function unique(x) result(y)
real(kind=dp), intent(in)  :: x(:)
real(kind=dp), allocatable :: y(:)
real(kind=dp), allocatable :: xsort(:)
integer                    :: i, j, n
xsort = sorted(x)
n = size(x)
allocate (y(n))
if (n == 0) return
y(1) = xsort(1)
j = 1
do i=2,n
   if (xsort(i) /= xsort(i-1)) then
      j = j + 1
      y(j) = xsort(i)
   end if
end do
y = y(:j)
end function unique

end module qsort_mod
