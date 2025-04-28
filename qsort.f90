module qsort_mod
  use kind_mod, only: dp
  implicit none
  private
  public :: indexx, quick_sort_in_place, quick_sort, sorted
  interface indexx
     module procedure indexx_real, indexx_int
  end interface indexx
contains

  function sorted(x) result(y)
  real(kind=dp), intent(in) :: x(:)
  real(kind=dp)             :: y(size(x))
  y = x
  call quick_sort_in_place(y)
  end function sorted

  pure function indexx_int(xx) result(iord)
    ! Returns indices that sort an integer array in ascending order
    integer, intent(in) :: xx(:)
    integer             :: iord(size(xx))
    iord = indexx_real(1.0_dp*xx)
  end function indexx_int

  pure function indexx_real(xx) result(iord)
    ! Returns indices that sort a real array in ascending order
    real(kind=dp), intent(in) :: xx(:)
    integer                   :: iord(size(xx))
    integer                   :: i
    do i = 1, size(xx)
      iord(i) = i
    end do
    if (any(isnan(xx))) return
    call quick_sort(xx, iord)
  end function indexx_real

  pure subroutine quick_sort_in_place(list)
    ! Sorts a real array in place in ascending order
    real(kind=dp), intent(in out) :: list(:)
    integer :: order(size(list))
    integer :: i
    do i = 1, size(order)
       order(i) = i
    end do
    call quick_sort(list, order)
    list = list(order)
  end subroutine quick_sort_in_place

  pure subroutine quick_sort(list, order)
    ! Computes sorting indices for a real array without modifying it
    real(kind=dp), intent(in)    :: list(:)
    integer, intent(in out)      :: order(:)
    integer                      :: temp_order(size(list))
    temp_order = order
    call quick_sort_1(list, temp_order, 1, size(list))
    order = temp_order
  contains
    pure recursive subroutine quick_sort_1(list, order, left_end, right_end)
      ! Recursively applies quicksort algorithm to a subset of indices
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
      ! Sorts a small subset of indices using interchange sort
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
end module qsort_mod
