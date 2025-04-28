module util_mod
use kind_mod, only: dp
implicit none
private
public :: matched_parentheses, matched_brackets, arange
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

end module util_mod
