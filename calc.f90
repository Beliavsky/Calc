module calc_mod
implicit none
private
public :: evaluate, eval_print
integer, parameter :: dp = kind(1.0d0)
contains

function evaluate(str) result(res)
  implicit none
  character(len=*), intent(in) :: str
  real(kind=dp)             :: res
  integer                    :: pos, lenstr
  character(len=:), allocatable :: expr
  character                  :: curr_char

  call init_parser(str)
  res = parse_expression()
  return

contains

  subroutine init_parser(s)
    implicit none
    character(len=*), intent(in) :: s
    expr    = s
    lenstr  = len_trim(s)
    pos     = 1
    call next_char()
  end subroutine init_parser

  subroutine next_char()
    implicit none
    if (pos > lenstr) then
      curr_char = char(0)
    else
      curr_char = expr(pos:pos)
      pos = pos + 1
    end if
  end subroutine next_char

  subroutine skip_spaces()
    implicit none
    do while (curr_char == ' ')
      call next_char()
    end do
  end subroutine skip_spaces

  function parse_number() result(num)
    implicit none
    real(kind=dp) :: num
    character(len=50) :: numstr
    integer :: i
    call skip_spaces()
    i = 0
    do while ((curr_char >= '0' .and. curr_char <= '9') .or. curr_char == '.')
      i = i + 1
      numstr(i:i) = curr_char
      call next_char()
    end do
    read(numstr(1:i), *) num
  end function parse_number

  recursive function parse_factor() result(f)
    implicit none
    real(kind=dp) :: f
    call skip_spaces()
    if (curr_char == '(') then
      call next_char()
      f = parse_expression()
      if (curr_char == ')') call next_char()
    else if ((curr_char >= '0' .and. curr_char <= '9') .or. curr_char == '.') then
      f = parse_number()
    else
      f = 0.0_dp
    end if
  end function parse_factor

  recursive function parse_term() result(t)
    implicit none
    real(kind=dp) :: t, f
    t = parse_factor()
    call skip_spaces()
    do while (curr_char == '*' .or. curr_char == '/')
      select case (curr_char)
      case ('*')
        call next_char()
        f = parse_factor()
        t = t * f
      case ('/')
        call next_char()
        f = parse_factor()
        t = t / f
      end select
      call skip_spaces()
    end do
  end function parse_term

  recursive function parse_expression() result(e)
    implicit none
    real(kind=dp) :: e, t
    e = parse_term()
    call skip_spaces()
    do while (curr_char == '+' .or. curr_char == '-')
      select case (curr_char)
      case ('+')
        call next_char()
        t = parse_term()
        e = e + t
      case ('-')
        call next_char()
        t = parse_term()
        e = e - t
      end select
      call skip_spaces()
    end do
  end function parse_expression

end function evaluate

subroutine eval_print(str)
character (len=*), intent(in) :: str
print "(a,' = ', f0.6)", str, evaluate(str)
end subroutine eval_print

end module calc_mod
