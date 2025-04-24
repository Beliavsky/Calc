module interpret_mod
  implicit none
  private
  public :: evaluate, eval_print, set_variable

  integer, parameter :: dp       = kind(1.0d0)
  integer, parameter :: max_vars = 100

  type :: var_t
    character(len=32)          :: name = ''
    real(kind=dp), allocatable :: val(:)
  end type var_t

  type(var_t) :: vars(max_vars)
  integer      :: n_vars = 0

contains

  subroutine set_variable(name, val)
    character(len=*),           intent(in) :: name
    real(kind=dp), dimension(:),intent(in) :: val
    integer                                  :: i
    character(len=32)                        :: nm

    nm = adjustl(name)
    ! overwrite if already exists
    do i = 1, n_vars
      if (trim(vars(i)%name) == trim(nm)) then
        if (allocated(vars(i)%val)) deallocate(vars(i)%val)
        allocate(vars(i)%val(size(val)))
        vars(i)%val = val
        return
      end if
    end do

    ! otherwise add new
    if (n_vars < max_vars) then
      n_vars = n_vars + 1
      vars(n_vars)%name = nm
      allocate(vars(n_vars)%val(size(val)))
      vars(n_vars)%val = val
    else
      print *, "Error: too many variables."
    end if
  end subroutine set_variable


  recursive function evaluate(str) result(res)
    implicit none
    character(len=*), intent(in)      :: str
    real(kind=dp), allocatable        :: res(:)

    ! parser state
    integer                             :: pos, lenstr, eqpos
    character(len=:), allocatable       :: expr, lhs, rhs
    character                           :: curr_char

    ! initialize parser
    expr   = trim(str)
    lenstr = len_trim(expr)
    pos    = 1
    call next_char()

    ! assignment?
    eqpos = index(expr, '=')
    if (eqpos > 0) then
      lhs = adjustl(expr(1:eqpos-1))
      rhs = expr(eqpos+1:)
      res = evaluate(rhs)
      call set_variable(lhs, res)
      return
    end if

    ! otherwise parse expression
    res = parse_expression()


  contains

    subroutine next_char()
      if (pos > lenstr) then
        curr_char = char(0)
      else
        curr_char = expr(pos:pos)
      end if
      pos = pos + 1
    end subroutine next_char

    subroutine skip_spaces()
      do while (curr_char == ' ')
        call next_char()
      end do
    end subroutine skip_spaces

    function parse_number() result(num)
      real(kind=dp), allocatable :: num(:)
      character(len=64)          :: buf
      integer                    :: i
      real(kind=dp)              :: tmp
      call skip_spaces()
      i = 0
      do while ((curr_char >= '0' .and. curr_char <= '9') .or. curr_char == '.')
        i = i + 1
        buf(i:i) = curr_char
        call next_char()
      end do
      read(buf(1:i), *) tmp
      allocate(num(1))
      num(1) = tmp
    end function parse_number

    function parse_identifier() result(name)
      character(len=32) :: name
      integer           :: i
      call skip_spaces()
      i = 0
      do while ( &
            (curr_char >= 'a' .and. curr_char <= 'z') .or. &
            (curr_char >= 'A' .and. curr_char <= 'Z') .or. &
            (curr_char >= '0' .and. curr_char <= '9') )
        i = i + 1
        name(i:i) = curr_char
        call next_char()
      end do
      name = adjustl(name(1:i))
    end function parse_identifier

    function get_variable(name) result(v)
      character(len=*), intent(in)       :: name
      real(kind=dp), allocatable         :: v(:)
      integer                             :: i
      do i = 1, n_vars
        if (trim(vars(i)%name) == trim(name)) then
          v = vars(i)%val
          return
        end if
      end do
      allocate(v(1))
      v(1) = 0.0_dp
    end function get_variable

    recursive function parse_array() result(arr)
      real(kind=dp), allocatable :: arr(:), tmp(:), elem(:)
      integer                     :: count
      call next_char()        ! skip '['
      call skip_spaces()
      count = 0
      do while (curr_char /= ']' .and. curr_char /= char(0))
        elem = parse_expression()
        count = count + 1
        if (count == 1) then
          allocate(arr(1))
        else
          allocate(tmp(count))
          tmp(1:count-1) = arr
          deallocate(arr)
          allocate(arr(count))
          arr = tmp
          deallocate(tmp)
        end if
        arr(count) = elem(1)
        call skip_spaces()
        if (curr_char == ',' .or. curr_char == ' ') then
          call next_char()
          call skip_spaces()
        end if
      end do
      if (curr_char == ']') call next_char()
    end function parse_array

    recursive function parse_factor() result(f)
      real(kind=dp), allocatable :: f(:), exponent(:), tmp(:)
      call skip_spaces()

      select case (curr_char)
      case ('(')
        call next_char()
        f = parse_expression()
        if (curr_char == ')') call next_char()

      case ('[')
        f = parse_array()

      case default
        if ((curr_char >= '0' .and. curr_char <= '9') .or. curr_char == '.') then
          f = parse_number()
        else if ( &
            (curr_char >= 'a' .and. curr_char <= 'z') .or. &
            (curr_char >= 'A' .and. curr_char <= 'Z') ) then
          f = get_variable(parse_identifier())
        else
          allocate(f(1)); f(1) = 0.0_dp
        end if
      end select

      ! exponentiation (^), right-associative
      call skip_spaces()
      if (curr_char == '^') then
        call next_char()
        exponent = parse_factor()
        if (size(f) == size(exponent)) then
          tmp = f ** exponent
        else if (size(exponent) == 1) then
          allocate(tmp(size(f))); tmp = f ** exponent(1)
        else if (size(f) == 1) then
          allocate(tmp(size(exponent))); tmp = f(1) ** exponent
        else
          print *, "Error: size mismatch in exponentiation"
          stop
        end if
        deallocate(f)
        f = tmp
      end if
    end function parse_factor

    recursive function parse_term() result(t)
      real(kind=dp), allocatable :: t(:), f2(:), tmp(:)
      integer                     :: nt, nf
      t = parse_factor()
      call skip_spaces()
      do while (curr_char == '*' .or. curr_char == '/')
        if (curr_char == '*') then
          call next_char()
          f2 = parse_factor()
          nt = size(t); nf = size(f2)
          if (nt == nf) then
            tmp = t * f2
          else if (nf == 1) then
            allocate(tmp(nt)); tmp = t * f2(1)
          else if (nt == 1) then
            allocate(tmp(nf)); tmp = t(1) * f2
          else
            print *, "Error: size mismatch in multiplication"
            stop
          end if
        else
          call next_char()
          f2 = parse_factor()
          nt = size(t); nf = size(f2)
          if (nt == nf) then
            tmp = t / f2
          else if (nf == 1) then
            allocate(tmp(nt)); tmp = t / f2(1)
          else if (nt == 1) then
            allocate(tmp(nf)); tmp = t(1) / f2
          else
            print *, "Error: size mismatch in division"
            stop
          end if
        end if
        deallocate(t)
        t = tmp
        call skip_spaces()
      end do
    end function parse_term

    recursive function parse_expression() result(e)
      real(kind=dp), allocatable :: e(:), t(:), tmp(:)
      integer                     :: ne, nt
      e = parse_term()
      call skip_spaces()
      do while (curr_char == '+' .or. curr_char == '-')
        if (curr_char == '+') then
          call next_char()
          t = parse_term()
          ne = size(e); nt = size(t)
          if (ne == nt) then
            tmp = e + t
          else if (nt == 1) then
            allocate(tmp(ne)); tmp = e + t(1)
          else if (ne == 1) then
            allocate(tmp(nt)); tmp = e(1) + t
          else
            print *, "Error: size mismatch in addition"
            stop
          end if
        else
          call next_char()
          t = parse_term()
          ne = size(e); nt = size(t)
          if (ne == nt) then
            tmp = e - t
          else if (nt == 1) then
            allocate(tmp(ne)); tmp = e - t(1)
          else if (ne == 1) then
            allocate(tmp(nt)); tmp = e(1) - t
          else
            print *, "Error: size mismatch in subtraction"
            stop
          end if
        end if
        deallocate(e)
        e = tmp
        call skip_spaces()
      end do
    end function parse_expression

  end function evaluate

  subroutine eval_print(str)
    character(len=*), intent(in) :: str
    real(kind=dp), allocatable    :: r(:)
    r = evaluate(str)
    write (*,"(/,'> ', a)") trim(str)
    if (size(r) == 1) then
      print "(f0.6)", r(1)
    else
      write(*,'("[", *(f0.6,:,", "), "]")', advance="no") r
      print "(']')"
    end if
  end subroutine eval_print

end module interpret_mod
