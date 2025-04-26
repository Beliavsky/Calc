module interpret_mod
  implicit none
  private
  public :: evaluate, eval_print, set_variable, runif
  interface runif
    module procedure runif_scalar, runif_vec
  end interface runif

  integer, parameter :: dp = kind(1.0d0)
  integer, parameter :: max_vars = 100

  type :: var_t
    character(len=32) :: name = ''
    real(kind=dp), allocatable :: val(:)
  end type var_t

  type(var_t) :: vars(max_vars)
  integer :: n_vars = 0
  logical, save :: eval_error = .false.
  character(len=1) :: curr_char

contains

  subroutine set_variable(name, val)
    character(len=*), intent(in) :: name
    real(kind=dp), intent(in), dimension(:) :: val
    integer :: i
    character(len=32) :: nm

    nm = adjustl(name)
    do i = 1, n_vars
      if (trim(vars(i)%name) == trim(nm)) then
        if (allocated(vars(i)%val)) then
          deallocate(vars(i)%val)
        end if
        allocate(vars(i)%val(size(val)))
        vars(i)%val = val
        return
      end if
    end do

    if (n_vars < max_vars) then
      n_vars = n_vars + 1
      vars(n_vars)%name = nm
      allocate(vars(n_vars)%val(size(val)))
      vars(n_vars)%val = val
    else
      print *, "Error: too many variables."
      eval_error = .true.
    end if
  end subroutine set_variable

  function apply_func(fname, arr) result(res)
    character(len=*), intent(in) :: fname
    real(kind=dp), intent(in), dimension(:) :: arr
    real(kind=dp) :: res

    select case (trim(fname))
    case ("sum")
      res = sum(arr)
    case ("minval")
      res = minval(arr)
    case ("maxval")
      res = maxval(arr)
    case default
      print *, "Error: function '", trim(fname), "' not defined"
      eval_error = .true.
      res = 0.0_dp
    end select
  end function apply_func

  recursive function evaluate(str) result(res)
    implicit none
    character(len=*), intent(in) :: str
    real(kind=dp), allocatable :: res(:)
    character(len=:), allocatable :: expr
    integer :: pos, lenstr, eqpos
    character(len=:), allocatable :: lhs, rhs

    call init_evaluator(trim(str), expr, lenstr, pos)
    eqpos = index(expr, '=')
    if (eqpos > 0) then
      lhs = adjustl(expr(1:eqpos-1))
      rhs = expr(eqpos+1:)
      res = evaluate(rhs)
      if (.not. eval_error) then
        if (index(lhs,'(') > 0 .and. index(lhs,')') > index(lhs,'(')) then
          call assign_element(lhs, res)
        else
          call set_variable(lhs, res)
        end if
      end if
      return
    end if

    res = parse_expression()

  contains

    subroutine init_evaluator(str_in, expr, lenstr, pos)
      character(len=*), intent(in) :: str_in
      character(len=:), allocatable, intent(out) :: expr
      integer, intent(out) :: lenstr, pos

      expr = str_in
      lenstr = len_trim(expr)
      pos = 1
      eval_error = .false.
      call next_char()
    end subroutine init_evaluator

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
      character(len=64) :: buf
      integer :: i
      real(kind=dp) :: tmp

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

    function parse_identifier() result(name_out)
      character(len=32) :: name_out
      integer :: i

      call skip_spaces()
      i = 0
      do while ((curr_char >= 'a' .and. curr_char <= 'z') .or. &
                (curr_char >= 'A' .and. curr_char <= 'Z') .or. &
                (curr_char >= '0' .and. curr_char <= '9'))
        i = i + 1
        name_out(i:i) = curr_char
        call next_char()
      end do
      name_out = adjustl(name_out(1:i))
    end function parse_identifier

    function get_variable(name) result(v)
      character(len=*), intent(in) :: name
      real(kind=dp), allocatable :: v(:)
      integer :: i

      do i = 1, n_vars
        if (trim(vars(i)%name) == trim(name)) then
          v = vars(i)%val
          return
        end if
      end do

      print *, "Error: undefined variable '", trim(name), "'"
      eval_error = .true.
      allocate(v(1))
      v(1) = 0.0_dp
    end function get_variable

    recursive function parse_array() result(arr)
      real(kind=dp), allocatable :: arr(:), tmp(:), elem(:)
      integer :: count

      call next_char()
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
      if (curr_char == ']') then
        call next_char()
      end if
    end function parse_array

    recursive function parse_factor() result(f)
      real(kind=dp), allocatable :: f(:), exponent(:), tmp(:), arr(:), vvar(:)
      character(len=32) :: id
      integer :: idx, nrand

      call skip_spaces()
      select case (curr_char)
      case ('(')
        call next_char()
        f = parse_expression()
        if (curr_char == ')') then
          call next_char()
        end if

      case ('[')
        f = parse_array()

      case default
        if ((curr_char >= '0' .and. curr_char <= '9') .or. curr_char == '.') then
          f = parse_number()

        else if ((curr_char >= 'a' .and. curr_char <= 'z') .or. &
                 (curr_char >= 'A' .and. curr_char <= 'Z')) then
          id = parse_identifier()
          call skip_spaces()
          if (curr_char == '(') then
            call next_char()
            call skip_spaces()
            ! zero-arg function?
            if (curr_char == ')') then
              call next_char()
              if (trim(id) == "runif") then
                allocate(f(1))
                f(1) = runif_scalar()
              else
                print *, "Error: function '", trim(id), "' needs arguments"
                eval_error = .true.
                allocate(f(1)); f(1) = 0.0_dp
              end if

            else
              ! one or more args
              arr = parse_expression()
              if (curr_char == ')') then
                call next_char()
              end if
              if (.not. eval_error) then

                if (trim(id) == "runif") then
                  nrand = int(arr(1))
                  if (nrand < 1) then
                    allocate(f(1)); f(1) = 0.0_dp
                  else
                    f = runif_vec(nrand)
                  end if

                else if (size(arr) > 1) then
                  allocate(f(1))
                  f(1) = apply_func(id, arr)

                else
                  vvar = get_variable(id)
                  if (.not. eval_error .and. size(vvar) > 1) then
                    idx = int(arr(1))
                    if (idx >= 1 .and. idx <= size(vvar)) then
                      allocate(f(1)); f(1) = vvar(idx)
                    else
                      print *, "Error: index out of bounds for '", trim(id), "'"
                      eval_error = .true.
                      allocate(f(1)); f(1) = 0.0_dp
                    end if
                  else
                    print *, trim(id)//"(x) not defined for scalar x"
                    eval_error = .true.
                    allocate(f(1)); f(1) = 0.0_dp
                  end if
                end if

              else
                allocate(f(1)); f(1) = 0.0_dp
              end if
            end if

          else
            f = get_variable(id)
          end if

        else
          allocate(f(1)); f(1) = 0.0_dp
        end if
      end select

      call skip_spaces()
      if (curr_char == '^') then
        call next_char()
        exponent = parse_factor()
        if (.not. eval_error) then
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
      end if

    end function parse_factor

    recursive function parse_term() result(t)
      real(kind=dp), allocatable :: t(:), f2(:), tmp(:)
      integer :: nt, nf

      t = parse_factor()
      call skip_spaces()
      do while (.not. eval_error .and. (curr_char=='*' .or. curr_char=='/'))
        if (curr_char=='*') then
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
      integer :: ne, nt

      e = parse_term()
      call skip_spaces()
      do while (.not. eval_error .and. (curr_char=='+' .or. curr_char=='-'))
        if (curr_char=='+') then
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

  subroutine assign_element(lhs, rval)
    character(len=*), intent(in) :: lhs
    real(kind=dp), allocatable, intent(in) :: rval(:)
    character(len=32) :: name
    character(len=:), allocatable :: idxs
    integer :: i1, i2, idx, vi
    real(kind=dp), allocatable :: tmp(:)

    i1 = index(lhs,'(')
    i2 = index(lhs,')')
    name = adjustl(lhs(1:i1-1))
    idxs = lhs(i1+1:i2-1)

    tmp = evaluate(idxs)
    if (eval_error .or. size(tmp) /= 1) then
      eval_error = .true.
      return
    end if

    idx = int(tmp(1))
    do vi = 1, n_vars
      if (trim(vars(vi)%name) == trim(name)) then
        if (idx < 1 .or. idx > size(vars(vi)%val)) then
          print *, "Error: index out of bounds for '", trim(name), "'"
          eval_error = .true.
        else
          vars(vi)%val(idx) = rval(1)
        end if
        return
      end if
    end do

    print *, "Error: undefined variable '", trim(name), "' in assignment"
    eval_error = .true.
  end subroutine assign_element

  subroutine eval_print(str)
    character(len=*), intent(in) :: str
    real(kind=dp), allocatable :: r(:)
    integer :: i

    if (len_trim(str) >= 2 .and. str(1:1) == '?') then
      write(*,*) 'Defined variables:'
      do i = 1, n_vars
        if (size(vars(i)%val) == 1) then
          write(*,"(a)", advance="no") trim(vars(i)%name)//': '
          print "(F0.6)", vars(i)%val(1)
        else
          write(*,"(a)", advance="no") trim(vars(i)%name)//': '
          write(*,'("[",*(F0.6,:,", "))', advance="no") vars(i)%val
          write(*,"(']')")
        end if
      end do
      return
    end if

    r = evaluate(str)
    if (eval_error) return

    write(*,"(/,'> ',a)") trim(str)
    if (size(r) == 1) then
      print "(F0.6)", r(1)
    else
      write(*,'("[",*(F0.6,:,", "),"]")', advance="no") r
      print "(']')"
    end if
  end subroutine eval_print

  function runif_scalar() result(r)
    real(kind=dp) :: r
    call random_number(r)
  end function runif_scalar

  function runif_vec(n) result(r)
    integer, intent(in)        :: n
    real(kind=dp), allocatable :: r(:)
    if (n < 1) then
      allocate(r(1))
      r(1) = 0.0_dp
    else
      allocate(r(n))
      call random_number(r)
    end if
  end function runif_vec

end module interpret_mod
