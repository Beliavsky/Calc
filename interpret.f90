module interpret_mod
  use kind_mod , only: dp
  use stats_mod, only: mean, sd, cor, cov, cumsum, diff, standardize
  use util_mod , only: matched_brackets, matched_parentheses, arange, &
                       head, tail
  use random_mod, only: random_normal
  use qsort_mod, only: sorted, indexx, rank, median
  implicit none
  private
  public :: evaluate, eval_print, set_variable, tunit, &
     code_transcript_file, clear, vars, mutable, slice_array
  interface runif
    module procedure runif_vec
  end interface runif

  integer, parameter :: max_vars = 100
  integer, parameter :: max_print = 15 ! for arrays larger than this, summary stats printed instead of elements

  type :: var_t
    character(len=32) :: name = ''
    real(kind=dp), allocatable :: val(:)
  end type var_t

  type(var_t) :: vars(max_vars)
  integer :: n_vars = 0, tunit
  logical, save :: eval_error = .false.
  character(len=1) :: curr_char
  character (len=*), parameter :: code_transcript_file = "code.fi" ! stores the commands issued
  logical, parameter :: stop_if_error = .false.
  real(kind=dp), parameter :: bad_value = -999.0_dp, tol = 1.0e-6_dp
  logical, parameter :: mutable = .false.   ! when .false., no reassignments allowed
contains

!---------------------------------------------------------------
!  Return a 1-D section of variable NAME described by the text
!  in IDXS ( e.g. "2:11:3", "5:", ":7:-2", … ).
!
subroutine slice_array(name, idxs, result)
   character(len=*), intent(in)            :: name
   character(len=*), intent(in)            :: idxs
   real(kind=dp),   allocatable, intent(out) :: result(:)

   real(kind=dp), allocatable :: v(:), larr(:), uarr(:), sarr(:)
   integer                    :: c1, c2       ! locations of ':' in IDXS
   integer                    :: i1, i2, step
   integer                    :: n            ! array size

   !------------------------------------------------------------
   ! evaluate the variable itself
   v = evaluate(name)
   if (eval_error) return
   n = size(v)

   !------------------------------------------------------------
   ! locate first and (optional) second ':' in the text
   c1 = index(idxs, ":")
   if (c1 == 0) then
      print *, "Error: bad slice syntax in '", trim(idxs), "'"
      eval_error = .true.
      allocate(result(0));  return
   end if
   c2 = index(idxs(c1+1:), ":")
   if (c2 > 0) c2 = c1 + c2        !→ absolute position, or 0 if none

   !------------------------------------------------------------
   ! lower bound
   if (c1 > 1) then
      call parse_index( idxs(:c1-1), larr, i1 )
   else
      i1 = 1
   end if

   !------------------------------------------------------------
   ! upper bound & stride
   if (c2 == 0) then                    ! only one ':'
      step = 1
      if (c1 < len_trim(idxs)) then
         call parse_index( idxs(c1+1:), uarr, i2 )
      else
         i2 = n
      end if
   else                                  ! two ':'  → stride present
      if (c2 - c1 > 1) then              ! ←  strictly “> 1” is the right check
         call parse_index( idxs(c1+1:c2-1), uarr, i2 )
      else
         i2 = n          ! omitted upper bound
      end if
      call parse_index( idxs(c2+1:), sarr, step )
   end if

   !------------------------------------------------------------
   ! sanity checks
   if (step == 0) then
      print *, "Error: slice stride cannot be zero"
      eval_error = .true.;  allocate(result(0));  return
   end if
   if (i1 < 1 .or. i1 > n .or. i2 < 0 .or. i2 > n) then
      print *, "Error: slice indices out of range"
      eval_error = .true.;  allocate(result(0));  return
   end if

   !------------------------------------------------------------
   ! empty slice situations that are nevertheless valid
   if ( (step > 0 .and. i1 >  i2)  .or. &
        (step < 0 .and. i1 <  i2) ) then
      allocate(result(0))
      return
   end if

   !------------------------------------------------------------
   ! finally deliver the section
   result = v(i1:i2:step)

contains
   !----------------------------------------------------------------
   subroutine parse_index(str, arr, idx)
      character(len=*), intent(in)            :: str
      real(kind=dp),   allocatable, intent(out) :: arr(:)
      integer,         intent(out)            :: idx

      arr = evaluate(str)
      if (eval_error) then
         idx = -1
      else
         idx = int(arr(1))
      end if
   end subroutine parse_index
end subroutine slice_array

  subroutine clear()
  ! delete all variables
  integer :: i
  do i=1,min(n_vars, max_vars)
     vars(i)%name = ""
     if (allocated(vars(i)%val)) deallocate (vars(i)%val)
  end do
  n_vars = 0
  end subroutine

  !------------------------------------------------------------------------
  ! Store or replace a variable
  subroutine set_variable(name, val)
    character(len=*), intent(in) :: name
    real(kind=dp),    intent(in) :: val(:)
    integer :: i
    character(len=32) :: nm

    nm = adjustl(name)
    do i = 1, n_vars
      if (vars(i)%name == nm) then
        if (.not. mutable) then
           print *, "Error: cannot reassign '" // trim(nm) // "' if mutable is .false."
           eval_error = .true.
           return
        end if
        vars(i)%val = val
        return
      end if
    end do

    if (n_vars < max_vars) then
      n_vars = n_vars + 1
      vars(n_vars)%name = nm
      vars(n_vars)%val = val
    else
      print *, "Error: too many variables."
      eval_error = .true.
    end if
  end subroutine set_variable

  !------------------------------------------------------------------------
  ! Apply a scalar-returning function: sum, minval, maxval, etc.
  function apply_scalar_func(fname, arr) result(r)
    character(len=*), intent(in)       :: fname
    real(kind=dp),    intent(in)       :: arr(:)
    real(kind=dp) :: r

    select case (trim(fname))
    case ("size")    ; r = size(arr)
    case ("sum")     ; r = sum(arr)
    case ("product") ; r = product(arr)
    case ("norm1")   ; r = sum(abs(arr))
    case ("norm2")   ; r = norm2(arr)
    case ("minval")  ; r = minval(arr)
    case ("maxval")  ; r = maxval(arr)
    case ("minloc")  ; r = minloc(arr, dim=1)
    case ("maxloc")  ; r = maxloc(arr, dim=1)
    case ("median")  ; r = median(arr)
    case ("mean")    ; r = mean(arr)
    case ("sd")      ; r = sd(arr)
    case default
      print *, "Error: function '", trim(fname), "' not defined"
      eval_error = .true.
      r = bad_value
    end select
  end function apply_scalar_func

  !------------------------------------------------------------------------
  ! Apply a function that takes an array and returns an array
  function apply_vec_func(fname, arr) result(res)
    character(len=*), intent(in)       :: fname
    real(kind=dp),    intent(in)       :: arr(:)
    real(kind=dp), allocatable :: res(:)
!    integer :: n

!    n = size(arr)
!    allocate(res(n))

    select case (trim(fname))
    case ("abs") ; res = abs(arr)
    case ("acos") ; res = acos(arr)
    case ("acosh") ; res = acosh(arr)
    case ("asin") ; res = asin(arr)
    case ("asinh") ; res = asinh(arr)
    case ("atan") ; res = atan(arr)
    case ("atanh") ; res = atanh(arr)
    case ("cos") ; res = cos(arr)
    case ("cosh") ; res = cosh(arr)
    case ("exp") ; res = exp(arr)
    case ("log") ; res = log(arr)
    case ("log10") ; res = log10(arr)
    case ("sin") ; res = sin(arr)
    case ("sinh") ; res = sinh(arr)
    case ("sqrt") ; res = sqrt(arr)
    case ("tan") ; res = tan(arr)
    case ("tanh") ; res = tanh(arr)
    case ("cumsum"); res = cumsum(arr)
    case ("diff"); res = diff(arr)
    case ("head"); res = head(arr)
    case ("tail"); res = tail(arr)
    case ("sort"); res = sorted(arr)
    case ("indexx"); res = indexx(arr)
    case ("rank"); res = rank(arr)
    case ("stdz"); res = standardize(arr)
    case default
      print *, "Error: function '", trim(fname), "' not defined"
      eval_error = .true.
      res = bad_value
    end select
  end function apply_vec_func

  !------------------------------------------------------------------
  recursive function evaluate(str) result(res)
  !------------------------------------------------------------------
    implicit none
    character(len=*), intent(in) :: str
    real(kind=dp),   allocatable :: res(:)

    !–- local to this outer shell –----------------------------------
    character(len=:), allocatable :: expr, lhs, rhs
    integer                      :: pos, lenstr      ! parser cursor & length
    integer                      :: i, eqpos         ! scan index & "=" position
  !------------------------------------------------------------------

    !–– prepare the string for parsing –––––––––––––––––––––––––––––
    call init_evaluator(trim(str), expr, lenstr, pos)

    !–– look for an *assignment* “=” that is **not** part of >= <= == !=
    eqpos = 0
    do i = 1, lenstr
       if (expr(i:i) == '=') then
          if ( i > 1  .and. any( expr(i-1:i-1) == ['>','<','!','='] ) ) cycle
          if ( i < lenstr .and. expr(i+1:i+1) == '='                 ) cycle
          eqpos = i
          exit                       ! first qualifying “=” wins
       end if
    end do

    !–– assignment found → evaluate RHS then store ––––––––––––––––
    if (eqpos > 0) then
       lhs = adjustl(expr(1:eqpos-1))
       rhs = expr(eqpos+1:)

       res = evaluate(rhs)           ! recursive call
       if (.not. eval_error) then
          if (index(lhs,'(') > 0 .and. index(lhs,')') > index(lhs,'(')) then
             call assign_element(lhs, res)   ! element assignment  a(i)=…
          else
             call set_variable  (lhs, res)   ! whole–variable assignment
          end if
       end if
       return
    end if

    !–– no “=” ⇒ treat the whole string as an expression ––––––––––
    res = parse_expression()


  contains

    !--------------------------------------------------
    subroutine init_evaluator(str_in, expr, lenstr, pos)
      character(len=*), intent(in)               :: str_in
      character(len=:), allocatable, intent(out) :: expr
      integer, intent(out)                       :: lenstr, pos

      expr   = str_in
      lenstr = len_trim(expr)
      pos    = 1
      eval_error = .false.
      call next_char()
    end subroutine init_evaluator

    !--------------------------------------------------
    subroutine next_char()
      if (pos > lenstr) then
        curr_char = char(0)
      else
        curr_char = expr(pos:pos)
      end if
      pos = pos + 1
    end subroutine next_char

    !--------------------------------------------------
    subroutine skip_spaces()
      do while (curr_char == ' ')
        call next_char()
      end do
    end subroutine skip_spaces

    !--------------------------------------------------
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

    !--------------------------------------------------
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

    !--------------------------------------------------
    function get_variable(name) result(v)
      character(len=*), intent(in) :: name
      real(kind=dp), allocatable :: v(:)
      integer :: i

      do i = 1, n_vars
        if (vars(i)%name == name) then
          v = vars(i)%val
          return
        end if
      end do

      print *, "Error: undefined variable '", trim(name), "'"
      eval_error = .true.
      v = [bad_value]
    end function get_variable

    !parse_array
    recursive function parse_array() result(arr)
      real(kind=dp), allocatable :: arr(:), tmp(:), elem(:)
      integer :: total, ne

      !── consume the '[' ────────────────────────────────────────────
      call next_char()
      call skip_spaces()

      !── empty array literal [] ────────────────────────────────────
      if (curr_char == ']') then
        allocate(arr(0))
        call next_char()
        return
      end if

      total = 0
      allocate(arr(0))

      do
        !── parse one element (may itself be an array) ─────────────
        elem = parse_expression()
        if (eval_error) return
        ne = size(elem)

        !── append elem to arr ─────────────────────────────────────
        if (allocated(tmp)) deallocate(tmp)
        allocate(tmp(total+ne))
        if (total > 0) tmp(1:total) = arr
        tmp(total+1:total+ne) = elem
        arr = tmp
        total = total + ne

        !── now skip any spaces, then decide what to do ────────────
        call skip_spaces()
        select case (curr_char)
        case (",")         ! explicit comma
          call next_char()
        case ("]")         ! end of array
          call next_char()
          exit
        end select
      end do
    end function parse_array

!--------------------------------------------------------------------
recursive function parse_factor() result(f)
!--------------------------------------------------------------------
   !===================  result  =====================================
   real(kind=dp), allocatable :: f(:)

   !===================  locals  =====================================
   real(kind=dp), allocatable :: arg1(:), arg2(:)
   real(kind=dp), allocatable :: exponent(:), vvar(:)
   integer,          allocatable :: idxv(:)
   character(len=32) :: id
   character(len=:), allocatable :: idxs
   integer :: nsize, pstart, pend, depth, n1, n2
   logical :: is_neg, have_second
   logical :: toplevel_colon, toplevel_comma
   !------------------------------------------------------------------

   call skip_spaces()

   !---------------- unary ± -----------------------------------------
   if (curr_char == '+' .or. curr_char == '-') then
      is_neg = (curr_char == '-')
      call next_char()
      f = parse_factor()
      if (.not. eval_error .and. is_neg) f = -f
      return
   end if

   select case (curr_char)
   case ('(')                                    ! parenthesised expr.
      call next_char()
      f = parse_expression()
      if (curr_char == ')') call next_char()

   case ('[')                                    ! array literal
      f = parse_array()

   case default
      if ((curr_char >= '0' .and. curr_char <= '9') .or. curr_char == '.') then
         f = parse_number()

      else if ((curr_char >= 'a' .and. curr_char <= 'z') .or. &
               (curr_char >= 'A' .and. curr_char <= 'Z')) then

         id = parse_identifier()
         call skip_spaces()

         !-----------------------------------------------------------------
         if (curr_char == '(') then            !  id( … )
            call next_char()                   !  consume '('
            call skip_spaces()

            !============ ZERO-ARGUMENT SPECIAL CASE ======================
            if (curr_char == ')') then         !  e.g. runif()
               call next_char()                !  consume ')'
               select case (trim(id))
               case ("runif")
                  allocate(f(1))
                  call random_number(f(1))
               case ("rnorm")
                  f = random_normal(1)
               case default
                  print *, "Error: function '"//trim(id)//"' needs arguments"
                  eval_error = .true.
                  f = [bad_value]
               end select
               return
            end if
            !========== end zero-argument special case ====================

            !--- examine the whole parenthesised chunk --------------------
            pstart = pos - 1                   ! first char _inside_ '('
            depth  = 1
            toplevel_colon  = .false.
            toplevel_comma  = .false.
            pend   = pstart - 1
            do while (pend < lenstr .and. depth > 0)
               pend = pend + 1
               select case (expr(pend:pend))
               case ('('); depth = depth + 1
               case (')'); depth = depth - 1
               case (':')
                  if (depth == 1) toplevel_colon = .true.
               case (',')
                  if (depth == 1) toplevel_comma = .true.
               end select
            end do
            if (depth /= 0) then
               print *, "Error: mismatched parentheses"
               eval_error = .true.;  f = [bad_value];  return
            end if

            !---------------- slice?  -------------------------------------
            if (toplevel_colon .and. .not. toplevel_comma) then
               idxs = expr(pstart:pend-1)
               call slice_array(id, idxs, f)

               ! advance cursor just past ')'
               pos = pend + 1
               if (pos > lenstr) then
                  curr_char = char(0)
               else
                  curr_char = expr(pos:pos);  pos = pos + 1
               end if
               return
            end if
            !----------------------------------------------------------------

            !------------- first argument -----------------------------------
            arg1 = parse_expression()
            if (eval_error) then
               f = [bad_value];  return
            end if

            call skip_spaces()
            have_second = .false.
            if (curr_char == ',') then
               have_second = .true.
               call next_char()
               arg2 = parse_expression()
               if (eval_error) then
                  f = [bad_value];  return
               end if
            end if
            if (curr_char == ')') call next_char()

            !------------- dispatch -----------------------------------------
            select case (trim(id))

            case ("cor", "cov", "dot") ! correlation, covariance, dot product
               if (.not. have_second) then
                  print *, "Error: function needs two arguments"
                  eval_error = .true.;  f = [bad_value]
               else if (size(arg1) /= size(arg2)) then
                  print "(a,i0,1x,i0,a)", "Error: function array arguments have sizes ", &
                                           size(arg1), size(arg2), " must be equal"
                  eval_error = .true.;  f = [bad_value]
               else if (id == "cor" .or. id == "cov" .and. size(arg1) < 2) then
                  print *, "Error: function array arguments must have sizes > 1"
                  eval_error = .true.;  f = [bad_value]
               else if (id == "dot") then
                  f = [dot_product(arg1, arg2)]
               else
                  if (id == "cor") then
                     f = [cor(arg1, arg2)]
                  else if (id == "cov") then
                     f = [cov(arg1, arg2)]
                  end if
               end if

            case ('min','max')                           ! two-arg intrinsics
               if (.not. have_second) then
                  print *, "Error: ", trim(id), "() needs two arguments"
                  eval_error = .true.;  f = [bad_value]
               else
                  n1 = size(arg1);  n2 = size(arg2)
                  if (n1 == n2) then
                     if (trim(id) == 'min') then
                        f = min(arg1, arg2)
                     else
                        f = max(arg1, arg2)
                     end if
                  else if (n1 == 1) then
                     if (trim(id) == 'min') then
                        f = min(arg1(1), arg2)
                     else
                        f = max(arg1(1), arg2)
                     end if
                  else if (n2 == 1) then
                     if (trim(id) == 'min') then
                        f = min(arg1, arg2(1))
                     else
                        f = max(arg1, arg2(1))
                     end if
                  else
                     print *, "Error: argument size mismatch in ", trim(id),"()"
                     eval_error = .true.;  f = [bad_value]
                  end if
               end if

            case ("runif","rnorm","arange")                       ! one-arg
               if (have_second) then
                  print *, "Error: function takes one argument"
                  eval_error = .true.;  f = [bad_value]
               else
                  nsize = int(arg1(1))
                  if (id == "runif") then
                     f = runif_vec(nsize)
                  else if (id == "rnorm") then
                     f = random_normal(nsize)
                  else
                     f = arange(nsize)
                  end if
               end if

            case ('abs','acos','acosh','asin','asinh','atan','atanh', &
                  'cos','cosh','exp','log','log10','sin','sinh','sqrt', &
                  'tan','tanh','size','sum','product', 'norm1', 'norm2','minval', &
                  'maxval','minloc','maxloc','mean','sd','cumsum','diff', &
                  'sort','indexx','rank','stdz','median','head','tail')
               if (have_second) then
                  print *, "Error: function '"//trim(id)//"' takes one argument"
                  eval_error = .true.;  f = [bad_value]
               else
                  if (index('size sum product norm1 norm2 minval maxval minloc maxloc mean sd median', &
                             trim(id)) > 0) then
                     f = [ apply_scalar_func(id, arg1) ] ! functions that take array and return scalar
                  else
                     f = apply_vec_func(id, arg1)
                  end if
               end if

            case default                                  ! subscript  x(i)
               if (have_second) then
                  print *, "Error: function '"//trim(id)//"' not defined"
                  eval_error = .true.;  f = [bad_value]
               else
                  vvar = get_variable(id)
                  if (.not. eval_error) then
                     if (any(abs(arg1 - nint(arg1)) > tol)) then
                        print *, "Error: non-integer subscript for '"//trim(id)//"'"
                        eval_error = .true.;  f = [bad_value]
                     else
                        idxv = nint(arg1)
                        if (any(idxv < 1) .or. any(idxv > size(vvar))) then
                           print *, "Error: index out of bounds for '"//trim(id)//"'"
                           eval_error = .true.;  f = [bad_value]
                        else
                           allocate(f(size(idxv)));  f = vvar(idxv)
                        end if
                     end if
                  else
                     f = [bad_value]
                  end if
               end if
            end select
            !------------------------------------------------------------------

         else                                            ! plain variable
            f = get_variable(id)
         end if

      else
         print *, "Error: unexpected character '"//curr_char//"'"
         eval_error = .true.;  f = [bad_value]
      end if
   end select

   !------------- exponentiation ------------------------------------
   call skip_spaces()
   if (curr_char == '^') then
      call next_char()
      exponent = parse_factor()
      if (.not. eval_error) then
         if (size(exponent) == 1) then
            f = f ** exponent(1)
         else if (size(f) == 1) then
            f = f(1) ** exponent
         else if (size(f) == size(exponent)) then
            f = f ** exponent
         else
            print *, "Error: size mismatch in exponentiation"
            eval_error = .true.;  f = [bad_value]
         end if
      else
         f = [bad_value]
      end if
   end if
end function parse_factor

    !--------------------------------------------------
    recursive function parse_term() result(t)
      real(kind=dp), allocatable :: t(:), f2(:), tmp(:)
      integer :: nt, nf

      t = parse_factor()
      call skip_spaces()
      do while (.not. eval_error .and. (curr_char=='*' .or. curr_char=='/'))
        if (curr_char=='*') then
          call next_char()
          f2 = parse_factor()
          nt = size(t)
          nf = size(f2)
          if (nt == nf) then
            tmp = t * f2
          else if (nf == 1) then
            tmp = t * f2(1)
          else if (nt == 1) then
            tmp = t(1) * f2
          else
            print *, "Error: size mismatch in multiplication"
            return
          end if
        else
          call next_char()
          f2 = parse_factor()
          nt = size(t)
          nf = size(f2)
          if (nt == nf) then
            tmp = t / f2
          else if (nf == 1) then
            tmp = t / f2(1)
          else if (nt == 1) then
            tmp = t(1) / f2
          else
            print *, "Error: size mismatch in division"
            return
          end if
        end if
        t = tmp
        call skip_spaces()
      end do
    end function parse_term

    !--------------------------------------------------
    !--------------------------------------------------
    recursive function parse_expression() result(e)
    !--------------------------------------------------
    ! Handles:
    !   – addition / subtraction        (+  -)
    !   – relational comparisons        (<  <=  >  >=  ==  !=)
    ! Comparison rules
    !   • scalar ∘ scalar               → size-1 array  (1 or 0)
    !   • vector ∘ vector (same size)   → size-n array
    !   • vector ∘ scalar (or vice-versa)→ size-n array
    ! If the sizes are incompatible an error is raised.
    !
      real(kind=dp), allocatable :: e(:), t(:), rhs(:)
      character(len=2)           :: op
      integer :: ne, nt
      logical :: more_rel

      !----------------  additive part (+ / -)  -------------------
      e = parse_term()
      call skip_spaces()
      do while (.not. eval_error .and. (curr_char=='+' .or. curr_char=='-'))
         if (curr_char=='+') then
            call next_char()
            t = parse_term()
            ne = size(e);  nt = size(t)
            if (ne == nt) then
               e = e + t
            else if (nt == 1) then
               e = e + t(1)
            else if (ne == 1) then
               e = e(1) + t
            else
               print *, "Error: size mismatch in addition"
               eval_error = .true.;  return
            end if
         else
            call next_char()
            t = parse_term()
            ne = size(e);  nt = size(t)
            if (ne == nt) then
               e = e - t
            else if (nt == 1) then
               e = e - t(1)
            else if (ne == 1) then
               e = e(1) - t
            else
               print *, "Error: size mismatch in subtraction"
               eval_error = .true.;  return
            end if
         end if
         call skip_spaces()
      end do

      !----------------  relational part (<  >  == …)  ------------
      call skip_spaces()
      more_rel = .true.
      do while (.not. eval_error .and. more_rel)

         !— detect operator ---------------------------------------
         op = '  '           ! blanks
         select case (curr_char)
         case ('<')
            call next_char()
            if (curr_char == '=') then
               op = '<=';  call next_char()
            else
               op = '< '
            end if
         case ('>')
            call next_char()
            if (curr_char == '=') then
               op = '>=';  call next_char()
            else
               op = '> '
            end if
         case ('=')
            call next_char()
            if (curr_char == '=') then
               op = '==';  call next_char()
            else
               op = '= '
            end if
         case ('!')
            call next_char()
            if (curr_char == '=') then
               op = '!=';  call next_char()
            else
               print *, "Error: unknown operator '!'.  Use != for < >."
               eval_error = .true.;  exit
            end if
         case default
            more_rel = .false.;  cycle
         end select

         call skip_spaces()
         rhs = parse_term()             ! RHS has same precedence chain
         if (eval_error) exit

         e = rel_compare(op, e, rhs)    ! perform comparison
         call skip_spaces()
      end do
    end function parse_expression

  end function evaluate

  !------------------------------------------------------------------------
  subroutine assign_element(lhs, rval)
    character(len=*), intent(in)                 :: lhs
    real(kind=dp), allocatable, intent(in)       :: rval(:)
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
      if (stop_if_error) stop "stopped with evaluation error"
      return
    end if

    idx = int(tmp(1))
    do vi = 1, n_vars
      if (vars(vi)%name == name) then
        if (.not. mutable) then
           print *, "Error: cannot assign element of '", trim(name), "'—mutable is .false."
           eval_error = .true.
        else
           if (idx < 1 .or. idx > size(vars(vi)%val)) then
              print *, "Error: index out of bounds for '", trim(name), "'"
              eval_error = .true.
           else
              vars(vi)%val(idx) = rval(1)
           end if
           return
        end if
      end if
    end do

    print *, "Error: undefined variable '", trim(name), "' in assignment"
    eval_error = .true.
  end subroutine assign_element

  !------------------------------------------------------------------------
  impure elemental subroutine eval_print(str)
    character(len=*), intent(in) :: str
    real(kind=dp), allocatable :: r(:)
    integer, allocatable :: rint(:)
    integer :: i, rsize
    write (tunit, "(a)") str
    if (str == "clear") then
       call clear()
       return
    else if (index(str, 'del ') == 1) then
       call delete_vars(str(5:))
       return
    else if (str == "") then
       return
    else if (.not. matched_parentheses(str)) then
       print*,"mismatched parentheses"
       return
    else if (.not. matched_brackets(str)) then
       print*,"mismatched brackets"
       return
    else if (index(str, "**") /= 0) then
       print*,"use ^ instead of ** for exponentiaton"
       return
    end if
    if (str == "?vars") then
      write(*,*) "Defined variables:"
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
    if (eval_error) then
       if (stop_if_error) stop "stopped with evaluation error"
       return
    end if

    write(*,"(/,'> ',a)") trim(str)
    rsize = size(r)
    rint = nint(r)
    if (rsize < 2) then
      if (rsize == 0) then
         print*
      else if (all(abs(r - rint) <= tol)) then
         print "(i0)", rint
      else
         if (abs(r(1)) < 1.0_dp) then
            if (r(1) >= 0) then
               print "(F8.6)", r
            else
               print "(F9.6)", r
            end if
         else
            print "(F0.6)", r
         end if
      end if
    else if (rsize <= max_print) then
      if (all(abs(r - rint) <= tol)) then
         write(*,'("[",*(i0,:," "),"]")', advance="no") rint
      else
         write(*,'("[",*(F0.6,:," "),"]")', advance="no") r
      end if
      print "(']')"
    else
      print "(*(a10))", "size", "mean", "sd", "min", "max", &
                        "first", "last"
      print "(i10, *(f10.4))", rsize, mean(r), sd(r), minval(r), &
                               maxval(r), r(1), r(rsize)
    end if
  end subroutine eval_print

  function runif_vec(n) result(r)
    integer, intent(in)        :: n
    real(kind=dp), allocatable :: r(:)
    if (n < 1) then
      allocate(r(0))
    else 
      allocate(r(n))
      call random_number(r)
    end if
  end function runif_vec

  subroutine delete_vars(list_str)
    character(len=*), intent(in) :: list_str
    character(len=32) :: nm
    integer :: start, pos, len_list, i_var, j_var
    logical :: found

    start = 1
    len_list = len_trim(list_str)
    do while (start <= len_list)
      ! find next comma
      pos = index(list_str(start:), ',')
      if (pos > 0) then
        nm = adjustl(trim(list_str(start:start+pos-2)))
        start = start + pos
      else
        nm = adjustl(trim(list_str(start:len_list)))
        start = len_list + 1
      end if

      ! try to find and delete nm
      found = .false.
      do i_var = 1, n_vars
        if (vars(i_var)%name == nm) then
          ! deallocate storage
          if (allocated(vars(i_var)%val)) deallocate(vars(i_var)%val)
          ! shift the rest down
          do j_var = i_var, n_vars-1
            vars(j_var) = vars(j_var+1)
          end do
          vars(n_vars)%name = ''
          if (allocated(vars(n_vars)%val)) deallocate(vars(n_vars)%val)
          n_vars = n_vars - 1
          found = .true.
          exit
        end if
      end do

      if (.not. found) print *, "Warning: variable '", trim(nm), "' not defined"
    end do
  end subroutine delete_vars

    !--------------------------------------------------
    function rel_compare(op, a, b) result(res)
    !--------------------------------------------------
    ! Element-wise comparison returning 1.0 or 0.0
      character(len=*), intent(in) :: op
      real(kind=dp),    intent(in) :: a(:), b(:)
      real(kind=dp), allocatable   :: res(:)
      logical, allocatable         :: mask(:)
      integer :: na, nb, n

      na = size(a);  nb = size(b)
      if (na == nb) then
         n  = na
         allocate (mask(n), source = .false.)
         select case (op)
         case ('< ') ; mask = a <  b
         case ('<='); mask = a <= b
         case ('> ') ; mask = a >  b
         case ('>='); mask = a >= b
         case ('= ') ; mask = abs(a-b) <= tol
         case ('=='); mask = abs(a-b) <= tol
         case ('!='); mask = abs(a-b) >  tol
         end select
         res = merge( 1.0_dp , 0.0_dp , mask )

      else if (nb == 1) then
         ! vector ∘ scalar
         n  = na
         allocate (mask(n), source = .false.)
         select case (op)
         case ('< ') ; mask = a <  b(1)
         case ('<='); mask = a <= b(1)
         case ('> ') ; mask = a >  b(1)
         case ('>='); mask = a >= b(1)
         case ('= ') ; mask = abs(a-b(1)) <= tol
         case ('=='); mask = abs(a-b(1)) <= tol
         case ('!='); mask = abs(a-b(1)) >  tol
         end select
         res = merge( 1.0_dp , 0.0_dp , mask )

      else if (na == 1) then
         ! scalar ∘ vector   (broadcast the scalar)
         n  = nb
         allocate (mask(n), source = .false.)
         select case (op)
         case ('< ') ; mask = a(1) <  b
         case ('<='); mask = a(1) <= b
         case ('> ') ; mask = a(1) >  b
         case ('>='); mask = a(1) >= b
         case ('= ') ; mask = abs(a(1)-b) <= tol
         case ('=='); mask = abs(a(1)-b) <= tol
         case ('!='); mask = abs(a(1)-b) >  tol
         end select
         res = merge( 1.0_dp , 0.0_dp , mask )

      else
         print *, "Error: size mismatch in relational comparison"
         eval_error = .true.
         res = [bad_value]
      end if
    end function rel_compare
end module interpret_mod
