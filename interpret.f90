module interpret_mod
  use kind_mod , only: dp
  use stats_mod, only: mean, sd, cor, cov, cumsum, diff, standardize, &
                       print_stats, skew, kurtosis
  use util_mod , only: matched_brackets, matched_parentheses, arange, &
                       head, tail, grid, print_real, is_alphanumeric, &
                       is_numeral, is_letter, zeros, ones
  use random_mod, only: random_normal, runif
  use qsort_mod, only: sorted, indexx, rank, median
  use iso_fortran_env, only: compiler_options, compiler_version
  use plot_mod, only: plot
  implicit none
  private
  public :: evaluate, eval_print, set_variable, tunit, write_code, &
     code_transcript_file, clear, vars, mutable, slice_array, &
     split_by_semicolon, delete_vars

  integer, parameter :: max_vars = 100
  integer, parameter :: max_print = 15 ! for arrays larger than this, summary stats printed instead of elements

  type :: var_t
    character(len=32) :: name = ''
    real(kind=dp), allocatable :: val(:)
  end type var_t

  type(var_t) :: vars(max_vars)
  integer :: n_vars = 0, tunit
  logical, save :: write_code = .true., eval_error = .false.
  character(len=1) :: curr_char
  character (len=*), parameter :: code_transcript_file = "code.fi" ! stores the commands issued
  logical, parameter :: stop_if_error = .false., echo_code = .true.
  real(kind=dp), parameter :: bad_value = -999.0_dp, tol = 1.0e-6_dp
  logical, parameter :: mutable = .true.   ! when .false., no reassignments allowed
contains

subroutine slice_array(name, idxs, result)
!  Return a 1-D section of variable NAME described by the text
!  in IDXS (e.g. "2:11:3", "5:", ":7:-2",).
   character(len=*), intent(in)            :: name
   character(len=*), intent(in)            :: idxs
   real(kind=dp),   allocatable, intent(out) :: result(:)

   real(kind=dp), allocatable :: v(:), larr(:), uarr(:), sarr(:)
   integer                    :: c1, c2       ! locations of ':' in IDXS
   integer                    :: i1, i2, step
   integer                    :: n            ! array size

   ! evaluate the variable itself
   v = evaluate(name)
   if (eval_error) return
   n = size(v)

   ! locate first and (optional) second ':' in the text
   c1 = index(idxs, ":")
   if (c1 == 0) then
      print*, "Error: bad slice syntax in '", trim(idxs), "'"
      eval_error = .true.
      allocate(result(0));  return
   end if
   c2 = index(idxs(c1+1:), ":")
   if (c2 > 0) c2 = c1 + c2        ! absolute position, or 0 if none

   ! lower bound
   if (c1 > 1) then
      call parse_index(idxs(:c1-1), larr, i1)
   else
      i1 = 1
   end if

   ! upper bound & stride
   if (c2 == 0) then                    ! only one ':'
      step = 1
      if (c1 < len_trim(idxs)) then
         call parse_index(idxs(c1+1:), uarr, i2)
      else
         i2 = n
      end if
   else                                  ! two ':'   stride present
      if (c2 - c1 > 1) then              !   strictly > 1 is the right check
         call parse_index(idxs(c1+1:c2-1), uarr, i2)
      else
         i2 = n          ! omitted upper bound
      end if
      call parse_index(idxs(c2+1:), sarr, step)
   end if

   ! sanity checks
   if (step == 0) then
      print*, "Error: slice stride cannot be zero"
      eval_error = .true.;  allocate(result(0));  return
   end if
   if (i1 < 1 .or. i1 > n .or. i2 < 0 .or. i2 > n) then
      print*, "Error: slice indices out of range"
      eval_error = .true.;  allocate(result(0));  return
   end if

   ! empty slice situations that are nevertheless valid
   if ((step > 0 .and. i1 >  i2)  .or. &
        (step < 0 .and. i1 <  i2)) then
      allocate(result(0))
      return
   end if

   ! finally deliver the section
   result = v(i1:i2:step)

   contains

   subroutine parse_index(str, arr, idx)
   ! Parse a slice index string str into its evaluated array arr and
   ! integer index idx.
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
  end subroutine clear

  subroutine set_variable(name, val)
  ! Store or replace a variable
    character(len=*), intent(in) :: name
    real(kind=dp),    intent(in) :: val(:)
    integer :: i
    character(len=32) :: nm

    nm = adjustl(name)
    do i = 1, n_vars
      if (vars(i)%name == nm) then
        if (.not. mutable) then
           print*, "Error: cannot reassign '" // trim(nm) // "' if mutable is .false."
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
      print*, "Error: too many variables."
      eval_error = .true.
    end if
  end subroutine set_variable

  function apply_scalar_func(fname, arr) result(r)
  ! Apply a scalar-returning function: sum, minval, maxval, etc.
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
    case ("count")   ; r = real(count(arr /= 0.0_dp), dp)
    case ("median")  ; r = median(arr)
    case ("mean")    ; r = mean(arr)
    case ("sd")      ; r = sd(arr)
    case ("skew")    ; r = skew(arr)
    case ("kurt")    ; r = kurtosis(arr)
    case ("print_stats"); call print_stats(arr); r = 0
    case default
      print*, "Error in apply_scalar_func: function '", trim(fname), "' not defined"
      eval_error = .true.
      r = bad_value
    end select
  end function apply_scalar_func

  function apply_vec_func(fname, arr) result(res)
  ! Apply a function that takes an array and returns an array
    character(len=*), intent(in)       :: fname
    real(kind=dp),    intent(in)       :: arr(:)
    real(kind=dp), allocatable :: res(:)

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
    case ("bessel_j0"); res = bessel_j0(arr)
    case ("bessel_j1"); res = bessel_j1(arr)
    case ("bessel_y0"); res = bessel_y0(arr)
    case ("bessel_y1"); res = bessel_y1(arr)
    case ("gamma"); res = gamma(arr)
    case ("log_gamma"); res = log_gamma(arr)
    case ("cosd"); res = cosd(arr)
    case ("sind"); res = sind(arr)
    case ("tand"); res = tand(arr)
    case ("acosd"); res = acosd(arr)
    case ("asind"); res = asind(arr)
    case ("atand"); res = atand(arr)
    case ("spacing"); res = spacing(arr)
    case ("cumsum"); res = cumsum(arr)
    case ("diff"); res = diff(arr)
    case ("head"); res = head(arr)
    case ("tail"); res = tail(arr)
    case ("sort"); res = sorted(arr)
    case ("indexx"); res = indexx(arr)
    case ("rank"); res = rank(arr)
    case ("stdz"); res = standardize(arr)
    case default
      print*, "Error in apply_vec_func: function '", trim(fname), "' not defined"
      eval_error = .true.
      res = [bad_value]
    end select
  end function apply_vec_func

  recursive function evaluate(str) result(res)
  ! Evaluate the input string str as an expression or assignment
  ! and return its result array res

    character(len=*), intent(in) :: str
    real(kind=dp),   allocatable :: res(:)

    !- local to this outer shell ----------------------------------
    character(len=:), allocatable :: expr, lhs, rhs
    integer                      :: pos, lenstr      ! parser cursor & length
    integer                      :: i, eqpos         ! scan index & "=" position
  !------------------------------------------------------------------

    ! prepare the string for parsing 
    call init_evaluator(trim(str), expr, lenstr, pos)

    ! look for an *assignment* = that is **not** part of >= <= == <=
    eqpos = 0
    do i = 1, lenstr
       if (expr(i:i) == '=') then
          if (i > 1  .and. any(expr(i-1:i-1) == ['>','<','!','=','/'])) cycle
          if (i < lenstr .and. expr(i+1:i+1) == '=') cycle
          eqpos = i
          exit                       ! first qualifying = wins
       end if
    end do

    ! assignment found  evaluate RHS then store 
    if (eqpos > 0) then
       lhs = adjustl(expr(1:eqpos-1))
       rhs = expr(eqpos+1:)
       res = evaluate(rhs)           ! recursive call
       if (.not. eval_error) then
          if (index(lhs,'(') > 0 .and. index(lhs,')') > index(lhs,'(')) then
             call assign_element(lhs, res)   ! element assignment  a(i)=
          else
             call set_variable  (lhs, res)   ! wholevariable assignment
          end if
       end if
       return
    end if

    ! no =  treat the whole string as an expression 
    res = parse_expression()
    ! detect any extraneous characters left on the line
    call skip_spaces()
    if (curr_char /= char(0)) then
       print*, "Error: unexpected input after valid expression: '", &
                trim(expr(pos-1:lenstr)), "'"
       eval_error = .true.
       ! return an empty result to signal failure
       res = [real(kind=dp) ::] 
    end if

  contains

    !--------------------------------------------------
    subroutine init_evaluator(str_in, expr, lenstr, pos)
    ! Prepare parser state: copy str_in into expr and set lenstr
    ! and reset pos for evaluation
      character(len=*), intent(in)               :: str_in
      character(len=:), allocatable, intent(out) :: expr
      integer, intent(out)                       :: lenstr, pos

      expr   = str_in
      lenstr = len_trim(expr)
      pos    = 1
      eval_error = .false.
      call next_char()
    end subroutine init_evaluator

    subroutine next_char()
    ! Advance the parser cursor to the next character in expr
    ! updating curr_char and pos
      if (pos > lenstr) then
        curr_char = char(0)
      else
        curr_char = expr(pos:pos)
      end if
      pos = pos + 1
    end subroutine next_char

    subroutine skip_spaces()
    ! Advance pos until non-space is found
      do while (curr_char == " ")
        call next_char()
      end do
    end subroutine skip_spaces

    function parse_number() result(num)
    ! Read a numeric literal starting at the current cursor
    ! and return it as a one-element array num
      real(kind=dp), allocatable :: num(:)
      character(len=64) :: buf
      integer :: i
      real(kind=dp) :: tmp
      call skip_spaces()
      i = 0
      do while (is_numeral(curr_char) .or. curr_char == '.')
        i = i + 1
        buf(i:i) = curr_char
        call next_char()
      end do
      read(buf(1:i), *) tmp
      num = [tmp]
    end function parse_number

    function parse_identifier() result(name_out)
    ! Read an alphanumeric identifier from the current cursor
    ! and return it as name_out
      character(len=32) :: name_out
      integer :: i
      call skip_spaces()
      i = 0
      do while (is_alphanumeric(curr_char) .or. curr_char == "_")
        i = i + 1
        name_out(i:i) = curr_char
        call next_char()
      end do
      name_out = adjustl(name_out(1:i))
    end function parse_identifier

    function get_variable(name) result(v)
    ! Look up variable name in storage and return its value array v
    ! or signal an undefined-variable error
      character(len=*), intent(in) :: name
      real(kind=dp), allocatable :: v(:)
      integer :: i

      do i = 1, n_vars
        if (vars(i)%name == name) then
          v = vars(i)%val
          return
        end if
      end do

      print*, "Error: undefined variable '", trim(name), "'"
      eval_error = .true.
      v = [bad_value]
    end function get_variable

    !parse_array
    recursive function parse_array() result(arr)
      real(kind=dp), allocatable :: arr(:), tmp(:), elem(:)
      integer :: total, ne

      ! consume the '[' 
      call next_char()
      call skip_spaces()

      ! empty array literal [] 
      if (curr_char == ']') then
        allocate(arr(0))
        call next_char()
        return
      end if

      total = 0
      allocate(arr(0))

      do
        ! parse one element (may itself be an array) 
        elem = parse_expression()
        if (eval_error) return
        ne = size(elem)

        ! append elem to arr 
        if (allocated(tmp)) deallocate(tmp)
        allocate(tmp(total+ne))
        if (total > 0) tmp(1:total) = arr
        tmp(total+1:total+ne) = elem
        arr = tmp
        total = total + ne

        ! now skip any spaces, then decide what to do 
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

recursive function parse_factor() result(f)
!--------------------------------------------------------------------
   real(kind=dp), allocatable :: f(:) ! result

   !===================  locals  =====================================
   real(kind=dp), allocatable :: arg1(:), arg2(:), arg3(:)
   real(kind=dp), allocatable :: exponent(:), vvar(:)
   integer,          allocatable :: idxv(:)
   character(len=32) :: id
   character(len=:), allocatable :: idxs
   integer :: nsize, pstart, pend, depth, n1, n2
   logical :: is_neg, have_second
   logical :: toplevel_colon, toplevel_comma

   call skip_spaces()

   !---------------- unary  -----------------------------------------
   if (curr_char == '+' .or. curr_char == '-') then
      is_neg = (curr_char == '-')
      call next_char()
      f = parse_factor()
      if (.not. eval_error .and. is_neg) f = -f
      return
   end if

   select case (curr_char)
   case ("(")                                    ! parenthesised expr.
      call next_char()
      f = parse_expression()
      if (curr_char == ")") call next_char()

   case ("[")                                    ! array literal
      f = parse_array()

   case default
      if (is_numeral(curr_char) .or. curr_char == ".") then
         f = parse_number()

      else if (is_letter(curr_char)) then

         id = parse_identifier()
         call skip_spaces()

         !-----------------------------------------------------------------
         if (curr_char == "(") then            !  id()
            call next_char()                   !  consume "("
            call skip_spaces()

            !============ ZERO-ARGUMENT SPECIAL CASE ======================
            if (curr_char == ")") then         !  e.g. runif()
               call next_char()                !  consume ")"
               select case (trim(id))
               case ("runif")
                  allocate(f(1))
                  call random_number(f(1))
               case ("rnorm")
                  f = random_normal(1)
               case default
                  print*, "Error: function '"//trim(id)//"' needs arguments"
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
               case ("("); depth = depth + 1
               case (")"); depth = depth - 1
               case (":")
                  if (depth == 1) toplevel_colon = .true.
               case (",")
                  if (depth == 1) toplevel_comma = .true.
               end select
            end do
            if (depth /= 0) then
               print*, "Error: mismatched parentheses"
               eval_error = .true.;  f = [bad_value];  return
            end if

            !---------------- slice?  -------------------------------------
            if (toplevel_colon .and. .not. toplevel_comma) then
               idxs = expr(pstart:pend-1)
               call slice_array(id, idxs, f)

               ! advance cursor just past ")"
               pos = pend + 1
               if (pos > lenstr) then
                  curr_char = char(0)
               else
                  curr_char = expr(pos:pos);  pos = pos + 1
               end if
               return
            end if

            !------------- first argument -----------------------------------
            arg1 = parse_expression()
            if (eval_error) then
               f = [bad_value];  return
            end if

            call skip_spaces()
            have_second = .false.
            if (curr_char == ",") then
               have_second = .true.
               call next_char()
               arg2 = parse_expression()
               if (eval_error) then
                  f = [bad_value];  return
               end if
            end if
            if (curr_char == ")") call next_char()

            !------------- dispatch -----------------------------------------
            select case (trim(id))

            case ("cor", "cov", "dot") ! correlation, covariance, dot product
               if (.not. have_second) then
                  print*, "Error: function needs two arguments"
                  eval_error = .true.;  f = [bad_value]
               else if (size(arg1) /= size(arg2)) then
                  print "(a,i0,1x,i0,a)", "Error: function array arguments have sizes ", &
                                           size(arg1), size(arg2), " must be equal"
                  eval_error = .true.;  f = [bad_value]
               else if (id == "cor" .or. id == "cov" .and. size(arg1) < 2) then
                  print*, "Error: function array arguments must have sizes > 1"
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

            case ("min","max")                           ! two-arg intrinsics
               if (.not. have_second) then
                  print*, "Error: ", trim(id), "() needs two arguments"
                  eval_error = .true.;  f = [bad_value]
               else
                  n1 = size(arg1);  n2 = size(arg2)
                  if (n1 == n2) then
                     if (trim(id) == "min") then
                        f = min(arg1, arg2)
                     else
                        f = max(arg1, arg2)
                     end if
                  else if (n1 == 1) then
                     if (trim(id) == "min") then
                        f = min(arg1(1), arg2)
                     else
                        f = max(arg1(1), arg2)
                     end if
                  else if (n2 == 1) then
                     if (trim(id) == "min") then
                        f = min(arg1, arg2(1))
                     else
                        f = max(arg1, arg2(1))
                     end if
                  else
                     print*, "Error: argument size mismatch in ", trim(id),"()"
                     eval_error = .true.;  f = [bad_value]
                  end if
               end if
 
            case ("pack")
              if (.not. have_second) then
                 print *, "Error: pack() needs two arguments"
                 eval_error = .true.
                 f = [bad_value]
              else if (size(arg1) /= size(arg2)) then
                 print *, "Error: pack() arguments must have same size"
                 eval_error = .true.
                 f = [bad_value]
              else
                 ! intrinsic PACK(source, mask) returns a 1-D array of those source(i)
                 ! for which mask(i) is .true.  Here we treat nonzero arg2 as .true.
                 f = pack(arg1, arg2 /= 0.0_dp)
              end if

            case ("runif","rnorm","arange","zeros","ones") ! one-arg
               if (have_second) then
                  print*, "Error: function takes one argument"
                  eval_error = .true.;  f = [bad_value]
               else
                  nsize = nint(arg1(1))
                  select case (id)
                     case ("runif") ; f = runif(nsize)
                     case ("rnorm") ; f = random_normal(nsize)
                     case ("arange"); f = arange(nsize)
                     case ("zeros") ; f = zeros(nsize)
                     case ("ones")  ; f = ones(nsize)
                  end select
               end if

   case ("grid") ! grid(n,x0,xh)
      if (.not. have_second) then
         ! we have only one argument so far  need two more
         print*, "Error: grid(n,x0,xh) needs three arguments"
         eval_error = .true.;  f = [bad_value]
      else
         ! arg1 and arg2 have already been parsed --> read arg3
         call skip_spaces()
         if (curr_char /= ",") then
            print*, "Error: grid(n,x0,xh) needs three arguments"
            eval_error = .true.;  f = [bad_value]
         else
            call next_char()
            call skip_spaces()
            ! ---------------- third argument ----------------
            arg3 = parse_expression()
            if (eval_error) then
               f = [bad_value]
            else
            ! ---- scalar-checks and the actual call -------
               if (size(arg1) /= 1 .or. size(arg2) /= 1 .or. size(arg3) /= 1) then
                  print*, "Error: grid arguments must be scalars"
                  eval_error = .true.;  f = [bad_value]
               else
                  f = grid(nint(arg1(1)), arg2(1), arg3(1))
               end if
               call skip_spaces()
               if (curr_char == ")") call next_char()
            end if
         end if
      end if

   case ("abs","acos","acosh","asin","asinh","atan","atanh","cos","cosh", &
         "exp","log","log10","sin","sinh","sqrt","tan","tanh","size", &
         "sum","product", "norm1", "norm2","minval","maxval","minloc", &
         "maxloc","count","mean","sd","cumsum","diff","sort","indexx","rank", &
         "stdz","median","head","tail","bessel_j0","bessel_j1", &
         "bessel_y0","bessel_y1","gamma","log_gamma","cosd","sind","tand", &
         "acosd","asind","atand","spacing","skew","kurt","print_stats")
      if (have_second) then
         print "(a)", "Error: function '" // trim(id) // "' takes one argument"
         eval_error = .true.;  f = [bad_value]
      else
         if (index("size sum product norm1 norm2 minval maxval minloc " // &
          "maxloc count mean sd median print_stats skew kurt", trim(id)) > 0) then
            f = [apply_scalar_func(id, arg1)] ! functions that take array and return scalar
         else
            f = apply_vec_func(id, arg1)
         end if
      end if

   case ("merge")                 ! *** NEW branch ***
      if (.not. have_second) then
         print*, "Error: merge() needs three arguments"
         eval_error = .true.;  f = [bad_value]

      else
         ! arg1 and arg2 have already been parsed
         call skip_spaces()
         if (curr_char /= ",") then
            print*, "Error: merge() needs three arguments"
            eval_error = .true.;  f = [bad_value]
         else
            call next_char()                 ! skip the comma
            call skip_spaces()
            arg3 = parse_expression()        ! ----- third argument -----
            if (.not. eval_error) then
               f = merge_array(arg1, arg2, arg3)
               call skip_spaces()
               if (curr_char == ")") call next_char()
            else
               f = [bad_value]
            end if
         end if
      end if

            case ("plot")                       ! 2-argument statement
               if (.not. have_second) then
                  print*, "Error: plot() needs two arguments"
                  eval_error = .true.
                  f = [bad_value]
               else if (size(arg1) /= size(arg2)) then
                  print*, "Error: plot() arguments must have same size"
                  eval_error = .true.
                  f = [bad_value]
               else
                  call plot(arg1, arg2)         ! <-- actual drawing
                  allocate(f(0))                ! return “nothing”
               end if

   case default ! subscript  x(i)
      if (have_second) then
         print*, "Error in have_second: function '"//trim(id)//"' not defined"
         eval_error = .true.;  f = [bad_value]
      else
         vvar = get_variable(id)
         if (.not. eval_error) then
            if (any(abs(arg1 - nint(arg1)) > tol)) then
               print*, "Error: non-integer subscript for '"//trim(id)//"'"
                        eval_error = .true.;  f = [bad_value]
            else
               idxv = nint(arg1)
               if (any(idxv < 1) .or. any(idxv > size(vvar))) then
                  print*, "Error: index out of bounds for '"//trim(id)//"'"
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

else                                            ! plain variable
   f = get_variable(id)
end if

else
   print*, "Error: unexpected character '"//curr_char//"'"
   eval_error = .true.;  f = [bad_value]
end if
end select

   !------------- exponentiation ------------------------------------
   call skip_spaces()
   if (curr_char == "^") then
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
            print*, "Error: size mismatch in exponentiation"
            eval_error = .true.;  f = [bad_value]
         end if
      else
         f = [bad_value]
      end if
   end if
end function parse_factor

    recursive function parse_term() result(t)
    ! Parse and evaluate a sequence of factors joined by "*" or "/"
    ! returning t
      real(kind=dp), allocatable :: t(:), f2(:), tmp(:)
      integer :: nt, nf

      t = parse_factor()
      call skip_spaces()
      do while (.not. eval_error .and. (curr_char=="*" .or. curr_char=="/"))
        if (curr_char=="*") then
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
            print*, "Error: size mismatch in multiplication"
            return
          end if
        else
          ! If the next character is "=", this is `/=`; leave it to the
          !  relational layer above and break out of the *term* loop.
          if (pos <= lenstr .and. expr(pos:pos) == "=") exit
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
            print*, "Error: size mismatch in division"
            return
          end if
        end if
        t = tmp
        call skip_spaces()
      end do
    end function parse_term

    recursive function parse_expression() result(e)
    ! Handles:
    !    addition / subtraction        (+  -)
    !    relational comparisons        (<  <=  >  >=  ==  <=)
    ! Comparison rules
    !    scalar  scalar                size-1 array  (1 or 0)
    !    vector  vector (same size)    size-n array
    !    vector  scalar (or vice-versa) size-n array
    ! If the sizes are incompatible an error is raised.
      real(kind=dp), allocatable :: e(:), t(:), rhs(:)
      character(len=2)           :: op
      integer :: ne, nt
      logical :: more_rel

      !----------------  additive part (+ / -)  -------------------
      e = parse_term()
      call skip_spaces()
      do while (.not. eval_error .and. (curr_char=="+" .or. curr_char=="-"))
         if (curr_char=="+") then
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
               print*, "Error: size mismatch in addition"
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
               print*, "Error: size mismatch in subtraction"
               eval_error = .true.;  return
            end if
         end if
         call skip_spaces()
      end do

      !----------------  relational part (<  >  ==)  ------------
      call skip_spaces()
      more_rel = .true.
      do while (.not. eval_error .and. more_rel)

         ! detect operator ---------------------------------------
         op = "  "           ! blanks
         select case (curr_char)
         case ("<")
            call next_char()
            if (curr_char == "=") then
               op = "<=";  call next_char()
            else
               op = "< "
            end if
         case (">")
            call next_char()
            if (curr_char == "=") then
               op = ">=";  call next_char()
            else
               op = "> "
            end if
         case ("=")
            call next_char()
            if (curr_char == "=") then
               op = "==";  call next_char()
            else
               op = "= "
            end if
         case ("/")
            call next_char()
            if (curr_char == "=") then
               op = "/=";  call next_char()
            else
               print*, "Error: error with /"
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

  subroutine assign_element(lhs, rval)
  ! ---------------------------------------------------------------------------
  ! Generalised element/section assignment.
  !
  ! * LHS is of the form  var(indices)  where **indices** may be a scalar
  !   or a vector.
  ! * If RVAL has size 1  -> broadcast to every index in INDICES
  ! * If RVAL size equals size(INDICES) -> element–wise assignment
  ! * Otherwise → size-mismatch error
  ! ---------------------------------------------------------------------------
     character(len=*),           intent(in)  :: lhs
     real(kind=dp), allocatable, intent(in)  :: rval(:)

     character(len=32)               :: name
     character(len=:),  allocatable  :: idx_txt
     real(kind=dp),     allocatable  :: idx_val(:)
     integer,           allocatable  :: idx(:)
     integer :: p_lpar, p_rpar, vi, n_idx

     ! ---- split "var( … )" into name and index string ---------------------
     p_lpar = index(lhs, "(")
     p_rpar = index(lhs, ")")
     name   = adjustl(lhs(1:p_lpar-1))
     idx_txt = lhs(p_lpar+1:p_rpar-1)

     ! ---- evaluate index expression ---------------------------------------
     idx_val = evaluate(idx_txt)
     if (eval_error) then
        if (stop_if_error) stop "stopped with evaluation error"
        return
     end if

     ! ---- convert to integer(s) -------------------------------------------
     if (any(abs(idx_val - nint(idx_val)) > tol)) then
        print*, "Error: non-integer subscript in assignment to '", trim(name), "'"
        eval_error = .true.
        return
     end if
     n_idx = size(idx_val)
     allocate(idx(n_idx))
     idx = nint(idx_val)

     ! ---- locate the variable ---------------------------------------------
     do vi = 1, n_vars
        if (vars(vi)%name == name) then
           if (.not. mutable) then
              print*, "Error: cannot assign to '"//trim(name)//"' when mutable=.false."
              eval_error = .true.
              return
           end if

           if (any(idx < 1) .or. any(idx > size(vars(vi)%val))) then
              print*, "Error: index out of bounds in assignment to '"//trim(name)//"'"
              eval_error = .true.
              return
           end if

           if (size(rval) == 1) then                 ! broadcast scalar
              vars(vi)%val(idx) = rval(1)
           else if (size(rval) == n_idx) then        ! element-wise vector
              vars(vi)%val(idx) = rval
           else
              print*, "Error: size mismatch in assignment to '" // trim(name) // "'"
              eval_error = .true.
           end if
           return
        end if
     end do

     ! ---- variable not found ----------------------------------------------
     print*, "Error: undefined variable '", trim(name), "' in assignment"
     eval_error = .true.
  end subroutine assign_element

impure elemental subroutine eval_print(line)
   character(len=*), intent(in) :: line
   ! --------------------------------------------------------------
   ! 1.  split the input at *top‑level* semicolons
   ! --------------------------------------------------------------
   integer                       :: n, k, rsize
   character(len=:), allocatable :: parts(:)
   logical       , allocatable   :: suppress(:)
   real(dp)      , allocatable   :: r(:)
   integer       , allocatable   :: rint(:)

   ! write to transcript just once, for the whole input line
   if (write_code) write(tunit,'(a)') line

   call split_by_semicolon(line, n, parts, suppress)

   do k = 1, n
      if (trim(parts(k)) == '') cycle          ! blank segment

      ! ---------- syntax checks exactly as before ----------
      if (.not. matched_parentheses(parts(k))) then
         print*, "mismatched parentheses" ; cycle
      end if
      if (.not. matched_brackets(parts(k))) then
         print*, "mismatched brackets"    ; cycle
      end if
      if (index(parts(k),'**') /= 0) then
         print*, "use ^ instead of ** for exponentiation" ; cycle
      end if

      ! ------------------------------------------------------
      r = evaluate(parts(k))
      if (eval_error) then
         if (stop_if_error) stop "stopped with evaluation error"
         cycle
      end if
      if (index(trim(parts(k)),'print_stats') == 1) cycle

      ! ---------- echo only when the segment is *not* suppressed ----------
      if (.not. suppress(k)) then
         if (echo_code) write(*,'(/,"> ",a)') trim(parts(k))
         rsize = size(r)
         if (rsize == 0) then
            print*
         else
            rint = nint(r)
            select case (rsize)
            case (1)
               if (abs(r(1)-rint(1)) <= tol) then
                  print "(i0)", rint
               else
                  call print_real(r(1))
               end if
            case default
                if (rsize <= max_print) then
                   if (all(abs(r-rint) <= tol)) then
                      ! integers ---------------------------------------------------
                      write(*,'("[",*(i0,:,", "))', advance="no") rint   ! open ‘[’ but no LF
                   else
                      ! reals -------------------------------------------------------
                      write(*,'("[",*(F0.6,:,", "))', advance="no") r    ! ditto
                   end if
                   print "(']')"            ! print the closing bracket and terminate the line
                else
                   call print_stats(r)
                end if
            end select
         end if
      end if
   end do
end subroutine eval_print

  subroutine delete_vars(list_str)
  ! Remove all variables named in the comma-separated list_str
  ! from storage, warning on missing names
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

      if (.not. found) print*, "Warning: variable '", trim(nm), "' not defined"
    end do
  end subroutine delete_vars

    function rel_compare(op, a, b) result(res)
    ! Element-wise comparison returning 1.0 or 0.0
      character(len=*), intent(in) :: op
      real(kind=dp),    intent(in) :: a(:), b(:)
      real(kind=dp), allocatable   :: res(:)
      logical, allocatable         :: mask(:)
      integer :: na, nb, n
      na = size(a)
      nb = size(b)
      if (na == nb) then
         n  = na
         allocate (mask(n), source = .false.)
         select case (op)
            case ("< ") ; mask = a <  b
            case ("<=") ; mask = a <= b
            case ("> ") ; mask = a >  b
            case (">=") ; mask = a >= b
            case ("= ") ; mask = abs(a-b) <= tol
            case ("==") ; mask = abs(a-b) <= tol
            case ("/=") ; mask = abs(a-b) >  tol
         end select
         res = merge(1.0_dp , 0.0_dp , mask)

      else if (nb == 1) then
         ! vector - scalar
         n  = na
         allocate (mask(n), source = .false.)
         select case (op)
            case ("< ") ; mask = a <  b(1)
            case ("<=") ; mask = a <= b(1)
            case ("> ") ; mask = a >  b(1)
            case (">=") ; mask = a >= b(1)
            case ("= ") ; mask = abs(a-b(1)) <= tol
            case ("==") ; mask = abs(a-b(1)) <= tol
            case ("/=") ; mask = abs(a-b(1)) >  tol
         end select
         res = merge(1.0_dp , 0.0_dp , mask)

      else if (na == 1) then
         ! scalar - vector   (broadcast the scalar)
         n  = nb
         allocate (mask(n), source = .false.)
         select case (op)
            case ("< ") ; mask = a(1) <  b
            case ("<=") ; mask = a(1) <= b
            case ("> ") ; mask = a(1) >  b
            case (">=") ; mask = a(1) >= b
            case ("= ") ; mask = abs(a(1)-b) <= tol
            case ("==") ; mask = abs(a(1)-b) <= tol
            case ("/=") ; mask = abs(a(1)-b) >  tol
         end select
         res = merge(1.0_dp , 0.0_dp , mask)
      else
         print*, "Error: size mismatch in relational comparison"
         eval_error = .true.
         res = [bad_value]
      end if
    end function rel_compare

  function merge_array(t_source, f_source, mask_val) result(res)
  !! Elemental-style MERGE for the interpreter.
  !! – Any of the three inputs may be size-1 (scalar) or an array.
     real(dp), intent(in)          :: t_source(:)
     real(dp), intent(in)          :: f_source(:)
     real(dp), intent(in)          :: mask_val(:)   ! zero → .false., non-zero → .true.
     real(dp), allocatable         :: res(:)

     integer :: nt, nf, nm, n
     logical, allocatable :: lmask(:)
     real(dp), allocatable :: t(:), f(:)

     nt = size(t_source);  nf = size(f_source);  nm = size(mask_val)
     n  = max(nt, nf, nm)

     ! ---- conformability checks -----------------------------------------
     if (   (nt /= 1 .and. nt /= n) &
        .or.(nf /= 1 .and. nf /= n) &
        .or.(nm /= 1 .and. nm /= n) ) then
        print*, "Error: merge() arguments are not conformable"
        eval_error = .true.;  res = [bad_value];  return
     end if

     ! ---- broadcast scalars where necessary -----------------------------
     allocate(t(n), f(n), lmask(n))
     if (nt == 1) then
        t = t_source(1)
     else
        t = t_source
     end if
     if (nf == 1) then
        f = f_source(1)
     else
        f = f_source
     end if
     if (nm == 1) then
        lmask = mask_val(1) /= 0.0_dp
     else
        lmask = mask_val /= 0.0_dp
     end if

     ! ---- element-wise selection ----------------------------------------
     allocate(res(n))
     res = merge(t, f, lmask)   ! use intrinsic MERGE now that shapes match
  end function merge_array

subroutine split_by_semicolon(line, n, parts, suppress)
!  Break LINE into statements separated by *top‑level* semicolons.
!  parts(i)   = i‑th statement (trimmed)
!  suppress(i)= .true. if that statement ended with a ';'
   character(len=*), intent(in)  :: line
   integer           , intent(out) :: n
   character(len=:),  allocatable  :: parts(:)
   logical           , allocatable  :: suppress(:)

   character(len=:), allocatable :: buf
   integer :: i, depth_par, depth_br

   buf        = ''
   depth_par  = 0      ! '(' … ')'
   depth_br   = 0      ! '[' … ']'
   n          = 0

   do i = 1, len_trim(line)
      select case (line(i:i))
      case ('('); depth_par = depth_par + 1
      case (')'); depth_par = depth_par - 1
      case ('['); depth_br  = depth_br  + 1
      case (']'); depth_br  = depth_br  - 1
      case (';')
         if (depth_par==0 .and. depth_br==0) then
            call append_statement(buf, .true.)
            buf = ''
            cycle
         end if
      end select
      buf = buf // line(i:i)
   end do

   ! last (or only) statement
   if (len_trim(buf) > 0) then
      call append_statement(buf, .false.)
   else if (len_trim(line) > 0 .and. line(len_trim(line):len_trim(line)) == ';') then
      call append_statement('', .true.)
   end if

contains
   subroutine append_statement(txt, semi)
      character(len=*), intent(in) :: txt
      logical        , intent(in) :: semi
      integer :: newlen

      newlen = max( len_trim(txt), merge(0, len(parts(1)), allocated(parts)) )

      ! ---- grow / (re)allocate PARTS ------------------------------------
      if (.not. allocated(parts)) then
         allocate(character(len=newlen) :: parts(1))
         allocate(suppress(1))
      else if (len(parts(1)) < newlen) then
         call enlarge_parts(newlen)
      else
         parts   = [character (len=len(parts)) :: parts, '']                     ! extend by one element
         suppress = [suppress, .false.]
      end if

      ! ---- store the new statement --------------------------------------
      n              = n + 1
      parts(n)       = adjustl(trim(txt))
      suppress(n)    = semi
   end subroutine append_statement

   subroutine enlarge_parts(newlen)
      integer, intent(in) :: newlen
      character(len=newlen), allocatable :: tmp(:)

      allocate(tmp(size(parts)))
      tmp = parts                             ! old contents, padded
      call move_alloc(tmp, parts)             ! now PARTS has the new length
      parts = [parts, '']                     ! add a new blank slot
      suppress = [suppress, .false.]
   end subroutine enlarge_parts
end subroutine split_by_semicolon

end module interpret_mod
