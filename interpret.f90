module interpret_mod
   use kind_mod, only: dp
   use stats_mod, only: mean, sd, cor, cov, cumsum, cumprod, diff, standardize, &
                        print_stats, skew, kurtosis, cummean, cummin, cummax, &
                        geomean, harmean
   use util_mod, only: matched_brackets, matched_parentheses, arange, &
                       head, tail, grid, print_real, is_alphanumeric, &
                       is_numeral, is_letter, zeros, ones, replace, &
                       rep, read_vec, reverse
   use random_mod, only: random_normal, runif
   use qsort_mod, only: sorted, indexx, rank, median, unique
   use iso_fortran_env, only: compiler_options, compiler_version
   use plot_mod, only: plot, plot_to_label
   implicit none
   private
   public :: eval_print, tunit, code_transcript_file, vars, write_code, &
             echo_code

   integer, parameter :: max_vars = 100, len_name = 32
   integer, parameter :: max_print = 15 ! for arrays larger than this, summary stats printed instead of elements

   type :: var_t
      character(len=len_name) :: name = ""
      real(kind=dp), allocatable :: val(:)
   end type var_t

   type(var_t) :: vars(max_vars)
   integer :: n_vars = 0, tunit
   logical, save :: write_code = .true., eval_error = .false., &
                    echo_code = .true.
   character(len=1) :: curr_char
   character(len=*), parameter :: code_transcript_file = "code.fi" ! stores the commands issued
   character(len=*), parameter :: comment_char = "!"
   logical, parameter :: stop_if_error = .false.
   real(kind=dp), parameter :: bad_value = -999.0_dp, tol = 1.0e-6_dp
   logical, parameter :: mutable = .true.   ! when .false., no reassignments allowed
   logical, save :: print_array_as_int_if_possible = .true.
   character(len=:), allocatable :: line_cp
   logical, save :: in_loop_execute = .false.   ! .true. only inside run_loop_body
   logical, save :: exit_loop = .false., cycle_loop = .false.
   logical, parameter :: debug_read = .false.

!––– support for DO … END DO loops –––––––––––––––––––––––––––––––––
!── Maximum nesting and a fixed buffer for every loop level
   integer, parameter :: max_loop_depth = 8
   character(len=4096), save :: loop_body(max_loop_depth) = ""   ! collected lines
   character(len=len_name), save :: loop_var(max_loop_depth) = ""   ! i , j , ...
   integer, save :: loop_start(max_loop_depth) = 0
   integer, save :: loop_end(max_loop_depth) = 0
   integer, save :: loop_step(max_loop_depth) = 1
   integer, save :: loop_depth = 0                     ! current level
!––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––

contains

   subroutine slice_array(name, idxs, result)
!  Return a 1-D section of variable NAME described by the text
!  in IDXS (e.g. "2:11:3", "5:", ":7:-2",).
      character(len=*), intent(in)            :: name
      character(len=*), intent(in)            :: idxs
      real(kind=dp), allocatable, intent(out) :: result(:)

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
         print *, "Error: bad slice syntax in '", trim(idxs), "'"
         eval_error = .true.
         allocate (result(0)); return
      end if
      c2 = index(idxs(c1 + 1:), ":")
      if (c2 > 0) c2 = c1 + c2        ! absolute position, or 0 if none

      ! lower bound
      if (c1 > 1) then
         call parse_index(idxs(:c1 - 1), larr, i1)
      else
         i1 = 1
      end if

      ! upper bound & stride
      if (c2 == 0) then                    ! only one ':'
         step = 1
         if (c1 < len_trim(idxs)) then
            call parse_index(idxs(c1 + 1:), uarr, i2)
         else
            i2 = n
         end if
      else                                  ! two ':'   stride present
         if (c2 - c1 > 1) then              !   strictly > 1 is the right check
            call parse_index(idxs(c1 + 1:c2 - 1), uarr, i2)
         else
            i2 = n          ! omitted upper bound
         end if
         call parse_index(idxs(c2 + 1:), sarr, step)
      end if

      ! sanity checks
      if (step == 0) then
         print *, "Error: slice stride cannot be zero"
         eval_error = .true.; allocate (result(0)); return
      end if
      if (i1 < 1 .or. i1 > n .or. i2 < 0 .or. i2 > n) then
         print *, "Error: slice indices out of range"
         eval_error = .true.; allocate (result(0)); return
      end if

      ! empty slice situations that are nevertheless valid
      if ((step > 0 .and. i1 > i2) .or. &
          (step < 0 .and. i1 < i2)) then
         allocate (result(0))
         return
      end if

      ! finally deliver the section
      result = v(i1:i2:step)

   contains

      subroutine parse_index(str, arr, idx)
         ! Parse a slice index string str into its evaluated array arr and
         ! integer index idx.
         character(len=*), intent(in)            :: str
         real(kind=dp), allocatable, intent(out) :: arr(:)
         integer, intent(out)            :: idx

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
      do i = 1, min(n_vars, max_vars)
         vars(i)%name = ""
         if (allocated(vars(i)%val)) deallocate (vars(i)%val)
      end do
      n_vars = 0
   end subroutine clear

   subroutine set_variable(name, val)
      ! Store or replace a variable
      character(len=*), intent(in) :: name
      real(kind=dp), intent(in) :: val(:)
      integer :: i
      character(len=len_name) :: nm

      nm = adjustl(name)
      do i = 1, n_vars
         if (vars(i)%name == nm) then
            if (.not. mutable) then
               print *, "Error: cannot reassign '"//trim(nm)//"' if mutable is .false."
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

   function apply_scalar_func(fname, arr) result(r)
      ! Apply a scalar-returning function: sum, minval, maxval, etc.
      character(len=*), intent(in)       :: fname
      real(kind=dp), intent(in)       :: arr(:)
      real(kind=dp) :: r

      select case (trim(fname))
      case ("size"); r = size(arr)
      case ("sum"); r = sum(arr)
      case ("product"); r = product(arr)
      case ("norm1"); r = sum(abs(arr))
      case ("norm2"); r = norm2(arr)
      case ("minval"); r = minval(arr)
      case ("maxval"); r = maxval(arr)
      case ("minloc"); r = minloc(arr, dim=1)
      case ("maxloc"); r = maxloc(arr, dim=1)
      case ("count"); r = real(count(arr /= 0.0_dp), dp)
      case ("median"); r = median(arr)
      case ("mean"); r = mean(arr)
      case ("geomean"); r = geomean(arr)
      case ("harmean"); r = harmean(arr)
      case ("sd"); r = sd(arr)
      case ("skew"); r = skew(arr)
      case ("kurt"); r = kurtosis(arr)
      case ("print_stats"); call print_stats(arr); r = 0
      case default
         print *, "Error in apply_scalar_func: function '", trim(fname), "' not defined"
         eval_error = .true.
         r = bad_value
      end select
   end function apply_scalar_func

   function apply_vec_func(fname, arr) result(res)
      ! Apply a function that takes an array and returns an array
      character(len=*), intent(in)    :: fname
      real(kind=dp), intent(in)       :: arr(:)
      real(kind=dp), allocatable :: res(:)

      select case (trim(fname))
      case ("abs"); res = abs(arr)
      case ("acos"); res = acos(arr)
      case ("acosh"); res = acosh(arr)
      case ("asin"); res = asin(arr)
      case ("asinh"); res = asinh(arr)
      case ("atan"); res = atan(arr)
      case ("atanh"); res = atanh(arr)
      case ("cos"); res = cos(arr)
      case ("cosh"); res = cosh(arr)
      case ("exp"); res = exp(arr)
      case ("log"); res = log(arr)
      case ("log10"); res = log10(arr)
      case ("sin"); res = sin(arr)
      case ("sinh"); res = sinh(arr)
      case ("sqrt"); res = sqrt(arr)
      case ("tan"); res = tan(arr)
      case ("tanh"); res = tanh(arr)
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
      case ("cummin"); res = cummin(arr)
      case ("cummax"); res = cummax(arr)
      case ("cummean"); res = cummean(arr)
      case ("cumprod"); res = cumprod(arr)
      case ("diff"); res = diff(arr)
      case ("head"); res = head(arr)
      case ("tail"); res = tail(arr)
      case ("sort"); res = sorted(arr)
      case ("indexx"); res = indexx(arr)
      case ("rank"); res = rank(arr)
      case ("unique"); res = unique(arr)
      case ("stdz"); res = standardize(arr)
      case ("reverse"); res = reverse(arr)
      case default
         print *, "Error in apply_vec_func: function '", trim(fname), "' not defined"
         eval_error = .true.
         res = [bad_value]
      end select
   end function apply_vec_func

   recursive function evaluate(str) result(res)
      ! Evaluate the input string str as an expression or assignment
      ! and return its result array res

      character(len=*), intent(in) :: str
      real(kind=dp), allocatable :: res(:)

      !- local to this outer shell ----------------------------------
      character(len=:), allocatable :: expr, lhs, rhs
      integer                      :: pos, lenstr      ! parser cursor & length
      integer                      :: i, eqpos         ! scan index & "=" position
      integer :: depth_b, depth_p
      !------------------------------------------------------------------

      ! prepare the string for parsing
      call init_evaluator(trim(str), expr, lenstr, pos)

      ! look for an *assignment* = that is **not** part of >= <= == <=
!------------------------------------------------------------------
!  find a top‑level “=” that is **not** part of  >= <= == /=  etc.
!------------------------------------------------------------------
      eqpos = 0
      depth_p = 0          ! nesting level ()
      depth_b = 0          ! nesting level []

      do i = 1, lenstr
         select case (expr(i:i))
         case ("("); depth_p = depth_p + 1
         case (")"); if (depth_p > 0) depth_p = depth_p - 1
         case ("["); depth_b = depth_b + 1
         case ("]"); if (depth_b > 0) depth_b = depth_b - 1
         case ("=")
            if (depth_p == 0 .and. depth_b == 0) then
               if (i > 1) then
                  if (any(expr(i - 1:i - 1) == [">", "<", "!", "=", "/"])) cycle
               end if
               if (i < lenstr .and. expr(i + 1:i + 1) == "=") cycle
               eqpos = i
               exit                            ! first *top‑level* “=” wins
            end if
         end select
      end do

!       eqpos = 0
!       do i = 1, lenstr
!          if (expr(i:i) == "=") then
!             if (i > 1 .and. any(expr(i - 1:i - 1) == [">", "<", "!", "=", "/"])) cycle
!             if (i < lenstr .and. expr(i + 1:i + 1) == "=") cycle
!             eqpos = i
!             exit                       ! first qualifying = wins
!          end if
!       end do

      ! assignment found  evaluate RHS then store
      if (eqpos > 0) then
         lhs = adjustl(expr(1:eqpos - 1))
         rhs = expr(eqpos + 1:)
         res = evaluate(rhs)           ! recursive call
         if (.not. eval_error) then
            if (index(lhs, "(") > 0 .and. index(lhs, ")") > index(lhs, "(")) then
               call assign_element(lhs, res)   ! element assignment  a(i)=
            else
               call set_variable(lhs, res)   ! wholevariable assignment
            end if
         end if
         return
      end if

      ! no =  treat the whole string as an expression
      res = parse_expression()
      ! detect any extraneous characters left on the line
      call skip_spaces()
      if (curr_char /= char(0)) then
         print *, "Error: unexpected input after valid expression: '", &
            trim(expr(pos - 1:lenstr)), "'"
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

         expr = str_in
         lenstr = len_trim(expr)
         pos = 1
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

      !---------------------------------------------------------------
      logical function at_token(tok)                                   ! TRUE if
         character(len=*), intent(in) :: tok                           !   the
         integer :: l                                                  !   text
         l = len_trim(tok)                                             !   TOK
         if (pos - 1 + l - 1 > lenstr) then                                  !   starts
            at_token = .false.                                         !   at the
         else                                                          !   current
            at_token = (expr(pos - 1:pos - 2 + l) == tok)                    !   cursor
         end if
      end function at_token

      subroutine advance_token(n)                                      ! skip the
         integer, intent(in) :: n                                      ! next N
         integer :: k                                                  ! letters
         do k = 1, n                                                   ! (calls
            call next_char()                                           ! next_char)
         end do
      end subroutine advance_token
      !---------------------------------------------------------------

      function parse_number() result(num)
         ! Read a numeric literal starting at the current cursor
         ! and return it as a one-element array num
         real(kind=dp), allocatable :: num(:)
         character(len=64) :: buf
         integer :: i
         real(kind=dp) :: tmp
         call skip_spaces()
         i = 0
         do while (is_numeral(curr_char) .or. curr_char == ".")
            i = i + 1
            buf(i:i) = curr_char
            call next_char()
         end do
         read (buf(1:i), *) tmp
         num = [tmp]
      end function parse_number

      function parse_identifier() result(name_out)
         ! Read an alphanumeric identifier from the current cursor
         ! and return it as name_out
         character(len=len_name) :: name_out
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

         print *, "Error: undefined variable '", trim(name), "'"
         eval_error = .true.
         v = [bad_value]
      end function get_variable

      recursive function parse_array() result(arr)
         ! Parse a bracketed array literal (e.g. '[1,2,3]') and return its elements
         ! as a 1-D real(kind=dp) allocatable array.
         real(kind=dp), allocatable :: arr(:), tmp(:), elem(:)
         integer :: total, ne

         ! consume the '['
         call next_char()
         call skip_spaces()

         ! empty array literal []
         if (curr_char == "]") then
            allocate (arr(0))
            call next_char()
            return
         end if

         total = 0
         allocate (arr(0))

         do
            ! parse one element (may itself be an array)
            elem = parse_expression()
            if (eval_error) return
            ne = size(elem)

            ! append elem to arr
            if (allocated(tmp)) deallocate (tmp)
            allocate (tmp(total + ne))
            if (total > 0) tmp(1:total) = arr
            tmp(total + 1:total + ne) = elem
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
         ! Parse a single factor in an expression, handling:
         !   - numeric literals
         !   - parenthesized sub‑expressions
         !   - array literals
         !   - identifiers (variable lookup, function calls, slicing)
         !   - unary +/– and exponentiation.
         real(kind=dp), allocatable :: f(:) ! result
         !===================  locals  =====================================
         real(kind=dp), allocatable :: arg1(:), arg2(:), arg3(:)
         real(kind=dp), allocatable :: exponent(:), vvar(:)
         integer, allocatable :: idxv(:)
         character(len=len_name) :: id
         character(len=:), allocatable :: idxs
         integer :: nsize, pstart, pend, depth, n1, n2, dim_val
         logical :: is_neg, have_second
         logical :: toplevel_colon, toplevel_comma, have_dim
         character(len=len_name) :: look_name    ! NEW
         have_dim = .false.
         dim_val = 1
         call skip_spaces()
         !-------------- logical NOT ---------------------------------
         if (at_token('.not.')) then
            call advance_token(5)                ! consume ".not."
            f = parse_factor()                   ! recurse on the operand
            if (.not. eval_error) then
               f = merge(1.0_dp, 0.0_dp, f == 0.0_dp)   ! element-wise .not.
            else
               f = [bad_value]
            end if
            return
         end if

         !---------------- unary  -----------------------------------------
         if (curr_char == "+" .or. curr_char == "-") then
            is_neg = (curr_char == "-")
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

!=================================================================
!  read("file.txt" [, col | col = n])
!      → calls  read_vec(file , f , icol = n)
!
!  • first argument must be a double‑quoted file name
!  • second argument is optional; if omitted defaults to column 1
!    It can be given positionally ( e.g. read("f.txt",3) )
!    or by keyword         ( e.g. read("f.txt", col = 3) )
!=================================================================
                  if (trim(id) == "read") then
                     block
                        character(len=:), allocatable :: fname
                        integer                       :: icol
                        real(dp), allocatable         :: tmp(:)
                        integer                       :: q1, q2, save_pos
                        character(len=len_name)       :: kw

                        icol = 1                     ! default column
                        call skip_spaces()

                        ! ---- first argument : a quoted string --------------------
                        if (curr_char /= '"') then
                           print *, "Error: read(): first argument must be a quoted file name"
                           eval_error = .true.; f = [bad_value]; return
                        end if

                        q1 = pos - 1                 ! opening quote location in EXPR
                        q2 = q1
                        if (debug_read) print *, "q1, q2 =", q1, q2
                        do
                           q2 = q2 + 1
                           if (q2 > lenstr) then
                              print *, "Error: unmatched quote in read()"
                              eval_error = .true.; f = [bad_value]; return
                           end if
                           if (expr(q2:q2) == '"') exit
                        end do
                        fname = expr(q1 + 1:q2 - 1)      ! file name without quotes
                        if (debug_read) then
                           print *, "fname =", trim(fname)
                        end if
                        ! advance cursor to first char after closing quote
                        pos = q2 + 1
                        if (pos > lenstr) then
                           curr_char = char(0)
                        else
                           curr_char = expr(pos:pos); pos = pos + 1
                        end if
                        call skip_spaces()

                        ! ---- optional  ,  [col =] n  -----------------------------
                        if (curr_char == ",") then
                           call next_char(); call skip_spaces()

                           save_pos = pos
                           if (is_letter(curr_char)) then
                              kw = parse_identifier()
                              call skip_spaces()
                              if (trim(kw) == "col") then      ! got keyword
                                 if (curr_char == "=") then
                                    call next_char()
                                    call skip_spaces()
                                 end if
                              else                              ! unknown keyword
                                 print *, "Error: unknown keyword '"//trim(kw)//"' in read()"
                                 eval_error = .true.; f = [bad_value]; return
                              end if
                           else
                              ! no keyword → rewind; treat as positional
                              pos = save_pos
                              curr_char = expr(pos - 1:pos - 1)
                           end if

                           tmp = parse_expression()
                           if (eval_error) return
                           if (size(tmp) /= 1) then
                              print *, "Error: col argument must be scalar"
                              eval_error = .true.; f = [bad_value]; return
                           end if
                           icol = nint(tmp(1))
                           call skip_spaces()
                        end if

                        ! ---- closing parenthesis ---------------------------------
                        if (curr_char /= ")") then
                           print *, "Error: expected ')' at end of read()"
                           eval_error = .true.; f = [bad_value]; return
                        end if
                        call next_char()                      ! consume ')'
                        if (debug_read) then
                           print *, "icol, fname =", icol, trim(fname)
                        end if
                        ! ---- actually read the file --------------------------------
                        call read_vec(fname, f, icol=icol)
                        if (debug_read) print*,"f =",f
                        return
                     end block
                  end if
!=================================================================

                  !============ ZERO-ARGUMENT SPECIAL CASE ======================
                  if (curr_char == ")") then         !  e.g. runif()
                     call next_char()                !  consume ")"
                     select case (trim(id))
                     case ("runif")
                        allocate (f(1))
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
                  depth = 1
                  toplevel_colon = .false.
                  toplevel_comma = .false.
                  pend = pstart - 1
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
                     print *, "Error: mismatched parentheses"
                     eval_error = .true.; f = [bad_value]; return
                  end if

                  !---------------- slice?  -------------------------------------
                  if (toplevel_colon .and. .not. toplevel_comma) then
                     idxs = expr(pstart:pend - 1)
                     call slice_array(id, idxs, f)

                     ! advance cursor just past ")"
                     pos = pend + 1
                     if (pos > lenstr) then
                        curr_char = char(0)
                     else
                        curr_char = expr(pos:pos); pos = pos + 1
                     end if
                     return
                  end if

                  !------------- first argument -----------------------------------
                  arg1 = parse_expression()
                  if (eval_error) then
                     f = [bad_value]; return
                  end if

! after ARG1 has been parsed
                  call skip_spaces()
                  have_second = .false.

                  if (curr_char == ",") then
                     if (any(trim(id) == [character(len=len_name) :: &
                                          "sum", "product", "minval", "maxval"])) then
                        !------------------------------------------------------------
                        !  2nd *token* can be either
                        !     • a positional DIM value       →  sum(x , 1)
                        !     • a named argument             →  sum(x , mask = …)
                        !------------------------------------------------------------
                        block
                           integer :: save_pos
                           logical :: is_name_eq
                           real(kind=dp), allocatable :: tmp(:)
                           save_pos = pos          ! index **after** the comma
                           call next_char()          ! step over ‘,’
                           call skip_spaces()

                           !–– look ahead:  identifier followed by '='  ? ––
                           is_name_eq = .false.
                           if (is_letter(curr_char)) then
                              look_name = parse_identifier()
                              call skip_spaces()
                              if (curr_char == "=") is_name_eq = .true.
                           end if

                           if (is_name_eq) then
                              !–– restore → named‑argument loop will handle it ––
                              pos = save_pos
                              curr_char = ","
                           else
                              !–––––––––––––––––––––––––––––––––––––––––––––––––––
                              !  **Positional DIM value**
                              !–––––––––––––––––––––––––––––––––––––––––––––––––––
                              pos = save_pos          ! we already skipped the comma
                              call next_char()
                              call skip_spaces()
                              tmp = parse_expression()               ! DIM expression
                              if (eval_error) then
                                 f = [bad_value]; return
                              end if
                              if (size(tmp) /= 1) then
                                 print *, "Error: dim argument must be scalar"
                                 eval_error = .true.; f = [bad_value]; return
                              end if
                              dim_val = nint(tmp(1))
                              have_dim = .true.
                              call skip_spaces()
                           end if
                        end block
                     else
                        !------------------------------------------------------------
                        !  Any other routine – 2‑nd positional argument as before
                        !------------------------------------------------------------
                        call next_char()          ! consume ','
                        call skip_spaces()
                        arg2 = parse_expression()
                        have_second = .true.
                        if (eval_error) then
                           f = [bad_value]; return
                        end if
                     end if
                  end if

                  if (curr_char == ")") call next_char()

                  !------------- dispatch -----------------------------------------
                  select case (trim(id))

                     !================================================================
                     !  SUM / PRODUCT / MINVAL / MAXVAL
                     !  – optional named arguments in any order
                     !        dim = 1      and/or     mask = logical array
                     !================================================================
                  case ("sum", "product", "minval", "maxval")
                     block
                        !---- local to this block only ---------------------------
                        logical                     :: have_mask
                        real(dp), allocatable      :: mask_arr(:)
                        logical, allocatable       :: lmask(:)
                        real(dp), allocatable      :: tmp(:)
                        character(len=len_name)     :: name_tok

                        have_mask = .false.

                        !---------------------------------------------------------
                        ! first positional argument already parsed  →  ARG1
                        ! now parse any  , name = expr  pairs
                        do
                           call skip_spaces()
                           if (curr_char /= ",") exit
                           call next_char(); call skip_spaces()

                           if (.not. is_letter(curr_char)) then
                              print *, "Error: expected named argument after ','"
                              eval_error = .true.; f = [bad_value]; return
                           end if
                           name_tok = parse_identifier()
                           call skip_spaces()
                           if (curr_char /= "=") then
                              print *, "Error: expected '=' after '"//trim(name_tok)//"'"
                              eval_error = .true.; f = [bad_value]; return
                           end if
                           call next_char(); call skip_spaces()

                           tmp = parse_expression()
                           if (eval_error) then
                              f = [bad_value]; return
                           end if

                           select case (trim(name_tok))
                           case ("mask")
                              if (have_mask) then
                                 print *, "Error: duplicate mask= argument"
                                 eval_error = .true.; f = [bad_value]; return
                              end if
                              mask_arr = tmp
                              have_mask = .true.

                           case ("dim")
                              if (have_dim) then
                                 print *, "Error: duplicate dim= argument"
                                 eval_error = .true.; f = [bad_value]; return
                              end if
                              if (size(tmp) /= 1) then
                                 print *, "Error: dim= must be scalar"
                                 eval_error = .true.; f = [bad_value]; return
                              end if
                              dim_val = nint(tmp(1))
                              have_dim = .true.

                           case default
                              print *, "Error: unknown named argument '"//trim(name_tok)//"'"
                              eval_error = .true.; f = [bad_value]; return
                           end select
                        end do
! -- eat any white-space and the final right-parenthesis -----------------
                        call skip_spaces()
                        if (curr_char == ")") then          ! make absolutely sure the ')' itself
                           call next_char()                 ! is consumed (curr_char -> next char)
                        end if

                        if (have_dim .and. dim_val /= 1) then
                           print *, "Error: only dim=1 is allowed for 1D argument, dim_val =", dim_val
                           eval_error = .true.; f = [bad_value]; return
                        end if

                        !---- build logical mask ---------------------------------
                        if (have_mask) then
                           if (size(mask_arr) == 1) then
                              allocate (lmask(size(arg1)))
                              lmask = mask_arr(1) /= 0.0_dp
                           else if (size(mask_arr) == size(arg1)) then
                              allocate (lmask(size(arg1)))
                              lmask = mask_arr /= 0.0_dp
                           else
                              print "(a,i0,1x,i0)", "Error: mask size mismatch in "//trim(id) &
                                 //", sizes of arg1 and mask are ", size(arg1), size(mask_arr)
                              eval_error = .true.; f = [bad_value]; return
                           end if

                           if (size(lmask) /= size(arg1)) then
                              print *, "Error: mask must match array size in "//trim(id)
                              eval_error = .true.; f = [bad_value]; return
                           end if
                        end if

                        !---- intrinsic call -------------------------------------
                        select case (trim(id))
                        case ("sum")
                           if (have_mask) then
                              f = [sum(arg1, mask=lmask)]
                           else
                              f = [sum(arg1)]
                           end if

                        case ("product")
                           if (have_mask) then
                              ! PRODUCT(mask=…) is F2003; use PACK for portability
                              f = [product(pack(arg1, lmask))]
                           else
                              f = [product(arg1)]
                           end if

                        case ("minval")
                           if (have_mask) then
                              f = [minval(arg1, mask=lmask)]
                           else
                              f = [minval(arg1)]
                           end if

                        case ("maxval")
                           if (have_mask) then
                              f = [maxval(arg1, mask=lmask)]
                           else
                              f = [maxval(arg1)]
                           end if
                        end select
                     end block

                  case ("cor", "cov", "dot") ! correlation, covariance, dot product
                     if (.not. have_second) then
                        print *, "Error: function needs two arguments"
                        eval_error = .true.; f = [bad_value]
                     else if (size(arg1) /= size(arg2)) then
                        print "(a,i0,1x,i0,a)", "Error: function array arguments have sizes ", &
                           size(arg1), size(arg2), " must be equal"
                        eval_error = .true.; f = [bad_value]
                     else if ((id == "cor" .or. id == "cov") .and. size(arg1) < 2) then
                        print *, "Error: function array arguments must have sizes > 1, sizes are ", size(arg1), size(arg2)
                        eval_error = .true.; f = [bad_value]
                     else if (id == "dot") then
                        f = [dot_product(arg1, arg2)]
                     else
                        if (id == "cor") then
                           f = [cor(arg1, arg2)]
                        else if (id == "cov") then
                           f = [cov(arg1, arg2)]
                        end if
                     end if

                  case ("min", "max")                           ! two-arg intrinsics
                     if (.not. have_second) then
                        print *, "Error: ", trim(id), "() needs two arguments"
                        eval_error = .true.; f = [bad_value]
                     else
                        n1 = size(arg1); n2 = size(arg2)
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
                           print *, "Error: argument size mismatch in ", trim(id), "()"
                           eval_error = .true.; f = [bad_value]
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

                     !---------------------------------------------------------------
                  case ("rep")
                     !  rep(v , n)  =  v repeated n times
                     if (.not. have_second) then
                        print *, "Error: rep() needs two arguments"
                        eval_error = .true.
                        f = [bad_value]
                     else                           ! we already have arg1 and arg2
                        if (size(arg2) /= 1) then
                           print *, "Error: second argument of rep() must be scalar"
                           eval_error = .true.
                           f = [bad_value]
                        else
                           f = rep(arg1, nint(arg2(1)))
                        end if
                     end if
                     !---------------------------------------------------------------

                  case ("runif", "rnorm", "arange", "zeros", "ones") ! one-arg
                     if (have_second) then
                        print *, "Error: function takes one argument"
                        eval_error = .true.; f = [bad_value]
                     else
                        nsize = nint(arg1(1))
                        select case (id)
                        case ("runif"); f = runif(nsize)
                        case ("rnorm"); f = random_normal(nsize)
                        case ("arange"); f = arange(nsize)
                        case ("zeros"); f = zeros(nsize)
                        case ("ones"); f = ones(nsize)
                        end select
                     end if

                  case ("grid") ! grid(n,x0,xh)
                     if (.not. have_second) then
                        ! we have only one argument so far  need two more
                        print *, "Error: grid(n,x0,xh) needs three arguments"
                        eval_error = .true.; f = [bad_value]
                     else
                        ! arg1 and arg2 have already been parsed --> read arg3
                        call skip_spaces()
                        if (curr_char /= ",") then
                           print *, "Error: grid(n,x0,xh) needs three arguments"
                           eval_error = .true.; f = [bad_value]
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
                                 print *, "Error: grid arguments must be scalars"
                                 eval_error = .true.; f = [bad_value]
                              else
                                 f = grid(nint(arg1(1)), arg2(1), arg3(1))
                              end if
                              call skip_spaces()
                              if (curr_char == ")") call next_char()
                           end if
                        end if
                     end if

                  case ("abs", "acos", "acosh", "asin", "asinh", "atan", "atanh", "cos", "cosh", &
                        "exp", "log", "log10", "sin", "sinh", "sqrt", "tan", "tanh", "size", &
                        "norm1", "norm2", "minloc", "maxloc", "count", "mean", "geomean", &
                        "harmean", "sd", "cumsum", &
                        "cummin", "cummax", "cummean", "cumprod", "diff", "sort", "indexx", "rank", &
                        "unique", "stdz", "reverse", "median", "head", "tail", "bessel_j0", "bessel_j1", &
                        "bessel_y0", "bessel_y1", "gamma", "log_gamma", "cosd", "sind", "tand", &
                        "acosd", "asind", "atand", "spacing", "skew", "kurt", "print_stats")
                     if (have_second) then
                        print "(a)", "Error: function '"//trim(id)//"' takes one argument"
                        eval_error = .true.; f = [bad_value]
                     else
                        if (index("size sum product norm1 norm2 minval maxval minloc "// &
                                  "maxloc count mean geomean harmean sd median print_stats skew kurt", &
                               trim(id)) > 0) then
                           f = [apply_scalar_func(id, arg1)] ! functions that take array and return scalar
                        else
                           f = apply_vec_func(id, arg1)
                        end if
                     end if

                  case ("merge")
                     if (.not. have_second) then
                        print *, "Error: merge() needs three arguments"
                        eval_error = .true.; f = [bad_value]

                     else
                        ! arg1 and arg2 have already been parsed
                        call skip_spaces()
                        if (curr_char /= ",") then
                           print *, "Error: merge() needs three arguments"
                           eval_error = .true.; f = [bad_value]
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

                  case ("plot")
                     if (.not. have_second) then
                        call plot(arg1, title=plot_to_label(line_cp))
                        allocate (f(0))
                     else if (size(arg1) /= size(arg2)) then
                        print *, "Error: plot() arguments must have same size"
                        eval_error = .true.
                        f = [bad_value]
                     else
                        call plot(arg1, arg2, title=plot_to_label(line_cp))         ! <-- actual drawing
                        allocate (f(0))                ! return “nothing”
                     end if

                  case default ! subscript  x(i)
                     if (have_second) then
                        print *, "Error in have_second: function '"//trim(id)//"' not defined"
                        eval_error = .true.; f = [bad_value]
                     else
                        vvar = get_variable(id)
                        if (.not. eval_error) then
                           if (any(abs(arg1 - nint(arg1)) > tol)) then
                              print *, "Error: non-integer subscript for '"//trim(id)//"'"
                              eval_error = .true.; f = [bad_value]
                           else
                              idxv = nint(arg1)
                              if (any(idxv < 1) .or. any(idxv > size(vvar))) then
                                 print *, "Error: index out of bounds for '"//trim(id)//"'"
                                 eval_error = .true.; f = [bad_value]
                              else
                                 allocate (f(size(idxv))); f = vvar(idxv)
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
               print *, "Error: unexpected character '"//curr_char//"'"
               eval_error = .true.; f = [bad_value]
            end if
         end select

         !------------- exponentiation ------------------------------------
         call skip_spaces()
         if (curr_char == "^") then
            call next_char()
            exponent = parse_factor()
            if (.not. eval_error) then
               if (size(exponent) == 1) then
                  f = f**exponent(1)
               else if (size(f) == 1) then
                  f = f(1)**exponent
               else if (size(f) == size(exponent)) then
                  f = f**exponent
               else
                  print *, "Error: size mismatch in exponentiation"
                  eval_error = .true.; f = [bad_value]
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
         do while (.not. eval_error .and. (curr_char == "*" .or. curr_char == "/"))
            if (curr_char == "*") then
               call next_char()
               f2 = parse_factor()
               nt = size(t)
               nf = size(f2)
               if (nt == nf) then
                  tmp = t*f2
               else if (nf == 1) then
                  tmp = t*f2(1)
               else if (nt == 1) then
                  tmp = t(1)*f2
               else
                  print *, "Error: size mismatch in multiplication"
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
                  tmp = t/f2
               else if (nf == 1) then
                  tmp = t/f2(1)
               else if (nt == 1) then
                  tmp = t(1)/f2
               else
                  print *, "Error: size mismatch in division"
                  return
               end if
            end if
            t = tmp
            call skip_spaces()
         end do
      end function parse_term

      !===============================================================
      recursive function parse_relational() result(e)
         ! *** This is the old body of parse_expression ***
         ! (addition / subtraction + the existing relational chain)
         ! Paste the *whole* original code of parse_expression here
         ! up to its END FUNCTION, but DO NOT include any .and./.or.
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
         do while (.not. eval_error .and. (curr_char == "+" .or. curr_char == "-"))
            if (curr_char == "+") then
               call next_char()
               t = parse_term()
               ne = size(e); nt = size(t)
               if (ne == nt) then
                  e = e + t
               else if (nt == 1) then
                  e = e + t(1)
               else if (ne == 1) then
                  e = e(1) + t
               else
                  print *, "Error: size mismatch in addition"
                  eval_error = .true.; return
               end if
            else
               call next_char()
               t = parse_term()
               ne = size(e); nt = size(t)
               if (ne == nt) then
                  e = e - t
               else if (nt == 1) then
                  e = e - t(1)
               else if (ne == 1) then
                  e = e(1) - t
               else
                  print *, "Error: size mismatch in subtraction"
                  eval_error = .true.; return
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
                  op = "<="; call next_char()
               else
                  op = "<"
               end if
            case (">")
               call next_char()
               if (curr_char == "=") then
                  op = ">="; call next_char()
               else
                  op = ">"
               end if
            case ("=")
               call next_char()
               if (curr_char == "=") then
                  op = "=="; call next_char()
               else
                  op = "="
               end if
            case ("/")
               call next_char()
               if (curr_char == "=") then
                  op = "/="; call next_char()
               else
                  print *, "Error: error with /"
                  eval_error = .true.; exit
               end if
            case default
               more_rel = .false.; cycle
            end select

            call skip_spaces()
            rhs = parse_term()             ! RHS has same precedence chain
            if (eval_error) exit

            e = rel_compare(op, e, rhs)    ! perform comparison
            call skip_spaces()
         end do
      end function parse_relational

      recursive function parse_logical_and() result(e)
         real(dp), allocatable :: e(:), rhs(:)
         e = parse_relational()
         call skip_spaces()
         do while (.not. eval_error .and. at_token('.and.'))
            call advance_token(5)
            rhs = parse_relational()
            if (eval_error) exit
            e = logical_binary('.and.', e, rhs)
            call skip_spaces()
         end do
      end function parse_logical_and

      recursive function parse_expression() result(e)   !  top level  (= .or.)
         real(dp), allocatable :: e(:), rhs(:)
         e = parse_logical_and()
         call skip_spaces()
         do while (.not. eval_error .and. at_token('.or.'))
            call advance_token(4)
            rhs = parse_logical_and()
            if (eval_error) exit
            e = logical_binary('.or.', e, rhs)
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
      character(len=*), intent(in)  :: lhs
      real(kind=dp), allocatable, intent(in)  :: rval(:)

      character(len=len_name)               :: name
      character(len=:), allocatable  :: idx_txt
      real(kind=dp), allocatable  :: idx_val(:)
      integer, allocatable  :: idx(:)
      integer :: p_lpar, p_rpar, vi, n_idx

      ! ---- split "var( … )" into name and index string ---------------------
      p_lpar = index(lhs, "(")
      p_rpar = index(lhs, ")")
      name = adjustl(lhs(1:p_lpar - 1))
      idx_txt = lhs(p_lpar + 1:p_rpar - 1)

      ! ---- evaluate index expression ---------------------------------------
      idx_val = evaluate(idx_txt)
      if (eval_error) then
         if (stop_if_error) stop "stopped with evaluation error"
         return
      end if

      ! ---- convert to integer(s) -------------------------------------------
      if (any(abs(idx_val - nint(idx_val)) > tol)) then
         print *, "Error: non-integer subscript in assignment to '", trim(name), "'"
         eval_error = .true.
         return
      end if
      n_idx = size(idx_val)
      allocate (idx(n_idx))
      idx = nint(idx_val)

      ! ---- locate the variable ---------------------------------------------
      do vi = 1, n_vars
         if (vars(vi)%name == name) then
            if (.not. mutable) then
               print *, "Error: cannot assign to '"//trim(name)//"' when mutable=.false."
               eval_error = .true.
               return
            end if

            if (any(idx < 1) .or. any(idx > size(vars(vi)%val))) then
               print *, "Error: index out of bounds in assignment to '"//trim(name)//"'"
               eval_error = .true.
               return
            end if

            if (size(rval) == 1) then                 ! broadcast scalar
               vars(vi)%val(idx) = rval(1)
            else if (size(rval) == n_idx) then        ! element-wise vector
               vars(vi)%val(idx) = rval
            else
               print *, "Error: size mismatch in assignment to '"//trim(name)//"'"
               eval_error = .true.
            end if
            return
         end if
      end do

      ! ---- variable not found ----------------------------------------------
      print *, "Error: undefined variable '", trim(name), "' in assignment"
      eval_error = .true.
   end subroutine assign_element

   impure elemental recursive subroutine eval_print(line)
      character(len=*), intent(in) :: line
      ! --------------------------------------------------------------
      ! 1.  split the input at *top-level* semicolons
      ! --------------------------------------------------------------
      integer                       :: n, k, rsize, i, nsize, ivar, nlen_tail
      character(len=:), allocatable :: parts(:), rest, trimmed_line, tail, adj_line
      logical, allocatable   :: suppress(:)
      real(dp), allocatable   :: r(:), tmp(:)
      integer, allocatable   :: rint(:)
      integer                       :: p, repeat_count
      logical :: print_array_as_int, run_then
      character(len=*), parameter :: fmt_real_array = '("[",*(i0,:,", "))'
      character(len=:), allocatable :: lhs, rhs
      integer :: p_eq, p_com1, p_com2, p_lpar, p_rpar, depth, len_adj
      character(len=:), allocatable :: cond_txt, then_txt
      adj_line = adjustl(line)
      len_adj = len_trim(adj_line)
      line_cp = line
      ! write to transcript just once, for the whole input line
      if (write_code) write (tunit, "(a)") line
      if (adj_line == "compiler_version()") then
         print "(a)", trim(compiler_version())
         return
      else if (adj_line == "compiler_info()") then
         print "(a)", trim(compiler_version())
         print "(a)", trim(compiler_options())
         return
      else if (adj_line == "exit") then
         exit_loop = .true.
         return
      else if (adj_line == "print") then
         print*
         return
      else if (len_adj > 2) then
         if (adj_line(1:1) == '"' .and. adj_line(len_adj:len_adj) == '"') then
            ! if a line just contains a quoted non-empty string, print it after a blank line
            print "(/,a)",adj_line(2:len_adj-1)
            return
         end if
      end if

      if (len_trim(line) >= 2) then
         if (line(1:1) == "*") then
            ! find first space after the count
            p = index(line(2:), " ")
            if (p > 0) then
               ! parse the count expression between column 2 and p
               tmp = evaluate(line(2:p))     ! e.g. line(2:p) == "n" or "10"
               if (.not. eval_error .and. size(tmp) == 1) then
                  repeat_count = int(tmp(1))
                  rest = line(p + 1:)           ! the code to repeat
                  do i = 1, repeat_count
                     call eval_print(rest)         ! recursive call; will split again
                  end do
                  return                         ! done with this line
               end if
            end if
         end if
      end if

      if (loop_depth > 0 .and. .not. in_loop_execute) then
         block
            character(len=:), allocatable :: tl
            tl = adjustl(line)
            if (index(tl, "do ") == 1 &  ! a “do i=…” header
                .or. trim(tl) == "end do" &
                .or. trim(tl) == "enddo") then
               ! fall through into the normal do/end-do handlers
            else
               ! buffer everything else
               loop_body(loop_depth) = trim(loop_body(loop_depth))//trim(line)//new_line("a")
               return
            end if
         end block
      end if

      ! ─── run("file") : execute the contents of a text file ───
      if (index(adj_line, 'run(') == 1) then
         block
            integer :: p1, p2
            character(len=:), allocatable :: fn
            p1 = index(adj_line, '("') + 2
            p2 = index(adj_line, '")') - 1
            if (p1 > 2 .and. p2 >= p1) then
               fn = adj_line(p1:p2)
               call run(fn)
            else
               print *, "Error: run() expects a filename in double quotes"
            end if
            return
         end block
      end if

      if (in_loop_execute) then
         p_lpar = index(adj_line, "(")
         if (p_lpar > 0 .and. trim(adj_line(1:p_lpar - 1)) == "if") then

            ! find matching “)”
            p_rpar = p_lpar
            depth = 1
            do while (p_rpar < len_trim(adj_line) .and. depth > 0)
               p_rpar = p_rpar + 1
               select case (adj_line(p_rpar:p_rpar))
               case ("("); depth = depth + 1
               case (")"); depth = depth - 1
               end select
            end do

            cond_txt = adjustl(adj_line(p_lpar + 1:p_rpar - 1))
            then_txt = adjustl(adj_line(p_rpar + 1:))

            tmp = evaluate(cond_txt)
            if (.not. eval_error .and. size(tmp) == 1) then
               if (tmp(1) /= 0.0_dp) call eval_print(then_txt)
            end if

            return
         end if
      end if

      if (in_loop_execute .and. adjustl(line) == "cycle") then
         cycle_loop = .true.
         return        ! skip everything else in this iteration
      end if

!─────────────────────────────
!  Loop handling
!─────────────────────────────
      select case (adjustl(line))
      case ("end do", "enddo", "enddo;", "end do;")
         if (loop_depth == 0) then
            print *, "Error: 'end do' without matching 'do'"
            return
         end if

         print *
         do ivar = loop_start(loop_depth), loop_end(loop_depth), loop_step(loop_depth)
            call set_variable(loop_var(loop_depth), [real(ivar, dp)])
            call run_loop_body(loop_body(loop_depth))
            if (exit_loop) then        ! ← exit from the DO
               exit
            end if
         end do

         exit_loop = .false.          ! clear the flag for next loop
         call set_variable(loop_var(loop_depth), [real(ivar, dp)])
         loop_depth = loop_depth - 1
         return
      case default
         ! nothing – fall through
      end select

      adj_line = adjustl(line)
!──────────────────────────  one‑line IF  ──────────────────────────
! if (index(adj_line,'if') == 1 .and. len_trim(adj_line) > 4 .and.    &
!     adj_line(3:3) == '(' ) then

      p_lpar = index(adj_line, "(")                ! first left parenthesis
      if (p_lpar > 0 .and. trim(adj_line(1:p_lpar - 1)) == "if") then

         ! — locate the matching right parenthesis —
         p_rpar = p_lpar
         depth = 1
         do while (p_rpar < len_trim(adj_line) .and. depth > 0)
            p_rpar = p_rpar + 1
            select case (adj_line(p_rpar:p_rpar))
            case ("("); depth = depth + 1
            case (")"); depth = depth - 1
            end select
         end do
         if (depth /= 0) then
            print *, "Error: mismatched parentheses in IF statement"
            return
         end if

         ! — split into  condition  and  consequent —
         cond_txt = adjustl(adj_line(p_lpar + 1:p_rpar - 1))
         then_txt = adjustl(adj_line(p_rpar + 1:))

         if (len_trim(then_txt) == 0) then
            print *, "Error: null statement after IF"
            return
         end if

         ! — evaluate the condition (must be scalar) —
         tmp = evaluate(cond_txt)
         if (eval_error) return
         if (size(tmp) /= 1) then
            print *, "Error: IF condition must be scalar"
            return
         end if
         run_then = (tmp(1) /= 0.0_dp)

         ! — execute the single statement if TRUE —
         if (run_then) call eval_print(then_txt)
         return                                    ! one‑line IF handled
      end if
!───────────────────────────────────────────────────────────────────

!------------  Is this the beginning of a DO block?  -----------------
      if (index(adj_line, "do") == 1) then
         if (len_trim(adj_line) > 2) then
            if (adj_line(3:3) == " ") then
               if (loop_depth >= max_loop_depth) then
                  print *, "Error: loop nesting deeper than ", max_loop_depth
                  return
               end if

               ! Parse  “do  i = 1 , 5 , 2”   (step is optional)

               p_eq = index(line, "=")
               p_com1 = index(line, ",")
               if (p_eq == 0 .or. p_com1 == 0) then
                  print *, "Error: malformed DO header: ", trim(line)
                  return
               end if

               lhs = adjustl(line(3:p_eq - 1))              ! variable name
               rhs = adjustl(line(p_eq + 1:))

               p_com1 = index(rhs, ",")
               p_com2 = index(rhs(p_com1 + 1:), ",")
               if (p_com2 > 0) p_com2 = p_com1 + p_com2

               loop_depth = loop_depth + 1
               loop_var(loop_depth) = lhs

               loop_start(loop_depth) = parse_int_scalar(rhs(1:p_com1 - 1))          ! 1st field

               if (p_com2 == 0) then                                               ! 2nd field form
                  loop_end(loop_depth) = parse_int_scalar(rhs(p_com1 + 1:))
                  loop_step(loop_depth) = 1
               else                                                                ! 3nd field form
                  loop_end(loop_depth) = parse_int_scalar(rhs(p_com1 + 1:p_com2 - 1))
                  loop_step(loop_depth) = parse_int_scalar(rhs(p_com2 + 1:))
               end if
               call set_variable(loop_var(loop_depth), [real(loop_start(loop_depth), dp)])
               loop_body(loop_depth) = ""   ! empty buffer, start collecting
               return                       ! finished with the DO line
            end if
         end if
      end if

      trimmed_line = adjustl(line)

      if (len_trim(trimmed_line) >= 3 .and. trimmed_line(1:3) == "del" &
          .and. (len_trim(trimmed_line) == 3 &  ! just "del"
                 .or. trimmed_line(4:4) == " " &  ! "del a b"
                 .or. trimmed_line(4:4) == ",")) then ! "del,a,b"

         ! everything *after* "del"
         tail = adjustl(trimmed_line(4:))

         ! turn any spaces into commas, collapse duplicate commas,
         ! and strip leading/trailing commas exactly as before
         tail = replace(tail, " ", ",")
         do while (index(tail, ",,") > 0)
            i = index(tail, ",,")
            tail = tail(1:i - 1)//","//tail(i + 2:)
         end do
         nlen_tail = len_trim(tail)
         do while (nlen_tail > 0 .and. tail(1:1) == ",")
            tail = tail(2:)
         end do
         do while (len_trim(tail) > 0 .and. tail(nlen_tail:nlen_tail) == ",")
            tail = tail(:len_trim(tail) - 1)
         end do

         if (nlen_tail > 0) then
            call delete_vars(tail)
         else
            print *, "Error: no variables specified in 'del'"
         end if
         return
      end if

! ————————————————————————— end “del” —————————————————————

      if (adjustl(line) == "clear") then
         call clear()
         return
      end if
      if (adjustl(line) == "?vars") then
         write (*, *) "Defined variables:"
         do i = 1, n_vars
            nsize = size(vars(i)%val)
            if (nsize == 1) then
               write (*, "(a)", advance="no") trim(vars(i)%name)//": "
               print "(F0.6)", vars(i)%val(1)
            else if (nsize <= max_print) then
               write (*, "(a)", advance="no") trim(vars(i)%name)//": "
               write (*, '("[",*(F0.6,:,", "))', advance="no") vars(i)%val
               write (*, "(']')")
            else
               write (*, "(a,': array(',i0,')')") trim(vars(i)%name), nsize
            end if
         end do
         return
      end if
      call split_by_semicolon(line, n, parts, suppress)

      do k = 1, n
         if (parts(k) == "") cycle          ! blank segment

         ! ---------- syntax checks exactly as before ----------
         if (.not. matched_parentheses(parts(k))) then
            print *, "mismatched parentheses"; cycle
         end if
         if (.not. matched_brackets(parts(k))) then
            print *, "mismatched brackets"; cycle
         end if
         if (index(parts(k), "**") /= 0) then
            print *, "use ^ instead of ** for exponentiation"; cycle
         end if

         ! ------------------------------------------------------
         r = evaluate(parts(k))
         if (eval_error) then
            if (stop_if_error) stop "stopped with evaluation error"
            cycle
         end if
         if (index(trim(parts(k)), "print_stats") == 1) cycle

         ! ---------- echo only when the segment is *not* suppressed ----------
         if (.not. suppress(k)) then
            if (echo_code) write (*, '(/,"> ",a)') trim(parts(k))
            rsize = size(r)
            if (rsize == 0) then
               print *
            else
               rint = nint(r)
               select case (rsize)
               case (1)
                  if (abs(r(1) - rint(1)) <= tol) then
                     print "(i0)", rint
                  else
                     call print_real(r(1))
                  end if
               case default
                  if (rsize <= max_print) then
                     if (print_array_as_int_if_possible) then
                        print_array_as_int = all(abs(r - rint) <= tol)
                     else
                        print_array_as_int = .false.
                     end if
                     if (print_array_as_int) then
                        write (*, fmt_real_array, advance="no") rint   ! open ‘[’ but no LF
                     else
                        write (*, '("[",*(F0.6,:,", "))', advance="no") r    ! ditto
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
      ! Remove all variables named in list_str, where names may be
      ! separated by commas and/or spaces.  Warn on any name not defined.
      character(len=*), intent(in) :: list_str
      character(len=len_name) :: nm
      integer :: start, pos, len_list, i_var, j_var
      logical :: found

      start = 1
      len_list = len_trim(list_str)

      do while (start <= len_list)
         ! skip any leading commas or spaces
         do while (start <= len_list .and. &
                   (list_str(start:start) == "," .or. list_str(start:start) == " "))
            start = start + 1
         end do
         if (start > len_list) exit

         ! find end of this token (up to next comma or space)
         pos = start
         do while (pos <= len_list)
            if (list_str(pos:pos) == "," .or. list_str(pos:pos) == " ") exit
            pos = pos + 1
         end do

         ! extract the variable name
         nm = adjustl(trim(list_str(start:pos - 1)))
         start = pos + 1

         ! try to delete it
         found = .false.
         do i_var = 1, n_vars
            if (vars(i_var)%name == nm) then
               if (allocated(vars(i_var)%val)) deallocate (vars(i_var)%val)
               do j_var = i_var, n_vars - 1
                  vars(j_var) = vars(j_var + 1)
               end do
               vars(n_vars)%name = ""
               if (allocated(vars(n_vars)%val)) deallocate (vars(n_vars)%val)
               n_vars = n_vars - 1
               found = .true.
               exit
            end if
         end do

         if (.not. found) print "(a)", "Warning: variable '"//trim(nm)//"' not defined"
      end do
   end subroutine delete_vars

   function rel_compare(op, a, b) result(res)
      ! Element-wise comparison returning 1.0 or 0.0
      character(len=*), intent(in) :: op
      real(kind=dp), intent(in) :: a(:), b(:)
      real(kind=dp), allocatable   :: res(:)
      logical, allocatable         :: mask(:)
      integer :: na, nb, n
      na = size(a)
      nb = size(b)
      if (na == nb) then
         n = na
         allocate (mask(n), source=.false.)
         select case (op)
         case ("<"); mask = a < b
         case ("<="); mask = a <= b
         case (">"); mask = a > b
         case (">="); mask = a >= b
         case ("="); mask = abs(a - b) <= tol
         case ("=="); mask = abs(a - b) <= tol
         case ("/="); mask = abs(a - b) > tol
         end select
         res = merge(1.0_dp, 0.0_dp, mask)

      else if (nb == 1) then
         ! vector - scalar
         n = na
         allocate (mask(n), source=.false.)
         select case (op)
         case ("< "); mask = a < b(1)
         case ("<="); mask = a <= b(1)
         case ("> "); mask = a > b(1)
         case (">="); mask = a >= b(1)
         case ("= "); mask = abs(a - b(1)) <= tol
         case ("=="); mask = abs(a - b(1)) <= tol
         case ("/="); mask = abs(a - b(1)) > tol
         end select
         res = merge(1.0_dp, 0.0_dp, mask)

      else if (na == 1) then
         ! scalar - vector   (broadcast the scalar)
         n = nb
         allocate (mask(n), source=.false.)
         select case (op)
         case ("< "); mask = a(1) < b
         case ("<="); mask = a(1) <= b
         case ("> "); mask = a(1) > b
         case (">="); mask = a(1) >= b
         case ("= "); mask = abs(a(1) - b) <= tol
         case ("=="); mask = abs(a(1) - b) <= tol
         case ("/="); mask = abs(a(1) - b) > tol
         end select
         res = merge(1.0_dp, 0.0_dp, mask)
      else
         print *, "Error: size mismatch in relational comparison"
         eval_error = .true.
         res = [bad_value]
      end if
   end function rel_compare

   function logical_binary(op, a, b) result(res)
      character(len=*), intent(in) :: op               ! ".and." / ".or."
      real(dp), intent(in) :: a(:), b(:)
      real(dp), allocatable        :: res(:)
      logical, allocatable         :: mask(:)
      integer :: na, nb, n

      na = size(a); nb = size(b)
      select case (op)
      case ('.and.', '.or.')
      case default
         print *, "Error: logical operator '"//trim(op)//"' not recognised"
         eval_error = .true.; res = [bad_value]; return
      end select

      ! -------- conformability & broadcasting ----------------------
      if (na == nb) then
         n = na
      else if (na == 1) then
         n = nb
      else if (nb == 1) then
         n = na
      else
         print *, "Error: size mismatch in logical "//trim(op)
         eval_error = .true.; res = [bad_value]; return
      end if
      allocate (mask(n))

      ! -------- build element‑wise truth masks ---------------------
      if (na == 1) then
         mask = (a(1) /= 0.0_dp)
      else
         mask = (a /= 0.0_dp)
      end if

      if (op == '.and.') then
         if (nb == 1) then
            mask = mask .and. (b(1) /= 0.0_dp)
         else
            mask = mask .and. (b /= 0.0_dp)
         end if
      else                            ! ".or."
         if (nb == 1) then
            mask = mask .or. (b(1) /= 0.0_dp)
         else
            mask = mask .or. (b /= 0.0_dp)
         end if
      end if

      res = merge(1.0_dp, 0.0_dp, mask)              ! back to 0/1 numbers
   end function logical_binary

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

      nt = size(t_source); nf = size(f_source); nm = size(mask_val)
      n = max(nt, nf, nm)

      ! ---- conformability checks -----------------------------------------
      if ((nt /= 1 .and. nt /= n) &
          .or. (nf /= 1 .and. nf /= n) &
          .or. (nm /= 1 .and. nm /= n)) then
         print *, "Error: merge() arguments are not conformable"
         eval_error = .true.; res = [bad_value]; return
      end if

      ! ---- broadcast scalars where necessary -----------------------------
      allocate (t(n), f(n), lmask(n))
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
      allocate (res(n))
      res = merge(t, f, lmask)   ! use intrinsic MERGE now that shapes match
   end function merge_array

   subroutine split_by_semicolon(line, n, parts, suppress)
!  Break LINE into statements separated by *top‑level* semicolons.
!  parts(i)   = i-th statement (trimmed)
!  suppress(i)= .true. if that statement ended with a ';'
      character(len=*), intent(in)  :: line
      integer, intent(out) :: n
      character(len=:), allocatable  :: parts(:)
      logical, allocatable  :: suppress(:)

      character(len=:), allocatable :: buf
      integer :: i, depth_par, depth_br, ntrim

      buf = ""
      depth_par = 0      ! '(' … ')'
      depth_br = 0      ! '[' … ']'
      n = 0

      do i = 1, len_trim(line)
         select case (line(i:i))
         case ("("); depth_par = depth_par + 1
         case (")"); depth_par = depth_par - 1
         case ("["); depth_br = depth_br + 1
         case ("]"); depth_br = depth_br - 1
         case (";")
            if (depth_par == 0 .and. depth_br == 0) then
               call append_statement(buf, .true.)
               buf = ""
               cycle
            end if
         end select
         buf = buf//line(i:i)
      end do

      ! last (or only) statement
      if (len_trim(buf) > 0) then
         call append_statement(buf, .false.)
      else
         ntrim = len_trim(line)
         if (ntrim > 0) then
            if (line(ntrim:ntrim) == ";") call append_statement("", .true.)
         end if
      end if

   contains
      subroutine append_statement(txt, semi)
         ! Append the trimmed statement TXT to the PARTS array, marking it as
         ! suppressed (SEMI=.true.) if it ended with a top-level semicolon,
         ! growing the buffer as needed
         character(len=*), intent(in) :: txt
         logical, intent(in) :: semi
         integer :: newlen

         newlen = max(len_trim(txt), merge(0, len(parts(1)), allocated(parts)))

         ! ---- grow / (re)allocate PARTS ------------------------------------
         if (.not. allocated(parts)) then
            allocate (character(len=newlen) :: parts(1))
            allocate (suppress(1))
         else if (len(parts(1)) < newlen) then
            call enlarge_parts(newlen)
         else
            parts = [character(len=len(parts)) :: parts, ""] ! extend by one element
            suppress = [suppress, .false.]
         end if

         ! ---- store the new statement --------------------------------------
         n = n + 1
         parts(n) = adjustl(trim(txt))
         suppress(n) = semi
      end subroutine append_statement

      subroutine enlarge_parts(newlen)
         ! Resize the PARTS and SUPPRESS arrays to length NEWLEN, preserving
         ! existing contents and adding an extra slot for a new statement.
         integer, intent(in) :: newlen
         character(len=newlen), allocatable :: tmp(:)

         allocate (tmp(size(parts)))
         tmp = parts                             ! old contents, padded
         call move_alloc(tmp, parts)             ! now PARTS has the new length
         parts = [character(len=len(parts)) :: parts, ""]  ! add a new blank slot
         suppress = [suppress, .false.]
      end subroutine enlarge_parts
   end subroutine split_by_semicolon

   subroutine run_loop_body(body)
      ! Execute the buffered DO‑loop BODY one line at a time by calling
      ! eval_print, handling CYCLE and EXIT via cycle_loop and exit_loop flags.
      character(len=*), intent(in) :: body
      character(len=:), allocatable :: line
      integer :: p1, p2, nlen
      in_loop_execute = .true.          ! >>> tell eval_print to *execute*
      nlen = len_trim(body)
      p1 = 1
      do
         p2 = index(body(p1:), new_line("a"))             ! next newline
         if (p2 == 0) then
            line = body(p1:nlen)
         else
            line = body(p1:p1 + p2 - 2)
         end if
         call eval_print(line)                            ! recursion
         if (cycle_loop) then
            ! — we’ve seen a “cycle” in this iteration,
            !   so drop the rest of the body and go back to the DO
            cycle_loop = .false.
            in_loop_execute = .false.
            return
         end if
         if (exit_loop) then
            in_loop_execute = .false.
            return
         end if
         if (p2 == 0) exit
         p1 = p1 + p2
      end do
      in_loop_execute = .false.         ! <<< back to normal typing mode
   end subroutine run_loop_body

   integer function parse_int_scalar(txt) result(iv)
      ! Evaluate the expression txt as a single real(dp), round to the
      ! nearest integer, and return for use in parsing do-loop bounds.
      character(len=*), intent(in) :: txt
      real(dp), allocatable        :: tmp(:)

      tmp = evaluate(txt)
      if (eval_error .or. size(tmp) /= 1) then
         print *, "Error: bad scalar expression in DO header: '", trim(txt), "'"
         iv = 0        ! any value – the loop will not run anyway
         return
      end if
      iv = nint(tmp(1))
   end function parse_int_scalar

   subroutine run(filename)
      !  Read the text file FILENAME line-by-line and feed every line to the
      !  interpreter as if the user had typed it.
      character(len=*), intent(in) :: filename
      character(len=1000) :: ln
      integer :: u, ios, comment_pos, neval
      logical :: verbose_
      verbose_ = .false.
      if (verbose_) neval = 0
      open (newunit=u, file=trim(filename), status='old', action='read', iostat=ios)
      if (ios /= 0) then
         write (*, '("Error: cannot open file ''",a,"'' (iostat=",i0,")")') trim(filename), ios
         return
      end if

      do                                   ! read until EOF
         read (u, '(A)', iostat=ios) ln
         if (ios /= 0) exit
         if (len_trim(ln) == 0) cycle       ! ignore blank lines
         comment_pos = index(ln, comment_char)
         if (comment_pos > 0) ln = ln(1:max(1, comment_pos - 1))
         if (ln /= comment_char) then
            if (verbose_ .and. neval > 0) print "(/)"
            if (verbose_) print "(a)", trim(ln)
            call eval_print(ln)
            if (verbose_) neval = neval + 1
         end if
         if (stop_if_error .and. eval_error) exit
      end do
      close (u)
   end subroutine run

end module interpret_mod
