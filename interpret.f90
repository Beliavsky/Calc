module interpret_mod
   use kind_mod, only: dp
   use stats_mod, only: mean, sd, cor, cov, acf, pacf, fiacf, fracdiff, arcoef, aracf, maacf, arpacf, mapacf, armaacf, arfimaacf, armapacf, arsim, masim, armasim, arfimasim, resample, regress, regress_multi, arfit, mafit, armafit, armafitgrid, armafitaic, arfimafit, mssk, mssk_exp, mssk_gamma, mssk_lnorm, mssk_t, mssk_chisq, mssk_f, mssk_beta, mssk_logis, mssk_sech, mssk_laplace, dunif, dexp, dgamma, dlnorm, dnorm, dt, dchisq, df, dbeta, dlogis, dsech, dlaplace, dcauchy, dged, dhyperb, punif, pexp, pgamma, plnorm, pnorm, pt, pchisq, pf, pbeta, plogis, psech, plaplace, pcauchy, pged, phyperb, qunif, qexp, qgamma, qlnorm, qnorm, qt, qchisq, qf, qbeta, qlogis, qsech, qlaplace, qcauchy, qged, qhyperb, rhyperb, fit_norm, fit_exp, fit_gamma, fit_lnorm, fit_t, fit_chisq, fit_f, fit_beta, fit_logis, fit_sech, fit_laplace, fit_cauchy, fit_ged, fit_hyperb, cumsum, cumprod, diff, standardize, &
                        print_stats, skew, kurtosis, cummean, cummin, cummax, &
                        geomean, harmean
   use util_mod, only: matched_brackets, matched_parentheses, arange, &
                       head, tail, grid, print_real, is_alphanumeric, &
                       is_numeral, is_letter, zeros, ones, replace, &
                       rep, read_vec, reverse
   use random_mod, only: random_normal, runif, rexp, rgamma, rlnorm, rt, rchisq, rf, rbeta, rlogis, rsech, rlaplace, rcauchy, rged
   use qsort_mod, only: sorted, indexx, rank, median, unique, quantile
   use iso_fortran_env, only: compiler_options, compiler_version
   use plot_mod, only: plot, plot_to_label
   implicit none
   private
   public :: eval_print, tunit, code_transcript_file, vars, write_code, &
             echo_code, get_loop_depth, get_prompt_depth

   integer, parameter :: max_vars = 100, len_name = 32
   integer, parameter :: max_print = 15 ! for arrays larger than this, summary stats printed instead of elements

   type :: var_t
      character(len=len_name) :: name = ""
      real(kind=dp), allocatable :: val(:)
      logical :: is_const = .false.
   end type var_t
   type :: arr_t
      real(kind=dp), allocatable :: v(:)
   end type arr_t

   type(var_t) :: vars(max_vars)
   integer :: n_vars = 0, tunit
   logical, save :: write_code = .true., eval_error = .false., &
                    echo_code = .true.
   logical, save :: const_assign = .false.
   logical, save :: suppress_result = .false.
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
   integer, save :: exit_target_depth = 0       ! loop depth targeted by EXIT
   integer, save :: cycle_target_depth = 0      ! loop depth targeted by CYCLE
   integer, save :: loop_exec_base_depth = 0    ! depth being executed by run_loop_body
   logical, save :: if_collecting = .false.
   integer, save :: if_collect_depth = 0
   character(len=32768), save :: if_collect_body = ""
   integer, save :: loop_if_collect_depth = 0
   logical, parameter :: debug_read = .false.

!––– support for DO … END DO loops –––––––––––––––––––––––––––––––––
!── Maximum nesting and a fixed buffer for every loop level
   integer, parameter :: max_loop_depth = 8
   character(len=4096), save :: loop_body(max_loop_depth) = ""   ! collected lines
   character(len=len_name), save :: loop_var(max_loop_depth) = ""   ! i , j , ...
   integer, save :: loop_start(max_loop_depth) = 0
   integer, save :: loop_end(max_loop_depth) = 0
   integer, save :: loop_step(max_loop_depth) = 1
   logical, save :: loop_is_unbounded(max_loop_depth) = .false.
   logical, save :: loop_is_for(max_loop_depth) = .false.
   character(len=2048), save :: loop_for_expr(max_loop_depth) = ""
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
         vars(i)%is_const = .false.
         if (allocated(vars(i)%val)) deallocate (vars(i)%val)
      end do
      n_vars = 0
   end subroutine clear

   subroutine print_cor_matrix_args(args, labels)
      type(arr_t), intent(in) :: args(:)
      character(len=*), intent(in) :: labels(:)
      integer :: i, j, n, nsize, max_name, col_width, pad
      real(kind=dp) :: cval

      n = size(args)
      if (n < 2) then
         print *, "Error: cor() needs at least two arguments"
         eval_error = .true.
         return
      end if
      nsize = size(args(1)%v)
      if (nsize < 2) then
         print *, "Error: function array arguments must have sizes > 1"
         eval_error = .true.
         return
      end if
      do i = 2, n
         if (size(args(i)%v) /= nsize) then
            print "(a,i0,1x,i0,a)", "Error: function array arguments have sizes ", &
               nsize, size(args(i)%v), " must be equal"
            eval_error = .true.
            return
         end if
      end do

      max_name = 1
      do i = 1, n
         max_name = max(max_name, len_trim(labels(i)))
      end do
      col_width = max(10, max_name)

      write (*, "(a,i0,a)") "Correlation matrix (n=", nsize, "):"
      write (*, "(a)", advance="no") repeat(" ", max_name)//" "
      do j = 1, n
         call write_padded(trim(labels(j)), col_width)
      end do
      print *

      do i = 1, n
         call write_padded(trim(labels(i)), max_name)
         do j = 1, n
            cval = cor(args(i)%v, args(j)%v)
            write (*, "(f10.6)", advance="no") cval
            pad = col_width - 10
            if (pad < 0) pad = 0
            write (*, "(a)", advance="no") repeat(" ", pad + 1)
         end do
         print *
      end do
   contains
      subroutine write_padded(str, width)
         character(len=*), intent(in) :: str
         integer, intent(in) :: width
         integer :: nsp
         nsp = width - len_trim(str)
         if (nsp < 0) nsp = 0
         write (*, "(a)", advance="no") trim(str)//repeat(" ", nsp + 1)
      end subroutine write_padded
   end subroutine print_cor_matrix_args

   subroutine print_cor_matrices()
      integer, allocatable :: sizes(:), idx(:)
      integer :: i, j, k, nsize, n_sizes, n_group
      integer :: max_name, col_width, pad
      real(kind=dp) :: cval
      logical :: any_printed

      allocate (sizes(n_vars))
      n_sizes = 0
      do i = 1, n_vars
         nsize = size(vars(i)%val)
         if (nsize < 2) cycle
         if (n_sizes == 0) then
            n_sizes = 1
            sizes(1) = nsize
         else if (.not. any(sizes(1:n_sizes) == nsize)) then
            n_sizes = n_sizes + 1
            sizes(n_sizes) = nsize
         end if
      end do

      if (n_sizes == 0) then
         print *, "No array variables with size > 1"
         return
      end if

      any_printed = .false.
      do k = 1, n_sizes
         nsize = sizes(k)
         n_group = 0
         max_name = 1
         do i = 1, n_vars
            if (size(vars(i)%val) == nsize) then
               n_group = n_group + 1
               max_name = max(max_name, len_trim(vars(i)%name))
            end if
         end do
         if (n_group < 2) cycle

         allocate (idx(n_group))
         n_group = 0
         do i = 1, n_vars
            if (size(vars(i)%val) == nsize) then
               n_group = n_group + 1
               idx(n_group) = i
            end if
         end do

         col_width = max(10, max_name)
         write (*, "(a,i0,a)") "Correlation matrix (n=", nsize, "):"

         write (*, "(a)", advance="no") repeat(" ", max_name)//" "
         do j = 1, n_group
            call write_padded(trim(vars(idx(j))%name), col_width)
         end do
         print *

         do i = 1, n_group
            call write_padded(trim(vars(idx(i))%name), max_name)
            do j = 1, n_group
               cval = cor(vars(idx(i))%val, vars(idx(j))%val)
               write (*, "(f10.6)", advance="no") cval
               pad = col_width - 10
               if (pad < 0) pad = 0
               write (*, "(a)", advance="no") repeat(" ", pad + 1)
            end do
            print *
         end do

         deallocate (idx)
         any_printed = .true.
      end do

      if (.not. any_printed) then
         print *, "No matching array groups with size > 1"
      end if
   contains
      subroutine write_padded(str, width)
         character(len=*), intent(in) :: str
         integer, intent(in) :: width
         integer :: nsp
         nsp = width - len_trim(str)
         if (nsp < 0) nsp = 0
         write (*, "(a)", advance="no") trim(str)//repeat(" ", nsp + 1)
      end subroutine write_padded
   end subroutine print_cor_matrices

   subroutine read_vars_from_file(fname)
      character(len=*), intent(in) :: fname
      integer :: u, ios, i, ncol, nrow, comment_pos
      character(len=1000) :: line
      character(len=:), allocatable :: header, words(:)
      real(kind=dp), allocatable :: vals(:), tmp(:)
      type(arr_t), allocatable :: cols(:)
      logical :: found_data

      open(newunit=u, file=trim(fname), action='read', status='old', iostat=ios)
      if (ios /= 0) then
         write(*,'("Error: cannot open file ''",a,"'' (iostat=",i0,")")') trim(fname), ios
         eval_error = .true.
         return
      end if

      header = ""
      do
         read(u,'(A)', iostat=ios) line
         if (ios /= 0) then
            print *, "Error: could not read header from file"
            eval_error = .true.
            close(u)
            return
         end if
         comment_pos = index(line,'!')
         if (comment_pos > 0) line = line(:comment_pos-1)
         if (len_trim(line) == 0) cycle
         header = replace(line, ",", " ")
         header = replace(header, char(9), " ")
         exit
      end do

      call split_by_spaces(header, ncol, words)
      if (ncol < 1) then
         print *, "Error: no column names found in header"
         eval_error = .true.
         close(u)
         return
      end if

      allocate (cols(ncol))
      do i = 1, ncol
         allocate (cols(i)%v(0))
      end do

      found_data = .false.
      nrow = 0
      allocate (vals(ncol))
      do
         read(u,'(A)', iostat=ios) line
         if (ios /= 0) exit
         comment_pos = index(line,'!')
         if (comment_pos > 0) line = line(:comment_pos-1)
         if (len_trim(line) == 0) cycle
         line = replace(line, ",", " ")
         read(line,*, iostat=ios) vals
         if (ios /= 0) then
            if (.not. found_data) then
               cycle
            else
               exit
            end if
         end if
         found_data = .true.
         nrow = nrow + 1
         do i = 1, ncol
            if (allocated(tmp)) deallocate (tmp)
            allocate (tmp(nrow))
            if (nrow > 1) tmp(1:nrow-1) = cols(i)%v
            tmp(nrow) = vals(i)
            call move_alloc(tmp, cols(i)%v)
         end do
      end do
      close(u)

      if (.not. found_data) then
         print *, "Error: no data rows found in file"
         eval_error = .true.
         return
      end if

      do i = 1, ncol
         call set_variable(words(i), cols(i)%v)
         if (eval_error) return
      end do
   contains
      subroutine split_by_spaces(line_in, n, parts)
         character(len=*), intent(in) :: line_in
         integer, intent(out) :: n
         character(len=:), allocatable :: parts(:)
         integer :: i, start, len_line, newlen, nlen_tail

         n = 0
         len_line = len_trim(line_in)
         i = 1
      do while (i <= len_line)
         do
            if (i > len_line) exit
            if (line_in(i:i) /= " ") exit
            i = i + 1
         end do
         if (i > len_line) exit
         start = i
         do
            if (i > len_line) exit
            if (line_in(i:i) == " ") exit
            i = i + 1
         end do
         nlen_tail = min(i - 1, len_line)
            if (nlen_tail < start) cycle
            newlen = max(nlen_tail - start + 1, merge(0, len(parts(1)), allocated(parts)))
            if (.not. allocated(parts)) then
               allocate (character(len=newlen) :: parts(1))
            else if (len(parts(1)) < newlen) then
               block
                  character(len=newlen), allocatable :: tmp(:)
                  allocate (tmp(size(parts)))
                  tmp = parts
                  call move_alloc(tmp, parts)
                  parts = [character(len=len(parts)) :: parts, ""]
               end block
            else
               parts = [character(len=len(parts)) :: parts, ""]
            end if
            n = n + 1
            parts(n) = adjustl(line_in(start:nlen_tail))
         end do
      end subroutine split_by_spaces
   end subroutine read_vars_from_file

   subroutine set_variable(name, val, is_const)
      ! Store or replace a variable
      character(len=*), intent(in) :: name
      real(kind=dp), intent(in) :: val(:)
      logical, intent(in), optional :: is_const
      integer :: i
      character(len=len_name) :: nm
      logical :: make_const

      if (present(is_const)) then
         make_const = is_const
      else
         make_const = .false.
      end if

      nm = adjustl(name)
      do i = 1, n_vars
         if (vars(i)%name == nm) then
            if (vars(i)%is_const) then
               print *, "Error: cannot reassign const variable '"//trim(nm)//"'"
               eval_error = .true.
               return
            end if
            if (make_const) then
               print *, "Error: const variable '"//trim(nm)//"' already exists"
               eval_error = .true.
               return
            end if
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
         vars(n_vars)%is_const = make_const
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
      case ("mssk"); res = mssk(arr)
      case ("fit_norm"); res = fit_norm(arr)
      case ("fit_exp"); res = fit_exp(arr)
      case ("fit_gamma"); res = fit_gamma(arr)
      case ("fit_lnorm"); res = fit_lnorm(arr)
      case ("fit_t"); res = fit_t(arr)
      case ("fit_chisq"); res = fit_chisq(arr)
      case ("fit_f"); res = fit_f(arr)
      case ("fit_beta"); res = fit_beta(arr)
      case ("fit_logis"); res = fit_logis(arr)
      case ("fit_sech"); res = fit_sech(arr)
      case ("fit_laplace"); res = fit_laplace(arr)
      case ("fit_cauchy"); res = fit_cauchy(arr)
      case ("fit_ged"); res = fit_ged(arr)
      case ("fit_hyperb"); res = fit_hyperb(arr)
      case ("dsech"); res = dsech(arr)
      case ("psech"); res = psech(arr)
      case ("qsech"); res = qsech(arr)
      case default
         print *, "Error in apply_vec_func: function '", trim(fname), "' not defined"
         eval_error = .true.
         res = [bad_value]
      end select
   end function apply_vec_func

   elemental function lower_str(s) result(out)
      character(len=*), intent(in) :: s
      character(len=len(s)) :: out
      integer :: i, c
      do i = 1, len(s)
         c = iachar(s(i:i))
         if (c >= iachar('A') .and. c <= iachar('Z')) then
            out(i:i) = achar(c + (iachar('a') - iachar('A')))
         else
            out(i:i) = s(i:i)
         end if
      end do
   end function lower_str

   pure logical function is_end_if_line(tl) result(ok)
      character(len=*), intent(in) :: tl
      character(len=:), allocatable :: t
      t = trim(lower_str(adjustl(tl)))
      ok = (t == "end if" .or. t == "endif" .or. t == "end if;" .or. t == "endif;")
   end function is_end_if_line

   pure logical function is_else_line(tl) result(ok)
      character(len=*), intent(in) :: tl
      character(len=:), allocatable :: t
      t = trim(lower_str(adjustl(tl)))
      ok = (t == "else" .or. t == "else;")
   end function is_else_line

   pure logical function is_end_for_line(tl) result(ok)
      character(len=*), intent(in) :: tl
      character(len=:), allocatable :: t
      t = trim(lower_str(adjustl(tl)))
      ok = (t == "end for" .or. t == "endfor" .or. t == "end for;" .or. t == "endfor;")
   end function is_end_for_line

   pure logical function is_op_char(ch) result(ok)
      character(len=1), intent(in) :: ch
      ok = (index("+-*/^<>=:&|", ch) > 0)
   end function is_op_char

   pure subroutine split_expr_tail(rem, expr_part, tail_part)
      character(len=*), intent(in) :: rem
      character(len=:), allocatable, intent(out) :: expr_part, tail_part
      character(len=:), allocatable :: left, right
      integer :: i, n, dpar, dbr
      logical :: in_str

      expr_part = trim(rem)
      tail_part = ""
      n = len_trim(rem)
      dpar = 0
      dbr = 0
      in_str = .false.
      do i = 1, n
         if (rem(i:i) == '"') then
            in_str = .not. in_str
         else if (.not. in_str) then
            select case (rem(i:i))
            case ("(")
               dpar = dpar + 1
            case (")")
               if (dpar > 0) dpar = dpar - 1
            case ("[")
               dbr = dbr + 1
            case ("]")
               if (dbr > 0) dbr = dbr - 1
            case (" ")
               if (dpar == 0 .and. dbr == 0) then
                  if (i > 1 .and. i < n) then
                     left = trim(rem(1:i - 1))
                     right = adjustl(rem(i + 1:n))
                     if (len_trim(left) > 0 .and. len_trim(right) > 0) then
                        if (.not. is_op_char(left(len_trim(left):len_trim(left))) .and. &
                            .not. is_op_char(right(1:1))) then
                           expr_part = left
                           tail_part = right
                           exit
                        end if
                     end if
                  end if
               end if
            end select
         end if
      end do
   end subroutine split_expr_tail

   pure subroutine parse_for_header(line, lhs, rhs_expr, rhs_tail, ok)
      character(len=*), intent(in) :: line
      character(len=:), allocatable, intent(out) :: lhs, rhs_expr, rhs_tail
      logical, intent(out) :: ok
      character(len=:), allocatable :: s, low, rem
      integer :: p_in

      lhs = ""
      rhs_expr = ""
      rhs_tail = ""
      ok = .false.

      s = adjustl(line)
      low = lower_str(s)
      if (index(low, "for ") /= 1) return
      p_in = index(low, " in ")
      if (p_in <= 5) return
      lhs = adjustl(s(5:p_in - 1))
      rem = adjustl(s(p_in + 4:))
      if (.not. is_alnum_string(lhs) .or. len_trim(rem) == 0) return
      call split_expr_tail(rem, rhs_expr, rhs_tail)
      if (len_trim(rhs_expr) == 0) return
      ok = .true.
   end subroutine parse_for_header

   pure subroutine parse_do_header(line, lhs, start_expr, end_expr, step_expr, rhs_tail, ok)
      character(len=*), intent(in) :: line
      character(len=:), allocatable, intent(out) :: lhs, start_expr, end_expr, step_expr, rhs_tail
      logical, intent(out) :: ok
      character(len=:), allocatable :: s, low, rem, rhs
      integer :: p_eq, p_com1, p_com2

      lhs = ""
      start_expr = ""
      end_expr = ""
      step_expr = ""
      rhs_tail = ""
      ok = .false.

      s = adjustl(line)
      low = lower_str(s)
      if (index(low, "do ") /= 1) return
      if (trim(low) == "do") return
      p_eq = index(s, "=")
      if (p_eq == 0) return
      lhs = adjustl(s(3:p_eq - 1))
      if (.not. is_alnum_string(lhs)) return
      rem = adjustl(s(p_eq + 1:))
      if (len_trim(rem) == 0) return
      call split_expr_tail(rem, rhs, rhs_tail)
      p_com1 = index(rhs, ",")
      if (p_com1 == 0) return
      p_com2 = index(rhs(p_com1 + 1:), ",")
      if (p_com2 > 0) p_com2 = p_com1 + p_com2
      start_expr = adjustl(rhs(1:p_com1 - 1))
      if (p_com2 == 0) then
         end_expr = adjustl(rhs(p_com1 + 1:))
         step_expr = "1"
      else
         end_expr = adjustl(rhs(p_com1 + 1:p_com2 - 1))
         step_expr = adjustl(rhs(p_com2 + 1:))
      end if
      if (len_trim(start_expr) == 0 .or. len_trim(end_expr) == 0 .or. len_trim(step_expr) == 0) return
      ok = .true.
   end subroutine parse_do_header

   pure subroutine parse_if_then_header(line, is_else_if, cond, ok)
      character(len=*), intent(in) :: line
      logical, intent(in) :: is_else_if
      character(len=:), allocatable, intent(out) :: cond
      logical, intent(out) :: ok
      character(len=:), allocatable :: s, ls, prefix, tail
      integer :: p_lpar, p_rpar, depth, n

      ok = .false.
      cond = ""
      s = adjustl(line)
      ls = lower_str(s)
      n = len_trim(s)
      if (n <= 0) return

      p_lpar = index(s, "(")
      if (p_lpar <= 1) return

      prefix = trim(lower_str(adjustl(s(1:p_lpar - 1))))
      if (is_else_if) then
         if (prefix /= "else if" .and. prefix /= "elseif") return
      else
         if (prefix /= "if") return
      end if

      p_rpar = p_lpar
      depth = 1
      do while (p_rpar < n .and. depth > 0)
         p_rpar = p_rpar + 1
         select case (s(p_rpar:p_rpar))
         case ("(")
            depth = depth + 1
         case (")")
            depth = depth - 1
         end select
      end do
      if (depth /= 0) return

      if (p_rpar < n) then
         tail = trim(lower_str(adjustl(s(p_rpar + 1:n))))
      else
         tail = ""
      end if
      if (tail /= "then" .and. tail /= "then;") return

      cond = adjustl(s(p_lpar + 1:p_rpar - 1))
      if (len_trim(cond) == 0) return
      ok = .true.
   end subroutine parse_if_then_header

   pure logical function is_block_if_start_line(line) result(ok)
      character(len=*), intent(in) :: line
      character(len=:), allocatable :: cond
      logical :: parsed
      call parse_if_then_header(line, .false., cond, parsed)
      ok = parsed
   end function is_block_if_start_line

   pure logical function is_else_if_line(line) result(ok)
      character(len=*), intent(in) :: line
      character(len=:), allocatable :: cond
      logical :: parsed
      call parse_if_then_header(line, .true., cond, parsed)
      ok = parsed
   end function is_else_if_line

   subroutine execute_if_block(body)
      character(len=*), intent(in) :: body
      integer, parameter :: max_if_branches = 16
      character(len=4096) :: branch_cond(max_if_branches)
      character(len=32768) :: branch_body(max_if_branches)
      logical :: branch_else(max_if_branches)
      character(len=:), allocatable :: line, tline, cond
      real(kind=dp), allocatable :: cv(:)
      integer :: nlen, p1, p2, depth_if, b, active_branch, n_branch
      logical :: ok, have_else, take_branch

      branch_cond = ""
      branch_body = ""
      branch_else = .false.
      nlen = len_trim(body)
      if (nlen == 0) return

      p1 = 1
      p2 = index(body(p1:), new_line("a"))
      if (p2 == 0) then
         line = body(p1:nlen)
      else
         line = body(p1:p1 + p2 - 2)
      end if
      call parse_if_then_header(line, .false., cond, ok)
      if (.not. ok) then
         print *, "Error: malformed IF header"
         eval_error = .true.
         return
      end if
      n_branch = 1
      active_branch = 1
      branch_cond(1) = trim(cond)
      depth_if = 1
      have_else = .false.
      if (p2 == 0) then
         print *, "Error: missing END IF"
         eval_error = .true.
         return
      end if
      p1 = p1 + p2

      do
         p2 = index(body(p1:), new_line("a"))
         if (p2 == 0) then
            line = body(p1:nlen)
         else
            line = body(p1:p1 + p2 - 2)
         end if
         tline = lower_str(adjustl(line))

         if (is_block_if_start_line(line)) then
            depth_if = depth_if + 1
            if (len_trim(branch_body(active_branch)) + len_trim(line) + 1 > len(branch_body(active_branch))) then
               print *, "Error: IF body too large"
               eval_error = .true.
               return
            end if
            branch_body(active_branch) = trim(branch_body(active_branch))//trim(line)//new_line("a")
         else if (is_end_if_line(tline)) then
            depth_if = depth_if - 1
            if (depth_if == 0) exit
            if (depth_if < 0) then
               print *, "Error: unmatched END IF"
               eval_error = .true.
               return
            end if
            if (len_trim(branch_body(active_branch)) + len_trim(line) + 1 > len(branch_body(active_branch))) then
               print *, "Error: IF body too large"
               eval_error = .true.
               return
            end if
            branch_body(active_branch) = trim(branch_body(active_branch))//trim(line)//new_line("a")
         else if (depth_if == 1 .and. is_else_if_line(line)) then
            if (have_else) then
               print *, "Error: ELSE IF after ELSE is not allowed"
               eval_error = .true.
               return
            end if
            if (n_branch >= max_if_branches) then
               print *, "Error: too many ELSE IF branches"
               eval_error = .true.
               return
            end if
            call parse_if_then_header(line, .true., cond, ok)
            if (.not. ok) then
               print *, "Error: malformed ELSE IF header"
               eval_error = .true.
               return
            end if
            n_branch = n_branch + 1
            active_branch = n_branch
            branch_cond(active_branch) = trim(cond)
            branch_body(active_branch) = ""
            branch_else(active_branch) = .false.
         else if (depth_if == 1 .and. is_else_line(tline)) then
            if (have_else) then
               print *, "Error: duplicate ELSE branch"
               eval_error = .true.
               return
            end if
            if (n_branch >= max_if_branches) then
               print *, "Error: too many IF branches"
               eval_error = .true.
               return
            end if
            have_else = .true.
            n_branch = n_branch + 1
            active_branch = n_branch
            branch_cond(active_branch) = ""
            branch_body(active_branch) = ""
            branch_else(active_branch) = .true.
         else
            if (len_trim(branch_body(active_branch)) + len_trim(line) + 1 > len(branch_body(active_branch))) then
               print *, "Error: IF body too large"
               eval_error = .true.
               return
            end if
            branch_body(active_branch) = trim(branch_body(active_branch))//trim(line)//new_line("a")
         end if

         if (p2 == 0) exit
         p1 = p1 + p2
      end do

      if (depth_if /= 0) then
         print *, "Error: missing END IF"
         eval_error = .true.
         return
      end if

      do b = 1, n_branch
         if (branch_else(b)) then
            take_branch = .true.
         else
            cv = evaluate(trim(branch_cond(b)))
            if (eval_error) return
            if (size(cv) /= 1) then
               print *, "Error: IF condition must be scalar"
               eval_error = .true.
               return
            end if
            take_branch = (cv(1) /= 0.0_dp)
         end if
         if (take_branch) then
            if (len_trim(branch_body(b)) > 0) call run_loop_body(branch_body(b))
            return
         end if
      end do
   end subroutine execute_if_block

   subroutine collect_loop_definition_line(line_in, had_error, consumed)
      character(len=*), intent(in) :: line_in
      logical, intent(out) :: had_error, consumed
      character(len=:), allocatable :: tl, low, lhs, rhs, rhs_tail, dstart, dend, dstep
      integer :: i
      logical :: ok_for, ok_do

      had_error = .false.
      consumed = .false.
      tl = adjustl(line_in)
      low = lower_str(tl)

      if (index(low, "for ") == 1) then
         call parse_for_header(tl, lhs, rhs, rhs_tail, ok_for)
         if (.not. ok_for) then
            print *, "Error: malformed FOR header: ", trim(line_in)
            had_error = .true.
            consumed = .true.
            return
         end if
         if (len_trim(rhs_tail) > 0) then
            ! One-line FOR stays within the current body; do not change nesting depth.
            loop_body(1) = trim(loop_body(1))//trim(line_in)//new_line("a")
            consumed = .true.
            return
         end if
         do i = 1, loop_depth
            if (trim(loop_var(i)) == trim(lhs)) then
               print *, "Error: nested loop variable '", trim(lhs), "' already used by an outer loop"
               had_error = .true.
               consumed = .true.
               return
            end if
         end do
         loop_body(1) = trim(loop_body(1))//trim(line_in)//new_line("a")
         if (loop_depth >= max_loop_depth) then
            print *, "Error: loop nesting deeper than ", max_loop_depth
            had_error = .true.
            consumed = .true.
            return
         end if
         loop_depth = loop_depth + 1
         loop_var(loop_depth) = lhs
         loop_is_unbounded(loop_depth) = .false.
         loop_is_for(loop_depth) = .true.
         loop_for_expr(loop_depth) = rhs
         consumed = .true.
         return
      else if (index(low, "do ") == 1 .or. trim(low) == "do") then
         if (trim(tl) /= "do") then
            call parse_do_header(tl, lhs, dstart, dend, dstep, rhs_tail, ok_do)
            if (.not. ok_do) then
               print *, "Error: malformed DO header: ", trim(line_in)
               had_error = .true.
               consumed = .true.
               return
            end if
            if (len_trim(rhs_tail) > 0) then
               loop_body(1) = trim(loop_body(1))//trim(line_in)//new_line("a")
               consumed = .true.
               return
            end if
            do i = 1, loop_depth
               if (trim(loop_var(i)) == trim(lhs)) then
                  print *, "Error: nested loop variable '", trim(lhs), "' already used by an outer loop"
                  had_error = .true.
                  consumed = .true.
                  return
               end if
            end do
         else
            lhs = ""
         end if
         loop_body(1) = trim(loop_body(1))//trim(line_in)//new_line("a")
         if (loop_depth >= max_loop_depth) then
            print *, "Error: loop nesting deeper than ", max_loop_depth
            had_error = .true.
            consumed = .true.
            return
         end if
         loop_depth = loop_depth + 1
         loop_var(loop_depth) = lhs
         loop_is_unbounded(loop_depth) = (trim(low) == "do")
         loop_is_for(loop_depth) = .false.
         loop_for_expr(loop_depth) = ""
         consumed = .true.
         return
      else if (trim(low) == "end do" .or. trim(low) == "enddo" .or. is_end_for_line(low)) then
         if (loop_depth > 1) then
            loop_body(1) = trim(loop_body(1))//trim(line_in)//new_line("a")
            loop_var(loop_depth) = ""
            loop_is_unbounded(loop_depth) = .false.
            loop_is_for(loop_depth) = .false.
            loop_for_expr(loop_depth) = ""
            loop_depth = loop_depth - 1
            consumed = .true.
            return
         end if
         consumed = .false.
         return
      else
         if (index(tl, "const") > 0) then
            print *, "Error: const not allowed inside loops or blocks"
            had_error = .true.
            consumed = .true.
            return
         end if
         loop_body(1) = trim(loop_body(1))//trim(line_in)//new_line("a")
         if (is_block_if_start_line(tl)) then
            loop_if_collect_depth = loop_if_collect_depth + 1
         else if (is_end_if_line(tl)) then
            loop_if_collect_depth = max(0, loop_if_collect_depth - 1)
         end if
         consumed = .true.
         return
      end if
   end subroutine collect_loop_definition_line

   pure integer function get_loop_depth() result(d)
      d = loop_depth
   end function get_loop_depth

   pure integer function get_prompt_depth() result(d)
      d = max(0, loop_depth + if_collect_depth + loop_if_collect_depth)
   end function get_prompt_depth

   pure logical function is_alnum_string(s) result(ok)
      character(len=*), intent(in) :: s
      integer :: i, n
      n = len_trim(s)
      if (n < 1) then
         ok = .false.
         return
      end if
      if (.not. (is_letter(s(1:1)) .or. s(1:1) == "_")) then
         ok = .false.
         return
      end if
      do i = 2, n
         if (.not. (is_alphanumeric(s(i:i)) .or. s(i:i) == "_")) then
            ok = .false.
            return
         end if
      end do
      ok = .true.
   end function is_alnum_string

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
               call set_variable(lhs, res, const_assign)   ! wholevariable assignment
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
         real(kind=dp), allocatable :: arg1(:), arg2(:), arg3(:), arg4(:), xmat(:,:)
         real(kind=dp), allocatable :: exponent(:), vvar(:)
         integer, allocatable :: idxv(:)
         character(len=len_name) :: id
         character(len=:), allocatable :: idxs
         integer :: nsize, pstart, pend, depth, n1, n2, dim_val
         integer :: n_args, i_arg
         logical :: is_neg, have_second
         logical :: toplevel_colon, toplevel_comma, have_dim
         character(len=len_name) :: look_name    ! NEW
         type(arr_t), allocatable :: args(:)
         character(len=:), allocatable :: labels(:), pred_labels(:)
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
                     case ("mssk_exp")
                        f = mssk_exp(1.0_dp)
                     case ("mssk_lnorm")
                        f = mssk_lnorm(0.0_dp, 1.0_dp)
                     case ("mssk_logis")
                        f = mssk_logis(0.0_dp, 1.0_dp)
                     case ("mssk_sech")
                        f = mssk_sech()
                     case ("mssk_laplace")
                        f = mssk_laplace(0.0_dp, 1.0_dp)
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
                     else if (trim(id) == "resample") then
                        !------------------------------------------------------------
                        !  resample: parse all args from text; just consume to ')'
                        !------------------------------------------------------------
                        pos = pend + 1
                        if (pos > lenstr) then
                           curr_char = char(0)
                        else
                           curr_char = expr(pos:pos); pos = pos + 1
                        end if
                        have_second = .false.
                     else if (trim(id) == "armafitaic") then
                        !------------------------------------------------------------
                        !  armafitaic: allow keyword-only argument after first arg
                        !------------------------------------------------------------
                        block
                           integer :: save_pos
                           logical :: is_name_eq
                           save_pos = pos
                           call next_char()
                           call skip_spaces()
                           is_name_eq = .false.
                           if (is_letter(curr_char)) then
                              look_name = parse_identifier()
                              call skip_spaces()
                              if (curr_char == "=") is_name_eq = .true.
                           end if
                           if (is_name_eq) then
                              pos = save_pos
                              curr_char = ","
                              have_second = .false.
                           else
                              pos = save_pos
                              call next_char()
                              call skip_spaces()
                              arg2 = parse_expression()
                              have_second = .true.
                              if (eval_error) then
                                 f = [bad_value]; return
                              end if
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

                  if (trim(id) == "cor") then
                     call split_by_comma(expr(pstart:pend - 1), n_args, labels)
                     if (n_args > 2) then
                        if (.not. have_second) then
                           print *, "Error: function needs two arguments"
                           eval_error = .true.; f = [bad_value]; return
                        end if
                        allocate (args(n_args))
                        args(1)%v = arg1
                        args(2)%v = arg2
                        do i_arg = 3, n_args
                           call skip_spaces()
                           if (curr_char /= ",") then
                              print *, "Error: cor() needs arguments separated by commas"
                              eval_error = .true.; f = [bad_value]; return
                           end if
                           call next_char()
                           call skip_spaces()
                           args(i_arg)%v = parse_expression()
                           if (eval_error) then
                              f = [bad_value]; return
                           end if
                        end do
                        call skip_spaces()
                        if (curr_char == ")") call next_char()
                        call print_cor_matrix_args(args, labels)
                        if (eval_error) then
                           f = [bad_value]
                           return
                        end if
                        suppress_result = .true.
                        f = [real(kind=dp) ::]
                        return
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
                  case ("resample")
                     block
                        logical :: have_n, replace_flag
                        integer :: n_rs, eqpos
                        character(len=:), allocatable :: tok, ltok, rval
                        real(kind=dp), allocatable :: tmp(:)

                        call split_by_comma(expr(pstart:pend - 1), n_args, labels)
                        have_n = .false.
                        replace_flag = .true.
                        do i_arg = 2, n_args
                           tok = adjustl(labels(i_arg))
                           if (len_trim(tok) > 0) then
                              if (tok(len_trim(tok):len_trim(tok)) == ")") tok = tok(:len_trim(tok) - 1)
                           end if
                           ltok = lower_str(tok)
                           if (index(ltok, "replace") == 1) then
                              eqpos = index(tok, "=")
                              if (eqpos == 0) then
                                 print *, "Error: replace must be given as replace=..."
                                 eval_error = .true.; f = [bad_value]; return
                              end if
                              rval = adjustl(tok(eqpos + 1:))
                              rval = lower_str(rval)
                              if (rval == ".false." .or. rval == "false" .or. rval == "f") then
                                 replace_flag = .false.
                              else if (rval == ".true." .or. rval == "true" .or. rval == "t") then
                                 replace_flag = .true.
                              else
                                 tmp = evaluate(rval)
                                 if (eval_error) then
                                    f = [bad_value]; return
                                 end if
                                 if (size(tmp) /= 1) then
                                    print *, "Error: replace must be scalar"
                                    eval_error = .true.; f = [bad_value]; return
                                 end if
                                 replace_flag = (tmp(1) /= 0.0_dp)
                              end if
                           else
                              tmp = evaluate(tok)
                              if (eval_error) then
                                 f = [bad_value]; return
                              end if
                              if (size(tmp) /= 1) then
                                 print *, "Error: resample length must be scalar"
                                 eval_error = .true.; f = [bad_value]; return
                              end if
                              n_rs = nint(tmp(1))
                              have_n = .true.
                           end if
                        end do

                        if (.not. have_n) n_rs = size(arg1)
                        if (n_rs < 0) then
                           print *, "Error: resample length must be >= 0"
                           eval_error = .true.; f = [bad_value]; return
                        end if
                        if (.not. replace_flag .and. n_rs > size(arg1)) then
                           print *, "Error: resample length exceeds size(x) with replace=.false."
                           eval_error = .true.; f = [bad_value]; return
                        end if
                        if (have_n) then
                           f = resample(arg1, n_rs, replace_flag)
                        else
                           f = resample(arg1, replace=replace_flag)
                        end if
                     end block

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

                  case ("acf")
                     block
                        logical :: do_plot
                        integer :: eqpos
                        character(len=:), allocatable :: tok, ltok, rval, acf_title
                        real(kind=dp), allocatable :: tmp(:), lags(:)

                        do_plot = .false.

                        call split_by_comma(expr(pstart:pend - 1), n_args, labels)
                        if (n_args > 3) then
                           print *, "Error: acf() takes at most three arguments"
                           eval_error = .true.; f = [bad_value]
                        else
                           if (n_args > 2) then
                              ! consume everything through ')' for named third-argument parsing
                              pos = pend + 1
                              if (pos > lenstr) then
                                 curr_char = char(0)
                              else
                                 curr_char = expr(pos:pos)
                                 pos = pos + 1
                              end if
                              tok = adjustl(labels(3))
                              ltok = lower_str(tok)
                              if (index(ltok, "plot") /= 1) then
                                 print *, "Error: third argument of acf() must be plot=..."
                                 eval_error = .true.; f = [bad_value]
                              else
                                 eqpos = index(tok, "=")
                                 if (eqpos == 0) then
                                    print *, "Error: plot must be given as plot=..."
                                    eval_error = .true.; f = [bad_value]
                                 else
                                    rval = adjustl(tok(eqpos + 1:))
                                    rval = lower_str(rval)
                                    if (rval == ".false." .or. rval == "false" .or. rval == "f") then
                                       do_plot = .false.
                                    else if (rval == ".true." .or. rval == "true" .or. rval == "t") then
                                       do_plot = .true.
                                    else
                                       tmp = evaluate(rval)
                                       if (eval_error) then
                                          f = [bad_value]
                                       else if (size(tmp) /= 1) then
                                          print *, "Error: plot must be scalar"
                                          eval_error = .true.; f = [bad_value]
                                       else
                                          do_plot = (tmp(1) /= 0.0_dp)
                                       end if
                                    end if
                                 end if
                              end if
                           end if

                           if (.not. eval_error) then
                              if (.not. have_second) then
                                 print *, "Error: function needs two arguments"
                                 eval_error = .true.; f = [bad_value]
                              else if (size(arg2) /= 1) then
                                 print *, "Error: second argument of acf() must be scalar"
                                 eval_error = .true.; f = [bad_value]
                              else if (size(arg1) < 2) then
                                 print *, "Error: function array arguments must have sizes > 1, size is ", size(arg1)
                                 eval_error = .true.; f = [bad_value]
                              else
                                 n1 = nint(arg2(1))
                                 if (n1 < 1 .or. n1 > size(arg1) - 1) then
                                    print *, "Error: acf() lag count must be between 1 and ", size(arg1) - 1
                                    eval_error = .true.; f = [bad_value]
                                 else
                                    f = acf(arg1, n1)
                                    if (do_plot) then
                                       lags = arange(size(f))
                                       acf_title = "acf(" // trim(labels(1)) // ", " // trim(labels(2)) // ")"
                                       call plot(lags, f, title=acf_title)
                                    end if
                                 end if
                              end if
                           end if
                        end if
                     end block

                  case ("pacf")
                     block
                        logical :: do_plot
                        integer :: eqpos
                        character(len=:), allocatable :: tok, ltok, rval, pacf_title
                        real(kind=dp), allocatable :: tmp(:), lags(:)

                        do_plot = .false.

                        call split_by_comma(expr(pstart:pend - 1), n_args, labels)
                        if (n_args > 3) then
                           print *, "Error: pacf() takes at most three arguments"
                           eval_error = .true.; f = [bad_value]
                        else
                           if (n_args > 2) then
                              ! consume everything through ')' for named third-argument parsing
                              pos = pend + 1
                              if (pos > lenstr) then
                                 curr_char = char(0)
                              else
                                 curr_char = expr(pos:pos)
                                 pos = pos + 1
                              end if
                              tok = adjustl(labels(3))
                              ltok = lower_str(tok)
                              if (index(ltok, "plot") /= 1) then
                                 print *, "Error: third argument of pacf() must be plot=..."
                                 eval_error = .true.; f = [bad_value]
                              else
                                 eqpos = index(tok, "=")
                                 if (eqpos == 0) then
                                    print *, "Error: plot must be given as plot=..."
                                    eval_error = .true.; f = [bad_value]
                                 else
                                    rval = adjustl(tok(eqpos + 1:))
                                    rval = lower_str(rval)
                                    if (rval == ".false." .or. rval == "false" .or. rval == "f") then
                                       do_plot = .false.
                                    else if (rval == ".true." .or. rval == "true" .or. rval == "t") then
                                       do_plot = .true.
                                    else
                                       tmp = evaluate(rval)
                                       if (eval_error) then
                                          f = [bad_value]
                                       else if (size(tmp) /= 1) then
                                          print *, "Error: plot must be scalar"
                                          eval_error = .true.; f = [bad_value]
                                       else
                                          do_plot = (tmp(1) /= 0.0_dp)
                                       end if
                                    end if
                                 end if
                              end if
                           end if

                           if (.not. eval_error) then
                              if (.not. have_second) then
                                 print *, "Error: function needs two arguments"
                                 eval_error = .true.; f = [bad_value]
                              else if (size(arg2) /= 1) then
                                 print *, "Error: second argument of pacf() must be scalar"
                                 eval_error = .true.; f = [bad_value]
                              else if (size(arg1) < 2) then
                                 print *, "Error: function array arguments must have sizes > 1, size is ", size(arg1)
                                 eval_error = .true.; f = [bad_value]
                              else
                                 n1 = nint(arg2(1))
                                 if (n1 < 1 .or. n1 > size(arg1) - 1) then
                                    print *, "Error: pacf() lag count must be between 1 and ", size(arg1) - 1
                                    eval_error = .true.; f = [bad_value]
                                 else
                                    f = pacf(arg1, n1)
                                    if (do_plot) then
                                       lags = arange(size(f))
                                       pacf_title = "pacf(" // trim(labels(1)) // ", " // trim(labels(2)) // ")"
                                       call plot(lags, f, title=pacf_title)
                                    end if
                                 end if
                              end if
                           end if
                        end if
                     end block

                  case ("fiacf")
                     if (.not. have_second) then
                        print *, "Error: function needs two arguments"
                        eval_error = .true.; f = [bad_value]
                     else if (size(arg1) /= 1) then
                        print *, "Error: first argument of fiacf() must be scalar"
                        eval_error = .true.; f = [bad_value]
                     else if (size(arg2) /= 1) then
                        print *, "Error: second argument of fiacf() must be scalar"
                        eval_error = .true.; f = [bad_value]
                     else
                        n1 = nint(arg2(1))
                        if (n1 < 1) then
                           print *, "Error: fiacf() lag count must be >= 1"
                           eval_error = .true.; f = [bad_value]
                        else
                           f = fiacf(arg1(1), n1)
                        end if
                     end if

                  case ("fracdiff")
                     block
                        integer :: mfd
                        if (.not. have_second) then
                           print *, "Error: function needs two arguments"
                           eval_error = .true.; f = [bad_value]
                        else if (size(arg2) /= 1) then
                           print *, "Error: second argument of fracdiff() must be scalar"
                           eval_error = .true.; f = [bad_value]
                        else if (size(arg1) < 1) then
                           print *, "Error: first argument of fracdiff() must be non-empty"
                           eval_error = .true.; f = [bad_value]
                        else
                           call split_by_comma(expr(pstart:pend - 1), n_args, labels)
                           if (n_args > 3) then
                              print *, "Error: fracdiff() takes at most three arguments"
                              eval_error = .true.; f = [bad_value]
                           else if (n_args == 3) then
                              call skip_spaces()
                              if (curr_char /= ",") then
                                 print *, "Error: fracdiff() third argument parse failed"
                                 eval_error = .true.; f = [bad_value]
                              else
                                 call next_char()
                                 call skip_spaces()
                                 arg3 = parse_expression()
                                 if (eval_error) then
                                    f = [bad_value]
                                 else if (size(arg3) /= 1) then
                                    print *, "Error: third argument of fracdiff() must be scalar"
                                    eval_error = .true.; f = [bad_value]
                                 else
                                    mfd = nint(arg3(1))
                                    if (mfd < 0) then
                                       print *, "Error: fracdiff() truncation lag must be >= 0"
                                       eval_error = .true.; f = [bad_value]
                                    else
                                       f = fracdiff(arg1, arg2(1), mfd)
                                       call skip_spaces()
                                       if (curr_char == ")") call next_char()
                                    end if
                                 end if
                              end if
                              call skip_spaces()
                              if (curr_char == ")") call next_char()
                           else
                              f = fracdiff(arg1, arg2(1))
                           end if
                        end if
                     end block

                  case ("acfpacf")
                     block
                        logical :: do_plot
                        integer :: eqpos, j
                        character(len=:), allocatable :: tok, ltok, rval, tbl_title
                        real(kind=dp), allocatable :: tmp(:), ac(:), pc(:), lags(:), y2(:,:)
                        character(len=4) :: legends(2)

                        do_plot = .false.
                        legends = [character(len=4) :: "ACF", "PACF"]

                        call split_by_comma(expr(pstart:pend - 1), n_args, labels)
                        if (n_args > 3) then
                           print *, "Error: acfpacf() takes at most three arguments"
                           eval_error = .true.; f = [bad_value]
                        else
                           if (n_args > 2) then
                              ! consume everything through ')' when a third argument is present
                              pos = pend + 1
                              if (pos > lenstr) then
                                 curr_char = char(0)
                              else
                                 curr_char = expr(pos:pos)
                                 pos = pos + 1
                              end if
                              tok = adjustl(labels(3))
                              ltok = lower_str(tok)
                              eqpos = index(tok, "=")
                              if (eqpos > 0) then
                                 if (index(ltok, "plot") /= 1) then
                                    print *, "Error: third argument of acfpacf() must be plot=... or a scalar"
                                    eval_error = .true.; f = [bad_value]
                                 else
                                    rval = adjustl(tok(eqpos + 1:))
                                 end if
                              else
                                 rval = tok
                              end if
                              if (.not. eval_error) then
                                 rval = lower_str(rval)
                                 if (rval == ".false." .or. rval == "false" .or. rval == "f") then
                                    do_plot = .false.
                                 else if (rval == ".true." .or. rval == "true" .or. rval == "t") then
                                    do_plot = .true.
                                 else
                                    tmp = evaluate(rval)
                                    if (eval_error) then
                                       f = [bad_value]
                                    else if (size(tmp) /= 1) then
                                       print *, "Error: plot argument must be scalar"
                                       eval_error = .true.; f = [bad_value]
                                    else
                                       do_plot = (tmp(1) /= 0.0_dp)
                                    end if
                                 end if
                              end if
                           end if

                           if (.not. eval_error) then
                              if (.not. have_second) then
                                 print *, "Error: function needs two arguments"
                                 eval_error = .true.; f = [bad_value]
                              else if (size(arg2) /= 1) then
                                 print *, "Error: second argument of acfpacf() must be scalar"
                                 eval_error = .true.; f = [bad_value]
                              else if (size(arg1) < 2) then
                                 print *, "Error: function array arguments must have sizes > 1, size is ", size(arg1)
                                 eval_error = .true.; f = [bad_value]
                              else
                                 n1 = nint(arg2(1))
                                 if (n1 < 1 .or. n1 > size(arg1) - 1) then
                                    print *, "Error: acfpacf() lag count must be between 1 and ", size(arg1) - 1
                                    eval_error = .true.; f = [bad_value]
                                 else
                                    ac = acf(arg1, n1)
                                    pc = pacf(arg1, n1)
                                    print *
                                    print "(a6,2a14)", "lag", "ACF", "PACF"
                                    do j = 1, size(ac)
                                       print "(i6,2f14.6)", j, ac(j), pc(j)
                                    end do
                                    if (do_plot) then
                                       lags = arange(size(ac))
                                       allocate (y2(size(ac), 2))
                                       y2(:, 1) = ac
                                       y2(:, 2) = pc
                                       tbl_title = "acfpacf(" // trim(labels(1)) // ", " // trim(labels(2)) // ")"
                                       call plot(lags, y2, title=tbl_title, xlabel="lag", legend_labels=legends)
                                    end if
                                    suppress_result = .true.
                                    f = [real(kind=dp) ::]
                                 end if
                              end if
                           end if
                        end if
                     end block

                  case ("acfpacfar")
                     block
                        logical :: do_plot
                        integer :: eqpos, j
                        character(len=:), allocatable :: tok, ltok, rval, tbl_title
                        real(kind=dp), allocatable :: tmp(:), ac(:), pc(:), ar(:), lags(:), y3(:,:)
                        character(len=4) :: legends(3)

                        do_plot = .false.
                        legends = [character(len=4) :: "ACF", "PACF", "AR"]

                        call split_by_comma(expr(pstart:pend - 1), n_args, labels)
                        if (n_args > 3) then
                           print *, "Error: acfpacfar() takes at most three arguments"
                           eval_error = .true.; f = [bad_value]
                        else
                           if (n_args > 2) then
                              ! consume everything through ')' when a third argument is present
                              pos = pend + 1
                              if (pos > lenstr) then
                                 curr_char = char(0)
                              else
                                 curr_char = expr(pos:pos)
                                 pos = pos + 1
                              end if
                              tok = adjustl(labels(3))
                              ltok = lower_str(tok)
                              eqpos = index(tok, "=")
                              if (eqpos > 0) then
                                 if (index(ltok, "plot") /= 1) then
                                    print *, "Error: third argument of acfpacfar() must be plot=... or a scalar"
                                    eval_error = .true.; f = [bad_value]
                                 else
                                    rval = adjustl(tok(eqpos + 1:))
                                 end if
                              else
                                 rval = tok
                              end if
                              if (.not. eval_error) then
                                 rval = lower_str(rval)
                                 if (rval == ".false." .or. rval == "false" .or. rval == "f") then
                                    do_plot = .false.
                                 else if (rval == ".true." .or. rval == "true" .or. rval == "t") then
                                    do_plot = .true.
                                 else
                                    tmp = evaluate(rval)
                                    if (eval_error) then
                                       f = [bad_value]
                                    else if (size(tmp) /= 1) then
                                       print *, "Error: plot argument must be scalar"
                                       eval_error = .true.; f = [bad_value]
                                    else
                                       do_plot = (tmp(1) /= 0.0_dp)
                                    end if
                                 end if
                              end if
                           end if

                           if (.not. eval_error) then
                              if (.not. have_second) then
                                 print *, "Error: function needs two arguments"
                                 eval_error = .true.; f = [bad_value]
                              else if (size(arg2) /= 1) then
                                 print *, "Error: second argument of acfpacfar() must be scalar"
                                 eval_error = .true.; f = [bad_value]
                              else if (size(arg1) < 2) then
                                 print *, "Error: function array arguments must have sizes > 1, size is ", size(arg1)
                                 eval_error = .true.; f = [bad_value]
                              else
                                 n1 = nint(arg2(1))
                                 if (n1 < 1 .or. n1 > size(arg1) - 1) then
                                    print *, "Error: acfpacfar() lag count must be between 1 and ", size(arg1) - 1
                                    eval_error = .true.; f = [bad_value]
                                 else
                                    ac = acf(arg1, n1)
                                    pc = pacf(arg1, n1)
                                    ar = arcoef(arg1, n1)
                                    print *
                                    print "(a6,3a14)", "lag", "ACF", "PACF", "AR"
                                    do j = 1, size(ac)
                                       print "(i6,3f14.6)", j, ac(j), pc(j), ar(j)
                                    end do
                                    if (do_plot) then
                                       lags = arange(size(ac))
                                       allocate (y3(size(ac), 3))
                                       y3(:, 1) = ac
                                       y3(:, 2) = pc
                                       y3(:, 3) = ar
                                       tbl_title = "acfpacfar(" // trim(labels(1)) // ", " // trim(labels(2)) // ")"
                                       call plot(lags, y3, title=tbl_title, xlabel="lag", legend_labels=legends)
                                    end if
                                    suppress_result = .true.
                                    f = [real(kind=dp) ::]
                                 end if
                              end if
                           end if
                        end if
                     end block

                  case ("aracf")
                     if (.not. have_second) then
                        print *, "Error: function needs two arguments"
                        eval_error = .true.; f = [bad_value]
                     else if (size(arg2) /= 1) then
                        print *, "Error: second argument of aracf() must be scalar"
                        eval_error = .true.; f = [bad_value]
                     else if (size(arg1) < 1) then
                        print *, "Error: first argument of aracf() must be non-empty"
                        eval_error = .true.; f = [bad_value]
                     else
                        n1 = nint(arg2(1))
                        if (n1 < 1) then
                           print *, "Error: aracf() lag count must be >= 1"
                           eval_error = .true.; f = [bad_value]
                        else
                           f = aracf(arg1, n1)
                        end if
                     end if

                  case ("arpacf")
                     if (.not. have_second) then
                        print *, "Error: function needs two arguments"
                        eval_error = .true.; f = [bad_value]
                     else if (size(arg2) /= 1) then
                        print *, "Error: second argument of arpacf() must be scalar"
                        eval_error = .true.; f = [bad_value]
                     else if (size(arg1) < 1) then
                        print *, "Error: first argument of arpacf() must be non-empty"
                        eval_error = .true.; f = [bad_value]
                     else
                        n1 = nint(arg2(1))
                        if (n1 < 1) then
                           print *, "Error: arpacf() lag count must be >= 1"
                           eval_error = .true.; f = [bad_value]
                        else
                           f = arpacf(arg1, n1)
                        end if
                     end if

                  case ("maacf")
                     if (.not. have_second) then
                        print *, "Error: function needs two arguments"
                        eval_error = .true.; f = [bad_value]
                     else if (size(arg2) /= 1) then
                        print *, "Error: second argument of maacf() must be scalar"
                        eval_error = .true.; f = [bad_value]
                     else if (size(arg1) < 1) then
                        print *, "Error: first argument of maacf() must be non-empty"
                        eval_error = .true.; f = [bad_value]
                     else
                        n1 = nint(arg2(1))
                        if (n1 < 1) then
                           print *, "Error: maacf() lag count must be >= 1"
                           eval_error = .true.; f = [bad_value]
                        else
                           f = maacf(arg1, n1)
                        end if
                     end if

                  case ("quantile")
                     if (.not. have_second) then
                        print *, "Error: function needs two arguments"
                        eval_error = .true.; f = [bad_value]
                     else if (size(arg1) < 1) then
                        print *, "Error: first argument of quantile() must be non-empty"
                        eval_error = .true.; f = [bad_value]
                     else if (any(arg2 < 0.0_dp) .or. any(arg2 > 1.0_dp)) then
                        print *, "Error: quantile() probabilities must be between 0 and 1"
                        eval_error = .true.; f = [bad_value]
                     else
                        f = quantile(arg1, arg2)
                     end if

                  case ("mssk_exp", "mssk_t", "mssk_chisq")
                     if (have_second) then
                        print *, "Error: function takes one argument"
                        eval_error = .true.; f = [bad_value]
                     else if (size(arg1) /= 1) then
                        print *, "Error: argument must be scalar"
                        eval_error = .true.; f = [bad_value]
                     else
                        select case (trim(id))
                        case ("mssk_exp"); f = mssk_exp(arg1(1))
                        case ("mssk_t"); f = mssk_t(arg1(1))
                        case ("mssk_chisq"); f = mssk_chisq(arg1(1))
                        end select
                     end if

                  case ("mssk_gamma", "mssk_lnorm", "mssk_f", "mssk_beta", "mssk_logis", "mssk_laplace")
                     if (.not. have_second) then
                        if (size(arg1) /= 1) then
                           print *, "Error: first argument must be scalar"
                           eval_error = .true.; f = [bad_value]
                        else
                           select case (trim(id))
                           case ("mssk_gamma"); f = mssk_gamma(arg1(1), 1.0_dp)
                           case ("mssk_lnorm"); f = mssk_lnorm(arg1(1), 1.0_dp)
                           case ("mssk_logis"); f = mssk_logis(arg1(1), 1.0_dp)
                           case ("mssk_laplace"); f = mssk_laplace(arg1(1), 1.0_dp)
                           case default
                              print *, "Error: function needs two arguments"
                              eval_error = .true.; f = [bad_value]
                           end select
                        end if
                     else if (size(arg1) /= 1 .or. size(arg2) /= 1) then
                        print *, "Error: arguments must be scalar"
                        eval_error = .true.; f = [bad_value]
                     else
                        select case (trim(id))
                        case ("mssk_gamma"); f = mssk_gamma(arg1(1), arg2(1))
                        case ("mssk_lnorm"); f = mssk_lnorm(arg1(1), arg2(1))
                        case ("mssk_f"); f = mssk_f(arg1(1), arg2(1))
                        case ("mssk_beta"); f = mssk_beta(arg1(1), arg2(1))
                        case ("mssk_logis"); f = mssk_logis(arg1(1), arg2(1))
                        case ("mssk_laplace"); f = mssk_laplace(arg1(1), arg2(1))
                        end select
                     end if

                  case ("dunif")
                     if (.not. have_second) then
                        f = dunif(arg1)
                     else if (size(arg2) /= 1) then
                        print *, "Error: second argument must be scalar"
                        eval_error = .true.; f = [bad_value]
                     else
                        call skip_spaces()
                        if (curr_char == ",") then
                           call next_char()
                           call skip_spaces()
                           arg3 = parse_expression()
                           if (eval_error) then
                              f = [bad_value]
                           else if (size(arg3) /= 1) then
                              print *, "Error: third argument must be scalar"
                              eval_error = .true.; f = [bad_value]
                           else
                              f = dunif(arg1, arg2(1), arg3(1))
                              call skip_spaces()
                              if (curr_char == ")") call next_char()
                           end if
                        else
                           f = dunif(arg1, arg2(1))
                           call skip_spaces()
                           if (curr_char == ")") call next_char()
                        end if
                     end if

                  case ("dexp", "dt", "dchisq")
                     if (.not. have_second) then
                        print *, "Error: function needs two arguments"
                        eval_error = .true.; f = [bad_value]
                     else if (size(arg2) /= 1) then
                        print *, "Error: second argument must be scalar"
                        eval_error = .true.; f = [bad_value]
                     else
                        select case (trim(id))
                        case ("dexp"); f = dexp(arg1, arg2(1))
                        case ("dt"); f = dt(arg1, arg2(1))
                        case ("dchisq"); f = dchisq(arg1, arg2(1))
                        end select
                     end if

                  case ("dged", "dhyperb")
                     if (.not. have_second) then
                        print *, "Error: function needs four arguments"
                        eval_error = .true.; f = [bad_value]
                     else if (size(arg2) /= 1) then
                        print *, "Error: second argument must be scalar"
                        eval_error = .true.; f = [bad_value]
                     else
                        call skip_spaces()
                        if (curr_char /= ",") then
                           print *, "Error: function needs four arguments"
                           eval_error = .true.; f = [bad_value]
                        else
                           call next_char()
                           call skip_spaces()
                           arg3 = parse_expression()
                           if (eval_error) then
                              f = [bad_value]
                           else if (size(arg3) /= 1) then
                              print *, "Error: third argument must be scalar"
                              eval_error = .true.; f = [bad_value]
                           else
                              call skip_spaces()
                              if (curr_char /= ",") then
                                 print *, "Error: function needs four arguments"
                                 eval_error = .true.; f = [bad_value]
                              else
                                 call next_char()
                                 call skip_spaces()
                                 arg4 = parse_expression()
                                 if (eval_error) then
                                    f = [bad_value]
                                 else if (size(arg4) /= 1) then
                                    print *, "Error: fourth argument must be scalar"
                                    eval_error = .true.; f = [bad_value]
                                 else
                                    if (trim(id) == "dged") then
                                       f = dged(arg1, arg2(1), arg3(1), &
                                                arg4(1))
                                    else
                                       f = dhyperb(arg1, arg2(1), arg3(1), &
                                                   arg4(1))
                                    end if
                                    call skip_spaces()
                                    if (curr_char == ")") call next_char()
                                 end if
                              end if
                           end if
                        end if
                     end if

                  case ("dgamma", "dlnorm", "dnorm", "df", "dbeta", "dlogis", "dlaplace", "dcauchy")
                     if (.not. have_second) then
                        print *, "Error: function needs three arguments"
                        eval_error = .true.; f = [bad_value]
                     else if (size(arg2) /= 1) then
                        print *, "Error: second argument must be scalar"
                        eval_error = .true.; f = [bad_value]
                     else
                        call skip_spaces()
                        if (curr_char /= ",") then
                           print *, "Error: function needs three arguments"
                           eval_error = .true.; f = [bad_value]
                        else
                           call next_char()
                           call skip_spaces()
                           arg3 = parse_expression()
                           if (eval_error) then
                              f = [bad_value]
                           else if (size(arg3) /= 1) then
                              print *, "Error: third argument must be scalar"
                              eval_error = .true.; f = [bad_value]
                           else
                              select case (trim(id))
                              case ("dgamma"); f = dgamma(arg1, arg2(1), arg3(1))
                              case ("dlnorm"); f = dlnorm(arg1, arg2(1), arg3(1))
                              case ("dnorm"); f = dnorm(arg1, arg2(1), arg3(1))
                              case ("df"); f = df(arg1, arg2(1), arg3(1))
                              case ("dbeta"); f = dbeta(arg1, arg2(1), arg3(1))
                              case ("dlogis"); f = dlogis(arg1, arg2(1), arg3(1))
                              case ("dlaplace"); f = dlaplace(arg1, arg2(1), arg3(1))
                              case ("dcauchy"); f = dcauchy(arg1, arg2(1), arg3(1))
                              end select
                              call skip_spaces()
                              if (curr_char == ")") call next_char()
                           end if
                        end if
                     end if

                  case ("punif")
                     if (.not. have_second) then
                        f = punif(arg1)
                     else if (size(arg2) /= 1) then
                        print *, "Error: second argument must be scalar"
                        eval_error = .true.; f = [bad_value]
                     else
                        call skip_spaces()
                        if (curr_char == ",") then
                           call next_char()
                           call skip_spaces()
                           arg3 = parse_expression()
                           if (eval_error) then
                              f = [bad_value]
                           else if (size(arg3) /= 1) then
                              print *, "Error: third argument must be scalar"
                              eval_error = .true.; f = [bad_value]
                           else
                              f = punif(arg1, arg2(1), arg3(1))
                              call skip_spaces()
                              if (curr_char == ")") call next_char()
                           end if
                        else
                           f = punif(arg1, arg2(1))
                           call skip_spaces()
                           if (curr_char == ")") call next_char()
                        end if
                     end if

                  case ("pexp", "pt", "pchisq")
                     if (.not. have_second) then
                        print *, "Error: function needs two arguments"
                        eval_error = .true.; f = [bad_value]
                     else if (size(arg2) /= 1) then
                        print *, "Error: second argument must be scalar"
                        eval_error = .true.; f = [bad_value]
                     else
                        select case (trim(id))
                        case ("pexp"); f = pexp(arg1, arg2(1))
                        case ("pt"); f = pt(arg1, arg2(1))
                        case ("pchisq"); f = pchisq(arg1, arg2(1))
                        end select
                     end if

                  case ("pged", "phyperb")
                     if (.not. have_second) then
                        print *, "Error: function needs four arguments"
                        eval_error = .true.; f = [bad_value]
                     else if (size(arg2) /= 1) then
                        print *, "Error: second argument must be scalar"
                        eval_error = .true.; f = [bad_value]
                     else
                        call skip_spaces()
                        if (curr_char /= ",") then
                           print *, "Error: function needs four arguments"
                           eval_error = .true.; f = [bad_value]
                        else
                           call next_char()
                           call skip_spaces()
                           arg3 = parse_expression()
                           if (eval_error) then
                              f = [bad_value]
                           else if (size(arg3) /= 1) then
                              print *, "Error: third argument must be scalar"
                              eval_error = .true.; f = [bad_value]
                           else
                              call skip_spaces()
                              if (curr_char /= ",") then
                                 print *, "Error: function needs four arguments"
                                 eval_error = .true.; f = [bad_value]
                              else
                                 call next_char()
                                 call skip_spaces()
                                 arg4 = parse_expression()
                                 if (eval_error) then
                                    f = [bad_value]
                                 else if (size(arg4) /= 1) then
                                    print *, "Error: fourth argument must be scalar"
                                    eval_error = .true.; f = [bad_value]
                                 else
                                    if (trim(id) == "pged") then
                                       f = pged(arg1, arg2(1), arg3(1), &
                                                arg4(1))
                                    else
                                       f = phyperb(arg1, arg2(1), arg3(1), &
                                                   arg4(1))
                                    end if
                                    call skip_spaces()
                                    if (curr_char == ")") call next_char()
                                 end if
                              end if
                           end if
                        end if
                     end if

                  case ("pgamma", "plnorm", "pf", "pbeta", "plogis", "pnorm", "plaplace", "pcauchy")
                     if (.not. have_second) then
                        print *, "Error: function needs three arguments"
                        eval_error = .true.; f = [bad_value]
                     else if (size(arg2) /= 1) then
                        print *, "Error: second argument must be scalar"
                        eval_error = .true.; f = [bad_value]
                     else
                        call skip_spaces()
                        if (curr_char /= ",") then
                           print *, "Error: function needs three arguments"
                           eval_error = .true.; f = [bad_value]
                        else
                           call next_char()
                           call skip_spaces()
                           arg3 = parse_expression()
                           if (eval_error) then
                              f = [bad_value]
                           else if (size(arg3) /= 1) then
                              print *, "Error: third argument must be scalar"
                              eval_error = .true.; f = [bad_value]
                           else
                              select case (trim(id))
                              case ("pgamma"); f = pgamma(arg1, arg2(1), arg3(1))
                              case ("plnorm"); f = plnorm(arg1, arg2(1), arg3(1))
                              case ("pf"); f = pf(arg1, arg2(1), arg3(1))
                              case ("pbeta"); f = pbeta(arg1, arg2(1), arg3(1))
                              case ("plogis"); f = plogis(arg1, arg2(1), arg3(1))
                              case ("pnorm"); f = pnorm(arg1, arg2(1), arg3(1))
                              case ("plaplace"); f = plaplace(arg1, arg2(1), arg3(1))
                              case ("pcauchy"); f = pcauchy(arg1, arg2(1), arg3(1))
                              end select
                              call skip_spaces()
                              if (curr_char == ")") call next_char()
                           end if
                        end if
                     end if

                  case ("qunif")
                     if (.not. have_second) then
                        f = qunif(arg1)
                     else if (size(arg2) /= 1) then
                        print *, "Error: second argument must be scalar"
                        eval_error = .true.; f = [bad_value]
                     else
                        call skip_spaces()
                        if (curr_char == ",") then
                           call next_char()
                           call skip_spaces()
                           arg3 = parse_expression()
                           if (eval_error) then
                              f = [bad_value]
                           else if (size(arg3) /= 1) then
                              print *, "Error: third argument must be scalar"
                              eval_error = .true.; f = [bad_value]
                           else
                              f = qunif(arg1, arg2(1), arg3(1))
                              call skip_spaces()
                              if (curr_char == ")") call next_char()
                           end if
                        else
                           f = qunif(arg1, arg2(1))
                           call skip_spaces()
                           if (curr_char == ")") call next_char()
                        end if
                     end if

                  case ("qexp", "qt", "qchisq")
                     if (.not. have_second) then
                        print *, "Error: function needs two arguments"
                        eval_error = .true.; f = [bad_value]
                     else if (size(arg2) /= 1) then
                        print *, "Error: second argument must be scalar"
                        eval_error = .true.; f = [bad_value]
                     else
                        select case (trim(id))
                        case ("qexp"); f = qexp(arg1, arg2(1))
                        case ("qt"); f = qt(arg1, arg2(1))
                        case ("qchisq"); f = qchisq(arg1, arg2(1))
                        end select
                     end if

                  case ("qged", "qhyperb")
                     if (.not. have_second) then
                        print *, "Error: function needs four arguments"
                        eval_error = .true.; f = [bad_value]
                     else if (size(arg2) /= 1) then
                        print *, "Error: second argument must be scalar"
                        eval_error = .true.; f = [bad_value]
                     else
                        call skip_spaces()
                        if (curr_char /= ",") then
                           print *, "Error: function needs four arguments"
                           eval_error = .true.; f = [bad_value]
                        else
                           call next_char()
                           call skip_spaces()
                           arg3 = parse_expression()
                           if (eval_error) then
                              f = [bad_value]
                           else if (size(arg3) /= 1) then
                              print *, "Error: third argument must be scalar"
                              eval_error = .true.; f = [bad_value]
                           else
                              call skip_spaces()
                              if (curr_char /= ",") then
                                 print *, "Error: function needs four arguments"
                                 eval_error = .true.; f = [bad_value]
                              else
                                 call next_char()
                                 call skip_spaces()
                                 arg4 = parse_expression()
                                 if (eval_error) then
                                    f = [bad_value]
                                 else if (size(arg4) /= 1) then
                                    print *, "Error: fourth argument must be scalar"
                                    eval_error = .true.; f = [bad_value]
                                 else
                                    if (trim(id) == "qged") then
                                       f = qged(arg1, arg2(1), arg3(1), &
                                                arg4(1))
                                    else
                                       f = qhyperb(arg1, arg2(1), arg3(1), &
                                                   arg4(1))
                                    end if
                                    call skip_spaces()
                                    if (curr_char == ")") call next_char()
                                 end if
                              end if
                           end if
                        end if
                     end if

                  case ("qgamma", "qlnorm", "qf", "qbeta", "qlogis", "qnorm", "qlaplace", "qcauchy")
                     if (.not. have_second) then
                        print *, "Error: function needs three arguments"
                        eval_error = .true.; f = [bad_value]
                     else if (size(arg2) /= 1) then
                        print *, "Error: second argument must be scalar"
                        eval_error = .true.; f = [bad_value]
                     else
                        call skip_spaces()
                        if (curr_char /= ",") then
                           print *, "Error: function needs three arguments"
                           eval_error = .true.; f = [bad_value]
                        else
                           call next_char()
                           call skip_spaces()
                           arg3 = parse_expression()
                           if (eval_error) then
                              f = [bad_value]
                           else if (size(arg3) /= 1) then
                              print *, "Error: third argument must be scalar"
                              eval_error = .true.; f = [bad_value]
                           else
                              select case (trim(id))
                              case ("qgamma"); f = qgamma(arg1, arg2(1), arg3(1))
                              case ("qlnorm"); f = qlnorm(arg1, arg2(1), arg3(1))
                              case ("qf"); f = qf(arg1, arg2(1), arg3(1))
                              case ("qbeta"); f = qbeta(arg1, arg2(1), arg3(1))
                              case ("qlogis"); f = qlogis(arg1, arg2(1), arg3(1))
                              case ("qnorm"); f = qnorm(arg1, arg2(1), arg3(1))
                              case ("qlaplace"); f = qlaplace(arg1, arg2(1), arg3(1))
                              case ("qcauchy"); f = qcauchy(arg1, arg2(1), arg3(1))
                              end select
                              call skip_spaces()
                              if (curr_char == ")") call next_char()
                           end if
                        end if
                     end if

                  case ("mapacf")
                     if (.not. have_second) then
                        print *, "Error: function needs two arguments"
                        eval_error = .true.; f = [bad_value]
                     else if (size(arg2) /= 1) then
                        print *, "Error: second argument of mapacf() must be scalar"
                        eval_error = .true.; f = [bad_value]
                     else if (size(arg1) < 1) then
                        print *, "Error: first argument of mapacf() must be non-empty"
                        eval_error = .true.; f = [bad_value]
                     else
                        n1 = nint(arg2(1))
                        if (n1 < 1) then
                           print *, "Error: mapacf() lag count must be >= 1"
                           eval_error = .true.; f = [bad_value]
                        else
                           f = mapacf(arg1, n1)
                        end if
                     end if

                  case ("armaacf")
                     if (.not. have_second) then
                        print *, "Error: function needs three arguments"
                        eval_error = .true.; f = [bad_value]
                     else
                        call skip_spaces()
                        if (curr_char /= ",") then
                           print *, "Error: function needs three arguments"
                           eval_error = .true.; f = [bad_value]
                        else
                           call next_char()
                           call skip_spaces()
                           arg3 = parse_expression()
                           if (eval_error) then
                              f = [bad_value]
                           else
                              if (size(arg3) /= 1) then
                                 print *, "Error: third argument of armaacf() must be scalar"
                                 eval_error = .true.; f = [bad_value]
                              else
                                 n1 = nint(arg3(1))
                                 if (n1 < 1) then
                                    print *, "Error: armaacf() lag count must be >= 1"
                                    eval_error = .true.; f = [bad_value]
                                 else
                                    f = armaacf(arg1, arg2, n1)
                                 end if
                              end if
                              call skip_spaces()
                              if (curr_char == ")") call next_char()
                           end if
                        end if
                     end if

                  case ("arfimaacf")
                     if (.not. have_second) then
                        print *, "Error: function needs four arguments"
                        eval_error = .true.; f = [bad_value]
                     else
                        call skip_spaces()
                        if (curr_char /= ",") then
                           print *, "Error: function needs four arguments"
                           eval_error = .true.; f = [bad_value]
                        else
                           call next_char()
                           call skip_spaces()
                           arg3 = parse_expression()
                           if (eval_error) then
                              f = [bad_value]
                           else if (size(arg3) /= 1) then
                              print *, "Error: third argument of arfimaacf() must be scalar"
                              eval_error = .true.; f = [bad_value]
                           else
                              call skip_spaces()
                              if (curr_char /= ",") then
                                 print *, "Error: function needs four arguments"
                                 eval_error = .true.; f = [bad_value]
                              else
                                 call next_char()
                                 call skip_spaces()
                                 arg4 = parse_expression()
                                 if (eval_error) then
                                    f = [bad_value]
                                 else if (size(arg4) /= 1) then
                                    print *, "Error: fourth argument of arfimaacf() must be scalar"
                                    eval_error = .true.; f = [bad_value]
                                 else
                                    n1 = nint(arg4(1))
                                    if (n1 < 1) then
                                       print *, "Error: arfimaacf() lag count must be >= 1"
                                       eval_error = .true.; f = [bad_value]
                                    else if (abs(arg3(1)) >= 0.5_dp) then
                                       print *, "Error: arfimaacf() requires |d| < 0.5"
                                       eval_error = .true.; f = [bad_value]
                                    else
                                       f = arfimaacf(arg1, arg2, arg3(1), n1)
                                    end if
                                 end if
                                 call skip_spaces()
                                 if (curr_char == ")") call next_char()
                              end if
                           end if
                        end if
                     end if

                  case ("armapacf")
                     if (.not. have_second) then
                        print *, "Error: function needs three arguments"
                        eval_error = .true.; f = [bad_value]
                     else
                        call skip_spaces()
                        if (curr_char /= ",") then
                           print *, "Error: function needs three arguments"
                           eval_error = .true.; f = [bad_value]
                        else
                           call next_char()
                           call skip_spaces()
                           arg3 = parse_expression()
                           if (eval_error) then
                              f = [bad_value]
                           else
                              if (size(arg3) /= 1) then
                                 print *, "Error: third argument of armapacf() must be scalar"
                                 eval_error = .true.; f = [bad_value]
                              else
                                 n1 = nint(arg3(1))
                                 if (n1 < 1) then
                                    print *, "Error: armapacf() lag count must be >= 1"
                                    eval_error = .true.; f = [bad_value]
                                 else
                                    f = armapacf(arg1, arg2, n1)
                                 end if
                              end if
                              call skip_spaces()
                              if (curr_char == ")") call next_char()
                           end if
                        end if
                     end if

                  case ("rexp", "rt", "rchisq")
                     if (.not. have_second) then
                        if (trim(id) == "rexp") then
                           if (size(arg1) /= 1) then
                              print *, "Error: first argument must be scalar"
                              eval_error = .true.; f = [bad_value]
                           else
                              n1 = nint(arg1(1))
                              if (n1 < 1) then
                                 print *, "Error: length must be > 0"
                                 eval_error = .true.; f = [bad_value]
                              else
                                 f = rexp(n1, 1.0_dp)
                              end if
                           end if
                        else
                           print *, "Error: function needs two arguments"
                           eval_error = .true.; f = [bad_value]
                        end if
                     else if (size(arg1) /= 1) then
                        print *, "Error: first argument must be scalar"
                        eval_error = .true.; f = [bad_value]
                     else if (size(arg2) /= 1) then
                        print *, "Error: second argument must be scalar"
                        eval_error = .true.; f = [bad_value]
                     else
                        n1 = nint(arg1(1))
                        if (n1 < 1) then
                           print *, "Error: length must be > 0"
                           eval_error = .true.; f = [bad_value]
                        else if (arg2(1) <= 0.0_dp) then
                           print *, "Error: parameter must be > 0"
                           eval_error = .true.; f = [bad_value]
                        else
                           select case (trim(id))
                           case ("rexp"); f = rexp(n1, arg2(1))
                           case ("rt"); f = rt(n1, arg2(1))
                           case ("rchisq"); f = rchisq(n1, arg2(1))
                           end select
                           call skip_spaces()
                           if (curr_char == ")") call next_char()
                        end if
                     end if

                  case ("rhyperb")
                     if (.not. have_second) then
                        if (size(arg1) /= 1) then
                           print *, "Error: first argument must be scalar"
                           eval_error = .true.; f = [bad_value]
                        else
                           n1 = nint(arg1(1))
                           if (n1 < 1) then
                              print *, "Error: length must be > 0"
                              eval_error = .true.; f = [bad_value]
                           else
                              f = rhyperb(n1, 0.0_dp, 1.0_dp, 1.0_dp)
                           end if
                        end if
                     else if (size(arg1) /= 1) then
                        print *, "Error: first argument must be scalar"
                        eval_error = .true.; f = [bad_value]
                     else if (size(arg2) /= 1) then
                        print *, "Error: second argument must be scalar"
                        eval_error = .true.; f = [bad_value]
                     else
                        call skip_spaces()
                        if (curr_char == ",") then
                           call next_char()
                           call skip_spaces()
                           arg3 = parse_expression()
                           if (eval_error) then
                              f = [bad_value]
                           else if (size(arg3) /= 1) then
                              print *, "Error: third argument must be scalar"
                              eval_error = .true.; f = [bad_value]
                           else
                              call skip_spaces()
                              if (curr_char == ",") then
                                 call next_char()
                                 call skip_spaces()
                                 arg4 = parse_expression()
                                 if (eval_error) then
                                    f = [bad_value]
                                 else if (size(arg4) /= 1) then
                                    print *, "Error: fourth argument must be scalar"
                                    eval_error = .true.; f = [bad_value]
                                 else
                                    n1 = nint(arg1(1))
                                    if (n1 < 1) then
                                       print *, "Error: length must be > 0"
                                       eval_error = .true.; f = [bad_value]
                                    else if (arg3(1) <= 0.0_dp .or. arg4(1) <= 0.0_dp) then
                                       print *, "Error: scale and alpha must be > 0"
                                       eval_error = .true.; f = [bad_value]
                                    else
                                       f = rhyperb(n1, arg2(1), arg3(1), arg4(1))
                                    end if
                                 end if
                              else
                                 n1 = nint(arg1(1))
                                 if (n1 < 1) then
                                    print *, "Error: length must be > 0"
                                    eval_error = .true.; f = [bad_value]
                                 else if (arg3(1) <= 0.0_dp) then
                                    print *, "Error: scale must be > 0"
                                    eval_error = .true.; f = [bad_value]
                                 else
                                    f = rhyperb(n1, arg2(1), arg3(1), 1.0_dp)
                                 end if
                              end if
                              call skip_spaces()
                              if (curr_char == ")") call next_char()
                           end if
                        else
                           n1 = nint(arg1(1))
                           if (n1 < 1) then
                              print *, "Error: length must be > 0"
                              eval_error = .true.; f = [bad_value]
                           else
                              f = rhyperb(n1, arg2(1), 1.0_dp, 1.0_dp)
                           end if
                        end if
                     end if

                  case ("rged")
                     if (.not. have_second) then
                        if (size(arg1) /= 1) then
                           print *, "Error: first argument must be scalar"
                           eval_error = .true.; f = [bad_value]
                        else
                           n1 = nint(arg1(1))
                           if (n1 < 1) then
                              print *, "Error: length must be > 0"
                              eval_error = .true.; f = [bad_value]
                           else
                              f = rged(n1, 0.0_dp, 1.0_dp, 2.0_dp)
                           end if
                        end if
                     else if (size(arg1) /= 1) then
                        print *, "Error: first argument must be scalar"
                        eval_error = .true.; f = [bad_value]
                     else if (size(arg2) /= 1) then
                        print *, "Error: second argument must be scalar"
                        eval_error = .true.; f = [bad_value]
                     else
                        call skip_spaces()
                        if (curr_char == ",") then
                           call next_char()
                           call skip_spaces()
                           arg3 = parse_expression()
                           if (eval_error) then
                              f = [bad_value]
                           else if (size(arg3) /= 1) then
                              print *, "Error: third argument must be scalar"
                              eval_error = .true.; f = [bad_value]
                           else
                              call skip_spaces()
                              if (curr_char == ",") then
                                 call next_char()
                                 call skip_spaces()
                                 arg4 = parse_expression()
                                 if (eval_error) then
                                    f = [bad_value]
                                 else if (size(arg4) /= 1) then
                                    print *, "Error: fourth argument must be scalar"
                                    eval_error = .true.; f = [bad_value]
                                 else
                                    n1 = nint(arg1(1))
                                    if (n1 < 1) then
                                       print *, "Error: length must be > 0"
                                       eval_error = .true.; f = [bad_value]
                                    else if (arg3(1) <= 0.0_dp .or. &
                                             arg4(1) <= 0.0_dp) then
                                       print *, "Error: scale and beta must be > 0"
                                       eval_error = .true.; f = [bad_value]
                                    else
                                       f = rged(n1, arg2(1), arg3(1), &
                                                arg4(1))
                                    end if
                                 end if
                              else
                                 n1 = nint(arg1(1))
                                 if (n1 < 1) then
                                    print *, "Error: length must be > 0"
                                    eval_error = .true.; f = [bad_value]
                                 else if (arg3(1) <= 0.0_dp) then
                                    print *, "Error: scale must be > 0"
                                    eval_error = .true.; f = [bad_value]
                                 else
                                    f = rged(n1, arg2(1), arg3(1), 2.0_dp)
                                 end if
                              end if
                              call skip_spaces()
                              if (curr_char == ")") call next_char()
                           end if
                        else
                           n1 = nint(arg1(1))
                           if (n1 < 1) then
                              print *, "Error: length must be > 0"
                              eval_error = .true.; f = [bad_value]
                           else
                              f = rged(n1, arg2(1), 1.0_dp, 2.0_dp)
                           end if
                        end if
                     end if

                  case ("rgamma", "rlnorm", "rf", "rbeta", "rlogis", "rlaplace", "rcauchy")
                     if (.not. have_second) then
                        if (trim(id) == "rlnorm") then
                           if (size(arg1) /= 1) then
                              print *, "Error: first argument must be scalar"
                              eval_error = .true.; f = [bad_value]
                           else
                              n1 = nint(arg1(1))
                              if (n1 < 1) then
                                 print *, "Error: length must be > 0"
                                 eval_error = .true.; f = [bad_value]
                              else
                                 f = rlnorm(n1, 0.0_dp, 1.0_dp)
                              end if
                           end if
                        else if (trim(id) == "rlogis") then
                           if (size(arg1) /= 1) then
                              print *, "Error: first argument must be scalar"
                              eval_error = .true.; f = [bad_value]
                           else
                              n1 = nint(arg1(1))
                              if (n1 < 1) then
                                 print *, "Error: length must be > 0"
                                 eval_error = .true.; f = [bad_value]
                              else
                                 f = rlogis(n1, 0.0_dp, 1.0_dp)
                              end if
                           end if
                        else if (trim(id) == "rlaplace") then
                           if (size(arg1) /= 1) then
                              print *, "Error: first argument must be scalar"
                              eval_error = .true.; f = [bad_value]
                           else
                              n1 = nint(arg1(1))
                              if (n1 < 1) then
                                 print *, "Error: length must be > 0"
                                 eval_error = .true.; f = [bad_value]
                              else
                                 f = rlaplace(n1, 0.0_dp, 1.0_dp)
                              end if
                           end if
                        else if (trim(id) == "rcauchy") then
                           if (size(arg1) /= 1) then
                              print *, "Error: first argument must be scalar"
                              eval_error = .true.; f = [bad_value]
                           else
                              n1 = nint(arg1(1))
                              if (n1 < 1) then
                                 print *, "Error: length must be > 0"
                                 eval_error = .true.; f = [bad_value]
                              else
                                 f = rcauchy(n1, 0.0_dp, 1.0_dp)
                              end if
                           end if
                        else
                           print *, "Error: function needs three arguments"
                           eval_error = .true.; f = [bad_value]
                        end if
                     else if (size(arg1) /= 1) then
                        print *, "Error: first argument must be scalar"
                        eval_error = .true.; f = [bad_value]
                     else if (size(arg2) /= 1) then
                        print *, "Error: second argument must be scalar"
                        eval_error = .true.; f = [bad_value]
                     else
                        call skip_spaces()
                        if (curr_char == ",") then
                           call next_char()
                           call skip_spaces()
                           arg3 = parse_expression()
                           if (eval_error) then
                              f = [bad_value]
                           else if (size(arg3) /= 1) then
                              print *, "Error: third argument must be scalar"
                              eval_error = .true.; f = [bad_value]
                           else
                              n1 = nint(arg1(1))
                              if (n1 < 1) then
                                 print *, "Error: length must be > 0"
                                 eval_error = .true.; f = [bad_value]
                              else
                                 select case (trim(id))
                                 case ("rgamma")
                                    if (arg2(1) <= 0.0_dp .or. arg3(1) <= 0.0_dp) then
                                       print *, "Error: shape and scale must be > 0"
                                       eval_error = .true.; f = [bad_value]
                                    else
                                       f = rgamma(n1, arg2(1), arg3(1))
                                    end if
                                 case ("rlnorm")
                                    if (arg3(1) <= 0.0_dp) then
                                       print *, "Error: sdlog must be > 0"
                                       eval_error = .true.; f = [bad_value]
                                    else
                                       f = rlnorm(n1, arg2(1), arg3(1))
                                    end if
                                 case ("rf")
                                    if (arg2(1) <= 0.0_dp .or. arg3(1) <= 0.0_dp) then
                                       print *, "Error: df1 and df2 must be > 0"
                                       eval_error = .true.; f = [bad_value]
                                    else
                                       f = rf(n1, arg2(1), arg3(1))
                                    end if
                                 case ("rbeta")
                                    if (arg2(1) <= 0.0_dp .or. arg3(1) <= 0.0_dp) then
                                       print *, "Error: a and b must be > 0"
                                       eval_error = .true.; f = [bad_value]
                                    else
                                       f = rbeta(n1, arg2(1), arg3(1))
                                    end if
                                 case ("rlogis")
                                    if (arg3(1) <= 0.0_dp) then
                                       print *, "Error: scale must be > 0"
                                       eval_error = .true.; f = [bad_value]
                                    else
                                       f = rlogis(n1, arg2(1), arg3(1))
                                    end if
                                 case ("rlaplace")
                                    if (arg3(1) <= 0.0_dp) then
                                       print *, "Error: scale must be > 0"
                                       eval_error = .true.; f = [bad_value]
                                    else
                                       f = rlaplace(n1, arg2(1), arg3(1))
                                    end if
                                 case ("rcauchy")
                                    if (arg3(1) <= 0.0_dp) then
                                       print *, "Error: scale must be > 0"
                                       eval_error = .true.; f = [bad_value]
                                    else
                                       f = rcauchy(n1, arg2(1), arg3(1))
                                    end if
                                 end select
                              end if
                              call skip_spaces()
                              if (curr_char == ")") call next_char()
                           end if
                        else
                           n1 = nint(arg1(1))
                           if (n1 < 1) then
                              print *, "Error: length must be > 0"
                              eval_error = .true.; f = [bad_value]
                           else
                              select case (trim(id))
                              case ("rgamma")
                                 if (arg2(1) <= 0.0_dp) then
                                    print *, "Error: shape must be > 0"
                                    eval_error = .true.; f = [bad_value]
                                 else
                                    f = rgamma(n1, arg2(1), 1.0_dp)
                                 end if
                              case ("rlnorm")
                                 f = rlnorm(n1, arg2(1), 1.0_dp)
                              case ("rlogis")
                                 f = rlogis(n1, arg2(1), 1.0_dp)
                              case ("rlaplace")
                                 f = rlaplace(n1, arg2(1), 1.0_dp)
                              case ("rcauchy")
                                 f = rcauchy(n1, arg2(1), 1.0_dp)
                              case default
                                 print *, "Error: function needs three arguments"
                                 eval_error = .true.; f = [bad_value]
                              end select
                           end if
                           call skip_spaces()
                           if (curr_char == ")") call next_char()
                        end if
                     end if

                  case ("arsim")
                     if (.not. have_second) then
                        print *, "Error: function needs two arguments"
                        eval_error = .true.; f = [bad_value]
                     else if (size(arg1) /= 1) then
                        print *, "Error: first argument of arsim() must be scalar"
                        eval_error = .true.; f = [bad_value]
                     else
                        call split_by_comma(expr(pstart:pend - 1), n_args, labels)
                        if (size(arg2) == 1 .and. n_args >= 2) then
                           if (index(labels(2), "[") == 0) then
                              print *, "Error: second argument of arsim() must be an explicit 1D array"
                              eval_error = .true.; f = [bad_value]
                           end if
                        end if
                     end if
                     if (eval_error) then
                        f = [bad_value]
                        return
                     else if (size(arg2) < 1) then
                        print *, "Error: second argument of arsim() must be non-empty"
                        eval_error = .true.; f = [bad_value]
                     else
                        n1 = nint(arg1(1))
                        if (n1 < 1) then
                           print *, "Error: arsim() length must be > 0"
                           eval_error = .true.; f = [bad_value]
                        else
                           f = arsim(n1, arg2)
                        end if
                     end if

                  case ("masim")
                     if (.not. have_second) then
                        print *, "Error: function needs two arguments"
                        eval_error = .true.; f = [bad_value]
                     else if (size(arg1) /= 1) then
                        print *, "Error: first argument of masim() must be scalar"
                        eval_error = .true.; f = [bad_value]
                     else
                        call split_by_comma(expr(pstart:pend - 1), n_args, labels)
                        if (size(arg2) == 1 .and. n_args >= 2) then
                           if (index(labels(2), "[") == 0) then
                              print *, "Error: second argument of masim() must be an explicit 1D array"
                              eval_error = .true.; f = [bad_value]
                           end if
                        end if
                     end if
                     if (eval_error) then
                        f = [bad_value]
                        return
                     else if (size(arg2) < 1) then
                        print *, "Error: second argument of masim() must be non-empty"
                        eval_error = .true.; f = [bad_value]
                     else
                        n1 = nint(arg1(1))
                        if (n1 < 1) then
                           print *, "Error: masim() length must be > 0"
                           eval_error = .true.; f = [bad_value]
                        else
                           f = masim(n1, arg2)
                        end if
                     end if

                  case ("armasim")
                     if (.not. have_second) then
                        print *, "Error: function needs three arguments"
                        eval_error = .true.; f = [bad_value]
                     else if (size(arg1) /= 1) then
                        print *, "Error: first argument of armasim() must be scalar"
                        eval_error = .true.; f = [bad_value]
                     else
                        call skip_spaces()
                        if (curr_char /= ",") then
                           print *, "Error: function needs three arguments"
                           eval_error = .true.; f = [bad_value]
                        else
                           call next_char()
                           call skip_spaces()
                           arg3 = parse_expression()
                           if (eval_error) then
                              f = [bad_value]
                           else
                              call split_by_comma(expr(pstart:pend - 1), n_args, labels)
                              if (size(arg2) == 1 .and. n_args >= 2) then
                                 if (index(labels(2), "[") == 0) then
                                    print *, "Error: second argument of armasim() must be an explicit 1D array"
                                    eval_error = .true.; f = [bad_value]
                                 end if
                              end if
                              if (size(arg3) == 1 .and. n_args >= 3) then
                                 if (index(labels(3), "[") == 0) then
                                    print *, "Error: third argument of armasim() must be an explicit 1D array"
                                    eval_error = .true.; f = [bad_value]
                                 end if
                              end if
                              if (eval_error) then
                                 f = [bad_value]
                                 return
                              end if
                              if (size(arg2) < 1 .or. size(arg3) < 1) then
                                 print *, "Error: second and third arguments of armasim() must be non-empty"
                                 eval_error = .true.; f = [bad_value]
                              else
                                 n1 = nint(arg1(1))
                                 if (n1 < 1) then
                                    print *, "Error: armasim() length must be > 0"
                                    eval_error = .true.; f = [bad_value]
                                 else
                                    f = armasim(n1, arg2, arg3)
                                 end if
                              end if
                              call skip_spaces()
                              if (curr_char == ")") call next_char()
                           end if
                        end if
                     end if

                  case ("arfimasim")
                     block
                        integer :: n_sim, burn_sim, m_sim, eqpos
                        logical :: have_burn, have_m, have_phi, have_theta
                        real(kind=dp) :: d_sim
                        character(len=:), allocatable :: tok, ltok, rval
                        real(kind=dp), allocatable :: tmp(:), phi_sim(:), theta_sim(:)

                        call split_by_comma(expr(pstart:pend - 1), n_args, labels)
                        pos = pend + 1
                        if (pos > lenstr) then
                           curr_char = char(0)
                        else
                           curr_char = expr(pos:pos); pos = pos + 1
                        end if
                        if (n_args < 2) then
                           print *, "Error: function needs two arguments"
                           eval_error = .true.; f = [bad_value]; return
                        end if

                        if (size(arg1) /= 1) then
                           print *, "Error: first argument of arfimasim() must be scalar"
                           eval_error = .true.; f = [bad_value]; return
                        end if
                        n_sim = nint(arg1(1))
                        if (n_sim < 1) then
                           print *, "Error: arfimasim() length must be > 0"
                           eval_error = .true.; f = [bad_value]; return
                        end if

                        if (.not. have_second) then
                           print *, "Error: second argument of arfimasim() must be scalar"
                           eval_error = .true.; f = [bad_value]; return
                        else if (size(arg2) /= 1) then
                           print *, "Error: second argument of arfimasim() must be scalar"
                           eval_error = .true.; f = [bad_value]; return
                        end if
                        d_sim = arg2(1)
                        if (abs(d_sim) >= 0.5_dp) then
                           print *, "Error: arfimasim() requires |d| < 0.5"
                           eval_error = .true.; f = [bad_value]; return
                        end if

                        have_burn = .false.; have_m = .false.; have_phi = .false.; have_theta = .false.
                        phi_sim = [real(kind=dp) ::]
                        theta_sim = [real(kind=dp) ::]
                        do i_arg = 3, n_args
                           tok = adjustl(labels(i_arg))
                           if (len_trim(tok) > 0) then
                              if (tok(len_trim(tok):len_trim(tok)) == ")") tok = tok(:len_trim(tok) - 1)
                           end if
                           ltok = lower_str(tok)
                           eqpos = index(tok, "=")
                           if (eqpos == 0) then
                              if (.not. have_phi) then
                                 phi_sim = evaluate(tok)
                                 if (eval_error) then
                                    f = [bad_value]; return
                                 end if
                                 have_phi = .true.
                              else if (.not. have_theta) then
                                 theta_sim = evaluate(tok)
                                 if (eval_error) then
                                    f = [bad_value]; return
                                 end if
                                 have_theta = .true.
                              else if (.not. have_burn) then
                                 tmp = evaluate(tok)
                                 if (eval_error) then
                                    f = [bad_value]; return
                                 end if
                                 if (size(tmp) /= 1) then
                                    print *, "Error: positional burn argument must be scalar"
                                    eval_error = .true.; f = [bad_value]; return
                                 end if
                                 burn_sim = nint(tmp(1))
                                 have_burn = .true.
                              else if (.not. have_m) then
                                 tmp = evaluate(tok)
                                 if (eval_error) then
                                    f = [bad_value]; return
                                 end if
                                 if (size(tmp) /= 1) then
                                    print *, "Error: positional m argument must be scalar"
                                    eval_error = .true.; f = [bad_value]; return
                                 end if
                                 m_sim = nint(tmp(1))
                                 have_m = .true.
                              else
                                 print *, "Error: too many positional arguments for arfimasim()"
                                 eval_error = .true.; f = [bad_value]; return
                              end if
                           else if (index(ltok, "phi") == 1) then
                              if (have_phi) then
                                 print *, "Error: duplicate phi= argument"
                                 eval_error = .true.; f = [bad_value]; return
                              end if
                              rval = adjustl(tok(eqpos + 1:))
                              phi_sim = evaluate(rval)
                              if (eval_error) then
                                 f = [bad_value]; return
                              end if
                              have_phi = .true.
                           else if (index(ltok, "theta") == 1) then
                              if (have_theta) then
                                 print *, "Error: duplicate theta= argument"
                                 eval_error = .true.; f = [bad_value]; return
                              end if
                              rval = adjustl(tok(eqpos + 1:))
                              theta_sim = evaluate(rval)
                              if (eval_error) then
                                 f = [bad_value]; return
                              end if
                              have_theta = .true.
                           else if (index(ltok, "burn") == 1) then
                              if (have_burn) then
                                 print *, "Error: duplicate burn= argument"
                                 eval_error = .true.; f = [bad_value]; return
                              end if
                              rval = adjustl(tok(eqpos + 1:))
                              tmp = evaluate(rval)
                              if (eval_error) then
                                 f = [bad_value]; return
                              end if
                              if (size(tmp) /= 1) then
                                 print *, "Error: burn must be scalar"
                                 eval_error = .true.; f = [bad_value]; return
                              end if
                              burn_sim = nint(tmp(1))
                              have_burn = .true.
                           else if (index(ltok, "m") == 1) then
                              if (have_m) then
                                 print *, "Error: duplicate m= argument"
                                 eval_error = .true.; f = [bad_value]; return
                              end if
                              rval = adjustl(tok(eqpos + 1:))
                              tmp = evaluate(rval)
                              if (eval_error) then
                                 f = [bad_value]; return
                              end if
                              if (size(tmp) /= 1) then
                                 print *, "Error: m must be scalar"
                                 eval_error = .true.; f = [bad_value]; return
                              end if
                              m_sim = nint(tmp(1))
                              have_m = .true.
                           else
                              print *, "Error: arfimasim() optional args are phi/theta and burn/m"
                              eval_error = .true.; f = [bad_value]; return
                           end if
                        end do
                        if (have_burn .and. burn_sim < 0) then
                           print *, "Error: burn must be >= 0"
                           eval_error = .true.; f = [bad_value]; return
                        end if
                        if (have_m .and. m_sim < 0) then
                           print *, "Error: m must be >= 0"
                           eval_error = .true.; f = [bad_value]; return
                        end if
                        if (have_burn .and. have_m) then
                           f = arfimasim(n_sim, d_sim, phi_sim, theta_sim, burn=burn_sim, m=m_sim)
                        else if (have_burn) then
                           f = arfimasim(n_sim, d_sim, phi_sim, theta_sim, burn=burn_sim)
                        else if (have_m) then
                           f = arfimasim(n_sim, d_sim, phi_sim, theta_sim, m=m_sim)
                        else
                           f = arfimasim(n_sim, d_sim, phi_sim, theta_sim)
                        end if
                     end block

                  case ("armafit")
                     block
                        integer :: p, q, iter_arg, eqpos
                        logical :: have_p, have_q, have_iter
                        character(len=:), allocatable :: tok, ltok, rval
                        real(kind=dp), allocatable :: tmp(:)

                        call split_by_comma(expr(pstart:pend - 1), n_args, labels)
                        if (n_args < 3) then
                           print *, "Error: function needs three arguments"
                           eval_error = .true.; f = [bad_value]; return
                        end if
                        have_p = .false.; have_q = .false.; have_iter = .false.
                        iter_arg = 0
                        do i_arg = 2, n_args
                           tok = adjustl(labels(i_arg))
                           if (len_trim(tok) > 0) then
                              if (tok(len_trim(tok):len_trim(tok)) == ")") tok = tok(:len_trim(tok) - 1)
                           end if
                           ltok = lower_str(tok)
                           if (index(ltok, "iter") == 1) then
                              eqpos = index(tok, "=")
                              if (eqpos == 0) then
                                 print *, "Error: iter must be given as iter=..."
                                 eval_error = .true.; f = [bad_value]; return
                              end if
                              rval = adjustl(tok(eqpos + 1:))
                              tmp = evaluate(rval)
                              if (eval_error) then
                                 f = [bad_value]; return
                              end if
                              if (size(tmp) /= 1) then
                                 print *, "Error: iter must be scalar"
                                 eval_error = .true.; f = [bad_value]; return
                              end if
                              iter_arg = nint(tmp(1))
                              have_iter = .true.
                           else if (.not. have_p) then
                              tmp = evaluate(tok)
                              if (eval_error) then
                                 f = [bad_value]; return
                              end if
                              if (size(tmp) /= 1) then
                                 print *, "Error: second argument of armafit() must be scalar"
                                 eval_error = .true.; f = [bad_value]; return
                              end if
                              p = nint(tmp(1))
                              have_p = .true.
                           else if (.not. have_q) then
                              tmp = evaluate(tok)
                              if (eval_error) then
                                 f = [bad_value]; return
                              end if
                              if (size(tmp) /= 1) then
                                 print *, "Error: third argument of armafit() must be scalar"
                                 eval_error = .true.; f = [bad_value]; return
                              end if
                              q = nint(tmp(1))
                              have_q = .true.
                           else
                              print *, "Error: armafit() takes two orders plus iter=..."
                              eval_error = .true.; f = [bad_value]; return
                           end if
                        end do
                        if (.not. have_p .or. .not. have_q) then
                           print *, "Error: armafit() requires p and q"
                           eval_error = .true.; f = [bad_value]; return
                        end if
                        if (p < 0 .or. q < 0) then
                           print *, "Error: armafit() orders must be >= 0"
                           eval_error = .true.; f = [bad_value]; return
                        end if
                        if (have_iter .and. iter_arg < 1) then
                           print *, "Error: iter must be >= 1"
                           eval_error = .true.; f = [bad_value]; return
                        end if
                        if (have_iter) then
                           call armafit(arg1, p, q, niter=iter_arg)
                        else
                           call armafit(arg1, p, q)
                        end if
                        suppress_result = .true.
                        f = [real(kind=dp) ::]
                     end block

                  case ("arfimafit")
                     block
                        integer :: p, q, iter_arg, eqpos
                        logical :: have_p, have_q, have_iter
                        character(len=:), allocatable :: tok, ltok, rval
                        real(kind=dp), allocatable :: tmp(:)

                        call split_by_comma(expr(pstart:pend - 1), n_args, labels)
                        if (n_args < 3) then
                           print *, "Error: function needs three arguments"
                           eval_error = .true.; f = [bad_value]; return
                        end if
                        have_p = .false.; have_q = .false.; have_iter = .false.
                        iter_arg = 0
                        do i_arg = 2, n_args
                           tok = adjustl(labels(i_arg))
                           if (len_trim(tok) > 0) then
                              if (tok(len_trim(tok):len_trim(tok)) == ")") tok = tok(:len_trim(tok) - 1)
                           end if
                           ltok = lower_str(tok)
                           if (index(ltok, "iter") == 1) then
                              eqpos = index(tok, "=")
                              if (eqpos == 0) then
                                 print *, "Error: iter must be given as iter=..."
                                 eval_error = .true.; f = [bad_value]; return
                              end if
                              rval = adjustl(tok(eqpos + 1:))
                              tmp = evaluate(rval)
                              if (eval_error) then
                                 f = [bad_value]; return
                              end if
                              if (size(tmp) /= 1) then
                                 print *, "Error: iter must be scalar"
                                 eval_error = .true.; f = [bad_value]; return
                              end if
                              iter_arg = nint(tmp(1))
                              have_iter = .true.
                           else if (.not. have_p) then
                              tmp = evaluate(tok)
                              if (eval_error) then
                                 f = [bad_value]; return
                              end if
                              if (size(tmp) /= 1) then
                                 print *, "Error: second argument of arfimafit() must be scalar"
                                 eval_error = .true.; f = [bad_value]; return
                              end if
                              p = nint(tmp(1))
                              have_p = .true.
                           else if (.not. have_q) then
                              tmp = evaluate(tok)
                              if (eval_error) then
                                 f = [bad_value]; return
                              end if
                              if (size(tmp) /= 1) then
                                 print *, "Error: third argument of arfimafit() must be scalar"
                                 eval_error = .true.; f = [bad_value]; return
                              end if
                              q = nint(tmp(1))
                              have_q = .true.
                           else
                              print *, "Error: arfimafit() takes two orders plus iter=..."
                              eval_error = .true.; f = [bad_value]; return
                           end if
                        end do
                        if (.not. have_p .or. .not. have_q) then
                           print *, "Error: arfimafit() requires p and q"
                           eval_error = .true.; f = [bad_value]; return
                        end if
                        if (p < 0 .or. q < 0) then
                           print *, "Error: arfimafit() orders must be >= 0"
                           eval_error = .true.; f = [bad_value]; return
                        end if
                        if (have_iter .and. iter_arg < 1) then
                           print *, "Error: iter must be >= 1"
                           eval_error = .true.; f = [bad_value]; return
                        end if
                        if (have_iter) then
                           call arfimafit(arg1, p, q, niter=iter_arg)
                        else
                           call arfimafit(arg1, p, q)
                        end if
                        suppress_result = .true.
                        f = [real(kind=dp) ::]
                     end block

                  case ("armafitgrid")
                     block
                        integer :: p1, p2, q1, q2, iter_arg, eqpos
                        logical :: have_p1, have_p2, have_q1, have_q2, have_iter
                        character(len=:), allocatable :: tok, ltok, rval
                        real(kind=dp), allocatable :: tmp(:)

                        call split_by_comma(expr(pstart:pend - 1), n_args, labels)
                        if (n_args < 5) then
                           print *, "Error: function needs five arguments"
                           eval_error = .true.; f = [bad_value]; return
                        end if
                        have_p1 = .false.; have_p2 = .false.; have_q1 = .false.; have_q2 = .false.
                        have_iter = .false.; iter_arg = 0
                        do i_arg = 2, n_args
                           tok = adjustl(labels(i_arg))
                           ltok = lower_str(tok)
                           if (index(ltok, "iter") == 1) then
                              eqpos = index(tok, "=")
                              if (eqpos == 0) then
                                 print *, "Error: iter must be given as iter=..."
                                 eval_error = .true.; f = [bad_value]; return
                              end if
                              rval = adjustl(tok(eqpos + 1:))
                              tmp = evaluate(rval)
                              if (eval_error) then
                                 f = [bad_value]; return
                              end if
                              if (size(tmp) /= 1) then
                                 print *, "Error: iter must be scalar"
                                 eval_error = .true.; f = [bad_value]; return
                              end if
                              iter_arg = nint(tmp(1))
                              have_iter = .true.
                           else if (.not. have_p1) then
                              tmp = evaluate(tok)
                              if (eval_error) then
                                 f = [bad_value]; return
                              end if
                              if (size(tmp) /= 1) then
                                 print *, "Error: second argument of armafitgrid() must be scalar"
                                 eval_error = .true.; f = [bad_value]; return
                              end if
                              p1 = nint(tmp(1)); have_p1 = .true.
                           else if (.not. have_p2) then
                              tmp = evaluate(tok)
                              if (eval_error) then
                                 f = [bad_value]; return
                              end if
                              if (size(tmp) /= 1) then
                                 print *, "Error: third argument of armafitgrid() must be scalar"
                                 eval_error = .true.; f = [bad_value]; return
                              end if
                              p2 = nint(tmp(1)); have_p2 = .true.
                           else if (.not. have_q1) then
                              tmp = evaluate(tok)
                              if (eval_error) then
                                 f = [bad_value]; return
                              end if
                              if (size(tmp) /= 1) then
                                 print *, "Error: fourth argument of armafitgrid() must be scalar"
                                 eval_error = .true.; f = [bad_value]; return
                              end if
                              q1 = nint(tmp(1)); have_q1 = .true.
                           else if (.not. have_q2) then
                              tmp = evaluate(tok)
                              if (eval_error) then
                                 f = [bad_value]; return
                              end if
                              if (size(tmp) /= 1) then
                                 print *, "Error: fifth argument of armafitgrid() must be scalar"
                                 eval_error = .true.; f = [bad_value]; return
                              end if
                              q2 = nint(tmp(1)); have_q2 = .true.
                           else
                              print *, "Error: armafitgrid() takes four orders plus iter=..."
                              eval_error = .true.; f = [bad_value]; return
                           end if
                        end do
                        if (.not. have_p1 .or. .not. have_p2 .or. .not. have_q1 .or. .not. have_q2) then
                           print *, "Error: armafitgrid() requires p1,p2,q1,q2"
                           eval_error = .true.; f = [bad_value]; return
                        end if
                        if (p1 < 0 .or. p2 < p1 .or. q1 < 0 .or. q2 < q1) then
                           print *, "Error: armafitgrid() order ranges invalid"
                           eval_error = .true.; f = [bad_value]; return
                        end if
                        if (have_iter .and. iter_arg < 1) then
                           print *, "Error: iter must be >= 1"
                           eval_error = .true.; f = [bad_value]; return
                        end if
                        if (have_iter) then
                           call armafitgrid(arg1, p1, p2, q1, q2, niter=iter_arg)
                        else
                           call armafitgrid(arg1, p1, p2, q1, q2)
                        end if
                        suppress_result = .true.
                        f = [real(kind=dp) ::]
                     end block

                  case ("armafitaic")
                     block
                        integer :: pmax, qmax, iter_arg, eqpos
                        logical :: have_iter
                        character(len=:), allocatable :: tok, ltok, rval
                        real(kind=dp), allocatable :: tmp(:)
                        call split_by_comma(expr(pstart:pend - 1), n_args, labels)
                        pmax = 5; qmax = 5
                        have_iter = .false.; iter_arg = 0
                        if (n_args > 1) then
                           do i_arg = 2, n_args
                              tok = adjustl(labels(i_arg))
                              if (len_trim(tok) > 0) then
                                 if (tok(len_trim(tok):len_trim(tok)) == ")") tok = tok(:len_trim(tok) - 1)
                              end if
                              ltok = lower_str(tok)
                              if (index(ltok, "iter") == 1) then
                                 eqpos = index(tok, "=")
                                 if (eqpos == 0) then
                                    print *, "Error: iter must be given as iter=..."
                                    eval_error = .true.; f = [bad_value]; return
                                 end if
                                 rval = adjustl(tok(eqpos + 1:))
                                 tmp = evaluate(rval)
                                 if (eval_error) then
                                    f = [bad_value]; return
                                 end if
                                 if (size(tmp) /= 1) then
                                    print *, "Error: iter must be scalar"
                                    eval_error = .true.; f = [bad_value]; return
                                 end if
                                 iter_arg = nint(tmp(1))
                                 have_iter = .true.
                              else if (pmax == 5 .and. qmax == 5) then
                                 tmp = evaluate(tok)
                                 if (eval_error) then
                                    f = [bad_value]; return
                                 end if
                                 if (size(tmp) /= 1) then
                                    print *, "Error: second argument of armafitaic() must be scalar"
                                    eval_error = .true.; f = [bad_value]; return
                                 end if
                                 pmax = nint(tmp(1))
                              else if (qmax == 5) then
                                 tmp = evaluate(tok)
                                 if (eval_error) then
                                    f = [bad_value]; return
                                 end if
                                 if (size(tmp) /= 1) then
                                    print *, "Error: third argument of armafitaic() must be scalar"
                                    eval_error = .true.; f = [bad_value]; return
                                 end if
                                 qmax = nint(tmp(1))
                              else
                                 print *, "Error: armafitaic() takes up to two order args plus iter=..."
                                 eval_error = .true.; f = [bad_value]; return
                              end if
                           end do
                        end if
                        if (pmax < 0 .or. qmax < 0) then
                           print *, "Error: armafitaic() max orders must be >= 0"
                           eval_error = .true.; f = [bad_value]; return
                        end if
                        if (have_iter .and. iter_arg < 1) then
                           print *, "Error: iter must be >= 1"
                           eval_error = .true.; f = [bad_value]; return
                        end if
                        if (have_iter) then
                           call armafitaic(arg1, pmax, qmax, niter=iter_arg)
                        else
                           call armafitaic(arg1, pmax, qmax)
                        end if
                        suppress_result = .true.
                        f = [real(kind=dp) ::]
                     end block

                  case ("regress")
                     block
                        logical :: use_intcp
                        integer :: n_pred, eqpos
                        character(len=:), allocatable :: tok, ltok, rval
                        real(kind=dp), allocatable :: tmp(:)

                        call split_by_comma(expr(pstart:pend - 1), n_args, labels)
                        pos = pend + 1
                        if (pos > lenstr) then
                           curr_char = char(0)
                        else
                           curr_char = expr(pos:pos); pos = pos + 1
                        end if
                        if (n_args < 2 .or. .not. have_second) then
                           print *, "Error: function needs two arguments"
                           eval_error = .true.; f = [bad_value]
                           return
                        end if
                        use_intcp = .true.
                        allocate (args(n_args - 1))
                        n_pred = 0
                        do i_arg = 2, n_args
                           tok = adjustl(labels(i_arg))
                           ltok = lower_str(tok)
                           if (index(ltok, "intcp") == 1) then
                              eqpos = index(tok, "=")
                              if (eqpos == 0) then
                                 print *, "Error: intcp must be given as intcp=..."
                                 eval_error = .true.; f = [bad_value]; return
                              end if
                              rval = adjustl(tok(eqpos + 1:))
                              rval = lower_str(rval)
                              if (rval == ".false." .or. rval == "false" .or. rval == "f") then
                                 use_intcp = .false.
                              else if (rval == ".true." .or. rval == "true" .or. rval == "t") then
                                 use_intcp = .true.
                              else
                                 tmp = evaluate(rval)
                                 if (eval_error) then
                                    f = [bad_value]; return
                                 end if
                                 if (size(tmp) /= 1) then
                                    print *, "Error: intcp must be scalar"
                                    eval_error = .true.; f = [bad_value]; return
                                 end if
                                 use_intcp = (tmp(1) /= 0.0_dp)
                              end if
                           else
                              tmp = evaluate(tok)
                              if (eval_error) then
                                 f = [bad_value]; return
                              end if
                              if (size(tmp) < 1) then
                                 print *, "Error: predictor must be non-empty"
                                 eval_error = .true.; f = [bad_value]; return
                              end if
                              n_pred = n_pred + 1
                              args(n_pred)%v = tmp
                           end if
                        end do
                        if (eval_error) return
                        if (n_pred < 1) then
                           print *, "Error: regress() needs at least one predictor"
                           eval_error = .true.; f = [bad_value]; return
                        end if
                        if (allocated(pred_labels)) deallocate(pred_labels)
                        allocate (character(len=len(labels(1))) :: pred_labels(n_pred))
                        n_pred = 0
                        do i_arg = 2, n_args
                           tok = adjustl(labels(i_arg))
                           ltok = lower_str(tok)
                           if (index(ltok, "intcp") == 1) cycle
                           n_pred = n_pred + 1
                           pred_labels(n_pred) = tok
                        end do

                        n1 = size(arg1)
                        if (n1 < 2) then
                           print *, "Error: function array arguments must have sizes > 1"
                           eval_error = .true.; f = [bad_value]; return
                        end if
                        do i_arg = 1, size(pred_labels)
                           if (size(args(i_arg)%v) /= n1) then
                              print "(a,i0,1x,i0,a)", "Error: function array arguments have sizes ", &
                                 n1, size(args(i_arg)%v), " must be equal"
                              eval_error = .true.; f = [bad_value]; return
                           end if
                        end do
                        if (eval_error) return

                        if (size(pred_labels) == 1) then
                           call regress(arg1, args(1)%v, intcp=use_intcp)
                        else
                           allocate (xmat(n1, size(pred_labels)))
                           do i_arg = 1, size(pred_labels)
                              xmat(:, i_arg) = args(i_arg)%v
                           end do
                           call regress_multi(arg1, xmat, pred_labels, intcp=use_intcp)
                        end if
                       suppress_result = .true.
                       f = [real(kind=dp) ::]
                     end block

                  case ("arfit")
                     block
                        logical :: have_k1, have_k2, have_acf, have_lb
                        integer :: acf_lags, lb_lags, eqpos
                        character(len=:), allocatable :: tok, ltok, rval
                        real(kind=dp), allocatable :: tmp(:)

                        call split_by_comma(expr(pstart:pend - 1), n_args, labels)
                        if (n_args < 2) then
                           print *, "Error: function needs two arguments"
                           eval_error = .true.; f = [bad_value]; return
                        end if
                        have_k1 = .false.
                        have_k2 = .false.
                        have_acf = .false.
                        have_lb = .false.
                        acf_lags = 0
                        lb_lags = 0
                        do i_arg = 2, n_args
                           tok = adjustl(labels(i_arg))
                           ltok = lower_str(tok)
                           if (index(ltok, "acf") == 1) then
                              eqpos = index(tok, "=")
                              if (eqpos == 0) then
                                 print *, "Error: acf must be given as acf=..."
                                 eval_error = .true.; f = [bad_value]; return
                              end if
                              rval = adjustl(tok(eqpos + 1:))
                              tmp = evaluate(rval)
                              if (eval_error) then
                                 f = [bad_value]; return
                              end if
                              if (size(tmp) /= 1) then
                                 print *, "Error: acf must be scalar"
                                 eval_error = .true.; f = [bad_value]; return
                              end if
                              acf_lags = nint(tmp(1))
                              have_acf = .true.
                           else if (index(ltok, "lb") == 1) then
                              eqpos = index(tok, "=")
                              if (eqpos == 0) then
                                 print *, "Error: lb must be given as lb=..."
                                 eval_error = .true.; f = [bad_value]; return
                              end if
                              rval = adjustl(tok(eqpos + 1:))
                              tmp = evaluate(rval)
                              if (eval_error) then
                                 f = [bad_value]; return
                              end if
                              if (size(tmp) /= 1) then
                                 print *, "Error: lb must be scalar"
                                 eval_error = .true.; f = [bad_value]; return
                              end if
                              lb_lags = nint(tmp(1))
                              have_lb = .true.
                           else if (.not. have_k1) then
                              tmp = evaluate(tok)
                              if (eval_error) then
                                 f = [bad_value]; return
                              end if
                              if (size(tmp) /= 1) then
                                 print *, "Error: second argument of arfit() must be scalar"
                                 eval_error = .true.; f = [bad_value]; return
                              end if
                              n1 = nint(tmp(1))
                              have_k1 = .true.
                           else if (.not. have_k2) then
                              tmp = evaluate(tok)
                              if (eval_error) then
                                 f = [bad_value]; return
                              end if
                              if (size(tmp) /= 1) then
                                 print *, "Error: third argument of arfit() must be scalar"
                                 eval_error = .true.; f = [bad_value]; return
                              end if
                              n2 = nint(tmp(1))
                              have_k2 = .true.
                           else
                              print *, "Error: arfit() takes at most 3 arguments plus acf=..."
                              eval_error = .true.; f = [bad_value]; return
                           end if
                        end do
                        if (.not. have_k1) then
                           print *, "Error: arfit() requires a lag order"
                           eval_error = .true.; f = [bad_value]; return
                        end if
                        if (n1 < 0 .or. (have_k2 .and. n2 < 0) .or. acf_lags < 0 .or. lb_lags < 0) then
                           print *, "Error: arfit() lag order must be >= 0"
                           eval_error = .true.; f = [bad_value]; return
                        end if
                        if (have_k2) then
                           if (have_acf .and. have_lb) then
                              call arfit(arg1, n1, n2, nacf=acf_lags, nlb=lb_lags)
                           else if (have_acf) then
                              call arfit(arg1, n1, n2, nacf=acf_lags)
                           else if (have_lb) then
                              call arfit(arg1, n1, n2, nlb=lb_lags)
                           else
                              call arfit(arg1, n1, n2)
                           end if
                        else
                           if (have_acf .and. have_lb) then
                              call arfit(arg1, n1, nacf=acf_lags, nlb=lb_lags)
                           else if (have_acf) then
                              call arfit(arg1, n1, nacf=acf_lags)
                           else if (have_lb) then
                              call arfit(arg1, n1, nlb=lb_lags)
                           else
                              call arfit(arg1, n1)
                           end if
                        end if
                        suppress_result = .true.
                        f = [real(kind=dp) ::]
                     end block

                  case ("mafit")
                     block
                        logical :: have_k1, have_k2, have_acf, have_lb, have_iter
                        integer :: acf_lags, lb_lags, iter_arg, eqpos
                        character(len=:), allocatable :: tok, ltok, rval
                        real(kind=dp), allocatable :: tmp(:)

                        call split_by_comma(expr(pstart:pend - 1), n_args, labels)
                        if (n_args < 2) then
                           print *, "Error: function needs two arguments"
                           eval_error = .true.; f = [bad_value]; return
                        end if
                        have_k1 = .false.
                        have_k2 = .false.
                        have_acf = .false.
                        have_lb = .false.
                        have_iter = .false.
                        acf_lags = 0
                        lb_lags = 0
                        iter_arg = 0
                        do i_arg = 2, n_args
                           tok = adjustl(labels(i_arg))
                           ltok = lower_str(tok)
                           if (index(ltok, "acf") == 1) then
                              eqpos = index(tok, "=")
                              if (eqpos == 0) then
                                 print *, "Error: acf must be given as acf=..."
                                 eval_error = .true.; f = [bad_value]; return
                              end if
                              rval = adjustl(tok(eqpos + 1:))
                              tmp = evaluate(rval)
                              if (eval_error) then
                                 f = [bad_value]; return
                              end if
                              if (size(tmp) /= 1) then
                                 print *, "Error: acf must be scalar"
                                 eval_error = .true.; f = [bad_value]; return
                              end if
                              acf_lags = nint(tmp(1))
                              have_acf = .true.
                           else if (index(ltok, "lb") == 1) then
                              eqpos = index(tok, "=")
                              if (eqpos == 0) then
                                 print *, "Error: lb must be given as lb=..."
                                 eval_error = .true.; f = [bad_value]; return
                              end if
                              rval = adjustl(tok(eqpos + 1:))
                              tmp = evaluate(rval)
                              if (eval_error) then
                                 f = [bad_value]; return
                              end if
                              if (size(tmp) /= 1) then
                                 print *, "Error: lb must be scalar"
                                 eval_error = .true.; f = [bad_value]; return
                              end if
                              lb_lags = nint(tmp(1))
                              have_lb = .true.
                           else if (index(ltok, "iter") == 1) then
                              eqpos = index(tok, "=")
                              if (eqpos == 0) then
                                 print *, "Error: iter must be given as iter=..."
                                 eval_error = .true.; f = [bad_value]; return
                              end if
                              rval = adjustl(tok(eqpos + 1:))
                              tmp = evaluate(rval)
                              if (eval_error) then
                                 f = [bad_value]; return
                              end if
                              if (size(tmp) /= 1) then
                                 print *, "Error: iter must be scalar"
                                 eval_error = .true.; f = [bad_value]; return
                              end if
                              iter_arg = nint(tmp(1))
                              have_iter = .true.
                           else if (.not. have_k1) then
                              tmp = evaluate(tok)
                              if (eval_error) then
                                 f = [bad_value]; return
                              end if
                              if (size(tmp) /= 1) then
                                 print *, "Error: second argument of mafit() must be scalar"
                                 eval_error = .true.; f = [bad_value]; return
                              end if
                              n1 = nint(tmp(1))
                              have_k1 = .true.
                           else if (.not. have_k2) then
                              tmp = evaluate(tok)
                              if (eval_error) then
                                 f = [bad_value]; return
                              end if
                              if (size(tmp) /= 1) then
                                 print *, "Error: third argument of mafit() must be scalar"
                                 eval_error = .true.; f = [bad_value]; return
                              end if
                              n2 = nint(tmp(1))
                              have_k2 = .true.
                           else
                              print *, "Error: mafit() takes at most 3 arguments plus acf=..."
                              eval_error = .true.; f = [bad_value]; return
                           end if
                        end do
                        if (.not. have_k1) then
                           print *, "Error: mafit() requires a lag order"
                           eval_error = .true.; f = [bad_value]; return
                        end if
                        if (n1 < 0 .or. (have_k2 .and. n2 < 0) .or. acf_lags < 0 .or. lb_lags < 0) then
                           print *, "Error: mafit() lag order must be >= 0"
                           eval_error = .true.; f = [bad_value]; return
                        end if
                        if (have_iter .and. iter_arg < 1) then
                           print *, "Error: iter must be >= 1"
                           eval_error = .true.; f = [bad_value]; return
                        end if
                        if (have_k2) then
                           if (have_acf .and. have_lb .and. have_iter) then
                              call mafit(arg1, n1, n2, nacf=acf_lags, nlb=lb_lags, niter=iter_arg)
                           else if (have_acf .and. have_lb) then
                              call mafit(arg1, n1, n2, nacf=acf_lags, nlb=lb_lags)
                           else if (have_acf .and. have_iter) then
                              call mafit(arg1, n1, n2, nacf=acf_lags, niter=iter_arg)
                           else if (have_lb .and. have_iter) then
                              call mafit(arg1, n1, n2, nlb=lb_lags, niter=iter_arg)
                           else if (have_acf) then
                              call mafit(arg1, n1, n2, nacf=acf_lags)
                           else if (have_lb) then
                              call mafit(arg1, n1, n2, nlb=lb_lags)
                           else if (have_iter) then
                              call mafit(arg1, n1, n2, niter=iter_arg)
                           else
                              call mafit(arg1, n1, n2)
                           end if
                        else
                           if (have_acf .and. have_lb .and. have_iter) then
                              call mafit(arg1, n1, nacf=acf_lags, nlb=lb_lags, niter=iter_arg)
                           else if (have_acf .and. have_lb) then
                              call mafit(arg1, n1, nacf=acf_lags, nlb=lb_lags)
                           else if (have_acf .and. have_iter) then
                              call mafit(arg1, n1, nacf=acf_lags, niter=iter_arg)
                           else if (have_lb .and. have_iter) then
                              call mafit(arg1, n1, nlb=lb_lags, niter=iter_arg)
                           else if (have_acf) then
                              call mafit(arg1, n1, nacf=acf_lags)
                           else if (have_lb) then
                              call mafit(arg1, n1, nlb=lb_lags)
                           else if (have_iter) then
                              call mafit(arg1, n1, niter=iter_arg)
                           else
                              call mafit(arg1, n1)
                           end if
                        end if
                        suppress_result = .true.
                        f = [real(kind=dp) ::]
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

                  case ("head", "tail")
                     if (.not. have_second) then
                        if (trim(id) == "head") then
                           f = head(arg1)
                        else
                           f = tail(arg1)
                        end if
                     else if (size(arg2) /= 1) then
                        print *, "Error: second argument of ", trim(id), "() must be scalar"
                        eval_error = .true.; f = [bad_value]
                     else
                        nsize = nint(arg2(1))
                        if (trim(id) == "head") then
                           f = head(arg1, nsize)
                        else
                           f = tail(arg1, nsize)
                        end if
                     end if

                  case ("runif", "rnorm", "rsech", "arange", "zeros", "ones") ! one-arg
                     if (have_second) then
                        print *, "Error: function takes one argument"
                        eval_error = .true.; f = [bad_value]
                     else
                        nsize = nint(arg1(1))
                        select case (id)
                        case ("runif"); f = runif(nsize)
                        case ("rnorm"); f = random_normal(nsize)
                        case ("rsech"); f = rsech(nsize)
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
                        "unique", "stdz", "reverse", "median", "mssk", "fit_norm", "fit_exp", "fit_gamma", "fit_lnorm", "fit_t", "fit_chisq", "fit_f", "fit_beta", "fit_logis", "fit_sech", "fit_laplace", "fit_cauchy", "fit_ged", "fit_hyperb", "dsech", "psech", "qsech", "bessel_j0", "bessel_j1", &
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

      do vi = 1, n_vars
         if (vars(vi)%name == name) then
            if (vars(vi)%is_const) then
               print *, "Error: cannot modify const variable '", trim(name), "'"
               eval_error = .true.
               return
            end if
            exit
         end if
      end do

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
      character(len=:), allocatable :: parts(:), rest, trimmed_line, tail, adj_line, part_eval, line_eval
      character(len=:), allocatable :: names(:)
      logical, allocatable   :: suppress(:)
      real(dp), allocatable   :: r(:), tmp(:)
      integer, allocatable   :: rint(:)
      integer                       :: p, repeat_count
      integer                       :: prev_loop_exec_base
      logical :: print_array_as_int, run_then, had_error, in_quote, comment_only, consumed_loop_line, ok_for, prev_exec
      character(len=*), parameter :: fmt_real_array = '("[",*(i0,:,", "))'
      character(len=:), allocatable :: lhs, rhs, rhs_tail
      integer :: p_lpar, p_rpar, depth, len_adj, comment_pos, i_c
      integer :: n_names
      character(len=:), allocatable :: cond_txt, then_txt, low_adj
      line_eval = line
      in_quote = .false.
      comment_pos = 0
      comment_only = .false.
      do i_c = 1, len_trim(line)
         if (line(i_c:i_c) /= " ") then
            if (line(i_c:i_c) == comment_char) comment_only = .true.
            exit
         end if
      end do
      do i_c = 1, len_trim(line_eval)
         if (line_eval(i_c:i_c) == '"') in_quote = .not. in_quote
         if (.not. in_quote .and. line_eval(i_c:i_c) == comment_char) then
            comment_pos = i_c
            exit
         end if
      end do
      if (comment_pos > 0) then
         if (comment_pos == 1) then
            line_eval = ""
            comment_only = .true.
         else
            line_eval = line_eval(1:comment_pos - 1)
            if (len_trim(line_eval) == 0) comment_only = .true.
         end if
      end if

      adj_line = adjustl(line_eval)
      len_adj = len_trim(adj_line)
      line_cp = line
      had_error = .false.
      if (if_collecting) then
         if (len_trim(line_eval) > 0) then
            if (len_trim(if_collect_body) + len_trim(line_eval) + 1 > len(if_collect_body)) then
               print *, "Error: IF block too large"
               eval_error = .true.
               goto 9000
            end if
            if_collect_body = trim(if_collect_body)//trim(line_eval)//new_line("a")
            if (is_block_if_start_line(adj_line)) if_collect_depth = if_collect_depth + 1
            if (is_end_if_line(adj_line)) if_collect_depth = if_collect_depth - 1
            if (if_collect_depth < 0) then
               print *, "Error: unmatched END IF"
               eval_error = .true.
               if_collecting = .false.
               if_collect_depth = 0
               if_collect_body = ""
               goto 9000
            end if
            if (if_collect_depth == 0) then
               if_collecting = .false.
               call execute_if_block(if_collect_body)
               if_collect_body = ""
            end if
         end if
         goto 9000
      end if
      if (len_trim(line_eval) == 0) goto 9000
      if (loop_depth > 0 .and. .not. in_loop_execute) then
         call collect_loop_definition_line(line_eval, had_error, consumed_loop_line)
         if (had_error .or. consumed_loop_line) goto 9000
      end if
      if (adj_line == "compiler_version()") then
         print "(a)", trim(compiler_version())
         goto 9000
      else if (adj_line == "compiler_info()") then
         print "(a)", trim(compiler_version())
         print "(a)", trim(compiler_options())
         goto 9000
      else if (index(adj_line, "exit") == 1 .and. .not. (loop_depth > 0 .and. .not. in_loop_execute)) then
         block
            character(len=:), allocatable :: exarg
            integer :: d
            if (len_trim(adj_line) > 4) then
               exarg = adjustl(adj_line(5:))
            else
               exarg = ""
            end if
            if (loop_depth == 0) then
               print *, "Error: exit used outside loop"
               had_error = .true.
               goto 9000
            end if
            if (len_trim(exarg) == 0) then
               exit_target_depth = loop_depth
               exit_loop = .true.
               goto 9000
            end if
            if (.not. is_alnum_string(exarg)) then
               print *, "Error: exit expects loop variable name, e.g. exit i"
               had_error = .true.
               goto 9000
            end if
            do d = loop_depth, 1, -1
               if (trim(loop_var(d)) == trim(exarg)) then
                  exit_target_depth = d
                  exit_loop = .true.
                  goto 9000
               end if
            end do
            print *, "Error: no active loop with variable '", trim(exarg), "'"
            had_error = .true.
            goto 9000
         end block
      else if (index(adj_line, "cycle") == 1 .and. .not. (loop_depth > 0 .and. .not. in_loop_execute)) then
         block
            character(len=:), allocatable :: cyarg
            integer :: d
            if (len_trim(adj_line) > 5) then
               cyarg = adjustl(adj_line(6:))
            else
               cyarg = ""
            end if
            if (loop_depth == 0) then
               print *, "Error: cycle used outside loop"
               had_error = .true.
               goto 9000
            end if
            if (len_trim(cyarg) == 0) then
               cycle_target_depth = loop_depth
               cycle_loop = .true.
               goto 9000
            end if
            if (.not. is_alnum_string(cyarg)) then
               print *, "Error: cycle expects loop variable name, e.g. cycle i"
               had_error = .true.
               goto 9000
            end if
            do d = loop_depth, 1, -1
               if (trim(loop_var(d)) == trim(cyarg)) then
                  cycle_target_depth = d
                  cycle_loop = .true.
                  goto 9000
               end if
            end do
            print *, "Error: no active loop with variable '", trim(cyarg), "'"
            had_error = .true.
            goto 9000
         end block
      else if (adj_line == "print") then
         print*
         goto 9000
      else if (len_adj > 2) then
         if (adj_line(1:1) == '"' .and. adj_line(len_adj:len_adj) == '"') then
            ! if a line just contains a quoted non-empty string, print it after a blank line
            print "(/,a)",adj_line(2:len_adj-1)
            goto 9000
         end if
      end if

      if (in_loop_execute .or. loop_depth > 0) then
         if (index(adj_line, "const") > 0) then
            print *, "Error: const not allowed inside loops or blocks"
            had_error = .true.
            goto 9000
         end if
      end if

      if (len_trim(line_eval) >= 2) then
         if (line_eval(1:1) == "*") then
            ! find first space after the count
            p = index(line_eval(2:), " ")
            if (p > 0) then
               ! parse the count expression between column 2 and p
               tmp = evaluate(line_eval(2:p))     ! e.g. line_eval(2:p) == "n" or "10"
               if (eval_error) then
                  had_error = .true.
                  goto 9000
               end if
               if (size(tmp) == 1) then
                  repeat_count = int(tmp(1))
                  rest = line_eval(p + 1:)           ! the code to repeat
                  block
                     logical :: prev_write
                     prev_write = write_code
                     write_code = .false.
                     do i = 1, repeat_count
                        call eval_print(rest)         ! recursive call; will split again
                     end do
                     write_code = prev_write
                  end block
                  goto 9000                         ! done with this line
               end if
            end if
         end if
      end if

      if (loop_depth > loop_exec_base_depth .and. in_loop_execute) then
         block
            character(len=:), allocatable :: tl, for_lhs, for_rhs, for_tail, do_lhs, do_start, do_end, do_step, do_tail
            logical :: is_for_header, is_do_header
            tl = adjustl(line_eval)
            is_for_header = .false.
            is_do_header = .false.
            if (index(lower_str(tl), "for ") == 1) then
               call parse_for_header(tl, for_lhs, for_rhs, for_tail, is_for_header)
               if (is_for_header .and. len_trim(for_tail) > 0) is_for_header = .false.
            end if
            if (index(lower_str(tl), "do ") == 1) then
               call parse_do_header(tl, do_lhs, do_start, do_end, do_step, do_tail, is_do_header)
               if (is_do_header .and. len_trim(do_tail) > 0) is_do_header = .false.
            end if
            if (is_for_header .or. is_end_for_line(tl) .or. is_do_header .or. &
                trim(tl) == "do" .or. trim(tl) == "end do" .or. trim(tl) == "enddo") then
               ! fall through into normal do/end-do handlers
            else
               if (index(tl, "const") > 0) then
                  print *, "Error: const not allowed inside loops or blocks"
                  had_error = .true.
                  goto 9000
               end if
               loop_body(loop_depth) = trim(loop_body(loop_depth))//trim(line_eval)//new_line("a")
               goto 9000
            end if
         end block
      end if

      ! ─── run("file") : execute the contents of a text file ───
      low_adj = lower_str(adj_line)
      if (is_block_if_start_line(adj_line)) then
         if_collecting = .true.
         if_collect_depth = 1
         if_collect_body = trim(line_eval)//new_line("a")
         goto 9000
      end if
      if (is_else_if_line(adj_line) .or. is_else_line(low_adj) .or. is_end_if_line(low_adj)) then
         print *, "Error: IF/ELSE branch without matching block IF"
         had_error = .true.
         goto 9000
      end if

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
              had_error = .true.
           end if
           goto 9000
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

            if (trim(lower_str(then_txt)) /= "then" .and. trim(lower_str(then_txt)) /= "then;") then
               tmp = evaluate(cond_txt)
               if (eval_error) had_error = .true.
               if (.not. eval_error .and. size(tmp) == 1) then
                  if (tmp(1) /= 0.0_dp) call eval_print(then_txt)
               end if
               goto 9000
            end if
         end if
      end if


!─────────────────────────────
!  Loop handling
!─────────────────────────────
      select case (adjustl(line_eval))
      case ("end do", "enddo", "enddo;", "end do;", "end for", "endfor", "endfor;", "end for;")
         if (loop_depth == 0) then
            print *, "Error: loop end without matching loop start"
            had_error = .true.
            goto 9000
         end if

         if (loop_is_for(loop_depth)) then
            tmp = evaluate(loop_for_expr(loop_depth))
            if (eval_error) then
               had_error = .true.
               goto 9000
            end if
            do ivar = 1, size(tmp)
               call set_variable(loop_var(loop_depth), [tmp(ivar)])
               call run_loop_body(loop_body(loop_depth))
               if (exit_loop) then
                  exit
               end if
               if (cycle_loop) then
                  if (cycle_target_depth < loop_depth) then
                     exit
                  else if (cycle_target_depth == loop_depth) then
                     cycle_loop = .false.
                     cycle_target_depth = 0
                     cycle
                  end if
               end if
            end do
         else if (loop_is_unbounded(loop_depth)) then
            do
               call run_loop_body(loop_body(loop_depth))
               if (exit_loop) exit
               if (cycle_loop) then
                  if (cycle_target_depth < loop_depth) then
                     exit
                  else if (cycle_target_depth == loop_depth) then
                     cycle_loop = .false.
                     cycle_target_depth = 0
                     cycle
                  end if
               end if
            end do
         else
            do ivar = loop_start(loop_depth), loop_end(loop_depth), loop_step(loop_depth)
               call set_variable(loop_var(loop_depth), [real(ivar, dp)])
               call run_loop_body(loop_body(loop_depth))
               if (exit_loop) then        ! ← exit from the DO
                  exit
               end if
               if (cycle_loop) then
                  if (cycle_target_depth < loop_depth) then
                     exit
                  else if (cycle_target_depth == loop_depth) then
                     cycle_loop = .false.
                     cycle_target_depth = 0
                     cycle
                  end if
               end if
            end do
            call set_variable(loop_var(loop_depth), [real(ivar, dp)])
         end if
         if (exit_loop) then
            if (exit_target_depth == loop_depth) then
               exit_loop = .false.
               exit_target_depth = 0
            end if
         end if
         if (cycle_loop) then
            if (cycle_target_depth == loop_depth) then
               cycle_loop = .false.
               cycle_target_depth = 0
            end if
         end if
         loop_var(loop_depth) = ""
         loop_is_unbounded(loop_depth) = .false.
         loop_is_for(loop_depth) = .false.
         loop_for_expr(loop_depth) = ""
         loop_depth = loop_depth - 1
         if (loop_depth == 0) loop_if_collect_depth = 0
         goto 9000
      case default
         ! nothing – fall through
      end select

      adj_line = adjustl(line_eval)
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
           had_error = .true.
           goto 9000
        end if

         ! — split into  condition  and  consequent —
         cond_txt = adjustl(adj_line(p_lpar + 1:p_rpar - 1))
         then_txt = adjustl(adj_line(p_rpar + 1:))

        if (len_trim(then_txt) == 0) then
           print *, "Error: null statement after IF"
           had_error = .true.
           goto 9000
        end if

         ! — evaluate the condition (must be scalar) —
         tmp = evaluate(cond_txt)
         if (eval_error) then
            had_error = .true.
            goto 9000
         end if
         if (size(tmp) /= 1) then
            print *, "Error: IF condition must be scalar"
            had_error = .true.
            goto 9000
         end if
         run_then = (tmp(1) /= 0.0_dp)

         ! — execute the single statement if TRUE —
         if (run_then) call eval_print(then_txt)
         goto 9000                                    ! one‑line IF handled
      end if
!───────────────────────────────────────────────────────────────────

!------------  Is this the beginning of a FOR/DO block?  -----------------
      if (index(lower_str(adj_line), "for ") == 1) then
         call parse_for_header(adj_line, lhs, rhs, rhs_tail, ok_for)
         ! parsed by parse_for_header: lhs, rhs expression, optional one-line rhs tail
         if (.not. ok_for) then
            print *, "Error: malformed FOR header: ", trim(line_eval)
            had_error = .true.
            goto 9000
         end if
         if (loop_depth >= max_loop_depth) then
            print *, "Error: loop nesting deeper than ", max_loop_depth
            had_error = .true.
            goto 9000
         end if
         do i = 1, loop_depth
            if (trim(loop_var(i)) == trim(lhs)) then
               print *, "Error: nested loop variable '", trim(lhs), "' already used by an outer loop"
               had_error = .true.
               goto 9000
            end if
         end do
         loop_depth = loop_depth + 1
         loop_var(loop_depth) = lhs
         loop_is_unbounded(loop_depth) = .false.
         loop_is_for(loop_depth) = .true.
         loop_for_expr(loop_depth) = rhs
         if (len_trim(rhs_tail) > 0) then
            if (is_block_if_start_line(rhs_tail) .or. index(lower_str(adjustl(rhs_tail)), "do ") == 1 .or. &
                index(lower_str(adjustl(rhs_tail)), "for ") == 1) then
               print *, "Error: one-line FOR body must be a single statement"
               had_error = .true.
               loop_var(loop_depth) = ""
               loop_is_unbounded(loop_depth) = .false.
               loop_is_for(loop_depth) = .false.
               loop_for_expr(loop_depth) = ""
               loop_depth = loop_depth - 1
               goto 9000
            end if
            tmp = evaluate(loop_for_expr(loop_depth))
            if (eval_error) then
               had_error = .true.
            else
               do ivar = 1, size(tmp)
                  call set_variable(loop_var(loop_depth), [tmp(ivar)])
                  prev_exec = in_loop_execute
                  prev_loop_exec_base = loop_exec_base_depth
                  loop_exec_base_depth = loop_depth
                  in_loop_execute = .true.
                  call eval_print(rhs_tail)
                  in_loop_execute = prev_exec
                  loop_exec_base_depth = prev_loop_exec_base
                  if (exit_loop) then
                     if (exit_target_depth == loop_depth) then
                        exit_loop = .false.
                        exit_target_depth = 0
                        exit
                     else
                        exit
                     end if
                  end if
                  if (cycle_loop) then
                     if (cycle_target_depth < loop_depth) then
                        exit
                     else if (cycle_target_depth == loop_depth) then
                        cycle_loop = .false.
                        cycle_target_depth = 0
                        cycle
                     end if
                  end if
               end do
            end if
            if (exit_loop) then
               if (exit_target_depth == loop_depth) then
                  exit_loop = .false.
                  exit_target_depth = 0
               end if
            end if
            if (cycle_loop) then
               if (cycle_target_depth == loop_depth) then
                  cycle_loop = .false.
                  cycle_target_depth = 0
               end if
            end if
            loop_var(loop_depth) = ""
            loop_is_unbounded(loop_depth) = .false.
            loop_is_for(loop_depth) = .false.
            loop_for_expr(loop_depth) = ""
            loop_depth = loop_depth - 1
            goto 9000
         end if
         loop_body(loop_depth) = ""
         goto 9000
      end if

      if (index(lower_str(adj_line), "do ") == 1 .or. trim(lower_str(adj_line)) == "do") then
         if (trim(lower_str(adj_line)) == "do") then
            if (loop_depth >= max_loop_depth) then
               print *, "Error: loop nesting deeper than ", max_loop_depth
               had_error = .true.
               goto 9000
            end if
            loop_depth = loop_depth + 1
            loop_var(loop_depth) = ""
            loop_is_unbounded(loop_depth) = .true.
            loop_is_for(loop_depth) = .false.
            loop_for_expr(loop_depth) = ""
            loop_body(loop_depth) = ""
            goto 9000
         else
            call parse_do_header(adj_line, lhs, cond_txt, then_txt, rhs, rhs_tail, ok_for)
            if (.not. ok_for) then
               print *, "Error: malformed DO header: ", trim(line_eval)
               had_error = .true.
               goto 9000
            end if
            if (loop_depth >= max_loop_depth) then
               print *, "Error: loop nesting deeper than ", max_loop_depth
               had_error = .true.
               goto 9000
            end if
            do i = 1, loop_depth
               if (trim(loop_var(i)) == trim(lhs)) then
                  print *, "Error: nested loop variable '", trim(lhs), "' already used by an outer loop"
                  had_error = .true.
                  goto 9000
               end if
            end do

            loop_depth = loop_depth + 1
            loop_var(loop_depth) = lhs
            loop_is_unbounded(loop_depth) = .false.
            loop_is_for(loop_depth) = .false.
            loop_for_expr(loop_depth) = ""
            loop_start(loop_depth) = parse_int_scalar(cond_txt)
            loop_end(loop_depth) = parse_int_scalar(then_txt)
            loop_step(loop_depth) = parse_int_scalar(rhs)
            call set_variable(loop_var(loop_depth), [real(loop_start(loop_depth), dp)])

            if (len_trim(rhs_tail) > 0) then
               if (is_block_if_start_line(rhs_tail) .or. index(lower_str(adjustl(rhs_tail)), "do ") == 1 .or. &
                   index(lower_str(adjustl(rhs_tail)), "for ") == 1) then
                  print *, "Error: one-line DO body must be a single statement"
                  had_error = .true.
                  loop_var(loop_depth) = ""
                  loop_is_unbounded(loop_depth) = .false.
                  loop_is_for(loop_depth) = .false.
                  loop_for_expr(loop_depth) = ""
                  loop_depth = loop_depth - 1
                  goto 9000
               end if
               do ivar = loop_start(loop_depth), loop_end(loop_depth), loop_step(loop_depth)
                  call set_variable(loop_var(loop_depth), [real(ivar, dp)])
                  prev_exec = in_loop_execute
                  prev_loop_exec_base = loop_exec_base_depth
                  loop_exec_base_depth = loop_depth
                  in_loop_execute = .true.
                  call eval_print(rhs_tail)
                  in_loop_execute = prev_exec
                  loop_exec_base_depth = prev_loop_exec_base
                  if (exit_loop) then
                     if (exit_target_depth == loop_depth) then
                        exit_loop = .false.
                        exit_target_depth = 0
                        exit
                     else
                        exit
                     end if
                  end if
                  if (cycle_loop) then
                     if (cycle_target_depth < loop_depth) then
                        exit
                     else if (cycle_target_depth == loop_depth) then
                        cycle_loop = .false.
                        cycle_target_depth = 0
                        cycle
                     end if
                  end if
               end do
               if (exit_loop) then
                  if (exit_target_depth == loop_depth) then
                     exit_loop = .false.
                     exit_target_depth = 0
                  end if
               end if
               if (cycle_loop) then
                  if (cycle_target_depth == loop_depth) then
                     cycle_loop = .false.
                     cycle_target_depth = 0
                  end if
               end if
               loop_var(loop_depth) = ""
               loop_is_unbounded(loop_depth) = .false.
               loop_is_for(loop_depth) = .false.
               loop_for_expr(loop_depth) = ""
               loop_depth = loop_depth - 1
               goto 9000
            end if
            loop_body(loop_depth) = ""
            goto 9000
         end if
      end if

      trimmed_line = adjustl(line_eval)

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
           had_error = .true.
        end if
        goto 9000
      end if

! ————————————————————————— end “del” —————————————————————

      if (adjustl(line_eval) == "clear") then
         call clear()
         goto 9000
      end if
      if (adjustl(line_eval) == "?vars") then
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
         goto 9000
      end if
      trimmed_line = adjustl(line_eval)
      if (len_trim(trimmed_line) >= 4 .and. trimmed_line(1:4) == "read" & 
          .and. (len_trim(trimmed_line) == 4 .or. trimmed_line(5:5) == " ")) then
         tail = adjustl(trimmed_line(5:))
        if (len_trim(tail) == 0) then
           print *, "Error: read needs a file name"
           had_error = .true.
           goto 9000
        end if
        call read_vars_from_file(trim(tail))
        if (eval_error) then
           had_error = .true.
           goto 9000
        end if
        goto 9000
      end if
      if (adjustl(line_eval) == "cor") then
         call print_cor_matrices()
         goto 9000
      end if
      call split_by_semicolon(line_eval, n, parts, suppress)

      do k = 1, n
         if (parts(k) == "") cycle          ! blank segment

         const_assign = .false.
         part_eval = parts(k)
         if (len_trim(part_eval) >= 6 .and. part_eval(1:5) == "const" .and. part_eval(6:6) == " ") then
            const_assign = .true.
            part_eval = adjustl(part_eval(6:))
            if (len_trim(part_eval) == 0) then
               print *, "Error: const needs an assignment"
               had_error = .true.
               const_assign = .false.
               cycle
            end if
            if (index(part_eval, "(") > 0 .and. index(part_eval, ")") > index(part_eval, "(") & 
                .and. index(part_eval, "=") > index(part_eval, "(")) then
               print *, "Error: const applies to whole-variable assignments only"
               had_error = .true.
               const_assign = .false.
               cycle
            end if
         end if

         call split_by_comma(part_eval, n_names, names)
         if (n_names > 1) then
            if (.not. suppress(k)) then
               if (echo_code) write (*, '(/,"> ",a)') trim(part_eval)
               do i = 1, n_names
                  if (len_trim(names(i)) >= 2 .and. names(i)(1:1) == '"' .and. &
                      names(i)(len_trim(names(i)):len_trim(names(i))) == '"') then
                     write (*, "(a)", advance="no") names(i)(2:len_trim(names(i)) - 1)
                  else
                     r = evaluate(names(i))
                     if (eval_error) then
                        had_error = .true.
                        exit
                     end if
                     rsize = size(r)
                     if (rsize == 0) then
                        write (*, "(a)", advance="no") ""
                     else
                        rint = nint(r)
                        select case (rsize)
                        case (1)
                           if (abs(r(1) - rint(1)) <= tol) then
                              write (*, "(i0)", advance="no") rint
                           else
                              write (*, "(F0.6)", advance="no") r(1)
                           end if
                        case default
                           if (rsize <= max_print) then
                              if (print_array_as_int_if_possible) then
                                 print_array_as_int = all(abs(r - rint) <= tol)
                              else
                                 print_array_as_int = .false.
                              end if
                              if (print_array_as_int) then
                                 write (*, fmt_real_array, advance="no") rint
                              else
                                 write (*, '("[",*(F0.6,:,", "))', advance="no") r
                              end if
                              write (*, "(']')", advance="no")
                           else
                              call print_stats(r)
                           end if
                        end select
                     end if
                  end if
                  if (i < n_names) write (*, "(a)", advance="no") " "
               end do
               print *
            end if
            cycle
         end if

         if (index(part_eval, '"') > 0) then
            block
               character(len=:), allocatable :: seg
               integer :: p1, p2, posq
               logical :: have_item
               have_item = .false.
               posq = index(part_eval, '"')
               do while (posq > 0)
                  if (posq > 1) then
                     seg = adjustl(part_eval(:posq - 1))
                     if (len_trim(seg) > 0) then
                        call split_by_spaces(seg, n_names, names)
                        if (n_names > 0) then
                           do i = 1, n_names
                              r = evaluate(names(i))
                              if (eval_error) then
                                 had_error = .true.
                                 exit
                              end if
                              if (size(r) == 1) then
                                 if (abs(r(1) - nint(r(1))) <= tol) then
                                    write (*, "(i0)", advance="no") nint(r(1))
                                 else
                                    write (*, "(F0.6)", advance="no") r(1)
                                 end if
                              else
                                 write (*, '("[",*(F0.6,:,", "))', advance="no") r
                                 write (*, "(']')", advance="no")
                              end if
                              write (*, "(a)", advance="no") " "
                           end do
                           have_item = .true.
                        end if
                     end if
                  end if
                  p1 = posq + 1
                  p2 = index(part_eval(p1:), '"')
                  if (p2 <= 0) then
                     print *, "Error: unmatched quote"
                     had_error = .true.
                     exit
                  end if
                  p2 = p1 + p2 - 2
                  write (*, "(a)", advance="no") part_eval(p1:p2)
                  write (*, "(a)", advance="no") " "
                  have_item = .true.
                  if (p2 + 2 <= len_trim(part_eval)) then
                     part_eval = part_eval(p2 + 2:)
                     posq = index(part_eval, '"')
                  else
                     part_eval = ""
                     posq = 0
                  end if
               end do
               if (len_trim(part_eval) > 0 .and. .not. had_error) then
                  call split_by_spaces(part_eval, n_names, names)
                  if (n_names > 0) then
                     do i = 1, n_names
                        r = evaluate(names(i))
                        if (eval_error) then
                           had_error = .true.
                           exit
                        end if
                        if (size(r) == 1) then
                           if (abs(r(1) - nint(r(1))) <= tol) then
                              write (*, "(i0)", advance="no") nint(r(1))
                           else
                              write (*, "(F0.6)", advance="no") r(1)
                           end if
                        else
                           write (*, '("[",*(F0.6,:,", "))', advance="no") r
                           write (*, "(']')", advance="no")
                        end if
                        write (*, "(a)", advance="no") " "
                     end do
                     have_item = .true.
                  end if
               end if
               if (have_item) then
                  print *
                  cycle
               end if
            end block
         end if

         call split_by_spaces(part_eval, n_names, names)
         if (n_names > 1) then
            block
               integer :: j, c
               logical :: all_ident
               all_ident = .true.
               do j = 1, n_names
                  if (len_trim(names(j)) == 0) then
                     all_ident = .false.; exit
                  end if
                  if (.not. is_letter(names(j)(1:1))) then
                     all_ident = .false.; exit
                  end if
                  do c = 1, len_trim(names(j))
                     if (.not. (is_alphanumeric(names(j)(c:c)) .or. names(j)(c:c) == "_")) then
                        all_ident = .false.; exit
                     end if
                  end do
                  if (.not. all_ident) exit
               end do

               if (all_ident) then
                  if (.not. suppress(k)) then
                     if (echo_code) write (*, '(/,"> ",a)') trim(part_eval)
                     do j = 1, n_names
                        r = evaluate(names(j))
                        if (eval_error) then
                           had_error = .true.
                           exit
                        end if
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
                                    write (*, fmt_real_array, advance="no") rint
                                 else
                                    write (*, '("[",*(F0.6,:,", "))', advance="no") r
                                 end if
                                 print "(']')"
                              else
                                 call print_stats(r)
                              end if
                           end select
                        end if
                     end do
                  end if
                  cycle
               end if
            end block
         end if

         ! ---------- syntax checks exactly as before ----------
         if (.not. matched_parentheses(part_eval)) then
            print *, "mismatched parentheses"; had_error = .true.; cycle
         end if
         if (.not. matched_brackets(part_eval)) then
            print *, "mismatched brackets"; had_error = .true.; cycle
         end if
         if (index(part_eval, "**") /= 0) then
            print *, "use ^ instead of ** for exponentiation"; had_error = .true.; cycle
         end if

         ! ------------------------------------------------------
         r = evaluate(part_eval)
         if (eval_error) then
            if (stop_if_error) stop "stopped with evaluation error"
            had_error = .true.; cycle
         end if
         const_assign = .false.
         if (suppress_result) then
            suppress_result = .false.
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
9000  continue
      const_assign = .false.
      if (write_code .and. (.not. had_error .or. comment_only)) write (tunit, "(a)") line
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
               vars(n_vars)%is_const = .false.
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

   subroutine split_by_comma(line, n, parts)
!  Break LINE into items separated by top-level commas.
!  parts(i) = i-th item (trimmed)
      character(len=*), intent(in)  :: line
      integer, intent(out) :: n
      character(len=:), allocatable  :: parts(:)

      character(len=:), allocatable :: buf
      integer :: i, depth_par, depth_br, ntrim
      logical :: in_quote

      buf = ""
      depth_par = 0
      depth_br = 0
      in_quote = .false.
      n = 0

      do i = 1, len_trim(line)
         select case (line(i:i))
         case ('"')
            in_quote = .not. in_quote
         case ("("); depth_par = depth_par + 1
         case (")"); depth_par = depth_par - 1
         case ("["); depth_br = depth_br + 1
         case ("]"); depth_br = depth_br - 1
         case (",")
            if (.not. in_quote .and. depth_par == 0 .and. depth_br == 0) then
               call append_part(buf)
               buf = ""
               cycle
            end if
         end select
         buf = buf//line(i:i)
      end do

      if (len_trim(buf) > 0) then
         call append_part(buf)
      else
         ntrim = len_trim(line)
         if (ntrim > 0) then
            if (line(ntrim:ntrim) == ",") call append_part("")
         end if
      end if

   contains
      subroutine append_part(txt)
         character(len=*), intent(in) :: txt
         integer :: newlen

         newlen = max(len_trim(txt), merge(0, len(parts(1)), allocated(parts)))

         if (.not. allocated(parts)) then
            allocate (character(len=newlen) :: parts(1))
         else if (len(parts(1)) < newlen) then
            call enlarge_parts(newlen)
         else
            parts = [character(len=len(parts)) :: parts, ""]
         end if

         n = n + 1
         parts(n) = adjustl(trim(txt))
      end subroutine append_part

      subroutine enlarge_parts(newlen)
         integer, intent(in) :: newlen
         character(len=newlen), allocatable :: tmp(:)

         allocate (tmp(size(parts)))
         tmp = parts
         call move_alloc(tmp, parts)
         parts = [character(len=len(parts)) :: parts, ""]
      end subroutine enlarge_parts
   end subroutine split_by_comma

   subroutine split_by_spaces(line_in, n, parts)
      character(len=*), intent(in) :: line_in
      integer, intent(out) :: n
      character(len=:), allocatable :: parts(:)
      integer :: i, start, len_line, newlen, nlen_tail

      n = 0
      len_line = len_trim(line_in)
      i = 1
      do while (i <= len_line)
         do
            if (i > len_line) exit
            if (line_in(i:i) /= " ") exit
            i = i + 1
         end do
         if (i > len_line) exit
         start = i
         do
            if (i > len_line) exit
            if (line_in(i:i) == " ") exit
            i = i + 1
         end do
         nlen_tail = min(i - 1, len_line)
         if (nlen_tail < start) cycle
         newlen = max(nlen_tail - start + 1, merge(0, len(parts(1)), allocated(parts)))
         if (.not. allocated(parts)) then
            allocate (character(len=newlen) :: parts(1))
         else if (len(parts(1)) < newlen) then
            block
               character(len=newlen), allocatable :: tmp(:)
               allocate (tmp(size(parts)))
               tmp = parts
               call move_alloc(tmp, parts)
               parts = [character(len=len(parts)) :: parts, ""]
            end block
         else
            parts = [character(len=len(parts)) :: parts, ""]
         end if
         n = n + 1
         parts(n) = adjustl(line_in(start:nlen_tail))
      end do
   end subroutine split_by_spaces

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
      integer :: p1, p2, nlen, prev_base_depth
      prev_base_depth = loop_exec_base_depth
      loop_exec_base_depth = loop_depth
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
         ! Nested run_loop_body calls may clear this flag; keep execution mode
         ! active for the current (outer) loop body.
         in_loop_execute = .true.
         if (cycle_loop) then
            ! — we’ve seen a “cycle” in this iteration,
            !   so drop the rest of the body and go back to the DO
            in_loop_execute = .false.
            loop_exec_base_depth = prev_base_depth
            return
         end if
         if (exit_loop) then
            in_loop_execute = .false.
            loop_exec_base_depth = prev_base_depth
            return
         end if
         if (p2 == 0) exit
         p1 = p1 + p2
      end do
      in_loop_execute = .false.         ! <<< back to normal typing mode
      loop_exec_base_depth = prev_base_depth
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
      integer :: u, ios, neval
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
         if (verbose_ .and. neval > 0) print "(/)"
         if (verbose_) print "(a)", trim(ln)
         call eval_print(ln)
         if (verbose_) neval = neval + 1
         if (stop_if_error .and. eval_error) exit
      end do
      close (u)
   end subroutine run

end module interpret_mod
