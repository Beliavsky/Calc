module interpret_mod
   use kind_mod, only: dp
  use stats_mod, only: mean, sd, cor, cov, trimmean, winsor_mean, huber_mean, bisquare_mean, mad, iqr, iqr_scale, jb_test, ttest1, ttest2, ks2_test, adf_stat, adf, phillips_perron_stat, phillips_perron, kernelreg, lowess, lowesscv, knnreg, knnregcv, acf, pacf, arspec, arspecaic, armaspec, armaspecaic, arma_mt_spec, armaaic_mt_spec, welchspec, pgramspec, acfspec, mtspec, fiacf, fracdiff, arcoef, aracf, maacf, arpacf, mapacf, armaacf, arfimaacf, armapacf, armastab, arsim, arsimfit, masim, masimfit, armasim, armasimfit, arfimasim, cpsim, cpfit, cpfitaic, resample, regress, huber_regress, bisquare_regress, dist_regress, regress_multi, poly1reg, splinereg, naturalspline, distaicscan, arfit, mafit, armafit, armafitgrid, armafitaic, araic, maaic, arfimafit, mssk, mssk_unif, mssk_norm, mssk_exp, mssk_gamma, mssk_lnorm, mssk_t, mssk_nct, mssk_mixnorm, mssk_chisq, mssk_f, mssk_beta, mssk_logis, mssk_sech, mssk_laplace, mssk_cauchy, mssk_ged, mssk_hyperb, skew_gamma, skew_lnorm, skew_nct, skew_chisq, skew_f, skew_beta, kurt_gamma, kurt_lnorm, kurt_t, kurt_nct, kurt_chisq, kurt_f, kurt_beta, kurt_ged, kurt_hyperb, dunif, dexp, dgamma, dlnorm, dnorm, dmixnorm, dt, dnct, dchisq, df, dbeta, dlogis, dsech, dlaplace, dcauchy, dged, dhyperb, punif, pexp, pgamma, plnorm, pnorm, pmixnorm, pt, pnct, pchisq, pf, pbeta, plogis, psech, plaplace, pcauchy, pged, phyperb, qunif, qexp, qgamma, qlnorm, qnorm, qmixnorm, qt, qnct, qchisq, qf, qbeta, qlogis, qsech, qlaplace, qcauchy, qged, qhyperb, rhyperb, kde, fit_norm, fit_exp, fit_gamma, fit_lnorm, fit_t, fit_nct, fit_mixnorm, fit_mixnorm_aic, fix_mixnorm_aic, fit_chisq, fit_f, fit_beta, fit_logis, fit_sech, fit_laplace, fit_cauchy, fit_ged, fit_hyperb, cumsum, cumprod, diff, standardize, &
                        print_stats, skew, kurtosis, cummean, cummin, cummax, &
                        geomean, harmean
  use util_mod, only: matched_brackets, matched_parentheses, arange, irange, &
                       head, tail, grid, print_real, is_alphanumeric, &
                       is_numeral, is_letter, zeros, ones, replace, &
                       rep, read_vec, reverse, runif1, polyroots
   use random_mod, only: random_normal, runif, rexp, rgamma, rlnorm, rt, rnct, &
      rmixnorm, mixnoise, rchisq, rf, rbeta, rlogis, rsech, rlaplace, rcauchy, &
      rged, random_seed_init, str_normal, str_student_t, str_laplace, str_ged, &
      str_sech
   use qsort_mod, only: sorted, indexx, rank, median, unique, quantile
   use iso_fortran_env, only: compiler_options, compiler_version, int64
   use plot_mod, only: plot, plot_to_label, set_plotout, get_plotout
   implicit none
   private
   public :: eval_print, tunit, code_transcript_file, vars, write_code, &
             echo_code, get_loop_depth, get_prompt_depth

   integer, parameter :: max_vars = 100, len_name = 32
   integer, parameter :: max_funcs = 64, max_func_args = 16
   integer, parameter :: max_subs = 64, max_sub_args = 16
   integer, parameter :: len_default_expr = 256
   integer, parameter :: max_print = 15 ! for arrays larger than this, summary stats printed instead of elements

   type :: var_t
      character(len=len_name) :: name = ""
      real(kind=dp), allocatable :: val(:)
      logical :: is_const = .false.
   end type var_t
   type :: arr_t
      real(kind=dp), allocatable :: v(:)
   end type arr_t
   type :: user_func_t
      character(len=len_name) :: name = ""
      integer :: nargs = 0
      character(len=len_name) :: args(max_func_args) = ""
      logical :: has_default(max_func_args) = .false.
      character(len=len_default_expr) :: defaults(max_func_args) = ""
      character(len=32768) :: body = ""
   end type user_func_t
   type :: user_sub_t
      character(len=len_name) :: name = ""
      integer :: nargs = 0
      character(len=len_name) :: args(max_sub_args) = ""
      integer :: intents(max_sub_args) = 0 ! 1=in, 2=inout, 3=out
      logical :: has_default(max_sub_args) = .false.
      character(len=len_default_expr) :: defaults(max_sub_args) = ""
      character(len=32768) :: body = ""
   end type user_sub_t

   type(var_t) :: vars(max_vars)
   type(user_func_t) :: user_funcs(max_funcs)
    type(user_sub_t) :: user_subs(max_subs)
   integer :: n_vars = 0, tunit
   integer :: n_user_funcs = 0
   integer :: n_user_subs = 0
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
    logical, save :: func_collecting = .false.
    character(len=len_name), save :: func_collect_name = ""
    integer, save :: func_collect_nargs = 0
    character(len=len_name), save :: func_collect_args(max_func_args) = ""
    logical, save :: func_collect_has_default(max_func_args) = .false.
    character(len=len_default_expr), save :: func_collect_defaults(max_func_args) = ""
    character(len=32768), save :: func_collect_body = ""
    logical, save :: in_user_function = .false.
    logical, save :: in_user_subroutine = .false.
    character(len=len_name), save :: active_sub_name = ""
    integer, save :: active_sub_nargs = 0
    character(len=len_name), save :: active_sub_args(max_sub_args) = ""
    integer, save :: active_sub_intents(max_sub_args) = 0
    logical, save :: active_sub_set(max_sub_args) = .false.
    logical, save :: sub_collecting = .false.
    character(len=len_name), save :: sub_collect_name = ""
    integer, save :: sub_collect_nargs = 0
    character(len=len_name), save :: sub_collect_args(max_sub_args) = ""
    integer, save :: sub_collect_intents(max_sub_args) = 0
    logical, save :: sub_collect_has_default(max_sub_args) = .false.
    character(len=len_default_expr), save :: sub_collect_defaults(max_sub_args) = ""
    character(len=32768), save :: sub_collect_body = ""
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
      n_user_funcs = 0
      do i = 1, max_funcs
         user_funcs(i)%name = ""
         user_funcs(i)%nargs = 0
         user_funcs(i)%args = ""
         user_funcs(i)%has_default = .false.
         user_funcs(i)%defaults = ""
         user_funcs(i)%body = ""
      end do
      n_user_subs = 0
      do i = 1, max_subs
         user_subs(i)%name = ""
         user_subs(i)%nargs = 0
         user_subs(i)%args = ""
         user_subs(i)%intents = 0
         user_subs(i)%has_default = .false.
         user_subs(i)%defaults = ""
         user_subs(i)%body = ""
      end do
      in_user_subroutine = .false.
      active_sub_name = ""
      active_sub_nargs = 0
      active_sub_args = ""
      active_sub_intents = 0
      active_sub_set = .false.
      func_collect_has_default = .false.
      func_collect_defaults = ""
      sub_collect_has_default = .false.
      sub_collect_defaults = ""
   end subroutine clear

   subroutine print_cor_matrix_args(args, labels, methods)
      type(arr_t), intent(in) :: args(:)
      character(len=*), intent(in) :: labels(:)
      integer, intent(in), optional :: methods(:)
      integer :: i, j, n, nsize, max_name, col_width, pad
      real(kind=dp) :: cval
      integer, allocatable :: mlist(:)
      integer :: im, mcode
      character(len=16) :: mname

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

      if (present(methods)) then
         if (size(methods) < 1) then
            allocate (mlist(1)); mlist = [1]
         else
            allocate (mlist(size(methods))); mlist = methods
         end if
      else
         allocate (mlist(1)); mlist = [1]
      end if

      do im = 1, size(mlist)
         mcode = mlist(im)
         select case (mcode)
         case (1); mname = "pearson"
         case (2); mname = "spearman"
         case (3); mname = "kendall"
         case default; mname = "pearson"
         end select

         write (*, "(a,a,a,i0,a)") "Correlation matrix (method=", trim(mname), ", n=", nsize, "):"
         write (*, "(a)", advance="no") repeat(" ", max_name)//" "
         do j = 1, n
            call write_padded(trim(labels(j)), col_width)
         end do
         print *

         do i = 1, n
            call write_padded(trim(labels(i)), max_name)
            do j = 1, n
               cval = cor_by_method_xy(args(i)%v, args(j)%v, mcode)
               write (*, "(f10.6)", advance="no") cval
               pad = col_width - 10
               if (pad < 0) pad = 0
               write (*, "(a)", advance="no") repeat(" ", pad + 1)
            end do
            print *
         end do
         if (im < size(mlist)) print *
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

   pure function cor_by_method_xy(x, y, mcode) result(cval)
      real(kind=dp), intent(in) :: x(:)
      real(kind=dp), intent(in) :: y(:)
      integer, intent(in) :: mcode
      real(kind=dp) :: cval
      select case (mcode)
      case (1)
         cval = cor(x, y)
      case (2)
         cval = cor_spearman_xy(x, y)
      case (3)
         cval = cor_kendall_xy(x, y)
      case default
         cval = bad_value
      end select
   end function cor_by_method_xy

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

   pure function cor_spearman_xy(x, y) result(rho)
      ! Spearman correlation: Pearson correlation of average ranks.
      real(kind=dp), intent(in) :: x(:)
      real(kind=dp), intent(in) :: y(:)
      real(kind=dp) :: rho
      real(kind=dp), allocatable :: rx(:), ry(:)
      if (size(x) /= size(y) .or. size(x) < 2) then
         rho = bad_value
         return
      end if
      rx = average_ranks(x)
      ry = average_ranks(y)
      rho = cor(rx, ry)
   end function cor_spearman_xy

   pure function cor_kendall_xy(x, y) result(tau)
      ! Kendall tau-b with tie correction.
      real(kind=dp), intent(in) :: x(:)
      real(kind=dp), intent(in) :: y(:)
      real(kind=dp) :: tau
      integer :: i, j, n
      real(kind=dp) :: dx, dy, den
      integer(kind=int64) :: ncon, ndis, ntx, nty, ntxy

      n = size(x)
      if (n /= size(y) .or. n < 2) then
         tau = bad_value
         return
      end if

      ncon = 0; ndis = 0; ntx = 0; nty = 0; ntxy = 0
      do i = 1, n - 1
         do j = i + 1, n
            dx = x(i) - x(j)
            dy = y(i) - y(j)
            if (dx == 0.0_dp .and. dy == 0.0_dp) then
               ntxy = ntxy + 1
            else if (dx == 0.0_dp) then
               ntx = ntx + 1
            else if (dy == 0.0_dp) then
               nty = nty + 1
            else if (dx*dy > 0.0_dp) then
               ncon = ncon + 1
            else
               ndis = ndis + 1
            end if
         end do
      end do
      den = sqrt(real(ncon + ndis + ntx, dp) * real(ncon + ndis + nty, dp))
      if (den <= 0.0_dp) then
         tau = bad_value
      else
         tau = real(ncon - ndis, dp) / den
      end if
   end function cor_kendall_xy

   pure function average_ranks(x) result(r)
      ! Average ranks for ties (1-based).
      real(kind=dp), intent(in) :: x(:)
      real(kind=dp), allocatable :: r(:)
      integer, allocatable :: ord(:)
      integer :: n, i, j, k
      n = size(x)
      allocate (r(n))
      if (n < 1) return
      ord = indexx(x)
      i = 1
      do while (i <= n)
         j = i
         do while (j < n)
            if (x(ord(j + 1)) /= x(ord(i))) exit
            j = j + 1
         end do
         do k = i, j
            r(ord(k)) = 0.5_dp * real(i + j, dp)
         end do
         i = j + 1
      end do
   end function average_ranks

   subroutine parse_cor_methods_spec(spec, methods, ok, errmsg)
      character(len=*), intent(in) :: spec
      integer, allocatable, intent(out) :: methods(:)
      logical, intent(out) :: ok
      character(len=*), intent(out) :: errmsg
      character(len=:), allocatable :: t, inside, tok
      character(len=:), allocatable :: parts(:)
      integer :: n, i

      ok = .false.
      errmsg = ""
      t = adjustl(trim(spec))
      if (len_trim(t) < 1) then
         errmsg = "Error: method must be provided"
         allocate (methods(0))
         return
      end if

      if (t(1:1) == "[" .and. t(len_trim(t):len_trim(t)) == "]") then
         if (len_trim(t) <= 2) then
            errmsg = "Error: method vector must be non-empty"
            allocate (methods(0))
            return
         end if
         inside = t(2:len_trim(t) - 1)
         call split_by_comma(inside, n, parts)
         if (n < 1) then
            errmsg = "Error: method vector must be non-empty"
            allocate (methods(0))
            return
         end if
         allocate (methods(n))
         do i = 1, n
            tok = normalize_method_token(parts(i))
            select case (tok)
            case ("pearson")
               methods(i) = 1
            case ("spearman")
               methods(i) = 2
            case ("kendall")
               methods(i) = 3
            case default
               errmsg = "Error: unknown cor() method '"//trim(tok)//"'"
               return
            end select
         end do
      else
         allocate (methods(1))
         tok = normalize_method_token(t)
         select case (tok)
         case ("pearson")
            methods(1) = 1
         case ("spearman")
            methods(1) = 2
         case ("kendall")
            methods(1) = 3
         case default
            errmsg = "Error: unknown cor() method '"//trim(tok)//"'"
            return
         end select
      end if

      ok = .true.
   contains
      pure function normalize_method_token(s) result(out)
         character(len=*), intent(in) :: s
         character(len=:), allocatable :: out
         character(len=:), allocatable :: t1
         integer :: n
         t1 = adjustl(trim(s))
         n = len_trim(t1)
         if (n >= 2) then
            if ((t1(1:1) == '"' .and. t1(n:n) == '"') .or. (t1(1:1) == "'" .and. t1(n:n) == "'")) then
               out = lower_str(adjustl(trim(t1(2:n - 1))))
               return
            end if
         end if
         out = lower_str(t1)
      end function normalize_method_token
   end subroutine parse_cor_methods_spec

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
         integer :: i, start, len_line, newlen, nlen_tail, oldlen

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
            if (allocated(parts)) then
               oldlen = len(parts(1))
            else
               oldlen = 0
            end if
            newlen = max(nlen_tail - start + 1, oldlen)
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

   subroutine mark_sub_arg_assigned(name)
      character(len=*), intent(in) :: name
      integer :: i
      if (.not. in_user_subroutine) return
      do i = 1, active_sub_nargs
         if (trim(active_sub_args(i)) == trim(name)) then
            if (active_sub_intents(i) == 3) active_sub_set(i) = .true.
            return
         end if
      end do
   end subroutine mark_sub_arg_assigned

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
            call mark_sub_arg_assigned(nm)
            return
         end if
      end do

      if (n_vars < max_vars) then
         n_vars = n_vars + 1
         vars(n_vars)%name = nm
         vars(n_vars)%val = val
         vars(n_vars)%is_const = make_const
         call mark_sub_arg_assigned(nm)
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
      case ("trimmean"); r = trimmean(arr)
      case ("winsor_mean"); r = winsor_mean(arr)
      case ("huber_mean"); r = huber_mean(arr)
      case ("bisquare_mean"); r = bisquare_mean(arr)
      case ("mad"); r = mad(arr)
      case ("iqr"); r = iqr(arr)
      case ("iqr_scale"); r = iqr_scale(arr)
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
      case ("jb_test"); res = jb_test(arr)
      case ("kde"); res = kde(arr)
      case ("fit_norm"); res = fit_norm(arr)
      case ("fit_exp"); res = fit_exp(arr)
      case ("fit_gamma"); res = fit_gamma(arr)
      case ("fit_lnorm"); res = fit_lnorm(arr)
      case ("fit_t"); res = fit_t(arr)
      case ("fit_nct"); res = fit_nct(arr)
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

   integer pure function top_level_keyword_eq_pos(tok) result(pos)
      character(len=*), intent(in) :: tok
      integer :: i, n, dpar, dbr
      character(len=1) :: ch, prev, nxt
      logical :: in_str
      pos = 0
      n = len_trim(tok)
      dpar = 0
      dbr = 0
      in_str = .false.
      do i = 1, n
         ch = tok(i:i)
         if (ch == '"') then
            in_str = .not. in_str
            cycle
         end if
         if (in_str) cycle
         select case (ch)
         case ("(")
            dpar = dpar + 1
         case (")")
            if (dpar > 0) dpar = dpar - 1
         case ("[")
            dbr = dbr + 1
         case ("]")
            if (dbr > 0) dbr = dbr - 1
         case ("=")
            if (dpar == 0 .and. dbr == 0) then
               prev = " "
               nxt = " "
               if (i > 1) prev = tok(i - 1:i - 1)
               if (i < n) nxt = tok(i + 1:i + 1)
               if (prev == "=" .or. prev == "<" .or. prev == ">" .or. prev == "/" .or. nxt == "=") cycle
               pos = i
               return
            end if
         end select
      end do
   end function top_level_keyword_eq_pos

   subroutine parse_call_actual(tok_in, is_named, argname, argexpr, ok)
      character(len=*), intent(in) :: tok_in
      logical, intent(out) :: is_named, ok
      character(len=len_name), intent(out) :: argname
      character(len=:), allocatable, intent(out) :: argexpr
      character(len=:), allocatable :: tok
      integer :: peq
      tok = adjustl(trim(tok_in))
      is_named = .false.
      ok = .true.
      argname = ""
      argexpr = tok
      peq = top_level_keyword_eq_pos(tok)
      if (peq == 0) return
      argname = adjustl(trim(tok(1:peq - 1)))
      argexpr = adjustl(trim(tok(peq + 1:)))
      if (.not. is_alnum_string(argname) .or. len_trim(argexpr) == 0) then
         ok = .false.
         return
      end if
      is_named = .true.
   end subroutine parse_call_actual

   pure logical function is_end_function_line(tl) result(ok)
      character(len=*), intent(in) :: tl
      ok = (trim(tl) == "end function" .or. trim(tl) == "endfunction")
   end function is_end_function_line

   subroutine parse_function_header(line, fname, nargs, fargs, fhas_default, fdefaults, ok)
      character(len=*), intent(in) :: line
      character(len=len_name), intent(out) :: fname
      integer, intent(out) :: nargs
      character(len=len_name), intent(out) :: fargs(max_func_args)
      logical, intent(out) :: fhas_default(max_func_args)
      character(len=len_default_expr), intent(out) :: fdefaults(max_func_args)
      logical, intent(out) :: ok
      character(len=:), allocatable :: tl, low, inside, part, argname, defexpr
      character(len=:), allocatable :: parts(:)
      integer :: p1, p2, i, n, peq
      logical :: seen_default

      ok = .false.
      fname = ""
      nargs = 0
      fargs = ""
      fhas_default = .false.
      fdefaults = ""
      tl = adjustl(trim(line))
      low = lower_str(tl)
      if (index(low, "function ") /= 1) return
      p1 = index(tl, "(")
      p2 = scan(tl, ")", back=.true.)
      if (p1 <= 9 .or. p2 <= p1) return
      fname = adjustl(trim(tl(10:p1 - 1)))
      if (.not. is_alnum_string(fname)) return
      if (len_trim(tl(p2 + 1:)) > 0) return
      inside = trim(tl(p1 + 1:p2 - 1))
      if (len_trim(inside) == 0) then
         ok = .true.
         return
      end if
      call split_by_comma(inside, n, parts)
      if (n < 1 .or. n > max_func_args) return
      seen_default = .false.
      do i = 1, n
         part = trim(parts(i))
         peq = index(part, "=")
         if (peq == 0) then
            if (seen_default) return
            if (.not. is_alnum_string(part)) return
            fargs(i) = part
         else
            argname = adjustl(trim(part(1:peq - 1)))
            defexpr = adjustl(trim(part(peq + 1:)))
            if (.not. is_alnum_string(argname)) return
            if (len_trim(defexpr) == 0) return
            if (len_trim(defexpr) > len_default_expr) then
               print *, "Error: default expression too long in function header"
               eval_error = .true.
               return
            end if
            fargs(i) = argname
            fhas_default(i) = .true.
            fdefaults(i) = defexpr
            seen_default = .true.
         end if
      end do
      nargs = n
      ok = .true.
   end subroutine parse_function_header

   integer function user_func_index(fname) result(idx)
      character(len=*), intent(in) :: fname
      integer :: i
      idx = 0
      do i = 1, n_user_funcs
         if (trim(user_funcs(i)%name) == trim(fname)) then
            idx = i
            return
         end if
      end do
   end function user_func_index

   subroutine set_user_function(fname, nargs, fargs, fhas_default, fdefaults, body)
      character(len=*), intent(in) :: fname
      integer, intent(in) :: nargs
      character(len=*), intent(in) :: fargs(:)
      logical, intent(in) :: fhas_default(:)
      character(len=*), intent(in) :: fdefaults(:)
      character(len=*), intent(in) :: body
      integer :: i, idx

      idx = user_func_index(fname)
      if (idx == 0) then
         if (n_user_funcs >= max_funcs) then
            print *, "Error: maximum number of user functions reached"
            eval_error = .true.
            return
         end if
         n_user_funcs = n_user_funcs + 1
         idx = n_user_funcs
      end if
      user_funcs(idx)%name = trim(fname)
      user_funcs(idx)%nargs = nargs
      user_funcs(idx)%args = ""
      user_funcs(idx)%has_default = .false.
      user_funcs(idx)%defaults = ""
      do i = 1, nargs
         user_funcs(idx)%args(i) = trim(fargs(i))
         user_funcs(idx)%has_default(i) = fhas_default(i)
         user_funcs(idx)%defaults(i) = trim(fdefaults(i))
      end do
      user_funcs(idx)%body = body
   end subroutine set_user_function

   function call_user_function(fname, args_text) result(res)
      character(len=*), intent(in) :: fname, args_text
      real(kind=dp), allocatable :: res(:)
      real(kind=dp), allocatable :: tmp(:), ret(:)
      type(arr_t) :: arg_vals(max_func_args)
      type(var_t) :: saved_vars(max_vars)
      integer :: saved_n_vars, idx, i, k, n_args, n_required, pos
      integer :: saved_exit_target, saved_cycle_target
      logical :: saved_exit_loop, saved_cycle_loop, saved_in_user, saw_named
      logical :: provided(max_func_args), is_named, ok_named
      character(len=:), allocatable :: labels(:), aexpr
      character(len=len_name) :: aname
      character(len=len_name) :: argn

      allocate (res(0))
      idx = user_func_index(fname)
      if (idx == 0) then
         print *, "Error: function '"//trim(fname)//"' not defined"
         eval_error = .true.
         return
      end if

      if (len_trim(args_text) == 0) then
         n_args = 0
      else
         call split_by_comma(args_text, n_args, labels)
      end if
      n_required = 0
      do i = 1, user_funcs(idx)%nargs
         if (.not. user_funcs(idx)%has_default(i)) n_required = i
      end do
      if (n_args < n_required .or. n_args > user_funcs(idx)%nargs) then
         print *, "Error: function '"//trim(fname)//"' expects between", n_required, "and", user_funcs(idx)%nargs, "arguments"
         eval_error = .true.
         return
      end if

      saved_n_vars = n_vars
      saved_vars = vars
      saved_exit_loop = exit_loop
      saved_cycle_loop = cycle_loop
      saved_exit_target = exit_target_depth
      saved_cycle_target = cycle_target_depth
      saved_in_user = in_user_function
      in_user_function = .true.
      exit_loop = .false.
      cycle_loop = .false.
      provided = .false.
      saw_named = .false.

      do i = 1, n_args
         call parse_call_actual(labels(i), is_named, aname, aexpr, ok_named)
         if (.not. ok_named) then
            print *, "Error: bad argument syntax in function call to '", trim(fname), "'"
            vars = saved_vars
            n_vars = saved_n_vars
            exit_loop = saved_exit_loop
            cycle_loop = saved_cycle_loop
            exit_target_depth = saved_exit_target
            cycle_target_depth = saved_cycle_target
            in_user_function = saved_in_user
            eval_error = .true.
            res = [bad_value]
            return
         end if
         if (is_named) then
            saw_named = .true.
            pos = 0
            do k = 1, user_funcs(idx)%nargs
               if (trim(user_funcs(idx)%args(k)) == trim(aname)) then
                  pos = k
                  exit
               end if
            end do
            if (pos == 0) then
               print *, "Error: unknown named argument '", trim(aname), "' in function call to '", trim(fname), "'"
               vars = saved_vars
               n_vars = saved_n_vars
               exit_loop = saved_exit_loop
               cycle_loop = saved_cycle_loop
               exit_target_depth = saved_exit_target
               cycle_target_depth = saved_cycle_target
               in_user_function = saved_in_user
               eval_error = .true.
               res = [bad_value]
               return
            end if
         else
            if (saw_named) then
               print *, "Error: positional arguments cannot follow named arguments in function call to '", trim(fname), "'"
               vars = saved_vars
               n_vars = saved_n_vars
               exit_loop = saved_exit_loop
               cycle_loop = saved_cycle_loop
               exit_target_depth = saved_exit_target
               cycle_target_depth = saved_cycle_target
               in_user_function = saved_in_user
               eval_error = .true.
               res = [bad_value]
               return
            end if
            pos = 0
            do k = 1, user_funcs(idx)%nargs
               if (.not. provided(k)) then
                  pos = k
                  exit
               end if
            end do
            aexpr = adjustl(trim(labels(i)))
         end if
         if (pos == 0 .or. provided(pos)) then
            print *, "Error: duplicate or invalid argument in function call to '", trim(fname), "'"
            vars = saved_vars
            n_vars = saved_n_vars
            exit_loop = saved_exit_loop
            cycle_loop = saved_cycle_loop
            exit_target_depth = saved_exit_target
            cycle_target_depth = saved_cycle_target
            in_user_function = saved_in_user
            eval_error = .true.
            res = [bad_value]
            return
         end if
         tmp = evaluate(aexpr)
         if (eval_error) then
            vars = saved_vars
            n_vars = saved_n_vars
            exit_loop = saved_exit_loop
            cycle_loop = saved_cycle_loop
            exit_target_depth = saved_exit_target
            cycle_target_depth = saved_cycle_target
            in_user_function = saved_in_user
            res = [bad_value]
            return
         end if
         arg_vals(pos)%v = tmp
         provided(pos) = .true.
      end do
      do i = 1, user_funcs(idx)%nargs
         if (provided(i)) cycle
         if (.not. user_funcs(idx)%has_default(i)) then
            print *, "Error: missing required argument '", trim(user_funcs(idx)%args(i)), "' in function call to '", trim(fname), "'"
            vars = saved_vars
            n_vars = saved_n_vars
            exit_loop = saved_exit_loop
            cycle_loop = saved_cycle_loop
            exit_target_depth = saved_exit_target
            cycle_target_depth = saved_cycle_target
            in_user_function = saved_in_user
            eval_error = .true.
            res = [bad_value]
            return
         end if
         tmp = evaluate(trim(user_funcs(idx)%defaults(i)))
         if (eval_error) then
            vars = saved_vars
            n_vars = saved_n_vars
            exit_loop = saved_exit_loop
            cycle_loop = saved_cycle_loop
            exit_target_depth = saved_exit_target
            cycle_target_depth = saved_cycle_target
            in_user_function = saved_in_user
            res = [bad_value]
            return
         end if
         arg_vals(i)%v = tmp
         provided(i) = .true.
      end do
      do i = 1, max_vars
         vars(i)%name = ""
         vars(i)%is_const = .false.
         if (allocated(vars(i)%val)) deallocate (vars(i)%val)
      end do
      n_vars = 0
      do i = 1, user_funcs(idx)%nargs
         call set_variable(trim(user_funcs(idx)%args(i)), arg_vals(i)%v, is_const=.true.)
         if (eval_error) then
            vars = saved_vars
            n_vars = saved_n_vars
            exit_loop = saved_exit_loop
            cycle_loop = saved_cycle_loop
            exit_target_depth = saved_exit_target
            cycle_target_depth = saved_cycle_target
            in_user_function = saved_in_user
            res = [bad_value]
            return
         end if
      end do

      call set_variable(trim(fname), [bad_value], is_const=.false.)
      call run_loop_body(user_funcs(idx)%body)

      if (eval_error) then
         vars = saved_vars
         n_vars = saved_n_vars
         exit_loop = saved_exit_loop
         cycle_loop = saved_cycle_loop
         exit_target_depth = saved_exit_target
         cycle_target_depth = saved_cycle_target
         in_user_function = saved_in_user
         res = [bad_value]
         return
      end if

      argn = trim(fname)
      do i = 1, n_vars
         if (trim(vars(i)%name) == trim(argn)) then
            if (allocated(vars(i)%val)) then
               ret = vars(i)%val
            else
               ret = [bad_value]
            end if
            exit
         end if
      end do
      if (.not. allocated(ret)) then
         print *, "Error: function '"//trim(fname)//"' did not assign a return value"
         eval_error = .true.
         ret = [bad_value]
      end if

      vars = saved_vars
      n_vars = saved_n_vars
      exit_loop = saved_exit_loop
      cycle_loop = saved_cycle_loop
      exit_target_depth = saved_exit_target
      cycle_target_depth = saved_cycle_target
      in_user_function = saved_in_user

      res = ret
   end function call_user_function

   subroutine parse_subroutine_header(line, sname, nargs, sargs, sintents, shas_default, sdefaults, ok)
      character(len=*), intent(in) :: line
      character(len=len_name), intent(out) :: sname
      integer, intent(out) :: nargs
      character(len=len_name), intent(out) :: sargs(max_sub_args)
      integer, intent(out) :: sintents(max_sub_args)
      logical, intent(out) :: shas_default(max_sub_args)
      character(len=len_default_expr), intent(out) :: sdefaults(max_sub_args)
      logical, intent(out) :: ok
      character(len=:), allocatable :: tl, low, inside, part, argname, defexpr
      character(len=:), allocatable :: parts(:)
      integer :: p1, p2, i, n, peq
      logical :: seen_default

      ok = .false.
      sname = ""
      nargs = 0
      sargs = ""
      sintents = 0
      shas_default = .false.
      sdefaults = ""
      tl = adjustl(trim(line))
      low = lower_str(tl)
      if (index(low, "subroutine ") /= 1) return
      p1 = index(tl, "(")
      p2 = scan(tl, ")", back=.true.)
      if (p1 <= 11 .or. p2 <= p1) return
      sname = adjustl(trim(tl(12:p1 - 1)))
      if (.not. is_alnum_string(sname)) return
      if (len_trim(tl(p2 + 1:)) > 0) return
      inside = trim(tl(p1 + 1:p2 - 1))
      if (len_trim(inside) == 0) then
         ok = .true.
         return
      end if
      call split_by_comma(inside, n, parts)
      if (n < 1 .or. n > max_sub_args) return
      seen_default = .false.
      do i = 1, n
         part = trim(parts(i))
         peq = index(part, "=")
         if (peq == 0) then
            if (seen_default) return
            if (.not. is_alnum_string(part)) return
            sargs(i) = part
            sintents(i) = 2
         else
            argname = adjustl(trim(part(1:peq - 1)))
            defexpr = adjustl(trim(part(peq + 1:)))
            if (.not. is_alnum_string(argname)) return
            if (len_trim(defexpr) == 0) return
            if (len_trim(defexpr) > len_default_expr) then
               print *, "Error: default expression too long in subroutine header"
               eval_error = .true.
               return
            end if
            sargs(i) = argname
            sintents(i) = 2
            shas_default(i) = .true.
            sdefaults(i) = defexpr
            seen_default = .true.
         end if
      end do
      nargs = n
      ok = .true.
   end subroutine parse_subroutine_header

   subroutine parse_sub_intent_decl(line, args, nargs, intents, ok, fatal)
      character(len=*), intent(in) :: line
      character(len=len_name), intent(in) :: args(max_sub_args)
      integer, intent(in) :: nargs
      integer, intent(inout) :: intents(max_sub_args)
      logical, intent(out) :: ok, fatal
      character(len=:), allocatable :: tl, low, smode, compact, rest, vars_part
      character(len=:), allocatable :: vars_l(:)
      integer :: p1, p2, nvars, i, j, mode, idx

      ok = .false.
      fatal = .false.
      tl = adjustl(trim(line))
      if (len_trim(tl) == 0) return
      low = lower_str(tl)
      if (index(low, "intent(") /= 1) return

      p1 = index(tl, "(")
      p2 = scan(tl, ")", back=.true.)
      if (p1 <= 0 .or. p2 <= p1) then
         fatal = .true.; return
      end if
      smode = lower_str(adjustl(trim(tl(p1 + 1:p2 - 1))))
      compact = ""
      do i = 1, len_trim(smode)
         if (smode(i:i) /= " ") compact = compact//smode(i:i)
      end do
      select case (trim(compact))
      case ("in")
         mode = 1
      case ("inout")
         mode = 2
      case ("out")
         mode = 3
      case default
         fatal = .true.; return
      end select

      rest = adjustl(trim(tl(p2 + 1:)))
      if (index(rest, "::") /= 1) then
         fatal = .true.; return
      end if
      vars_part = adjustl(trim(rest(3:)))
      if (len_trim(vars_part) == 0) then
         fatal = .true.; return
      end if
      call split_by_comma(vars_part, nvars, vars_l)
      if (nvars < 1) then
         fatal = .true.; return
      end if
      do i = 1, nvars
         if (.not. is_alnum_string(trim(vars_l(i)))) then
            fatal = .true.; return
         end if
         idx = 0
         do j = 1, nargs
            if (trim(args(j)) == trim(vars_l(i))) then
               idx = j
               exit
            end if
         end do
         if (idx == 0) then
            print *, "Error: intent declaration references unknown argument '", trim(vars_l(i)), "'"
            fatal = .true.
            return
         end if
         intents(idx) = mode
      end do
      ok = .true.
   end subroutine parse_sub_intent_decl

   subroutine strip_sub_intent_lines(raw_body, args, nargs, intents, exec_body, ok)
      character(len=*), intent(in) :: raw_body
      character(len=len_name), intent(in) :: args(max_sub_args)
      integer, intent(in) :: nargs
      integer, intent(inout) :: intents(max_sub_args)
      character(len=32768), intent(out) :: exec_body
      logical, intent(out) :: ok
      integer :: p1, p2, nlen
      character(len=:), allocatable :: line, tline
      logical :: is_intent, fatal, seen_exec

      exec_body = ""
      ok = .true.
      seen_exec = .false.
      nlen = len_trim(raw_body)
      p1 = 1
      do while (p1 <= nlen)
         p2 = index(raw_body(p1:), new_line("a"))
         if (p2 == 0) then
            line = raw_body(p1:nlen)
         else
            line = raw_body(p1:p1 + p2 - 2)
         end if
         tline = adjustl(trim(line))
         call parse_sub_intent_decl(tline, args, nargs, intents, is_intent, fatal)
         if (fatal) then
            print *, "Error: bad intent declaration in subroutine: ", trim(tline)
            ok = .false.
            return
         end if
         if (is_intent) then
            if (seen_exec) then
               print *, "Error: intent declarations must appear before executable statements"
               ok = .false.
               return
            end if
         else
            if (len_trim(tline) > 0) seen_exec = .true.
            if (len_trim(exec_body) + len_trim(line) + 1 > len(exec_body)) then
               print *, "Error: subroutine body too large"
               ok = .false.
               return
            end if
            exec_body = trim(exec_body)//trim(line)//new_line("a")
         end if
         if (p2 == 0) exit
         p1 = p1 + p2
      end do
   end subroutine strip_sub_intent_lines

   integer function user_sub_index(sname) result(idx)
      character(len=*), intent(in) :: sname
      integer :: i
      idx = 0
      do i = 1, n_user_subs
         if (trim(user_subs(i)%name) == trim(sname)) then
            idx = i
            return
         end if
      end do
   end function user_sub_index

   subroutine set_user_subroutine(sname, nargs, sargs, sintents, shas_default, sdefaults, body)
      character(len=*), intent(in) :: sname
      integer, intent(in) :: nargs
      character(len=*), intent(in) :: sargs(:)
      integer, intent(in) :: sintents(:)
      logical, intent(in) :: shas_default(:)
      character(len=*), intent(in) :: sdefaults(:)
      character(len=*), intent(in) :: body
      integer :: i, idx

      idx = user_sub_index(sname)
      if (idx == 0) then
         if (n_user_subs >= max_subs) then
            print *, "Error: maximum number of user subroutines reached"
            eval_error = .true.
            return
         end if
         n_user_subs = n_user_subs + 1
         idx = n_user_subs
      end if
      user_subs(idx)%name = trim(sname)
      user_subs(idx)%nargs = nargs
      user_subs(idx)%args = ""
      user_subs(idx)%intents = 0
      user_subs(idx)%has_default = .false.
      user_subs(idx)%defaults = ""
      do i = 1, nargs
         user_subs(idx)%args(i) = trim(sargs(i))
         user_subs(idx)%intents(i) = sintents(i)
         user_subs(idx)%has_default(i) = shas_default(i)
         user_subs(idx)%defaults(i) = trim(sdefaults(i))
      end do
      user_subs(idx)%body = body
   end subroutine set_user_subroutine

   subroutine call_user_subroutine(sname, args_text)
      character(len=*), intent(in) :: sname, args_text
      type(var_t) :: saved_vars(max_vars)
      type(arr_t) :: actual_vals(max_sub_args), wb_vals(max_sub_args)
      integer :: saved_n_vars, idx, i, j, k, n_args, n_required, pos
      integer :: saved_exit_target, saved_cycle_target
      logical :: saved_exit_loop, saved_cycle_loop, saved_in_user, saved_in_user_sub
      character(len=len_name) :: saved_active_sub_name
      integer :: saved_active_sub_nargs
      character(len=len_name) :: saved_active_sub_args(max_sub_args)
      integer :: saved_active_sub_intents(max_sub_args)
      logical :: saved_active_sub_set(max_sub_args)
      character(len=:), allocatable :: labels(:), tok, aexpr
      character(len=len_name) :: aname
      logical :: provided(max_sub_args), is_named, ok_named, saw_named
      character(len=4096) :: actual_expr(max_sub_args)
      character(len=len_name) :: tgt(max_sub_args)
      logical :: needs_write(max_sub_args)
      real(kind=dp), allocatable :: tmp(:)

      idx = user_sub_index(sname)
      if (idx == 0) then
         print *, "Error: subroutine '"//trim(sname)//"' not defined"
         eval_error = .true.
         return
      end if

      if (len_trim(args_text) == 0) then
         n_args = 0
      else
         call split_by_comma(args_text, n_args, labels)
      end if
      n_required = 0
      do i = 1, user_subs(idx)%nargs
         if (.not. user_subs(idx)%has_default(i)) n_required = i
      end do
      if (n_args < n_required .or. n_args > user_subs(idx)%nargs) then
         print *, "Error: subroutine '"//trim(sname)//"' expects between", n_required, "and", user_subs(idx)%nargs, "arguments"
         eval_error = .true.
         return
      end if

      saved_n_vars = n_vars
      saved_vars = vars
      saved_exit_loop = exit_loop
      saved_cycle_loop = cycle_loop
      saved_exit_target = exit_target_depth
      saved_cycle_target = cycle_target_depth
      saved_in_user = in_user_function
      saved_in_user_sub = in_user_subroutine
      saved_active_sub_name = active_sub_name
      saved_active_sub_nargs = active_sub_nargs
      saved_active_sub_args = active_sub_args
      saved_active_sub_intents = active_sub_intents
      saved_active_sub_set = active_sub_set
      needs_write = .false.
      tgt = ""
      provided = .false.
      actual_expr = ""
      saw_named = .false.

      do i = 1, n_args
         call parse_call_actual(labels(i), is_named, aname, aexpr, ok_named)
         if (.not. ok_named) then
            print *, "Error: bad argument syntax in subroutine call to '", trim(sname), "'"
            eval_error = .true.
            exit
         end if
         if (is_named) then
            saw_named = .true.
            pos = 0
            do k = 1, user_subs(idx)%nargs
               if (trim(user_subs(idx)%args(k)) == trim(aname)) then
                  pos = k
                  exit
               end if
            end do
            if (pos == 0) then
               print *, "Error: unknown named argument '", trim(aname), "' in subroutine call to '", trim(sname), "'"
               eval_error = .true.
               exit
            end if
         else
            if (saw_named) then
               print *, "Error: positional arguments cannot follow named arguments in subroutine call to '", trim(sname), "'"
               eval_error = .true.
               exit
            end if
            pos = 0
            do k = 1, user_subs(idx)%nargs
               if (.not. provided(k)) then
                  pos = k
                  exit
               end if
            end do
            aexpr = adjustl(trim(labels(i)))
         end if
         if (pos == 0 .or. provided(pos)) then
            print *, "Error: duplicate or invalid argument in subroutine call to '", trim(sname), "'"
            eval_error = .true.
            exit
         end if
         provided(pos) = .true.
         actual_expr(pos) = trim(aexpr)
      end do
      if (eval_error) then
         vars = saved_vars
         n_vars = saved_n_vars
         exit_loop = saved_exit_loop
         cycle_loop = saved_cycle_loop
         exit_target_depth = saved_exit_target
         cycle_target_depth = saved_cycle_target
         in_user_function = saved_in_user
         in_user_subroutine = saved_in_user_sub
         active_sub_name = saved_active_sub_name
         active_sub_nargs = saved_active_sub_nargs
         active_sub_args = saved_active_sub_args
         active_sub_intents = saved_active_sub_intents
         active_sub_set = saved_active_sub_set
         return
      end if

      do i = 1, user_subs(idx)%nargs
         if (provided(i)) then
            tok = trim(actual_expr(i))
         else
            tok = ""
         end if
         select case (user_subs(idx)%intents(i))
         case (1) ! in
            if (provided(i)) then
               actual_vals(i)%v = evaluate(tok)
            else
               if (.not. user_subs(idx)%has_default(i)) then
                  print *, "Error: missing required argument '", trim(user_subs(idx)%args(i)), "' in subroutine call to '", trim(sname), "'"
                  eval_error = .true.
                  exit
               end if
               actual_vals(i)%v = evaluate(trim(user_subs(idx)%defaults(i)))
            end if
            if (eval_error) exit
         case (2) ! inout
            if (.not. provided(i)) then
               print *, "Error: missing inout argument '", trim(user_subs(idx)%args(i)), "' in subroutine call to '", trim(sname), "'"
               eval_error = .true.
               exit
            end if
            if (.not. is_alnum_string(tok)) then
               print *, "Error: inout argument must be a variable name"
               eval_error = .true.
               exit
            end if
            tmp = evaluate(tok)
            if (eval_error) exit
            actual_vals(i)%v = tmp
            needs_write(i) = .true.
            tgt(i) = trim(tok)
         case (3) ! out
            if (.not. provided(i)) then
               print *, "Error: missing out argument '", trim(user_subs(idx)%args(i)), "' in subroutine call to '", trim(sname), "'"
               eval_error = .true.
               exit
            end if
            if (.not. is_alnum_string(tok)) then
               print *, "Error: out argument must be a variable name"
               eval_error = .true.
               exit
            end if
            actual_vals(i)%v = [bad_value]
            needs_write(i) = .true.
            tgt(i) = trim(tok)
         end select
      end do

      if (.not. eval_error) then
         do i = 1, max_vars
            vars(i)%name = ""
            vars(i)%is_const = .false.
            if (allocated(vars(i)%val)) deallocate (vars(i)%val)
         end do
         n_vars = 0
         ! Subroutines execute with normal interactive printing semantics.
         in_user_function = .false.
         exit_loop = .false.
         cycle_loop = .false.
         do i = 1, user_subs(idx)%nargs
            call set_variable(trim(user_subs(idx)%args(i)), actual_vals(i)%v, is_const=(user_subs(idx)%intents(i) == 1))
            if (eval_error) exit
         end do
         active_sub_nargs = user_subs(idx)%nargs
         active_sub_name = trim(sname)
         active_sub_args = ""
         active_sub_intents = 0
         active_sub_set = .false.
         if (user_subs(idx)%nargs > 0) then
            active_sub_args(1:user_subs(idx)%nargs) = user_subs(idx)%args(1:user_subs(idx)%nargs)
            active_sub_intents(1:user_subs(idx)%nargs) = user_subs(idx)%intents(1:user_subs(idx)%nargs)
            do i = 1, user_subs(idx)%nargs
               active_sub_set(i) = (active_sub_intents(i) /= 3)
            end do
         end if
         in_user_subroutine = .true.
      end if

      if (.not. eval_error) call run_loop_body(user_subs(idx)%body)

      if (.not. eval_error) then
         do i = 1, user_subs(idx)%nargs
            if (.not. needs_write(i)) cycle
            wb_vals(i)%v = [bad_value]
            do j = 1, n_vars
               if (trim(vars(j)%name) == trim(user_subs(idx)%args(i))) then
                  wb_vals(i)%v = vars(j)%val
                  exit
               end if
            end do
         end do
      end if

      vars = saved_vars
      n_vars = saved_n_vars
      if (.not. eval_error) then
         do i = 1, user_subs(idx)%nargs
            if (.not. needs_write(i)) cycle
            call set_variable(trim(tgt(i)), wb_vals(i)%v)
            if (eval_error) exit
         end do
      end if

      exit_loop = saved_exit_loop
      cycle_loop = saved_cycle_loop
      exit_target_depth = saved_exit_target
      cycle_target_depth = saved_cycle_target
      in_user_function = saved_in_user
      in_user_subroutine = saved_in_user_sub
      active_sub_name = saved_active_sub_name
      active_sub_nargs = saved_active_sub_nargs
      active_sub_args = saved_active_sub_args
      active_sub_intents = saved_active_sub_intents
      active_sub_set = saved_active_sub_set
   end subroutine call_user_subroutine

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

   pure logical function is_end_subroutine_line(tl) result(ok)
      character(len=*), intent(in) :: tl
      character(len=:), allocatable :: t
      t = trim(lower_str(adjustl(tl)))
      ok = (t == "end subroutine" .or. t == "endsubroutine" .or. t == "end subroutine;" .or. t == "endsubroutine;")
      if (.not. ok) ok = (index(t, "end subroutine ") == 1)
   end function is_end_subroutine_line

   pure logical function is_op_char(ch) result(ok)
      character(len=1), intent(in) :: ch
      ok = (index("+-*/^<>=:&|,", ch) > 0)
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
      integer :: p_eq, p_com1, p_com2, i, n_rhs, dpar, dbr
      logical :: in_quote
      character(len=1) :: qchar, ch

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
      n_rhs = len_trim(rhs)
      p_com1 = 0
      p_com2 = 0
      dpar = 0
      dbr = 0
      in_quote = .false.
      qchar = char(0)
      do i = 1, n_rhs
         ch = rhs(i:i)
         if (in_quote) then
            if (ch == qchar) in_quote = .false.
            cycle
         end if
         if (ch == '"' .or. ch == "'") then
            in_quote = .true.
            qchar = ch
            cycle
         end if
         select case (ch)
         case ("(")
            dpar = dpar + 1
         case (")")
            if (dpar > 0) dpar = dpar - 1
         case ("[")
            dbr = dbr + 1
         case ("]")
            if (dbr > 0) dbr = dbr - 1
         case (",")
            if (dpar == 0 .and. dbr == 0) then
               if (p_com1 == 0) then
                  p_com1 = i
               else if (p_com2 == 0) then
                  p_com2 = i
               else
                  return
               end if
            end if
         end select
      end do
      if (p_com1 == 0) return
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
         if (.not. is_assignment_lhs(lhs)) then
            eqpos = 0
         end if
      end if
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

      pure logical function is_assignment_lhs(lhs_txt) result(ok_lhs)
         character(len=*), intent(in) :: lhs_txt
         character(len=:), allocatable :: lt, name
         integer :: p1, p2
         lt = adjustl(trim(lhs_txt))
         ok_lhs = .false.
         if (len_trim(lt) == 0) return
         if (is_alnum_string(lt)) then
            ok_lhs = .true.
            return
         end if
         p1 = index(lt, "(")
         p2 = scan(lt, ")", back=.true.)
         if (p1 > 1 .and. p2 == len_trim(lt) .and. p2 > p1) then
            name = adjustl(trim(lt(1:p1 - 1)))
            if (is_alnum_string(name)) ok_lhs = .true.
         end if
      end function is_assignment_lhs

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
         integer :: i, ios
         real(kind=dp) :: tmp
         call skip_spaces()
         i = 0
         do while (is_numeral(curr_char) .or. curr_char == ".")
            i = i + 1
            buf(i:i) = curr_char
            call next_char()
         end do
         if (i <= 0) then
            eval_error = .true.
            print *, "Error: expected numeric literal"
            num = [bad_value]
            return
         end if
         read (buf(1:i), *, iostat=ios) tmp
         if (ios /= 0) then
            eval_error = .true.
            print *, "Error: invalid numeric literal: ", trim(buf(1:i))
            num = [bad_value]
            return
         end if
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
         integer :: i, j

         do i = 1, n_vars
            if (vars(i)%name == name) then
               if (in_user_subroutine) then
                  do j = 1, active_sub_nargs
                     if (trim(active_sub_args(j)) == trim(name)) then
                        if (active_sub_intents(j) == 3 .and. .not. active_sub_set(j)) then
                           print *, "Error: intent(out) argument '", trim(name), "' used before assignment in subroutine '", trim(active_sub_name), "'"
                           eval_error = .true.
                           v = [bad_value]
                           return
                        end if
                        exit
                     end if
                  end do
               end if
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
         integer :: nsize, pstart, pend, depth, n1, n2, dim_val, nstart_i, p0
         integer :: n_args, i_arg
         logical :: is_neg, have_second, verbose_opt, plot_opt, skip_positional
         logical :: toplevel_colon, toplevel_comma, have_dim
         character(len=len_name) :: look_name    ! NEW
      type(arr_t), allocatable :: args(:)
      character(len=:), allocatable :: labels(:), pred_labels(:)
         f = [real(kind=dp) ::]
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
            p0 = pos - 1
            if (p0 >= 1) then
               if (p0 + 5 <= lenstr) then
                  if (lower_str(expr(p0:p0 + 5)) == ".true.") then
                     f = [1.0_dp]
                     call advance_token(6)
                     return
                  end if
               end if
               if (p0 + 6 <= lenstr) then
                  if (lower_str(expr(p0:p0 + 6)) == ".false.") then
                     f = [0.0_dp]
                     call advance_token(7)
                     return
                  end if
               end if
            end if
            if (is_numeral(curr_char) .or. starts_decimal_literal()) then
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
                        integer                       :: q1, q2, save_pos, start_pos, end_pos
                        character(len=1)              :: quote
                        character(len=len_name)       :: kw

                        icol = 1                     ! default column
                        call skip_spaces()
                        ! ---- first argument : quoted or bare file name ------------
                        start_pos = pos - 1
                        do while (start_pos <= lenstr)
                           if (expr(start_pos:start_pos) /= " ") exit
                           start_pos = start_pos + 1
                        end do
                        if (start_pos > lenstr) then
                           print *, "Error: read(): first argument must be a quoted file name"
                           eval_error = .true.; f = [bad_value]; return
                        end if

                        q1 = index(expr(start_pos:), '"')
                        if (q1 > 0) q1 = q1 + start_pos - 1
                        q2 = index(expr(start_pos:), "'")
                        if (q2 > 0) q2 = q2 + start_pos - 1

                        if (q1 > 0 .or. q2 > 0) then
                           if (q1 == 0) then
                              q1 = q2
                              quote = "'"
                           else if (q2 == 0) then
                              quote = '"'
                           else if (q1 <= q2) then
                              quote = '"'
                           else
                              q1 = q2
                              quote = "'"
                           end if

                           q2 = index(expr(q1 + 1:), quote)
                           if (q2 == 0) then
                              print *, "Error: unmatched quote in read()"
                              eval_error = .true.; f = [bad_value]; return
                           end if
                           q2 = q2 + q1
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
                        else
                           end_pos = start_pos
                           do while (end_pos <= lenstr)
                              if (expr(end_pos:end_pos) == "," .or. &
                                  expr(end_pos:end_pos) == ")" .or. &
                                  expr(end_pos:end_pos) == " ") exit
                              end_pos = end_pos + 1
                           end do
                           if (end_pos <= start_pos) then
                              print *, "Error: read(): first argument must be a quoted file name"
                              eval_error = .true.; f = [bad_value]; return
                           end if
                           fname = expr(start_pos:end_pos - 1)
                           if (debug_read) then
                              print *, "fname =", trim(fname)
                           end if
                           pos = end_pos
                           if (pos > lenstr) then
                              curr_char = char(0)
                           else
                              curr_char = expr(pos:pos); pos = pos + 1
                           end if
                           call skip_spaces()
                        end if

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

                  ! Canonicalize common mssk aliases.
                  select case (trim(id))
                  case ("mssk_normal", "mssk_gaussian")
                     id = "mssk_norm"
                  case ("mssk_uniform")
                     id = "mssk_unif"
                  case ("mssk_exponential")
                     id = "mssk_exp"
                  case ("mssk_lognormal")
                     id = "mssk_lnorm"
                  case ("mssk_logistic")
                     id = "mssk_logis"
                  case ("mssk_chi2", "mssk_chisquare")
                     id = "mssk_chisq"
                  case ("mssk_hyperbolic")
                     id = "mssk_hyperb"
                  case ("mssk_student_t")
                     id = "mssk_t"
                  case ("mssk_noncentral_t")
                     id = "mssk_nct"
                  end select

                  !============ ZERO-ARGUMENT SPECIAL CASE ======================
                  if (curr_char == ")") then         !  e.g. runif()
                     call next_char()                !  consume ")"
                     select case (trim(id))
                     case ("runif")
                        f = [runif1()]
                     case ("rnorm")
                        f = random_normal(1)
                     case ("mssk_exp")
                        f = mssk_exp(1.0_dp)
                     case ("mssk_unif")
                        f = mssk_unif(0.0_dp, 1.0_dp)
                     case ("mssk_norm")
                        f = mssk_norm(0.0_dp, 1.0_dp)
                     case ("mssk_lnorm")
                        f = mssk_lnorm(0.0_dp, 1.0_dp)
                     case ("mssk_logis")
                        f = mssk_logis(0.0_dp, 1.0_dp)
                     case ("mssk_sech")
                        f = mssk_sech()
                     case ("mssk_laplace")
                        f = mssk_laplace(0.0_dp, 1.0_dp)
                     case ("mssk_cauchy")
                        f = mssk_cauchy(0.0_dp, 1.0_dp)
                     case default
                        if (user_func_index(trim(id)) > 0) then
                           f = call_user_function(trim(id), "")
                        else
                           print *, "Error: function '"//trim(id)//"' needs arguments"
                           eval_error = .true.
                           f = [bad_value]
                        end if
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

                  ! User-defined functions: pass raw argument text through so
                  ! named/default argument handling stays in call_user_function.
                  if (user_func_index(trim(id)) > 0) then
                     f = call_user_function(trim(id), expr(pstart:pend - 1))
                     pos = pend + 1
                     if (pos > lenstr) then
                        curr_char = char(0)
                     else
                        curr_char = expr(pos:pos); pos = pos + 1
                     end if
                     return
                  end if

                  if (trim(id) == "seed") then
                     block
                        integer :: n_args, iseed, nburn
                        logical :: print_seed
                        character(len=:), allocatable :: labels(:)
                        real(kind=dp), allocatable :: tmp(:)
                        call split_by_comma(expr(pstart:pend - 1), n_args, labels)
                        if (n_args < 1 .or. n_args > 3) then
                           print *, "Error: seed() expects 1 to 3 arguments"
                           eval_error = .true.; f = [bad_value]; return
                        end if
                        tmp = evaluate(adjustl(labels(1)))
                        if (eval_error .or. size(tmp) /= 1) then
                           print *, "Error: seed() requires scalar seed"
                           eval_error = .true.; f = [bad_value]; return
                        end if
                        iseed = nint(tmp(1))
                        nburn = 0
                        print_seed = .false.
                        if (n_args >= 2) then
                           tmp = evaluate(adjustl(labels(2)))
                           if (eval_error .or. size(tmp) /= 1) then
                              print *, "Error: seed() nburn must be scalar"
                              eval_error = .true.; f = [bad_value]; return
                           end if
                           nburn = nint(tmp(1))
                        end if
                        if (n_args >= 3) then
                           tmp = evaluate(adjustl(labels(3)))
                           if (eval_error .or. size(tmp) /= 1) then
                              print *, "Error: seed() print_seed must be scalar"
                              eval_error = .true.; f = [bad_value]; return
                           end if
                           print_seed = (tmp(1) /= 0.0_dp)
                        end if
                        if (n_args == 1) then
                           call random_seed_init(iseed)
                        else if (n_args == 2) then
                           call random_seed_init(iseed, nburn=nburn)
                        else
                           call random_seed_init(iseed, nburn=nburn, print_seed=print_seed)
                        end if
                        f = [0.0_dp]
                        pos = pend + 1
                        if (pos > lenstr) then
                           curr_char = char(0)
                        else
                           curr_char = expr(pos:pos); pos = pos + 1
                        end if
                        return
                     end block
                  end if

                  !------------- first argument -----------------------------------
                  skip_positional = .false.
                  if (trim(id) == "armaspec") then
                     block
                        integer :: nargs_tmp, eq_tmp
                        character(len=:), allocatable :: tok_tmp, key_tmp
                        character(len=:), allocatable :: labs_tmp(:)
                        call split_by_comma(expr(pstart:pend - 1), nargs_tmp, labs_tmp)
                        if (nargs_tmp >= 1) then
                           tok_tmp = adjustl(labs_tmp(1))
                           eq_tmp = index(tok_tmp, "=")
                           if (eq_tmp > 0) then
                              key_tmp = lower_str(adjustl(tok_tmp(:eq_tmp - 1)))
                              if (index(key_tmp, "ar") == 1 .or. index(key_tmp, "ma") == 1) then
                                 skip_positional = .true.
                              end if
                           end if
                        end if
                     end block
                  end if

                  if (trim(id) == "distaicscan") then
                     skip_positional = .true.
                  end if
                  if (trim(id) == "dist_regress") then
                     skip_positional = .true.
                  end if
                  if (trim(id) == "armastab") then
                     skip_positional = .true.
                  end if
                  if (trim(id) == "mssk_unif" .or. trim(id) == "mssk_norm" .or. trim(id) == "mssk_exp" .or. &
                      trim(id) == "mssk_lnorm" .or. trim(id) == "mssk_logis" .or. trim(id) == "mssk_laplace" .or. &
                      trim(id) == "mssk_cauchy") then
                     skip_positional = .true.
                  end if

                  if (.not. skip_positional) then
                     arg1 = parse_expression()
                     if (eval_error) then
                        f = [bad_value]; return
                     end if
                     call skip_spaces()
                  end if
                  have_second = .false.

                  if (.not. skip_positional .and. curr_char == ",") then
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
                     else if (trim(id) == "armafitaic" .or. trim(id) == "araic" .or. trim(id) == "maaic" .or. trim(id) == "arspecaic" .or. trim(id) == "armaspec" .or. trim(id) == "armaspecaic" .or. trim(id) == "arma_mt_spec" .or. trim(id) == "armaaic_mt_spec" .or. trim(id) == "welchspec" .or. trim(id) == "pgramspec" .or. trim(id) == "mtspec" .or. trim(id) == "cpfit" .or. trim(id) == "cpfitaic" .or. trim(id) == "cpfit_aic" .or. trim(id) == "fit_t" .or. trim(id) == "fit_nct" .or. trim(id) == "huber_mean" .or. trim(id) == "bisquare_mean" .or. trim(id) == "mad") then
                        !------------------------------------------------------------
                        !  armafitaic/cpfit: allow keyword-only argument after first arg
                        !------------------------------------------------------------
                        block
                           integer :: save_pos
                           logical :: is_name_eq
                           save_pos = pos
                           call next_char()
                           call skip_spaces()
                           if (trim(id) == "cpfit" .or. trim(id) == "cpfitaic" .or. trim(id) == "cpfit_aic") then
                              pos = save_pos
                              curr_char = ","
                              have_second = .false.
                           else
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
                     block
                        logical :: is_named_method, parse_ok
                        integer :: i_m, i_tok, n_data
                        integer, allocatable :: data_idx(:)
                        character(len=len_name) :: method_name
                        character(len=:), allocatable :: method_spec
                        character(len=:), allocatable :: arg_expr
                        integer, allocatable :: method_codes(:)
                        character(len=128) :: method_err
                        character(len=:), allocatable :: corr_labels(:)

                        call split_by_comma(expr(pstart:pend - 1), n_args, labels)
                        allocate (method_codes(1))
                        method_codes = [1]
                        allocate (data_idx(max(1, n_args)))
                        n_data = 0

                        do i_tok = 1, n_args
                           call parse_call_actual(labels(i_tok), is_named_method, method_name, method_spec, parse_ok)
                           if (.not. parse_ok) then
                              print *, "Error: invalid named argument in cor()"
                              eval_error = .true.; f = [bad_value]; return
                           end if
                           if (is_named_method) then
                              if (trim(lower_str(method_name)) /= "method") then
                                 print *, "Error: unknown named argument '"//trim(method_name)//"' in cor()"
                                 eval_error = .true.; f = [bad_value]; return
                              end if
                              call parse_cor_methods_spec(method_spec, method_codes, parse_ok, method_err)
                              if (.not. parse_ok) then
                                 print *, trim(method_err)
                                 eval_error = .true.; f = [bad_value]; return
                              end if
                           else
                              n_data = n_data + 1
                              data_idx(n_data) = i_tok
                           end if
                        end do

                        if (n_data < 2) then
                           print *, "Error: cor() needs at least two arguments"
                           eval_error = .true.; f = [bad_value]; return
                        end if

                        allocate (args(n_data))
                        allocate (character(len=len(labels(1))) :: corr_labels(n_data))
                        do i_tok = 1, n_data
                           arg_expr = trim(labels(data_idx(i_tok)))
                           args(i_tok)%v = evaluate(arg_expr)
                           if (eval_error) then
                              f = [bad_value]; return
                           end if
                           corr_labels(i_tok) = trim(arg_expr)
                        end do

                        if (n_data == 2) then
                           if (allocated(f)) deallocate (f)
                           allocate (f(size(method_codes)))
                           do i_m = 1, size(method_codes)
                              f(i_m) = cor_by_method_xy(args(1)%v, args(2)%v, method_codes(i_m))
                           end do
                        else
                           call print_cor_matrix_args(args, corr_labels, method_codes)
                           if (eval_error) then
                              f = [bad_value]
                              return
                           end if
                           suppress_result = .true.
                           f = [real(kind=dp) ::]
                        end if

                        call skip_spaces()
                        if (curr_char == ",") then
                           do while (curr_char /= ")" .and. pos <= len_trim(expr))
                              call next_char()
                           end do
                        end if
                        if (curr_char == ")") call next_char()
                        return
                     end block
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
                        character(len=256) :: tok, ltok, rval
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

                        do_plot = .true.

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

                        do_plot = .true.

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

                  case ("arspec")
                     block
                        logical :: do_plot
                        integer :: eqpos, nfreq_i, j
                        integer, allocatable :: orders(:)
                        character(len=:), allocatable :: tok, ltok, rval, sp_title
                        character(len=16) :: method_s
                        character(len=16), allocatable :: legends(:)
                        real(kind=dp), allocatable :: tmp(:), freq(:), y2(:,:), sj(:)

                        do_plot = .false.
                        nfreq_i = 256
                        method_s = "ls"

                        call split_by_comma(expr(pstart:pend - 1), n_args, labels)
                        if (n_args > 5) then
                           print *, "Error: arspec() takes at most five arguments"
                           eval_error = .true.; f = [bad_value]
                        else
                           if (n_args >= 3) then
                              ! consume everything through ')' for optional-argument parsing
                              pos = pend + 1
                              if (pos > lenstr) then
                                 curr_char = char(0)
                              else
                                 curr_char = expr(pos:pos)
                                 pos = pos + 1
                              end if
                              do i_arg = 3, n_args
                                 tok = adjustl(labels(i_arg))
                                 ltok = lower_str(tok)
                                 eqpos = index(tok, "=")
                                 if (eqpos /= 0) then
                                    rval = adjustl(tok(eqpos + 1:))
                                    if (index(ltok, "plot") == 1) then
                                       rval = lower_str(rval)
                                       if (rval == ".false." .or. rval == "false" .or. rval == "f") then
                                          do_plot = .false.
                                       else if (rval == ".true." .or. rval == "true" .or. rval == "t") then
                                          do_plot = .true.
                                       else
                                          tmp = evaluate(rval)
                                          if (eval_error) then
                                             f = [bad_value]
                                             exit
                                          else if (size(tmp) /= 1) then
                                             print *, "Error: plot must be scalar"
                                             eval_error = .true.; f = [bad_value]
                                             exit
                                          else
                                             do_plot = (tmp(1) /= 0.0_dp)
                                          end if
                                       end if
                                    else if (index(ltok, "nfreq") == 1) then
                                        tmp = evaluate(rval)
                                        if (eval_error) then
                                           f = [bad_value]
                                           exit
                                       else if (size(tmp) /= 1) then
                                          print *, "Error: nfreq must be scalar"
                                          eval_error = .true.; f = [bad_value]
                                          exit
                                        else
                                           nfreq_i = nint(tmp(1))
                                        end if
                                    else if (index(ltok, "method") == 1) then
                                       method_s = lower_str(trim(rval))
                                       if (len_trim(method_s) >= 2) then
                                          if (method_s(1:1) == "'" .or. method_s(1:1) == '"') method_s = method_s(2:)
                                          if (method_s(len_trim(method_s):len_trim(method_s)) == "'" .or. method_s(len_trim(method_s):len_trim(method_s)) == '"') &
                                             method_s = method_s(:len_trim(method_s) - 1)
                                       end if
                                    else
                                       print *, "Error: unknown named argument in arspec()"
                                       eval_error = .true.; f = [bad_value]
                                       exit
                                    end if
                                 else
                                    if (i_arg == 3) then
                                       if (ltok == ".false." .or. ltok == "false" .or. ltok == "f") then
                                          do_plot = .false.
                                       else if (ltok == ".true." .or. ltok == "true" .or. ltok == "t") then
                                          do_plot = .true.
                                       else
                                          tmp = evaluate(tok)
                                          if (eval_error) then
                                             f = [bad_value]
                                             exit
                                          else if (size(tmp) /= 1) then
                                             print *, "Error: third argument of arspec() must be scalar"
                                             eval_error = .true.; f = [bad_value]
                                             exit
                                          else
                                             nfreq_i = nint(tmp(1))
                                          end if
                                       end if
                                    else if (i_arg == 4) then
                                       tmp = evaluate(tok)
                                       if (eval_error) then
                                          f = [bad_value]
                                          exit
                                       else if (size(tmp) /= 1) then
                                          print *, "Error: plot must be scalar"
                                          eval_error = .true.; f = [bad_value]
                                          exit
                                       else
                                          do_plot = (tmp(1) /= 0.0_dp)
                                       end if
                                    end if
                                 end if
                              end do
                           end if

                           if (.not. eval_error) then
                              if (.not. have_second) then
                                 print *, "Error: function needs two arguments"
                                 eval_error = .true.; f = [bad_value]
                              else if (size(arg2) < 1) then
                                 print *, "Error: second argument of arspec() must be non-empty"
                                 eval_error = .true.; f = [bad_value]
                              else if (size(arg1) < 3) then
                                 print *, "Error: arspec() needs size(x) >= 3"
                                 eval_error = .true.; f = [bad_value]
                              else
                                    if (size(arg2) == 1) then
                                       n1 = nint(arg2(1))
                                    if (n1 < 0 .or. n1 > size(arg1) - 2) then
                                       print *, "Error: arspec() order must be between 0 and ", size(arg1) - 2
                                       eval_error = .true.; f = [bad_value]
                                    else
                                       f = arspec(arg1, n1, nfreq=max(16, nfreq_i), plot=.false., method=trim(method_s))
                                       if (do_plot) then
                                          freq = grid(size(f), 0.0_dp, 0.5_dp)
                                          sp_title = "arspec(" // trim(labels(1)) // ", " // trim(labels(2)) // ")"
                                          call plot(freq, f, title=sp_title)
                                       end if
                                    end if
                                 else
                                    allocate (orders(size(arg2)))
                                    orders = nint(arg2)
                                    if (any(orders < 0) .or. any(orders > size(arg1) - 2)) then
                                       print *, "Error: each arspec() order must be between 0 and ", size(arg1) - 2
                                       eval_error = .true.; f = [bad_value]
                                    else
                                       f = arspec(arg1, orders(size(orders)), nfreq=max(16, nfreq_i), plot=.false., method=trim(method_s))
                                       if (do_plot .and. size(f) > 0) then
                                          allocate (y2(size(f), size(orders)), legends(size(orders)))
                                          do j = 1, size(orders)
                                             sj = arspec(arg1, orders(j), nfreq=size(f), plot=.false., method=trim(method_s))
                                             if (size(sj) == size(f)) then
                                                y2(:, j) = sj
                                             else
                                                y2(:, j) = bad_value
                                             end if
                                             write (legends(j), "(a,i0)") "p=", orders(j)
                                          end do
                                          freq = grid(size(f), 0.0_dp, 0.5_dp)
                                          sp_title = "arspec(" // trim(labels(1)) // ", p)"
                                          call plot(freq, y2, title=sp_title, xlabel="frequency", legend_labels=legends)
                                       end if
                                    end if
                                    if (allocated(orders)) deallocate (orders)
                                    if (allocated(y2)) deallocate (y2)
                                    if (allocated(legends)) deallocate (legends)
                                 end if
                              end if
                           end if
                        end if
                     end block

                  case ("arspecaic")
                     block
                        logical :: do_plot
                        integer :: pmax_i, nfreq_i, iter_i, eqpos
                        logical :: have_pmax
                        character(len=256) :: tok, ltok, rval
                        character(len=16) :: method_s
                        real(kind=dp), allocatable :: tmp(:)

                        do_plot = .true.
                        pmax_i = min(10, max(0, size(arg1) - 2))
                        nfreq_i = 256
                        iter_i = 5
                        have_pmax = .false.
                        method_s = "ls"

                        call split_by_comma(expr(pstart:pend - 1), n_args, labels)
                        if (n_args > 6) then
                           print *, "Error: arspecaic() takes at most six arguments"
                           eval_error = .true.; f = [bad_value]
                        else
                           ! consume through ')' so parser does not leave trailing text
                           pos = pend + 1
                           if (pos > lenstr) then
                              curr_char = char(0)
                           else
                              curr_char = expr(pos:pos)
                              pos = pos + 1
                           end if

                           if (n_args >= 2) then
                              do i_arg = 2, n_args
                                 tok = adjustl(labels(i_arg))
                                 ltok = lower_str(tok)
                                 eqpos = index(tok, "=")
                                 if (eqpos > 0) then
                                    rval = adjustl(tok(eqpos + 1:))
                                    if (index(ltok, "plot") == 1) then
                                       rval = lower_str(rval)
                                       if (rval == ".false." .or. rval == "false" .or. rval == "f") then
                                          do_plot = .false.
                                       else if (rval == ".true." .or. rval == "true" .or. rval == "t") then
                                          do_plot = .true.
                                       else
                                          tmp = evaluate(rval)
                                          if (eval_error) then
                                             f = [bad_value]; exit
                                          else if (size(tmp) /= 1) then
                                             print *, "Error: plot must be scalar"
                                             eval_error = .true.; f = [bad_value]; exit
                                          else
                                             do_plot = (tmp(1) /= 0.0_dp)
                                          end if
                                       end if
                                    else if (index(ltok, "pmax") == 1) then
                                       tmp = evaluate(rval)
                                       if (eval_error) then
                                          f = [bad_value]; exit
                                       else if (size(tmp) /= 1) then
                                          print *, "Error: pmax must be scalar"
                                          eval_error = .true.; f = [bad_value]; exit
                                       else
                                          pmax_i = nint(tmp(1))
                                          have_pmax = .true.
                                       end if
                                    else if (index(ltok, "nfreq") == 1) then
                                       tmp = evaluate(rval)
                                       if (eval_error) then
                                          f = [bad_value]; exit
                                       else if (size(tmp) /= 1) then
                                          print *, "Error: nfreq must be scalar"
                                          eval_error = .true.; f = [bad_value]; exit
                                       else
                                          nfreq_i = nint(tmp(1))
                                       end if
                                    else if (index(ltok, "iter") == 1) then
                                       tmp = evaluate(rval)
                                       if (eval_error) then
                                          f = [bad_value]; exit
                                       else if (size(tmp) /= 1) then
                                          print *, "Error: iter must be scalar"
                                          eval_error = .true.; f = [bad_value]; exit
                                       else
                                          iter_i = nint(tmp(1))
                                       end if
                                    else if (index(ltok, "method") == 1) then
                                       method_s = lower_str(trim(rval))
                                       if (len_trim(method_s) >= 2) then
                                          if (method_s(1:1) == "'" .or. method_s(1:1) == '"') method_s = method_s(2:)
                                          if (method_s(len_trim(method_s):len_trim(method_s)) == "'" .or. method_s(len_trim(method_s):len_trim(method_s)) == '"') &
                                             method_s = method_s(:len_trim(method_s) - 1)
                                       end if
                                    else
                                       print *, "Error: unknown named argument in arspecaic()"
                                       eval_error = .true.; f = [bad_value]; exit
                                    end if
                                 else
                                    tmp = evaluate(tok)
                                    if (eval_error) then
                                       f = [bad_value]; exit
                                    else if (size(tmp) /= 1) then
                                       print *, "Error: positional optional arguments to arspecaic() must be scalar"
                                       eval_error = .true.; f = [bad_value]; exit
                                    else if (.not. have_pmax) then
                                       pmax_i = nint(tmp(1))
                                       have_pmax = .true.
                                    else if (nfreq_i == 256) then
                                       nfreq_i = nint(tmp(1))
                                    else if (i_arg == 4) then
                                       do_plot = (tmp(1) /= 0.0_dp)
                                    else
                                       iter_i = nint(tmp(1))
                                    end if
                                 end if
                              end do
                           end if

                           if (.not. eval_error) then
                              if (size(arg1) < 3) then
                                 print *, "Error: arspecaic() needs size(x) >= 3"
                                 eval_error = .true.; f = [bad_value]
                              else if (pmax_i < 0) then
                                 print *, "Error: arspecaic() pmax must be >= 0"
                                 eval_error = .true.; f = [bad_value]
                              else if (nfreq_i < 1) then
                                 print *, "Error: arspecaic() nfreq must be >= 1"
                                 eval_error = .true.; f = [bad_value]
                              else if (iter_i < 1) then
                                 print *, "Error: arspecaic() iter must be >= 1"
                                 eval_error = .true.; f = [bad_value]
                              else
                                 f = arspecaic(arg1, pmax=min(pmax_i, size(arg1) - 2), &
                                               nfreq=max(16, nfreq_i), plot=do_plot, niter=iter_i, method=trim(method_s))
                              end if
                           end if
                        end if
                     end block

                  case ("armaspec")
                     block
                        logical :: do_plot
                        integer :: p_i, q_i, nfreq_i, iter_i, eqpos
                        logical :: have_p, have_q, theory_mode, have_ar, have_ma, have_sigma2
                        character(len=:), allocatable :: tok, ltok, rval
                        real(kind=dp), allocatable :: tmp(:), ar_v(:), ma_v(:)
                        real(kind=dp) :: sigma2_v

                        do_plot = .true.
                        nfreq_i = 256
                        iter_i = 5
                        p_i = 0
                        q_i = 0
                        have_p = .false.
                        have_q = .false.
                        theory_mode = .false.
                        have_ar = .false.
                        have_ma = .false.
                        have_sigma2 = .false.
                        sigma2_v = 1.0_dp

                        call split_by_comma(expr(pstart:pend - 1), n_args, labels)
                        if (n_args < 1 .or. n_args > 9) then
                           print *, "Error: armaspec() argument count is invalid"
                           eval_error = .true.; f = [bad_value]
                        else
                           tok = adjustl(labels(1))
                           eqpos = index(tok, "=")
                           if (eqpos > 0) then
                              ltok = lower_str(adjustl(tok(:eqpos - 1)))
                              theory_mode = (index(ltok, "ar") == 1 .or. index(ltok, "ma") == 1)
                           end if

                           pos = pend + 1
                           if (pos > lenstr) then
                              curr_char = char(0)
                           else
                              curr_char = expr(pos:pos)
                              pos = pos + 1
                           end if

                           if (theory_mode) then
                              do i_arg = 1, n_args
                                 tok = adjustl(labels(i_arg))
                                 if (len_trim(tok) > 0) then
                                    if (tok(len_trim(tok):len_trim(tok)) == ")") tok = tok(:len_trim(tok) - 1)
                                 end if
                                 ltok = lower_str(tok)
                                 eqpos = index(tok, "=")
                                 if (eqpos <= 0) then
                                    print *, "Error: armaspec() theoretical mode requires named args only"
                                    eval_error = .true.; f = [bad_value]; exit
                                 end if
                                 rval = adjustl(tok(eqpos + 1:))
                                 if (index(ltok, "ar") == 1) then
                                    tmp = evaluate(rval)
                                    if (eval_error) then
                                       f = [bad_value]; exit
                                    end if
                                    ar_v = tmp
                                    have_ar = .true.
                                 else if (index(ltok, "ma") == 1) then
                                    tmp = evaluate(rval)
                                    if (eval_error) then
                                       f = [bad_value]; exit
                                    end if
                                    ma_v = tmp
                                    have_ma = .true.
                                 else if (index(ltok, "sigma2") == 1) then
                                    tmp = evaluate(rval)
                                    if (eval_error .or. size(tmp) /= 1) then
                                       print *, "Error: sigma2 must be scalar"
                                       eval_error = .true.; f = [bad_value]; exit
                                    end if
                                    sigma2_v = tmp(1)
                                    have_sigma2 = .true.
                                 else if (index(ltok, "nfreq") == 1) then
                                    tmp = evaluate(rval)
                                    if (eval_error .or. size(tmp) /= 1) then
                                       print *, "Error: nfreq must be scalar"
                                       eval_error = .true.; f = [bad_value]; exit
                                    end if
                                    nfreq_i = nint(tmp(1))
                                 else if (index(ltok, "iter") == 1) then
                                    tmp = evaluate(rval)
                                    if (eval_error .or. size(tmp) /= 1) then
                                       print *, "Error: iter must be scalar"
                                       eval_error = .true.; f = [bad_value]; exit
                                    end if
                                    iter_i = nint(tmp(1))
                                 else if (index(ltok, "plot") == 1) then
                                    rval = lower_str(trim(rval))
                                    if (rval == ".false." .or. rval == "false" .or. rval == "f") then
                                       do_plot = .false.
                                    else if (rval == ".true." .or. rval == "true" .or. rval == "t") then
                                       do_plot = .true.
                                    else
                                       print *, "Error: plot must be .true. or .false."
                                       eval_error = .true.; f = [bad_value]; exit
                                    end if
                                 else
                                    print *, "Error: unknown named argument in armaspec()"
                                    eval_error = .true.; f = [bad_value]; exit
                                 end if
                              end do

                              if (.not. eval_error) then
                                 if (.not. have_ar .and. .not. have_ma) then
                                    print *, "Error: armaspec() theoretical mode needs ar= or ma="
                                    eval_error = .true.; f = [bad_value]
                                 else if (nfreq_i < 1) then
                                    print *, "Error: armaspec() nfreq must be >= 1"
                                    eval_error = .true.; f = [bad_value]
                                 else if (iter_i < 1) then
                                    print *, "Error: armaspec() iter must be >= 1"
                                    eval_error = .true.; f = [bad_value]
                                 else if (have_ar .and. have_ma) then
                                    if (have_sigma2) then
                                       f = armaspec(ar=ar_v, ma=ma_v, sigma2=sigma2_v, &
                                                    nfreq=max(16, nfreq_i), plot=do_plot, niter=iter_i)
                                    else
                                       f = armaspec(ar=ar_v, ma=ma_v, nfreq=max(16, nfreq_i), &
                                                    plot=do_plot, niter=iter_i)
                                    end if
                                 else if (have_ar) then
                                    if (have_sigma2) then
                                       f = armaspec(ar=ar_v, sigma2=sigma2_v, nfreq=max(16, nfreq_i), &
                                                    plot=do_plot, niter=iter_i)
                                    else
                                       f = armaspec(ar=ar_v, nfreq=max(16, nfreq_i), plot=do_plot, niter=iter_i)
                                    end if
                                 else
                                    if (have_sigma2) then
                                       f = armaspec(ma=ma_v, sigma2=sigma2_v, nfreq=max(16, nfreq_i), &
                                                    plot=do_plot, niter=iter_i)
                                    else
                                       f = armaspec(ma=ma_v, nfreq=max(16, nfreq_i), plot=do_plot, niter=iter_i)
                                    end if
                                 end if
                              end if
                           else
                              if (n_args < 3 .or. n_args > 6) then
                                 print *, "Error: armaspec() takes x, p, q plus optional nfreq/plot/iter"
                                 eval_error = .true.; f = [bad_value]
                              else
                                 do i_arg = 2, n_args
                                    tok = adjustl(labels(i_arg))
                                    if (len_trim(tok) > 0) then
                                       if (tok(len_trim(tok):len_trim(tok)) == ")") tok = tok(:len_trim(tok) - 1)
                                    end if
                                    ltok = lower_str(tok)
                                    eqpos = index(tok, "=")
                                    if (eqpos > 0) then
                                       rval = adjustl(tok(eqpos + 1:))
                                       if (index(ltok, "p") == 1) then
                                          tmp = evaluate(rval)
                                          if (eval_error .or. size(tmp) /= 1) then
                                             print *, "Error: p must be scalar"
                                             eval_error = .true.; f = [bad_value]; exit
                                          end if
                                          p_i = nint(tmp(1)); have_p = .true.
                                       else if (index(ltok, "q") == 1) then
                                          tmp = evaluate(rval)
                                          if (eval_error .or. size(tmp) /= 1) then
                                             print *, "Error: q must be scalar"
                                             eval_error = .true.; f = [bad_value]; exit
                                          end if
                                          q_i = nint(tmp(1)); have_q = .true.
                                       else if (index(ltok, "nfreq") == 1) then
                                          tmp = evaluate(rval)
                                          if (eval_error .or. size(tmp) /= 1) then
                                             print *, "Error: nfreq must be scalar"
                                             eval_error = .true.; f = [bad_value]; exit
                                          end if
                                          nfreq_i = nint(tmp(1))
                                       else if (index(ltok, "iter") == 1) then
                                          tmp = evaluate(rval)
                                          if (eval_error .or. size(tmp) /= 1) then
                                             print *, "Error: iter must be scalar"
                                             eval_error = .true.; f = [bad_value]; exit
                                          end if
                                          iter_i = nint(tmp(1))
                                       else if (index(ltok, "plot") == 1) then
                                          rval = lower_str(trim(rval))
                                          if (rval == ".false." .or. rval == "false" .or. rval == "f") then
                                             do_plot = .false.
                                          else if (rval == ".true." .or. rval == "true" .or. rval == "t") then
                                             do_plot = .true.
                                          else
                                             print *, "Error: plot must be .true. or .false."
                                             eval_error = .true.; f = [bad_value]; exit
                                          end if
                                       else if (index(ltok, "ar") == 1 .or. index(ltok, "ma") == 1) then
                                          print *, "Error: cannot mix x/p/q with ar=/ma= in armaspec()"
                                          eval_error = .true.; f = [bad_value]; exit
                                       else
                                          print *, "Error: unknown named argument in armaspec()"
                                          eval_error = .true.; f = [bad_value]; exit
                                       end if
                                    else
                                       tmp = evaluate(tok)
                                       if (eval_error .or. size(tmp) /= 1) then
                                          print *, "Error: positional optional arguments to armaspec() must be scalar"
                                          eval_error = .true.; f = [bad_value]; exit
                                       end if
                                       if (.not. have_p) then
                                          p_i = nint(tmp(1)); have_p = .true.
                                       else if (.not. have_q) then
                                          q_i = nint(tmp(1)); have_q = .true.
                                       else if (i_arg == 4) then
                                          nfreq_i = nint(tmp(1))
                                       else if (i_arg == 5) then
                                          do_plot = (tmp(1) /= 0.0_dp)
                                       else
                                          iter_i = nint(tmp(1))
                                       end if
                                    end if
                                 end do

                                 if (.not. eval_error) then
                                    if (.not. have_p .or. .not. have_q) then
                                       print *, "Error: armaspec() requires p and q"
                                       eval_error = .true.; f = [bad_value]
                                    else if (size(arg1) < 3) then
                                       print *, "Error: armaspec() needs size(x) >= 3"
                                       eval_error = .true.; f = [bad_value]
                                    else if (p_i < 0 .or. q_i < 0) then
                                       print *, "Error: armaspec() p and q must be >= 0"
                                       eval_error = .true.; f = [bad_value]
                                    else if (max(p_i, q_i) > size(arg1) - 2) then
                                       print *, "Error: armaspec() requires max(p,q) <= ", size(arg1) - 2
                                       eval_error = .true.; f = [bad_value]
                                    else if (nfreq_i < 1) then
                                       print *, "Error: armaspec() nfreq must be >= 1"
                                       eval_error = .true.; f = [bad_value]
                                    else if (iter_i < 1) then
                                       print *, "Error: armaspec() iter must be >= 1"
                                       eval_error = .true.; f = [bad_value]
                                    else
                                       f = armaspec(arg1, p_i, q_i, nfreq=max(16, nfreq_i), plot=do_plot, niter=iter_i)
                                    end if
                                 end if
                              end if
                           end if
                        end if
                     end block

                  case ("armaspecaic")
                     block
                        integer :: pmax_i, qmax_i, nfreq_i, iter_i, eqpos
                        logical :: do_plot, have_pmax, have_qmax
                        character(len=:), allocatable :: tok, ltok, rval
                        real(kind=dp), allocatable :: tmp(:)

                        do_plot = .true.
                        pmax_i = 5
                        qmax_i = 5
                        nfreq_i = 256
                        iter_i = 5
                        have_pmax = .false.
                        have_qmax = .false.

                        call split_by_comma(expr(pstart:pend - 1), n_args, labels)
                        if (n_args > 6) then
                           print *, "Error: armaspecaic() takes at most six arguments"
                           eval_error = .true.; f = [bad_value]
                        else
                           pos = pend + 1
                           if (pos > lenstr) then
                              curr_char = char(0)
                           else
                              curr_char = expr(pos:pos)
                              pos = pos + 1
                           end if

                           if (n_args >= 2) then
                              do i_arg = 2, n_args
                                 tok = adjustl(labels(i_arg))
                                 if (len_trim(tok) > 0) then
                                    if (tok(len_trim(tok):len_trim(tok)) == ")") tok = tok(:len_trim(tok) - 1)
                                 end if
                                 ltok = lower_str(tok)
                                 eqpos = index(tok, "=")
                                 if (eqpos > 0) then
                                    rval = adjustl(tok(eqpos + 1:))
                                    if (index(rval, "!") > 0) rval = rval(:index(rval, "!") - 1)
                                    rval = trim(adjustl(rval))
                                    do while (len_trim(rval) > 0)
                                       if (rval(len_trim(rval):len_trim(rval)) == ")" .or. &
                                           rval(len_trim(rval):len_trim(rval)) == ";") then
                                          rval = trim(rval(:len_trim(rval) - 1))
                                       else
                                          exit
                                       end if
                                    end do
                                    if (index(ltok, "pmax") == 1) then
                                       tmp = evaluate(rval)
                                       if (eval_error .or. size(tmp) /= 1) then
                                          print *, "Error: pmax must be scalar"
                                          eval_error = .true.; f = [bad_value]; exit
                                       end if
                                       pmax_i = nint(tmp(1)); have_pmax = .true.
                                    else if (index(ltok, "qmax") == 1) then
                                       tmp = evaluate(rval)
                                       if (eval_error .or. size(tmp) /= 1) then
                                          print *, "Error: qmax must be scalar"
                                          eval_error = .true.; f = [bad_value]; exit
                                       end if
                                       qmax_i = nint(tmp(1)); have_qmax = .true.
                                    else if (index(ltok, "nfreq") == 1) then
                                       tmp = evaluate(rval)
                                       if (eval_error .or. size(tmp) /= 1) then
                                          print *, "Error: nfreq must be scalar"
                                          eval_error = .true.; f = [bad_value]; exit
                                       end if
                                       nfreq_i = nint(tmp(1))
                                    else if (index(ltok, "iter") == 1) then
                                       tmp = evaluate(rval)
                                       if (eval_error .or. size(tmp) /= 1) then
                                          print *, "Error: iter must be scalar"
                                          eval_error = .true.; f = [bad_value]; exit
                                       end if
                                       iter_i = nint(tmp(1))
                                    else if (index(ltok, "plot") == 1) then
                                       rval = lower_str(rval)
                                       if (rval == ".false." .or. rval == "false" .or. rval == "f") then
                                          do_plot = .false.
                                       else if (rval == ".true." .or. rval == "true" .or. rval == "t") then
                                          do_plot = .true.
                                       else
                                          print *, "Error: plot must be .true. or .false."
                                          eval_error = .true.; f = [bad_value]; exit
                                       end if
                                    else
                                       print *, "Error: unknown named argument in armaspecaic()"
                                       eval_error = .true.; f = [bad_value]; exit
                                    end if
                                 else
                                    tmp = evaluate(tok)
                                    if (eval_error .or. size(tmp) /= 1) then
                                       print *, "Error: positional optional arguments to armaspecaic() must be scalar"
                                       eval_error = .true.; f = [bad_value]; exit
                                    end if
                                    if (.not. have_pmax) then
                                       pmax_i = nint(tmp(1)); have_pmax = .true.
                                    else if (.not. have_qmax) then
                                       qmax_i = nint(tmp(1)); have_qmax = .true.
                                    else if (i_arg == 4) then
                                       nfreq_i = nint(tmp(1))
                                    else if (i_arg == 5) then
                                       do_plot = (tmp(1) /= 0.0_dp)
                                    else
                                       iter_i = nint(tmp(1))
                                    end if
                                 end if
                              end do
                           end if

                           if (.not. eval_error) then
                              if (size(arg1) < 3) then
                                 print *, "Error: armaspecaic() needs size(x) >= 3"
                                 eval_error = .true.; f = [bad_value]
                              else if (pmax_i < 0 .or. qmax_i < 0) then
                                 print *, "Error: armaspecaic() pmax and qmax must be >= 0"
                                 eval_error = .true.; f = [bad_value]
                              else if (nfreq_i < 1) then
                                 print *, "Error: armaspecaic() nfreq must be >= 1"
                                 eval_error = .true.; f = [bad_value]
                              else if (iter_i < 1) then
                                 print *, "Error: armaspecaic() iter must be >= 1"
                                 eval_error = .true.; f = [bad_value]
                              else
                                 f = armaspecaic(arg1, pmax=pmax_i, qmax=qmax_i, nfreq=max(16, nfreq_i), plot=do_plot, iter=iter_i)
                              end if
                           end if
                        end if
                     end block

                  case ("arma_mt_spec")
                     block
                        logical :: do_plot
                        integer :: p_i, q_i, nfreq_i, iter_i, k_i, eqpos
                        real(kind=dp) :: nw_r
                        logical :: have_p, have_q
                        character(len=:), allocatable :: tok, ltok, rval
                        real(kind=dp), allocatable :: tmp(:)

                        do_plot = .true.
                        nfreq_i = 256
                        iter_i = 5
                        nw_r = 3.5_dp
                        k_i = max(1, nint(2.0_dp*nw_r - 1.0_dp))
                        have_p = .false.
                        have_q = .false.

                        call split_by_comma(expr(pstart:pend - 1), n_args, labels)
                        if (n_args < 3 .or. n_args > 8) then
                           print *, "Error: arma_mt_spec() takes x, p, q plus optional nfreq/iter/nw/k/plot"
                           eval_error = .true.; f = [bad_value]
                        else
                           pos = pend + 1
                           if (pos > lenstr) then
                              curr_char = char(0)
                           else
                              curr_char = expr(pos:pos)
                              pos = pos + 1
                           end if

                           do i_arg = 2, n_args
                              tok = adjustl(labels(i_arg))
                              ltok = lower_str(tok)
                              eqpos = index(tok, "=")
                              if (eqpos > 0) then
                                 rval = adjustl(tok(eqpos + 1:))
                                 if (index(ltok, "p") == 1) then
                                    tmp = evaluate(rval)
                                    if (eval_error .or. size(tmp) /= 1) then
                                       print *, "Error: p must be scalar"
                                       eval_error = .true.; f = [bad_value]; exit
                                    end if
                                    p_i = nint(tmp(1)); have_p = .true.
                                 else if (index(ltok, "q") == 1) then
                                    tmp = evaluate(rval)
                                    if (eval_error .or. size(tmp) /= 1) then
                                       print *, "Error: q must be scalar"
                                       eval_error = .true.; f = [bad_value]; exit
                                    end if
                                    q_i = nint(tmp(1)); have_q = .true.
                                 else if (index(ltok, "nfreq") == 1) then
                                    tmp = evaluate(rval)
                                    if (eval_error .or. size(tmp) /= 1) then
                                       print *, "Error: nfreq must be scalar"
                                       eval_error = .true.; f = [bad_value]; exit
                                    end if
                                    nfreq_i = nint(tmp(1))
                                 else if (index(ltok, "iter") == 1) then
                                    tmp = evaluate(rval)
                                    if (eval_error .or. size(tmp) /= 1) then
                                       print *, "Error: iter must be scalar"
                                       eval_error = .true.; f = [bad_value]; exit
                                    end if
                                    iter_i = nint(tmp(1))
                                 else if (index(ltok, "nw") == 1) then
                                    tmp = evaluate(rval)
                                    if (eval_error .or. size(tmp) /= 1) then
                                       print *, "Error: nw must be scalar"
                                       eval_error = .true.; f = [bad_value]; exit
                                    end if
                                    nw_r = tmp(1)
                                 else if (index(ltok, "k") == 1) then
                                    tmp = evaluate(rval)
                                    if (eval_error .or. size(tmp) /= 1) then
                                       print *, "Error: k must be scalar"
                                       eval_error = .true.; f = [bad_value]; exit
                                    end if
                                    k_i = nint(tmp(1))
                                 else if (index(ltok, "plot") == 1) then
                                    rval = lower_str(rval)
                                    if (rval == ".false." .or. rval == "false" .or. rval == "f") then
                                       do_plot = .false.
                                    else if (rval == ".true." .or. rval == "true" .or. rval == "t") then
                                       do_plot = .true.
                                    else
                                       tmp = evaluate(rval)
                                       if (eval_error .or. size(tmp) /= 1) then
                                          print *, "Error: plot must be scalar"
                                          eval_error = .true.; f = [bad_value]; exit
                                       end if
                                       do_plot = (tmp(1) /= 0.0_dp)
                                    end if
                                 else
                                    print *, "Error: unknown named argument in arma_mt_spec()"
                                    eval_error = .true.; f = [bad_value]; exit
                                 end if
                              else
                                 tmp = evaluate(tok)
                                 if (eval_error .or. size(tmp) /= 1) then
                                    print *, "Error: positional optional arguments to arma_mt_spec() must be scalar"
                                    eval_error = .true.; f = [bad_value]; exit
                                 end if
                                 if (.not. have_p) then
                                    p_i = nint(tmp(1)); have_p = .true.
                                 else if (.not. have_q) then
                                    q_i = nint(tmp(1)); have_q = .true.
                                 else if (i_arg == 4) then
                                    nfreq_i = nint(tmp(1))
                                 else if (i_arg == 5) then
                                    iter_i = nint(tmp(1))
                                 else if (i_arg == 6) then
                                    nw_r = tmp(1)
                                 else if (i_arg == 7) then
                                    k_i = nint(tmp(1))
                                 else
                                    do_plot = (tmp(1) /= 0.0_dp)
                                 end if
                              end if
                           end do

                           if (.not. eval_error) then
                              if (.not. have_p .or. .not. have_q) then
                                 print *, "Error: arma_mt_spec() requires p and q"
                                 eval_error = .true.; f = [bad_value]
                              else if (size(arg1) < 3) then
                                 print *, "Error: arma_mt_spec() needs size(x) >= 3"
                                 eval_error = .true.; f = [bad_value]
                              else if (p_i < 0 .or. q_i < 0) then
                                 print *, "Error: arma_mt_spec() p and q must be >= 0"
                                 eval_error = .true.; f = [bad_value]
                              else if (max(p_i, q_i) > size(arg1) - 2) then
                                 print *, "Error: arma_mt_spec() requires max(p,q) <= ", size(arg1) - 2
                                 eval_error = .true.; f = [bad_value]
                              else if (nfreq_i < 1) then
                                 print *, "Error: arma_mt_spec() nfreq must be >= 1"
                                 eval_error = .true.; f = [bad_value]
                              else if (iter_i < 1) then
                                 print *, "Error: arma_mt_spec() iter must be >= 1"
                                 eval_error = .true.; f = [bad_value]
                              else if (nw_r <= 0.0_dp) then
                                 print *, "Error: arma_mt_spec() nw must be > 0"
                                 eval_error = .true.; f = [bad_value]
                              else if (k_i < 1) then
                                 print *, "Error: arma_mt_spec() k must be >= 1"
                                 eval_error = .true.; f = [bad_value]
                              else
                                 f = arma_mt_spec(arg1, p_i, q_i, nfreq=max(16, nfreq_i), iter=iter_i, &
                                                  nw=nw_r, k=k_i, plot=do_plot)
                              end if
                           end if
                        end if
                     end block

                  case ("armaaic_mt_spec")
                     block
                        logical :: do_plot
                        integer :: pmax_i, qmax_i, nfreq_i, iter_i, k_i, eqpos
                        logical :: have_pmax, have_qmax
                        real(kind=dp) :: nw_r
                        character(len=:), allocatable :: tok, ltok, rval
                        real(kind=dp), allocatable :: tmp(:)

                        do_plot = .true.
                        pmax_i = 5
                        qmax_i = 5
                        nfreq_i = 256
                        iter_i = 5
                        nw_r = 3.5_dp
                        k_i = max(1, nint(2.0_dp*nw_r - 1.0_dp))
                        have_pmax = .false.
                        have_qmax = .false.

                        call split_by_comma(expr(pstart:pend - 1), n_args, labels)
                        if (n_args > 8) then
                           print *, "Error: armaaic_mt_spec() takes at most eight arguments"
                           eval_error = .true.; f = [bad_value]
                        else
                           pos = pend + 1
                           if (pos > lenstr) then
                              curr_char = char(0)
                           else
                              curr_char = expr(pos:pos)
                              pos = pos + 1
                           end if

                           if (n_args >= 2) then
                              do i_arg = 2, n_args
                                 tok = adjustl(labels(i_arg))
                                 ltok = lower_str(tok)
                                 eqpos = index(tok, "=")
                                 if (eqpos > 0) then
                                    rval = adjustl(tok(eqpos + 1:))
                                    if (index(rval, "!") > 0) rval = rval(:index(rval, "!") - 1)
                                    rval = trim(adjustl(rval))
                                    if (len_trim(rval) > 0) then
                                       if (rval(len_trim(rval):len_trim(rval)) == ")") rval = rval(:len_trim(rval) - 1)
                                    end if
                                    if (index(ltok, "pmax") == 1) then
                                       tmp = evaluate(rval)
                                       if (eval_error .or. size(tmp) /= 1) then
                                          print *, "Error: pmax must be scalar"
                                          eval_error = .true.; f = [bad_value]; exit
                                       end if
                                       pmax_i = nint(tmp(1)); have_pmax = .true.
                                    else if (index(ltok, "qmax") == 1) then
                                       tmp = evaluate(rval)
                                       if (eval_error .or. size(tmp) /= 1) then
                                          print *, "Error: qmax must be scalar"
                                          eval_error = .true.; f = [bad_value]; exit
                                       end if
                                       qmax_i = nint(tmp(1)); have_qmax = .true.
                                    else if (index(ltok, "nfreq") == 1) then
                                       tmp = evaluate(rval)
                                       if (eval_error .or. size(tmp) /= 1) then
                                          print *, "Error: nfreq must be scalar"
                                          eval_error = .true.; f = [bad_value]; exit
                                       end if
                                       nfreq_i = nint(tmp(1))
                                    else if (index(ltok, "iter") == 1) then
                                       tmp = evaluate(rval)
                                       if (eval_error .or. size(tmp) /= 1) then
                                          print *, "Error: iter must be scalar"
                                          eval_error = .true.; f = [bad_value]; exit
                                       end if
                                       iter_i = nint(tmp(1))
                                    else if (index(ltok, "nw") == 1) then
                                       tmp = evaluate(rval)
                                       if (eval_error .or. size(tmp) /= 1) then
                                          print *, "Error: nw must be scalar"
                                          eval_error = .true.; f = [bad_value]; exit
                                       end if
                                       nw_r = tmp(1)
                                    else if (index(ltok, "k") == 1) then
                                       tmp = evaluate(rval)
                                       if (eval_error .or. size(tmp) /= 1) then
                                          print *, "Error: k must be scalar"
                                          eval_error = .true.; f = [bad_value]; exit
                                       end if
                                       k_i = nint(tmp(1))
                                    else if (index(ltok, "plot") == 1) then
                                       rval = lower_str(rval)
                                       if (rval == ".false." .or. rval == "false" .or. rval == "f") then
                                          do_plot = .false.
                                       else if (rval == ".true." .or. rval == "true" .or. rval == "t") then
                                          do_plot = .true.
                                       else
                                          print *, "Error: plot must be .true. or .false."
                                          eval_error = .true.; f = [bad_value]; exit
                                       end if
                                    else
                                       print *, "Error: unknown named argument in armaaic_mt_spec()"
                                       eval_error = .true.; f = [bad_value]; exit
                                    end if
                                 else
                                    tmp = evaluate(tok)
                                    if (eval_error .or. size(tmp) /= 1) then
                                       print *, "Error: positional optional arguments to armaaic_mt_spec() must be scalar"
                                       eval_error = .true.; f = [bad_value]; exit
                                    end if
                                    if (.not. have_pmax) then
                                       pmax_i = nint(tmp(1)); have_pmax = .true.
                                    else if (.not. have_qmax) then
                                       qmax_i = nint(tmp(1)); have_qmax = .true.
                                    else if (i_arg == 4) then
                                       nfreq_i = nint(tmp(1))
                                    else if (i_arg == 5) then
                                       iter_i = nint(tmp(1))
                                    else if (i_arg == 6) then
                                       nw_r = tmp(1)
                                    else if (i_arg == 7) then
                                       k_i = nint(tmp(1))
                                    else
                                       do_plot = (tmp(1) /= 0.0_dp)
                                    end if
                                 end if
                              end do
                           end if

                           if (.not. eval_error) then
                              if (size(arg1) < 3) then
                                 print *, "Error: armaaic_mt_spec() needs size(x) >= 3"
                                 eval_error = .true.; f = [bad_value]
                              else if (pmax_i < 0 .or. qmax_i < 0) then
                                 print *, "Error: armaaic_mt_spec() pmax and qmax must be >= 0"
                                 eval_error = .true.; f = [bad_value]
                              else if (nfreq_i < 1) then
                                 print *, "Error: armaaic_mt_spec() nfreq must be >= 1"
                                 eval_error = .true.; f = [bad_value]
                              else if (iter_i < 1) then
                                 print *, "Error: armaaic_mt_spec() iter must be >= 1"
                                 eval_error = .true.; f = [bad_value]
                              else if (nw_r <= 0.0_dp) then
                                 print *, "Error: armaaic_mt_spec() nw must be > 0"
                                 eval_error = .true.; f = [bad_value]
                              else if (k_i < 1) then
                                 print *, "Error: armaaic_mt_spec() k must be >= 1"
                                 eval_error = .true.; f = [bad_value]
                              else
                                 f = armaaic_mt_spec(arg1, pmax=pmax_i, qmax=qmax_i, nfreq=max(16, nfreq_i), &
                                                     iter=iter_i, nw=nw_r, k=k_i, plot=do_plot)
                              end if
                           end if
                        end if
                     end block

                  case ("welchspec")
                     block
                        integer :: seglen_i, nfreq_i, eqpos, pos_idx
                        real(kind=dp) :: overlap_r
                        logical :: do_plot
                        logical :: have_seglen_vec
                        character(len=16) :: window_s, detrend_s
                        character(len=:), allocatable :: tok, ltok, rval, sval
                        real(kind=dp), allocatable :: tmp(:), seglen_v(:)

                        seglen_i = 256
                        overlap_r = 0.5_dp
                        window_s = "hann"
                        detrend_s = "mean"
                        nfreq_i = 256
                        do_plot = .true.
                        have_seglen_vec = .false.
                        pos_idx = 0

                        call split_by_comma(expr(pstart:pend - 1), n_args, labels)
                        if (n_args > 7) then
                           print *, "Error: welchspec() takes at most seven arguments"
                           eval_error = .true.; f = [bad_value]
                        else
                           pos = pend + 1
                           if (pos > lenstr) then
                              curr_char = char(0)
                           else
                              curr_char = expr(pos:pos)
                              pos = pos + 1
                           end if

                           if (n_args >= 2) then
                              do i_arg = 2, n_args
                                 tok = adjustl(labels(i_arg))
                                 ltok = lower_str(tok)
                                 eqpos = index(tok, "=")
                                 if (eqpos > 0) then
                                    rval = adjustl(tok(eqpos + 1:))
                                    if (index(ltok, "seglen") == 1) then
                                       tmp = evaluate(rval)
                                       if (eval_error .or. size(tmp) < 1) then
                                          print *, "Error: seglen must be non-empty"
                                          eval_error = .true.; f = [bad_value]; exit
                                       end if
                                       if (size(tmp) == 1) then
                                          seglen_i = nint(tmp(1))
                                          have_seglen_vec = .false.
                                          if (allocated(seglen_v)) deallocate (seglen_v)
                                       else
                                          if (allocated(seglen_v)) deallocate (seglen_v)
                                          allocate (seglen_v(size(tmp)))
                                          seglen_v = tmp
                                          have_seglen_vec = .true.
                                       end if
                                    else if (index(ltok, "overlap") == 1) then
                                       tmp = evaluate(rval)
                                       if (eval_error .or. size(tmp) /= 1) then
                                          print *, "Error: overlap must be scalar"
                                          eval_error = .true.; f = [bad_value]; exit
                                       end if
                                       overlap_r = tmp(1)
                                    else if (index(ltok, "window") == 1) then
                                       sval = trim(rval)
                                       if (len_trim(sval) >= 2) then
                                          if (sval(1:1) == "'" .or. sval(1:1) == '"') sval = sval(2:)
                                          if (sval(len_trim(sval):len_trim(sval)) == "'" .or. sval(len_trim(sval):len_trim(sval)) == '"') &
                                             sval = sval(:len_trim(sval) - 1)
                                       end if
                                       window_s = lower_str(trim(sval))
                                    else if (index(ltok, "nfreq") == 1) then
                                       tmp = evaluate(rval)
                                       if (eval_error .or. size(tmp) /= 1) then
                                          print *, "Error: nfreq must be scalar"
                                          eval_error = .true.; f = [bad_value]; exit
                                       end if
                                       nfreq_i = nint(tmp(1))
                                    else if (index(ltok, "detrend") == 1) then
                                       sval = trim(rval)
                                       if (len_trim(sval) >= 2) then
                                          if (sval(1:1) == "'" .or. sval(1:1) == '"') sval = sval(2:)
                                          if (sval(len_trim(sval):len_trim(sval)) == "'" .or. sval(len_trim(sval):len_trim(sval)) == '"') &
                                             sval = sval(:len_trim(sval) - 1)
                                       end if
                                       detrend_s = lower_str(trim(sval))
                                    else if (index(ltok, "plot") == 1) then
                                       rval = lower_str(trim(rval))
                                       if (rval == ".false." .or. rval == "false" .or. rval == "f") then
                                          do_plot = .false.
                                       else if (rval == ".true." .or. rval == "true" .or. rval == "t") then
                                          do_plot = .true.
                                       else
                                          print *, "Error: plot must be .true. or .false."
                                          eval_error = .true.; f = [bad_value]; exit
                                       end if
                                    else
                                       print *, "Error: unknown named argument in welchspec()"
                                       eval_error = .true.; f = [bad_value]; exit
                                    end if
                                 else
                                    pos_idx = pos_idx + 1
                                    select case (pos_idx)
                                    case (1)
                                       tmp = evaluate(tok)
                                       if (eval_error .or. size(tmp) < 1) then
                                          print *, "Error: seglen must be non-empty"
                                          eval_error = .true.; f = [bad_value]; exit
                                       end if
                                       if (size(tmp) == 1) then
                                          seglen_i = nint(tmp(1))
                                          have_seglen_vec = .false.
                                          if (allocated(seglen_v)) deallocate (seglen_v)
                                       else
                                          if (allocated(seglen_v)) deallocate (seglen_v)
                                          allocate (seglen_v(size(tmp)))
                                          seglen_v = tmp
                                          have_seglen_vec = .true.
                                       end if
                                    case (2)
                                       tmp = evaluate(tok)
                                       if (eval_error .or. size(tmp) /= 1) then
                                          print *, "Error: overlap must be scalar"
                                          eval_error = .true.; f = [bad_value]; exit
                                       end if
                                       overlap_r = tmp(1)
                                    case (3)
                                       sval = trim(tok)
                                       if (len_trim(sval) >= 2) then
                                          if (sval(1:1) == "'" .or. sval(1:1) == '"') sval = sval(2:)
                                          if (sval(len_trim(sval):len_trim(sval)) == "'" .or. sval(len_trim(sval):len_trim(sval)) == '"') &
                                             sval = sval(:len_trim(sval) - 1)
                                       end if
                                       window_s = lower_str(trim(sval))
                                    case (4)
                                       tmp = evaluate(tok)
                                       if (eval_error .or. size(tmp) /= 1) then
                                          print *, "Error: nfreq must be scalar"
                                          eval_error = .true.; f = [bad_value]; exit
                                       end if
                                       nfreq_i = nint(tmp(1))
                                    case (5)
                                       sval = trim(tok)
                                       if (len_trim(sval) >= 2) then
                                          if (sval(1:1) == "'" .or. sval(1:1) == '"') sval = sval(2:)
                                          if (sval(len_trim(sval):len_trim(sval)) == "'" .or. sval(len_trim(sval):len_trim(sval)) == '"') &
                                             sval = sval(:len_trim(sval) - 1)
                                       end if
                                       detrend_s = lower_str(trim(sval))
                                    case default
                                       tmp = evaluate(tok)
                                       if (eval_error .or. size(tmp) /= 1) then
                                          print *, "Error: plot must be scalar"
                                          eval_error = .true.; f = [bad_value]; exit
                                       end if
                                       do_plot = (tmp(1) /= 0.0_dp)
                                    end select
                                 end if
                              end do
                           end if

                           if (.not. eval_error) then
                              if (size(arg1) < 2) then
                                 print *, "Error: welchspec() needs size(x) >= 2"
                                 eval_error = .true.; f = [bad_value]
                              else if (have_seglen_vec) then
                                 if (.not. allocated(seglen_v) .or. size(seglen_v) < 1) then
                                    print *, "Error: welchspec() seglen must be non-empty"
                                    eval_error = .true.; f = [bad_value]
                                 else if (any(nint(seglen_v) < 2)) then
                                    print *, "Error: welchspec() all seglen values must be >= 2"
                                    eval_error = .true.; f = [bad_value]
                                 end if
                              else if (seglen_i < 2) then
                                 print *, "Error: welchspec() seglen must be >= 2"
                                 eval_error = .true.; f = [bad_value]
                              end if
                              if (.not. eval_error .and. (overlap_r < 0.0_dp .or. overlap_r >= 1.0_dp)) then
                                 print *, "Error: welchspec() overlap must be in [0, 1)"
                                 eval_error = .true.; f = [bad_value]
                              end if
                              if (.not. eval_error .and. nfreq_i < 1) then
                                 print *, "Error: welchspec() nfreq must be >= 1"
                                 eval_error = .true.; f = [bad_value]
                              end if
                              if (.not. eval_error) then
                                 if (have_seglen_vec) then
                                    f = welchspec(arg1, seglen=nint(seglen_v), overlap=overlap_r, window=trim(window_s), &
                                                  nfreq=max(16, nfreq_i), detrend=trim(detrend_s), plot=do_plot)
                                 else
                                    f = welchspec(arg1, seglen=seglen_i, overlap=overlap_r, window=trim(window_s), &
                                                  nfreq=max(16, nfreq_i), detrend=trim(detrend_s), plot=do_plot)
                                 end if
                              end if
                           end if
                        end if
                     end block

                  case ("pgramspec")
                     block
                        integer :: nfreq_i, smooth_i, eqpos
                        integer :: n_sm, j_sm, i_ch, start_ch, end_ch, ios_sm, iv_sm
                        integer, allocatable :: smooth_v(:)
                        logical :: do_plot, do_demean
                        logical :: have_nfreq, have_smooth, have_smooth_vec
                        real(kind=dp) :: taper_r
                        character(len=256) :: tok, ltok, rval, keytok, sm_txt
                        real(kind=dp), allocatable :: tmp(:)

                        nfreq_i = 256
                        smooth_i = 1
                        do_plot = .true.
                        do_demean = .true.
                        taper_r = 0.0_dp
                        have_nfreq = .false.
                        have_smooth = .false.
                        have_smooth_vec = .false.

                        call split_by_comma(expr(pstart:pend - 1), n_args, labels)
                        if (n_args > 6) then
                           print *, "Error: pgramspec() takes at most six arguments"
                           eval_error = .true.; f = [bad_value]
                        else
                           pos = pend + 1
                           if (pos > lenstr) then
                              curr_char = char(0)
                           else
                              curr_char = expr(pos:pos)
                              pos = pos + 1
                           end if

                           if (n_args >= 2) then
                              do i_arg = 2, n_args
                                 tok = adjustl(labels(i_arg))
                                 eqpos = index(tok, "=")
                                 if (eqpos > 0) then
                                    keytok = adjustl(tok(:eqpos - 1))
                                    ltok = lower_str(trim(keytok))
                                    rval = adjustl(tok(eqpos + 1:))
                                    if (index(ltok, "nfreq") == 1) then
                                       tmp = evaluate(rval)
                                       if (eval_error .or. size(tmp) /= 1) then
                                          print *, "Error: nfreq must be scalar"
                                          eval_error = .true.; f = [bad_value]; exit
                                       end if
                                       nfreq_i = nint(tmp(1)); have_nfreq = .true.
                                    else if (index(ltok, "smooth") == 1) then
                                       if (len_trim(rval) >= 2 .and. rval(1:1) == "[" .and. &
                                           rval(len_trim(rval):len_trim(rval)) == "]") then
                                          sm_txt = adjustl(rval(2:len_trim(rval) - 1))
                                          if (len_trim(sm_txt) < 1) then
                                             print *, "Error: smooth must be non-empty"
                                             eval_error = .true.; f = [bad_value]; exit
                                          end if
                                          n_sm = 1
                                          do i_ch = 1, len_trim(sm_txt)
                                             if (sm_txt(i_ch:i_ch) == ",") n_sm = n_sm + 1
                                          end do
                                          if (allocated(smooth_v)) deallocate (smooth_v)
                                          allocate (smooth_v(n_sm))
                                          start_ch = 1
                                          j_sm = 0
                                          do i_ch = 1, len_trim(sm_txt) + 1
                                             if (i_ch > len_trim(sm_txt) .or. sm_txt(i_ch:i_ch) == ",") then
                                                end_ch = i_ch - 1
                                                do while (start_ch <= end_ch .and. sm_txt(start_ch:start_ch) == " ")
                                                   start_ch = start_ch + 1
                                                end do
                                                do while (end_ch >= start_ch .and. sm_txt(end_ch:end_ch) == " ")
                                                   end_ch = end_ch - 1
                                                end do
                                                if (end_ch < start_ch) then
                                                   print *, "Error: smooth values must be integers"
                                                   eval_error = .true.; f = [bad_value]; exit
                                                end if
                                                tok = ""
                                                tok(1:end_ch - start_ch + 1) = sm_txt(start_ch:end_ch)
                                                read (tok, *, iostat=ios_sm) iv_sm
                                                if (ios_sm /= 0) then
                                                   print *, "Error: smooth values must be integers"
                                                   eval_error = .true.; f = [bad_value]; exit
                                                end if
                                                j_sm = j_sm + 1
                                                smooth_v(j_sm) = iv_sm
                                                start_ch = i_ch + 1
                                             end if
                                          end do
                                          if (eval_error) exit
                                          have_smooth = .true.
                                          have_smooth_vec = (n_sm > 1)
                                          if (.not. have_smooth_vec) smooth_i = smooth_v(1)
                                       else
                                          tmp = evaluate(rval)
                                          if (eval_error .or. size(tmp) < 1) then
                                             print *, "Error: smooth must be non-empty"
                                             eval_error = .true.; f = [bad_value]; exit
                                          end if
                                          if (size(tmp) == 1) then
                                             smooth_i = nint(tmp(1))
                                             have_smooth = .true.
                                             have_smooth_vec = .false.
                                             if (allocated(smooth_v)) deallocate (smooth_v)
                                          else
                                             if (allocated(smooth_v)) deallocate (smooth_v)
                                             allocate (smooth_v(size(tmp)))
                                             smooth_v = nint(tmp)
                                             have_smooth = .true.
                                             have_smooth_vec = .true.
                                          end if
                                       end if
                                    else if (index(ltok, "taper") == 1) then
                                       tmp = evaluate(rval)
                                       if (eval_error .or. size(tmp) /= 1) then
                                          print *, "Error: taper must be scalar"
                                          eval_error = .true.; f = [bad_value]; exit
                                       end if
                                       taper_r = tmp(1)
                                    else if (index(ltok, "demean") == 1) then
                                       rval = lower_str(rval)
                                       if (rval == ".false." .or. rval == "false" .or. rval == "f") then
                                          do_demean = .false.
                                       else if (rval == ".true." .or. rval == "true" .or. rval == "t") then
                                          do_demean = .true.
                                       else
                                          tmp = evaluate(rval)
                                          if (eval_error .or. size(tmp) /= 1) then
                                             print *, "Error: demean must be scalar"
                                             eval_error = .true.; f = [bad_value]; exit
                                          end if
                                          do_demean = (tmp(1) /= 0.0_dp)
                                       end if
                                    else if (index(ltok, "plot") == 1) then
                                       rval = lower_str(rval)
                                       if (rval == ".false." .or. rval == "false" .or. rval == "f") then
                                          do_plot = .false.
                                       else if (rval == ".true." .or. rval == "true" .or. rval == "t") then
                                          do_plot = .true.
                                       else
                                          tmp = evaluate(rval)
                                          if (eval_error .or. size(tmp) /= 1) then
                                             print *, "Error: plot must be scalar"
                                             eval_error = .true.; f = [bad_value]; exit
                                          end if
                                          do_plot = (tmp(1) /= 0.0_dp)
                                       end if
                                    else
                                       print *, "Error: unknown named argument in pgramspec()"
                                       eval_error = .true.; f = [bad_value]; exit
                                    end if
                                 else
                                    tmp = evaluate(tok)
                                    if (eval_error .or. size(tmp) < 1) then
                                       print *, "Error: positional optional arguments to pgramspec() must be non-empty"
                                       eval_error = .true.; f = [bad_value]; exit
                                    end if
                                    if (.not. have_nfreq) then
                                       if (size(tmp) /= 1) then
                                          print *, "Error: nfreq must be scalar"
                                          eval_error = .true.; f = [bad_value]; exit
                                       end if
                                       nfreq_i = nint(tmp(1)); have_nfreq = .true.
                                    else if (i_arg == 3) then
                                       if (size(tmp) /= 1) then
                                          print *, "Error: demean must be scalar"
                                          eval_error = .true.; f = [bad_value]; exit
                                       end if
                                       do_demean = (tmp(1) /= 0.0_dp)
                                    else if (i_arg == 4) then
                                       if (size(tmp) /= 1) then
                                          print *, "Error: taper must be scalar"
                                          eval_error = .true.; f = [bad_value]; exit
                                       end if
                                       taper_r = tmp(1)
                                    else if (.not. have_smooth) then
                                       if (tok(1:1) == "[" .and. tok(len_trim(tok):len_trim(tok)) == "]") then
                                          sm_txt = adjustl(tok(2:len_trim(tok) - 1))
                                          if (len_trim(sm_txt) < 1) then
                                             print *, "Error: smooth must be non-empty"
                                             eval_error = .true.; f = [bad_value]; exit
                                          end if
                                          n_sm = 1
                                          do i_ch = 1, len_trim(sm_txt)
                                             if (sm_txt(i_ch:i_ch) == ",") n_sm = n_sm + 1
                                          end do
                                          if (allocated(smooth_v)) deallocate (smooth_v)
                                          allocate (smooth_v(n_sm))
                                          start_ch = 1
                                          j_sm = 0
                                          do i_ch = 1, len_trim(sm_txt) + 1
                                             if (i_ch > len_trim(sm_txt) .or. sm_txt(i_ch:i_ch) == ",") then
                                                end_ch = i_ch - 1
                                                do while (start_ch <= end_ch .and. sm_txt(start_ch:start_ch) == " ")
                                                   start_ch = start_ch + 1
                                                end do
                                                do while (end_ch >= start_ch .and. sm_txt(end_ch:end_ch) == " ")
                                                   end_ch = end_ch - 1
                                                end do
                                                if (end_ch < start_ch) then
                                                   print *, "Error: smooth values must be integers"
                                                   eval_error = .true.; f = [bad_value]; exit
                                                end if
                                                tok = ""
                                                tok(1:end_ch - start_ch + 1) = sm_txt(start_ch:end_ch)
                                                read (tok, *, iostat=ios_sm) iv_sm
                                                if (ios_sm /= 0) then
                                                   print *, "Error: smooth values must be integers"
                                                   eval_error = .true.; f = [bad_value]; exit
                                                end if
                                                j_sm = j_sm + 1
                                                smooth_v(j_sm) = iv_sm
                                                start_ch = i_ch + 1
                                             end if
                                          end do
                                          if (eval_error) exit
                                          have_smooth = .true.
                                          have_smooth_vec = (n_sm > 1)
                                          if (.not. have_smooth_vec) smooth_i = smooth_v(1)
                                       else if (size(tmp) == 1) then
                                          smooth_i = nint(tmp(1))
                                          have_smooth = .true.
                                          have_smooth_vec = .false.
                                          if (allocated(smooth_v)) deallocate (smooth_v)
                                       else
                                          if (allocated(smooth_v)) deallocate (smooth_v)
                                          allocate (smooth_v(size(tmp)))
                                          smooth_v = nint(tmp)
                                          have_smooth = .true.
                                          have_smooth_vec = .true.
                                       end if
                                    else
                                       if (size(tmp) /= 1) then
                                          print *, "Error: plot must be scalar"
                                          eval_error = .true.; f = [bad_value]; exit
                                       end if
                                       do_plot = (tmp(1) /= 0.0_dp)
                                    end if
                                 end if
                              end do
                           end if
                           if (.not. eval_error) then
                              if (size(arg1) < 2) then
                                 print *, "Error: pgramspec() needs size(x) >= 2"
                                 eval_error = .true.; f = [bad_value]
                              else if (nfreq_i < 1) then
                                 print *, "Error: pgramspec() nfreq must be >= 1"
                                 eval_error = .true.; f = [bad_value]
                              else if (have_smooth_vec) then
                                 if (.not. allocated(smooth_v)) then
                                    print *, "Error: pgramspec() smooth values must be >= 1"
                                    eval_error = .true.; f = [bad_value]
                                 else if (any(smooth_v < 1)) then
                                    print *, "Error: pgramspec() smooth values must be >= 1"
                                    eval_error = .true.; f = [bad_value]
                                 end if
                              else if (smooth_i < 1) then
                                 print *, "Error: pgramspec() smooth must be >= 1"
                                 eval_error = .true.; f = [bad_value]
                              end if
                              if (.not. eval_error) then
                                 if (taper_r < 0.0_dp .or. taper_r > 0.5_dp) then
                                    print *, "Error: pgramspec() taper must be in [0, 0.5]"
                                    eval_error = .true.; f = [bad_value]
                                 else if (have_smooth_vec) then
                                    f = pgramspec(arg1, nfreq=max(16, nfreq_i), demean=do_demean, taper=taper_r, &
                                                  smooth=smooth_v, plot=do_plot)
                                 else
                                    f = pgramspec(arg1, nfreq=max(16, nfreq_i), demean=do_demean, taper=taper_r, &
                                                  smooth=smooth_i, plot=do_plot)
                                 end if
                              end if
                           end if
                        end if
                     end block

                  case ("mtspec")
                     block
                        integer :: nfreq_i, k_i, eqpos
                        integer, allocatable :: k_v(:)
                        real(kind=dp) :: nw_r
                        real(kind=dp), allocatable :: nw_v(:)
                        logical :: do_plot, do_demean
                        logical :: have_nfreq, have_nw, have_k, have_k_vec, have_nw_vec
                        character(len=:), allocatable :: tok, ltok, rval
                        real(kind=dp), allocatable :: tmp(:)

                        nfreq_i = 256
                        nw_r = 3.5_dp
                        k_i = max(1, nint(2.0_dp*nw_r - 1.0_dp))
                        do_plot = .true.
                        do_demean = .true.
                        have_nfreq = .false.
                        have_nw = .false.
                        have_k = .false.
                        have_k_vec = .false.
                        have_nw_vec = .false.

                        call split_by_comma(expr(pstart:pend - 1), n_args, labels)
                        if (n_args > 6) then
                           print *, "Error: mtspec() takes at most six arguments"
                           eval_error = .true.; f = [bad_value]
                        else
                           pos = pend + 1
                           if (pos > lenstr) then
                              curr_char = char(0)
                           else
                              curr_char = expr(pos:pos)
                              pos = pos + 1
                           end if

                           if (n_args >= 2) then
                              do i_arg = 2, n_args
                                 tok = adjustl(labels(i_arg))
                                 ltok = lower_str(tok)
                                 eqpos = index(tok, "=")
                                 if (eqpos > 0) then
                                    rval = adjustl(tok(eqpos + 1:))
                                    if (index(ltok, "nfreq") == 1) then
                                       tmp = evaluate(rval)
                                       if (eval_error) then
                                          f = [bad_value]; exit
                                       else if (size(tmp) /= 1) then
                                          print *, "Error: nfreq must be scalar"
                                          eval_error = .true.; f = [bad_value]; exit
                                       end if
                                       nfreq_i = nint(tmp(1)); have_nfreq = .true.
                                    else if (index(ltok, "nw") == 1) then
                                       tmp = evaluate(rval)
                                       if (eval_error) then
                                          f = [bad_value]; exit
                                       else if (size(tmp) < 1) then
                                          print *, "Error: nw must be non-empty"
                                          eval_error = .true.; f = [bad_value]; exit
                                       end if
                                       if (size(tmp) == 1) then
                                          nw_r = tmp(1)
                                          have_nw_vec = .false.
                                          if (allocated(nw_v)) deallocate (nw_v)
                                       else
                                          if (allocated(nw_v)) deallocate (nw_v)
                                          allocate (nw_v(size(tmp)))
                                          nw_v = tmp
                                          have_nw_vec = .true.
                                          nw_r = nw_v(size(nw_v))
                                       end if
                                       have_nw = .true.
                                    else if (index(ltok, "k") == 1) then
                                       tmp = evaluate(rval)
                                       if (eval_error) then
                                          f = [bad_value]; exit
                                       else if (size(tmp) < 1) then
                                          print *, "Error: k must be non-empty"
                                          eval_error = .true.; f = [bad_value]; exit
                                       end if
                                       if (size(tmp) == 1) then
                                          k_i = nint(tmp(1))
                                          have_k_vec = .false.
                                          if (allocated(k_v)) deallocate (k_v)
                                       else
                                          if (allocated(k_v)) deallocate (k_v)
                                          allocate (k_v(size(tmp)))
                                          k_v = nint(tmp)
                                          have_k_vec = .true.
                                       end if
                                       have_k = .true.
                                    else if (index(ltok, "demean") == 1) then
                                       rval = lower_str(rval)
                                       if (rval == ".false." .or. rval == "false" .or. rval == "f") then
                                          do_demean = .false.
                                       else if (rval == ".true." .or. rval == "true" .or. rval == "t") then
                                          do_demean = .true.
                                       else
                                          tmp = evaluate(rval)
                                          if (eval_error) then
                                             f = [bad_value]; exit
                                          else if (size(tmp) /= 1) then
                                             print *, "Error: demean must be scalar"
                                             eval_error = .true.; f = [bad_value]; exit
                                          end if
                                          do_demean = (tmp(1) /= 0.0_dp)
                                       end if
                                    else if (index(ltok, "plot") == 1) then
                                       rval = lower_str(rval)
                                       if (rval == ".false." .or. rval == "false" .or. rval == "f") then
                                          do_plot = .false.
                                       else if (rval == ".true." .or. rval == "true" .or. rval == "t") then
                                          do_plot = .true.
                                       else
                                          tmp = evaluate(rval)
                                          if (eval_error) then
                                             f = [bad_value]; exit
                                          else if (size(tmp) /= 1) then
                                             print *, "Error: plot must be scalar"
                                             eval_error = .true.; f = [bad_value]; exit
                                          end if
                                          do_plot = (tmp(1) /= 0.0_dp)
                                       end if
                                    else
                                       print *, "Error: unknown named argument in mtspec()"
                                       eval_error = .true.; f = [bad_value]; exit
                                    end if
                                 else
                                    tmp = evaluate(tok)
                                    if (eval_error) then
                                       f = [bad_value]; exit
                                    else if (size(tmp) < 1) then
                                       print *, "Error: positional optional arguments to mtspec() must be non-empty"
                                       eval_error = .true.; f = [bad_value]; exit
                                    end if
                                    if (.not. have_nfreq) then
                                       if (size(tmp) /= 1) then
                                          print *, "Error: nfreq must be scalar"
                                          eval_error = .true.; f = [bad_value]; exit
                                       end if
                                       nfreq_i = nint(tmp(1)); have_nfreq = .true.
                                    else if (.not. have_nw) then
                                       if (size(tmp) < 1) then
                                          print *, "Error: nw must be non-empty"
                                          eval_error = .true.; f = [bad_value]; exit
                                       end if
                                       if (size(tmp) == 1) then
                                          nw_r = tmp(1)
                                          have_nw_vec = .false.
                                          if (allocated(nw_v)) deallocate (nw_v)
                                       else
                                          if (allocated(nw_v)) deallocate (nw_v)
                                          allocate (nw_v(size(tmp)))
                                          nw_v = tmp
                                          have_nw_vec = .true.
                                          nw_r = nw_v(size(nw_v))
                                       end if
                                       have_nw = .true.
                                    else if (.not. have_k) then
                                       if (size(tmp) == 1) then
                                          k_i = nint(tmp(1))
                                          have_k_vec = .false.
                                          if (allocated(k_v)) deallocate (k_v)
                                       else
                                          if (allocated(k_v)) deallocate (k_v)
                                          allocate (k_v(size(tmp)))
                                          k_v = nint(tmp)
                                          have_k_vec = .true.
                                       end if
                                       have_k = .true.
                                    else if (i_arg == 5) then
                                       if (size(tmp) /= 1) then
                                          print *, "Error: demean must be scalar"
                                          eval_error = .true.; f = [bad_value]; exit
                                       end if
                                       do_demean = (tmp(1) /= 0.0_dp)
                                    else
                                       if (size(tmp) /= 1) then
                                          print *, "Error: plot must be scalar"
                                          eval_error = .true.; f = [bad_value]; exit
                                       end if
                                       do_plot = (tmp(1) /= 0.0_dp)
                                    end if
                                 end if
                              end do
                           end if

                           if (.not. eval_error) then
                              if (.not. have_k) then
                                 if (have_nw_vec) then
                                    if (allocated(nw_v) .and. size(nw_v) > 0) then
                                       k_i = max(1, nint(2.0_dp*nw_v(size(nw_v)) - 1.0_dp))
                                    end if
                                 else
                                    k_i = max(1, nint(2.0_dp*nw_r - 1.0_dp))
                                 end if
                              end if
                              if (size(arg1) < 2) then
                                 print *, "Error: mtspec() needs size(x) >= 2"
                                 eval_error = .true.; f = [bad_value]
                              else if (nfreq_i < 1) then
                                 print *, "Error: mtspec() nfreq must be >= 1"
                                 eval_error = .true.; f = [bad_value]
                              else if (have_nw_vec) then
                                 if (.not. allocated(nw_v) .or. any(nw_v <= 0.0_dp)) then
                                    print *, "Error: mtspec() nw values must be > 0"
                                    eval_error = .true.; f = [bad_value]
                                 else if (have_k_vec) then
                                    if (.not. allocated(k_v) .or. any(k_v < 1)) then
                                       print *, "Error: mtspec() k values must be >= 1"
                                       eval_error = .true.; f = [bad_value]
                                    else
                                       f = mtspec(arg1, nfreq=max(16, nfreq_i), nw=nw_v, k=k_v, demean=do_demean, plot=do_plot)
                                    end if
                                 else if (k_i < 1) then
                                    print *, "Error: mtspec() k must be >= 1"
                                    eval_error = .true.; f = [bad_value]
                                 else
                                    f = mtspec(arg1, nfreq=max(16, nfreq_i), nw=nw_v, k=k_i, demean=do_demean, plot=do_plot)
                                 end if
                              else if (nw_r <= 0.0_dp) then
                                 print *, "Error: mtspec() nw must be > 0"
                                 eval_error = .true.; f = [bad_value]
                              else if (have_k_vec) then
                                 if (.not. allocated(k_v) .or. any(k_v < 1)) then
                                    print *, "Error: mtspec() k values must be >= 1"
                                    eval_error = .true.; f = [bad_value]
                                 else
                                    f = mtspec(arg1, nfreq=max(16, nfreq_i), nw=nw_r, k=k_v, demean=do_demean, plot=do_plot)
                                 end if
                              else if (k_i < 1) then
                                 print *, "Error: mtspec() k must be >= 1"
                                 eval_error = .true.; f = [bad_value]
                              else
                                 f = mtspec(arg1, nfreq=max(16, nfreq_i), nw=nw_r, k=k_i, demean=do_demean, plot=do_plot)
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

                  case ("trimmean", "winsor_mean", "huber_mean", "bisquare_mean", "mad")
                     block
                        logical :: is_named, parse_ok
                        character(len=len_name) :: argname
                        character(len=:), allocatable :: argexpr
                        integer :: n_local

                        call split_by_comma(expr(pstart:pend - 1), n_local, labels)
                        if (n_local >= 2) then
                           call parse_call_actual(labels(2), is_named, argname, argexpr, parse_ok)
                        else
                           is_named = .false.
                           parse_ok = .true.
                           argname = ""
                           argexpr = ""
                        end if

                        if (.not. parse_ok) then
                           print *, "Error: invalid named argument"
                           eval_error = .true.; f = [bad_value]
                        else if (is_named) then
                           select case (trim(id))
                           case ("huber_mean")
                              if (trim(lower_str(argname)) /= "c") then
                                 print *, "Error: huber_mean() named argument must be c"
                                 eval_error = .true.; f = [bad_value]
                              else
                                 arg2 = evaluate(argexpr)
                                 if (eval_error) then
                                    f = [bad_value]
                                 else if (size(arg2) == 1) then
                                    f = [huber_mean(arg1, arg2(1))]
                                 else
                                    f = huber_mean(arg1, arg2)
                                 end if
                              end if
                           case ("bisquare_mean")
                              if (trim(lower_str(argname)) /= "c") then
                                 print *, "Error: bisquare_mean() named argument must be c"
                                 eval_error = .true.; f = [bad_value]
                              else
                                 arg2 = evaluate(argexpr)
                                 if (eval_error) then
                                    f = [bad_value]
                                 else if (size(arg2) == 1) then
                                    f = [bisquare_mean(arg1, arg2(1))]
                                 else
                                    f = bisquare_mean(arg1, arg2)
                                 end if
                              end if
                           case ("mad")
                              if (trim(lower_str(argname)) /= "center") then
                                 print *, "Error: mad() named argument must be center"
                                 eval_error = .true.; f = [bad_value]
                              else
                                 arg2 = evaluate(argexpr)
                                 if (eval_error) then
                                    f = [bad_value]
                                 else if (size(arg2) /= 1) then
                                    print *, "Error: center must be scalar"
                                    eval_error = .true.; f = [bad_value]
                                 else
                                    f = [mad(arg1, arg2(1))]
                                 end if
                              end if
                           case default
                              print *, "Error: named arguments are not supported for this function"
                              eval_error = .true.; f = [bad_value]
                           end select
                        else if (.not. have_second) then
                           select case (trim(id))
                           case ("trimmean"); f = [trimmean(arg1)]
                           case ("winsor_mean"); f = [winsor_mean(arg1)]
                           case ("huber_mean"); f = [huber_mean(arg1)]
                           case ("bisquare_mean"); f = [bisquare_mean(arg1)]
                           case ("mad"); f = [mad(arg1)]
                           end select
                        else if (trim(id) /= "huber_mean" .and. trim(id) /= "bisquare_mean" .and. size(arg2) /= 1) then
                           print *, "Error: second argument must be scalar"
                           eval_error = .true.; f = [bad_value]
                        else
                           select case (trim(id))
                           case ("trimmean"); f = [trimmean(arg1, arg2(1))]
                           case ("winsor_mean"); f = [winsor_mean(arg1, arg2(1))]
                           case ("huber_mean")
                              if (size(arg2) == 1) then
                                 f = [huber_mean(arg1, arg2(1))]
                              else
                                 f = huber_mean(arg1, arg2)
                              end if
                           case ("bisquare_mean")
                              if (size(arg2) == 1) then
                                 f = [bisquare_mean(arg1, arg2(1))]
                              else
                                 f = bisquare_mean(arg1, arg2)
                              end if
                           case ("mad"); f = [mad(arg1, arg2(1))]
                           end select
                        end if
                        pos = pend + 1
                        if (pos > lenstr) then
                           curr_char = char(0)
                        else
                           curr_char = expr(pos:pos)
                           pos = pos + 1
                        end if
                     end block

                  case ("ttest1")
                     if (.not. have_second) then
                        print *, "Error: function needs two arguments"
                        eval_error = .true.; f = [bad_value]
                     else if (size(arg2) /= 1) then
                        print *, "Error: second argument must be scalar"
                        eval_error = .true.; f = [bad_value]
                     else
                        f = ttest1(arg1, arg2(1))
                     end if

                  case ("ttest2")
                     if (.not. have_second) then
                        print *, "Error: function needs two arguments"
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
                              f = ttest2(arg1, arg2, pooled=(arg3(1) /= 0.0_dp))
                              call skip_spaces()
                              if (curr_char == ")") call next_char()
                           end if
                        else
                           f = ttest2(arg1, arg2)
                        end if
                     end if

                  case ("ks2_test")
                     if (.not. have_second) then
                        print *, "Error: function needs two arguments"
                        eval_error = .true.; f = [bad_value]
                     else
                        f = ks2_test(arg1, arg2)
                     end if

                  case ("fit_t")
                     block
                        character(len=:), allocatable :: tok, ltok, rval
                        real(kind=dp), allocatable :: df_arg(:), tmpv(:)
                        integer :: eqpos
                        logical :: have_df, have_verbose, verbose_opt

                        call split_by_comma(expr(pstart:pend - 1), n_args, labels)
                        have_df = .false.
                        have_verbose = .false.
                        verbose_opt = .false.
                        if (n_args < 1 .or. n_args > 3) then
                           print *, "Error: fit_t() takes x and optional df, verbose"
                           eval_error = .true.; f = [bad_value]
                        else
                           do i_arg = 2, n_args
                              tok = adjustl(labels(i_arg))
                              eqpos = index(tok, "=")
                              if (eqpos > 0) then
                                 ltok = lower_str(adjustl(tok(:eqpos - 1)))
                                 rval = adjustl(tok(eqpos + 1:))
                                 tmpv = evaluate(rval)
                                 if (eval_error) then
                                    f = [bad_value]
                                    exit
                                 end if
                                 if (trim(ltok) == "df") then
                                    if (size(tmpv) < 1) then
                                       print *, "Error: df must be non-empty"
                                       eval_error = .true.; f = [bad_value]
                                       exit
                                    end if
                                    df_arg = tmpv
                                    have_df = .true.
                                 else if (trim(ltok) == "verbose") then
                                    if (size(tmpv) /= 1) then
                                       print *, "Error: verbose must be scalar"
                                       eval_error = .true.; f = [bad_value]
                                       exit
                                    end if
                                    verbose_opt = (tmpv(1) /= 0.0_dp)
                                    have_verbose = .true.
                                 else
                                    print *, "Error: unknown named argument in fit_t()"
                                    eval_error = .true.; f = [bad_value]
                                    exit
                                 end if
                              else
                                 if (.not. have_df) then
                                    df_arg = evaluate(tok)
                                    if (eval_error .or. size(df_arg) < 1) then
                                       print *, "Error: df must be non-empty"
                                       eval_error = .true.; f = [bad_value]
                                       exit
                                    end if
                                    have_df = .true.
                                 else if (.not. have_verbose) then
                                    tmpv = evaluate(tok)
                                    if (eval_error .or. size(tmpv) /= 1) then
                                       print *, "Error: verbose must be scalar"
                                       eval_error = .true.; f = [bad_value]
                                       exit
                                    end if
                                    verbose_opt = (tmpv(1) /= 0.0_dp)
                                    have_verbose = .true.
                                 else
                                    print *, "Error: too many positional arguments in fit_t()"
                                    eval_error = .true.; f = [bad_value]
                                    exit
                                 end if
                              end if
                           end do
                           if (.not. eval_error) then
                              if (have_df .and. have_verbose) then
                                 f = fit_t(arg1, df_arg, verbose_opt)
                              else if (have_df) then
                                 f = fit_t(arg1, df_arg)
                              else if (have_verbose) then
                                 f = fit_t(arg1, verbose=verbose_opt)
                              else
                                 f = fit_t(arg1)
                              end if
                           end if
                        end if
                        pos = pend + 1
                        if (pos > lenstr) then
                           curr_char = char(0)
                        else
                           curr_char = expr(pos:pos)
                           pos = pos + 1
                        end if
                     end block

                  case ("fit_nct")
                     block
                        character(len=:), allocatable :: tok, ltok, rval
                        real(kind=dp), allocatable :: df_arg(:), tmpv(:)
                        integer :: eqpos
                        logical :: have_df, have_verbose, have_full, verbose_opt, full_opt

                        if (eval_error) eval_error = .false.
                        call split_by_comma(expr(pstart:pend - 1), n_args, labels)
                        have_df = .false.
                        have_verbose = .false.
                        have_full = .false.
                        verbose_opt = .false.
                        full_opt = .true.
                        if (n_args < 1 .or. n_args > 4) then
                           print *, "Error: fit_nct() takes x and optional df, verbose, full"
                           eval_error = .true.; f = [bad_value]
                        else
                           do i_arg = 2, n_args
                              tok = adjustl(labels(i_arg))
                              eqpos = index(tok, "=")
                              if (eqpos > 0) then
                                 ltok = lower_str(adjustl(tok(:eqpos - 1)))
                                 rval = adjustl(tok(eqpos + 1:))
                                 tmpv = evaluate(rval)
                                 if (eval_error) then
                                    f = [bad_value]
                                    exit
                                 end if
                                 if (trim(ltok) == "df") then
                                    if (size(tmpv) < 1) then
                                       print *, "Error: df must be non-empty"
                                       eval_error = .true.; f = [bad_value]
                                       exit
                                    end if
                                    df_arg = tmpv
                                    have_df = .true.
                                 else if (trim(ltok) == "verbose") then
                                    if (size(tmpv) /= 1) then
                                       print *, "Error: verbose must be scalar"
                                       eval_error = .true.; f = [bad_value]
                                       exit
                                    end if
                                    verbose_opt = (tmpv(1) /= 0.0_dp)
                                    have_verbose = .true.
                                 else if (trim(ltok) == "full") then
                                    if (size(tmpv) /= 1) then
                                       print *, "Error: full must be scalar"
                                       eval_error = .true.; f = [bad_value]
                                       exit
                                    end if
                                    full_opt = (tmpv(1) /= 0.0_dp)
                                    have_full = .true.
                                 else
                                    print *, "Error: unknown named argument in fit_nct()"
                                    eval_error = .true.; f = [bad_value]
                                    exit
                                 end if
                              else
                                 if (.not. have_df) then
                                    df_arg = evaluate(tok)
                                    if (eval_error .or. size(df_arg) < 1) then
                                       print *, "Error: df must be non-empty"
                                       eval_error = .true.; f = [bad_value]
                                       exit
                                    end if
                                    have_df = .true.
                                 else if (.not. have_verbose) then
                                    tmpv = evaluate(tok)
                                    if (eval_error .or. size(tmpv) /= 1) then
                                       print *, "Error: verbose must be scalar"
                                       eval_error = .true.; f = [bad_value]
                                       exit
                                    end if
                                    verbose_opt = (tmpv(1) /= 0.0_dp)
                                    have_verbose = .true.
                                 else if (.not. have_full) then
                                    tmpv = evaluate(tok)
                                    if (eval_error .or. size(tmpv) /= 1) then
                                       print *, "Error: full must be scalar"
                                       eval_error = .true.; f = [bad_value]
                                       exit
                                    end if
                                    full_opt = (tmpv(1) /= 0.0_dp)
                                    have_full = .true.
                                 else
                                    print *, "Error: too many positional arguments in fit_nct()"
                                    eval_error = .true.; f = [bad_value]
                                    exit
                                 end if
                              end if
                           end do
                           if (.not. eval_error) then
                              if (have_df) then
                                 f = fit_nct(arg1, df_arg, verbose=verbose_opt, full=full_opt)
                              else
                                 f = fit_nct(arg1, verbose=verbose_opt, full=full_opt)
                              end if
                           end if
                        end if
                        pos = pend + 1
                        if (pos > lenstr) then
                           curr_char = char(0)
                        else
                           curr_char = expr(pos:pos)
                           pos = pos + 1
                        end if
                     end block

                  case ("fit_mixnorm")
                     if (.not. have_second) then
                        print *, "Error: function needs two arguments"
                        eval_error = .true.; f = [bad_value]
                     else if (size(arg2) /= 1) then
                        print *, "Error: second argument must be scalar"
                        eval_error = .true.; f = [bad_value]
                     else
                        n1 = nint(arg2(1))
                        if (n1 < 1) then
                           print *, "Error: second argument must be >= 1"
                           eval_error = .true.; f = [bad_value]
                        else
                           call skip_spaces()
                           if (curr_char == ",") then
                              call next_char()
                              call skip_spaces()
                              if (pos - 1 + 6 <= lenstr .and. lower_str(expr(pos - 1:pos - 1 + 6)) == "verbose") then
                                 call advance_token(7)
                                 call skip_spaces()
                                 if (curr_char /= "=") then
                                    print *, "Error: expected '=' after verbose"
                                    eval_error = .true.; f = [bad_value]
                                 else
                                    call next_char()
                                    call skip_spaces()
                                    arg3 = parse_expression()
                                    if (eval_error) then
                                       f = [bad_value]
                                    else if (size(arg3) /= 1) then
                                       print *, "Error: verbose must be scalar"
                                       eval_error = .true.; f = [bad_value]
                                    else
                                       f = fit_mixnorm(arg1, n1, arg3(1) /= 0.0_dp)
                                       call skip_spaces()
                                       if (curr_char == ")") call next_char()
                                    end if
                                 end if
                              else
                                 arg3 = parse_expression()
                                 if (eval_error) then
                                    f = [bad_value]
                                 else if (size(arg3) /= 1) then
                                    print *, "Error: third argument must be scalar"
                                    eval_error = .true.; f = [bad_value]
                                 else
                                    f = fit_mixnorm(arg1, n1, arg3(1) /= 0.0_dp)
                                    call skip_spaces()
                                    if (curr_char == ")") call next_char()
                                 end if
                              end if
                           else
                              f = fit_mixnorm(arg1, n1)
                           end if
                        end if
                     end if

                  case ("fit_mixnorm_aic", "fix_mixnorm_aic")
                     if (.not. have_second) then
                        print *, "Error: function needs at least three arguments"
                        eval_error = .true.; f = [bad_value]
                     else if (size(arg2) /= 1) then
                        print *, "Error: second argument must be scalar"
                        eval_error = .true.; f = [bad_value]
                     else
                        call skip_spaces()
                        if (curr_char /= ",") then
                           print *, "Error: function needs at least three arguments"
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
                              n1 = nint(arg2(1))
                              n2 = nint(arg3(1))
                              if (n1 < 1 .or. n2 < n1) then
                                 print *, "Error: require 1 <= kmin <= kmax"
                                 eval_error = .true.; f = [bad_value]
                              else
                                 nstart_i = 3
                                 verbose_opt = .true.
                                 plot_opt = .false.
                                 call skip_spaces()
                                 do while (curr_char == ",")
                                    call next_char()
                                    call skip_spaces()
                                    if (pos - 1 + 5 <= lenstr .and. lower_str(expr(pos - 1:pos - 1 + 5)) == "nstart") then
                                       call advance_token(6)
                                       call skip_spaces()
                                       if (curr_char /= "=") then
                                          print *, "Error: expected '=' after nstart"
                                          eval_error = .true.; exit
                                       end if
                                       call next_char()
                                       call skip_spaces()
                                       arg4 = parse_expression()
                                       if (eval_error .or. size(arg4) /= 1) then
                                          print *, "Error: nstart must be scalar"
                                          eval_error = .true.; exit
                                       end if
                                       nstart_i = max(1, nint(arg4(1)))
                                    else if (pos - 1 + 6 <= lenstr .and. lower_str(expr(pos - 1:pos - 1 + 6)) == "verbose") then
                                       call advance_token(7)
                                       call skip_spaces()
                                       if (curr_char /= "=") then
                                          print *, "Error: expected '=' after verbose"
                                          eval_error = .true.; exit
                                       end if
                                       call next_char()
                                       call skip_spaces()
                                       arg4 = parse_expression()
                                       if (eval_error .or. size(arg4) /= 1) then
                                          print *, "Error: verbose must be scalar"
                                          eval_error = .true.; exit
                                       end if
                                       verbose_opt = (arg4(1) /= 0.0_dp)
                                    else if (pos - 1 + 3 <= lenstr .and. lower_str(expr(pos - 1:pos - 1 + 3)) == "plot") then
                                       call advance_token(4)
                                       call skip_spaces()
                                       if (curr_char /= "=") then
                                          print *, "Error: expected '=' after plot"
                                          eval_error = .true.; exit
                                       end if
                                       call next_char()
                                       call skip_spaces()
                                       arg4 = parse_expression()
                                       if (eval_error .or. size(arg4) /= 1) then
                                          print *, "Error: plot must be scalar"
                                          eval_error = .true.; exit
                                       end if
                                       plot_opt = (arg4(1) /= 0.0_dp)
                                    else
                                       arg4 = parse_expression()
                                       if (eval_error .or. size(arg4) /= 1) then
                                          print *, "Error: optional argument must be scalar"
                                          eval_error = .true.; exit
                                       end if
                                       nstart_i = max(1, nint(arg4(1)))
                                    end if
                                    call skip_spaces()
                                 end do
                                 if (.not. eval_error) then
                                    if (trim(id) == "fix_mixnorm_aic") then
                                       f = fix_mixnorm_aic(arg1, n1, n2, nstart=nstart_i, verbose=verbose_opt, plot=plot_opt)
                                    else
                                       f = fit_mixnorm_aic(arg1, n1, n2, nstart=nstart_i, verbose=verbose_opt, plot=plot_opt)
                                    end if
                                    call skip_spaces()
                                    if (curr_char == ")") call next_char()
                                 else
                                    f = [bad_value]
                                 end if
                              end if
                           end if
                        end if
                     end if

                  case ("mssk_unif", "mssk_norm", "mssk_exp", "mssk_lnorm", "mssk_logis", "mssk_laplace", "mssk_cauchy")
                     block
                        integer :: n_args_local
                        real(kind=dp), allocatable :: t1(:), t2(:)
                        call split_by_comma(expr(pstart:pend - 1), n_args_local, labels)
                        if (n_args_local == 1 .and. len_trim(adjustl(labels(1))) == 0) n_args_local = 0

                        select case (trim(id))
                        case ("mssk_unif")
                           if (n_args_local == 0) then
                              f = mssk_unif(0.0_dp, 1.0_dp)
                           else if (n_args_local == 2) then
                              t1 = evaluate(labels(1))
                              if (eval_error .or. size(t1) /= 1) then
                                 print *, "Error: first argument must be scalar"
                                 eval_error = .true.; f = [bad_value]
                              else
                                 t2 = evaluate(labels(2))
                                 if (eval_error .or. size(t2) /= 1) then
                                    print *, "Error: second argument must be scalar"
                                    eval_error = .true.; f = [bad_value]
                                 else
                                    f = mssk_unif(t1(1), t2(1))
                                 end if
                              end if
                           else
                              print *, "Error: mssk_unif() takes zero or two arguments"
                              eval_error = .true.; f = [bad_value]
                           end if
                        case ("mssk_exp")
                           if (n_args_local == 0) then
                              f = mssk_exp(1.0_dp)
                           else if (n_args_local == 1) then
                              t1 = evaluate(labels(1))
                              if (eval_error .or. size(t1) /= 1) then
                                 print *, "Error: argument must be scalar"
                                 eval_error = .true.; f = [bad_value]
                              else
                                 f = mssk_exp(t1(1))
                              end if
                           else
                              print *, "Error: mssk_exp() takes zero or one argument"
                              eval_error = .true.; f = [bad_value]
                           end if
                        case ("mssk_lnorm")
                           if (n_args_local == 0) then
                              f = mssk_lnorm(0.0_dp, 1.0_dp)
                           else if (n_args_local == 1) then
                              t1 = evaluate(labels(1))
                              if (eval_error .or. size(t1) /= 1) then
                                 print *, "Error: first argument must be scalar"
                                 eval_error = .true.; f = [bad_value]
                              else
                                 f = mssk_lnorm(t1(1), 1.0_dp)
                              end if
                           else if (n_args_local == 2) then
                              t1 = evaluate(labels(1))
                              if (eval_error .or. size(t1) /= 1) then
                                 print *, "Error: first argument must be scalar"
                                 eval_error = .true.; f = [bad_value]
                              else
                                 t2 = evaluate(labels(2))
                                 if (eval_error .or. size(t2) /= 1) then
                                    print *, "Error: second argument must be scalar"
                                    eval_error = .true.; f = [bad_value]
                                 else
                                    f = mssk_lnorm(t1(1), t2(1))
                                 end if
                              end if
                           else
                              print *, "Error: mssk_lnorm() takes zero, one, or two arguments"
                              eval_error = .true.; f = [bad_value]
                           end if
                        case default
                           if (n_args_local == 0) then
                              select case (trim(id))
                              case ("mssk_norm"); f = mssk_norm(0.0_dp, 1.0_dp)
                              case ("mssk_logis"); f = mssk_logis(0.0_dp, 1.0_dp)
                              case ("mssk_laplace"); f = mssk_laplace(0.0_dp, 1.0_dp)
                              case ("mssk_cauchy"); f = mssk_cauchy(0.0_dp, 1.0_dp)
                              end select
                           else if (n_args_local == 1) then
                              t1 = evaluate(labels(1))
                              if (eval_error .or. size(t1) /= 1) then
                                 print *, "Error: first argument must be scalar"
                                 eval_error = .true.; f = [bad_value]
                              else
                                 select case (trim(id))
                                 case ("mssk_norm"); f = mssk_norm(t1(1), 1.0_dp)
                                 case ("mssk_logis"); f = mssk_logis(t1(1), 1.0_dp)
                                 case ("mssk_laplace"); f = mssk_laplace(t1(1), 1.0_dp)
                                 case ("mssk_cauchy"); f = mssk_cauchy(t1(1), 1.0_dp)
                                 end select
                              end if
                           else if (n_args_local == 2) then
                              t1 = evaluate(labels(1))
                              if (eval_error .or. size(t1) /= 1) then
                                 print *, "Error: first argument must be scalar"
                                 eval_error = .true.; f = [bad_value]
                              else
                                 t2 = evaluate(labels(2))
                                 if (eval_error .or. size(t2) /= 1) then
                                    print *, "Error: second argument must be scalar"
                                    eval_error = .true.; f = [bad_value]
                                 else
                                    select case (trim(id))
                                    case ("mssk_norm"); f = mssk_norm(t1(1), t2(1))
                                    case ("mssk_logis"); f = mssk_logis(t1(1), t2(1))
                                    case ("mssk_laplace"); f = mssk_laplace(t1(1), t2(1))
                                    case ("mssk_cauchy"); f = mssk_cauchy(t1(1), t2(1))
                                    end select
                                 end if
                              end if
                           else
                              print *, "Error: function takes zero, one, or two arguments"
                              eval_error = .true.; f = [bad_value]
                           end if
                        end select
                     end block

                  case ("mssk_t", "mssk_chisq")
                     if (have_second) then
                        print *, "Error: function ", trim(id), " takes one argument"
                        eval_error = .true.; f = [bad_value]
                     else if (size(arg1) /= 1) then
                        print *, "Error: argument must be scalar"
                        eval_error = .true.; f = [bad_value]
                     else
                        select case (trim(id))
                        case ("mssk_t"); f = mssk_t(arg1(1))
                        case ("mssk_chisq"); f = mssk_chisq(arg1(1))
                        end select
                     end if

                  case ("mssk_gamma", "mssk_f", "mssk_beta", "mssk_nct")
                     if (.not. have_second) then
                        if (size(arg1) /= 1) then
                           print *, "Error: first argument must be scalar"
                           eval_error = .true.; f = [bad_value]
                        else
                           select case (trim(id))
                           case ("mssk_gamma"); f = mssk_gamma(arg1(1), 1.0_dp)
                           case ("mssk_nct"); f = mssk_nct(arg1(1), 0.0_dp)
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
                        case ("mssk_f"); f = mssk_f(arg1(1), arg2(1))
                        case ("mssk_beta"); f = mssk_beta(arg1(1), arg2(1))
                        case ("mssk_nct"); f = mssk_nct(arg1(1), arg2(1))
                        end select
                     end if

                  case ("mssk_hyperb")
                     if (.not. have_second) then
                        print *, "Error: function needs three arguments"
                        eval_error = .true.; f = [bad_value]
                     else if (size(arg1) /= 1 .or. size(arg2) /= 1) then
                        print *, "Error: first two arguments must be scalar"
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
                              f = mssk_hyperb(arg1(1), arg2(1), arg3(1))
                              call skip_spaces()
                              if (curr_char == ")") call next_char()
                           end if
                        end if
                     end if

                  case ("mssk_ged")
                     if (.not. have_second) then
                        print *, "Error: function needs three arguments"
                        eval_error = .true.; f = [bad_value]
                     else if (size(arg1) /= 1 .or. size(arg2) /= 1) then
                        print *, "Error: first two arguments must be scalar"
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
                              f = mssk_ged(arg1(1), arg2(1), arg3(1))
                              call skip_spaces()
                              if (curr_char == ")") call next_char()
                           end if
                        end if
                     end if

                  case ("kurt_gamma", "kurt_lnorm", "kurt_t", "kurt_chisq", "kurt_ged", "kurt_hyperb")
                     if (have_second) then
                        print *, "Error: function ", trim(id), " takes one argument"
                        eval_error = .true.; f = [bad_value]
                     else
                        select case (trim(id))
                        case ("kurt_gamma"); f = kurt_gamma(arg1)
                        case ("kurt_lnorm"); f = kurt_lnorm(arg1)
                        case ("kurt_t"); f = kurt_t(arg1)
                        case ("kurt_chisq"); f = kurt_chisq(arg1)
                        case ("kurt_ged"); f = kurt_ged(arg1)
                        case ("kurt_hyperb"); f = kurt_hyperb(arg1)
                        end select
                     end if

                  case ("skew_gamma", "skew_lnorm", "skew_chisq")
                     if (have_second) then
                        print *, "Error: function ", trim(id), " takes one argument"
                        eval_error = .true.; f = [bad_value]
                     else
                        select case (trim(id))
                        case ("skew_gamma"); f = skew_gamma(arg1)
                        case ("skew_lnorm"); f = skew_lnorm(arg1)
                        case ("skew_chisq"); f = skew_chisq(arg1)
                        end select
                     end if

                  case ("skew_f", "skew_beta", "skew_nct")
                     if (.not. have_second) then
                        if (trim(id) == "skew_nct") then
                           f = skew_nct(arg1)
                        else
                           print *, "Error: function needs two arguments"
                           eval_error = .true.; f = [bad_value]
                        end if
                     else
                        if (size(arg1) /= size(arg2) .and. size(arg1) /= 1 .and. size(arg2) /= 1) then
                           print *, "Error: arguments must have same length or one must be scalar"
                           eval_error = .true.; f = [bad_value]
                        else
                           select case (trim(id))
                           case ("skew_f")
                              if (size(arg1) == 1 .and. size(arg2) > 1) then
                                 f = skew_f(arg1(1), arg2)
                              else if (size(arg2) == 1 .and. size(arg1) > 1) then
                                 f = skew_f(arg1, arg2(1))
                              else
                                 f = skew_f(arg1, arg2)
                              end if
                           case ("skew_beta")
                              if (size(arg1) == 1 .and. size(arg2) > 1) then
                                 f = skew_beta(arg1(1), arg2)
                              else if (size(arg2) == 1 .and. size(arg1) > 1) then
                                 f = skew_beta(arg1, arg2(1))
                              else
                                 f = skew_beta(arg1, arg2)
                              end if
                           case ("skew_nct")
                              if (size(arg1) == 1 .and. size(arg2) > 1) then
                                 f = skew_nct(arg1(1), arg2)
                              else if (size(arg2) == 1 .and. size(arg1) > 1) then
                                 f = skew_nct(arg1, arg2(1))
                              else
                                 f = skew_nct(arg1, arg2)
                              end if
                           end select
                        end if
                     end if

                  case ("kurt_f", "kurt_beta", "kurt_nct")
                     if (.not. have_second) then
                        if (trim(id) == "kurt_nct") then
                           f = kurt_nct(arg1)
                        else
                           print *, "Error: function needs two arguments"
                           eval_error = .true.; f = [bad_value]
                        end if
                     else
                        if (size(arg1) /= size(arg2) .and. size(arg1) /= 1 .and. size(arg2) /= 1) then
                           print *, "Error: arguments must have same length or one must be scalar"
                           eval_error = .true.; f = [bad_value]
                        else
                           select case (trim(id))
                           case ("kurt_f")
                              if (size(arg1) == 1 .and. size(arg2) > 1) then
                                 f = kurt_f(arg1(1), arg2)
                              else if (size(arg2) == 1 .and. size(arg1) > 1) then
                                 f = kurt_f(arg1, arg2(1))
                              else
                                 f = kurt_f(arg1, arg2)
                              end if
                           case ("kurt_beta")
                              if (size(arg1) == 1 .and. size(arg2) > 1) then
                                 f = kurt_beta(arg1(1), arg2)
                              else if (size(arg2) == 1 .and. size(arg1) > 1) then
                                 f = kurt_beta(arg1, arg2(1))
                              else
                                 f = kurt_beta(arg1, arg2)
                              end if
                           case ("kurt_nct")
                              if (size(arg1) == 1 .and. size(arg2) > 1) then
                                 f = kurt_nct(arg1(1), arg2)
                              else if (size(arg2) == 1 .and. size(arg1) > 1) then
                                 f = kurt_nct(arg1, arg2(1))
                              else
                                 f = kurt_nct(arg1, arg2)
                              end if
                           end select
                        end if
                     end if

                  case ("mssk_mixnorm")
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
                              f = mssk_mixnorm(arg1, arg2, arg3)
                              call skip_spaces()
                              if (curr_char == ")") call next_char()
                           end if
                        end if
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

                  case ("dmixnorm")
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
                                 else
                                    f = dmixnorm(arg1, arg2, arg3, arg4)
                                    call skip_spaces()
                                    if (curr_char == ")") call next_char()
                                 end if
                              end if
                           end if
                        end if
                     end if

                  case ("dgamma", "dlnorm", "dnorm", "dnct", "df", "dbeta", "dlogis", "dlaplace", "dcauchy")
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
                              case ("dnct"); f = dnct(arg1, arg2(1), arg3(1))
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

                  case ("pmixnorm")
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
                                 else
                                    f = pmixnorm(arg1, arg2, arg3, arg4)
                                    call skip_spaces()
                                    if (curr_char == ")") call next_char()
                                 end if
                              end if
                           end if
                        end if
                     end if

                  case ("pgamma", "plnorm", "pnct", "pf", "pbeta", "plogis", "pnorm", "plaplace", "pcauchy")
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
                              case ("pnct"); f = pnct(arg1, arg2(1), arg3(1))
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

                  case ("qmixnorm")
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
                                 else
                                    f = qmixnorm(arg1, arg2, arg3, arg4)
                                    call skip_spaces()
                                    if (curr_char == ")") call next_char()
                                 end if
                              end if
                           end if
                        end if
                     end if

                  case ("qgamma", "qlnorm", "qnct", "qf", "qbeta", "qlogis", "qnorm", "qlaplace", "qcauchy")
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
                              case ("qnct"); f = qnct(arg1, arg2(1), arg3(1))
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

                  case ("armastab")
                     block
                        integer :: n_args_local, eqpos
                        logical :: have_ar, have_ma
                        character(len=:), allocatable :: tok, ltok, rval
                        real(kind=dp), allocatable :: ar_v(:), ma_v(:), tmp(:)

                        call split_by_comma(expr(pstart:pend - 1), n_args_local, labels)
                        if (n_args_local < 1 .or. n_args_local > 2) then
                           print *, "Error: armastab() takes one or two arguments"
                           eval_error = .true.; f = [bad_value]
                        else
                           have_ar = .false.
                           have_ma = .false.
                           do i_arg = 1, n_args_local
                              tok = adjustl(labels(i_arg))
                              ltok = lower_str(tok)
                              eqpos = index(tok, "=")
                              if (eqpos > 0) then
                                 rval = adjustl(tok(eqpos + 1:))
                                 tmp = evaluate(rval)
                                 if (eval_error) then
                                    f = [bad_value]; exit
                                 end if
                                 if (index(ltok, "ar") == 1) then
                                    ar_v = tmp
                                    have_ar = .true.
                                 else if (index(ltok, "ma") == 1) then
                                    ma_v = tmp
                                    have_ma = .true.
                                 else
                                    print *, "Error: unknown named argument in armastab()"
                                    eval_error = .true.; f = [bad_value]; exit
                                 end if
                              else if (.not. have_ar) then
                                 tmp = evaluate(tok)
                                 if (eval_error) then
                                    f = [bad_value]; exit
                                 end if
                                 ar_v = tmp
                                 have_ar = .true.
                              else if (.not. have_ma) then
                                 tmp = evaluate(tok)
                                 if (eval_error) then
                                    f = [bad_value]; exit
                                 end if
                                 ma_v = tmp
                                 have_ma = .true.
                              else
                                 print *, "Error: armastab() takes one or two arguments"
                                 eval_error = .true.; f = [bad_value]; exit
                              end if
                           end do
                           if (.not. eval_error) then
                              if (have_ar .and. have_ma) then
                                 f = armastab(ar=ar_v, ma=ma_v)
                              else if (have_ar) then
                                 f = armastab(ar=ar_v)
                              else if (have_ma) then
                                 f = armastab(ma=ma_v)
                              else
                                 print *, "Error: armastab() requires ar or ma"
                                 eval_error = .true.; f = [bad_value]
                              end if
                           end if
                        end if
                     end block

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

                  case ("rmixnorm")
                     if (.not. have_second) then
                        print *, "Error: function needs four arguments"
                        eval_error = .true.; f = [bad_value]
                     else if (size(arg1) /= 1) then
                        print *, "Error: first argument must be scalar"
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
                                 else
                                    n1 = nint(arg1(1))
                                    if (n1 < 1) then
                                       print *, "Error: length must be > 0"
                                       eval_error = .true.; f = [bad_value]
                                    else
                                       f = rmixnorm(n1, arg2, arg3, arg4)
                                       call skip_spaces()
                                       if (curr_char == ")") call next_char()
                                    end if
                                 end if
                              end if
                           end if
                        end if
                     end if

                  case ("mixnoise")
                     if (.not. have_second) then
                        print *, "Error: function needs four arguments"
                        eval_error = .true.; f = [bad_value]
                     else if (size(arg1) < 1) then
                        print *, "Error: first argument must be non-empty"
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
                                 else
                                    f = mixnoise(arg1, arg2, arg3, arg4)
                                    call skip_spaces()
                                    if (curr_char == ")") call next_char()
                                 end if
                              end if
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

                  case ("rgamma", "rlnorm", "rf", "rbeta", "rlogis", "rlaplace", "rcauchy", "rnct")
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
                                 case ("rnct")
                                    if (arg2(1) <= 0.0_dp) then
                                       print *, "Error: df must be > 0"
                                       eval_error = .true.; f = [bad_value]
                                    else
                                       f = rnct(n1, arg2(1), arg3(1))
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
                     block
                        character(len=:), allocatable :: tok, ltok, rval
                        real(kind=dp), allocatable :: tmpn(:), noise_v(:)
                        integer :: eqpos
                        logical :: have_noise

                        have_noise = .false.
                        call split_by_comma(expr(pstart:pend - 1), n_args, labels)
                        if (n_args < 2 .or. n_args > 3) then
                           print *, "Error: arsim() takes n, phi, and optional noise"
                           eval_error = .true.; f = [bad_value]
                        else if (.not. have_second) then
                           print *, "Error: function needs two arguments"
                           eval_error = .true.; f = [bad_value]
                        else if (size(arg1) /= 1) then
                           print *, "Error: first argument of arsim() must be scalar"
                           eval_error = .true.; f = [bad_value]
                        else if (size(arg2) < 1) then
                           print *, "Error: second argument of arsim() must be non-empty"
                           eval_error = .true.; f = [bad_value]
                        else
                           n1 = nint(arg1(1))
                           if (n1 < 1) then
                              print *, "Error: arsim() length must be > 0"
                              eval_error = .true.; f = [bad_value]
                           else
                              if (n_args == 3) then
                                 tok = adjustl(labels(3))
                                 ltok = lower_str(tok)
                                 eqpos = index(tok, "=")
                                 if (eqpos > 0) then
                                    rval = adjustl(tok(eqpos + 1:))
                                    if (index(ltok, "noise") /= 1) then
                                       print *, "Error: unknown named argument in arsim()"
                                       eval_error = .true.; f = [bad_value]
                                    else
                                       tmpn = evaluate(rval)
                                       if (eval_error .or. size(tmpn) < 1) then
                                          print *, "Error: noise must be non-empty"
                                          eval_error = .true.; f = [bad_value]
                                       else
                                          noise_v = tmpn
                                          have_noise = .true.
                                       end if
                                    end if
                                 else
                                    tmpn = evaluate(tok)
                                    if (eval_error .or. size(tmpn) < 1) then
                                       print *, "Error: third argument of arsim() must be non-empty"
                                       eval_error = .true.; f = [bad_value]
                                    else
                                       noise_v = tmpn
                                       have_noise = .true.
                                    end if
                                 end if
                              end if
                              if (.not. eval_error) then
                                 if (have_noise) then
                                    if (size(noise_v) < n1) then
                                       print *, "Error: arsim() noise must have size >= n"
                                       eval_error = .true.; f = [bad_value]
                                    else
                                       f = arsim(n1, arg2, noise_v)
                                    end if
                                 else
                                    f = arsim(n1, arg2)
                                 end if
                              end if
                           end if
                        end if
                        pos = pend + 1
                        if (pos > lenstr) then
                           curr_char = char(0)
                        else
                           curr_char = expr(pos:pos)
                           pos = pos + 1
                        end if
                     end block

                  case ("masim")
                     block
                        character(len=:), allocatable :: tok, ltok, rval
                        real(kind=dp), allocatable :: tmpn(:), noise_v(:)
                        integer :: eqpos
                        logical :: have_noise

                        have_noise = .false.
                        call split_by_comma(expr(pstart:pend - 1), n_args, labels)
                        if (n_args < 2 .or. n_args > 3) then
                           print *, "Error: masim() takes n, theta, and optional noise"
                           eval_error = .true.; f = [bad_value]
                        else if (.not. have_second) then
                           print *, "Error: function needs two arguments"
                           eval_error = .true.; f = [bad_value]
                        else if (size(arg1) /= 1) then
                           print *, "Error: first argument of masim() must be scalar"
                           eval_error = .true.; f = [bad_value]
                        else if (size(arg2) < 1) then
                           print *, "Error: second argument of masim() must be non-empty"
                           eval_error = .true.; f = [bad_value]
                        else
                           n1 = nint(arg1(1))
                           if (n1 < 1) then
                              print *, "Error: masim() length must be > 0"
                              eval_error = .true.; f = [bad_value]
                           else
                              if (n_args == 3) then
                                 tok = adjustl(labels(3))
                                 ltok = lower_str(tok)
                                 eqpos = index(tok, "=")
                                 if (eqpos > 0) then
                                    rval = adjustl(tok(eqpos + 1:))
                                    if (index(ltok, "noise") /= 1) then
                                       print *, "Error: unknown named argument in masim()"
                                       eval_error = .true.; f = [bad_value]
                                    else
                                       tmpn = evaluate(rval)
                                       if (eval_error .or. size(tmpn) < 1) then
                                          print *, "Error: noise must be non-empty"
                                          eval_error = .true.; f = [bad_value]
                                       else
                                          noise_v = tmpn
                                          have_noise = .true.
                                       end if
                                    end if
                                 else
                                    tmpn = evaluate(tok)
                                    if (eval_error .or. size(tmpn) < 1) then
                                       print *, "Error: third argument of masim() must be non-empty"
                                       eval_error = .true.; f = [bad_value]
                                    else
                                       noise_v = tmpn
                                       have_noise = .true.
                                    end if
                                 end if
                              end if
                              if (.not. eval_error) then
                                 if (have_noise) then
                                    if (size(noise_v) < n1) then
                                       print *, "Error: masim() noise must have size >= n"
                                       eval_error = .true.; f = [bad_value]
                                    else
                                       f = masim(n1, arg2, noise_v)
                                    end if
                                 else
                                    f = masim(n1, arg2)
                                 end if
                              end if
                           end if
                        end if
                     end block

                  case ("armasim")
                     block
                        real(kind=dp), allocatable :: tmpn(:), phi_v(:), theta_v(:), noise_v(:)
                        character(len=:), allocatable :: tok, ltok, rval
                        integer :: eqpos
                        logical :: have_noise

                        have_noise = .false.
                        call split_by_comma(expr(pstart:pend - 1), n_args, labels)
                        if (n_args < 3 .or. n_args > 4) then
                           print *, "Error: armasim() takes n, phi, theta, and optional noise"
                           eval_error = .true.; f = [bad_value]
                        else
                           tmpn = evaluate(adjustl(labels(1)))
                           if (eval_error .or. size(tmpn) /= 1) then
                              print *, "Error: first argument of armasim() must be scalar"
                              eval_error = .true.; f = [bad_value]
                           else
                              n1 = nint(tmpn(1))
                              phi_v = evaluate(adjustl(labels(2)))
                              theta_v = evaluate(adjustl(labels(3)))
                              if (eval_error .or. size(phi_v) < 1 .or. size(theta_v) < 1) then
                                 print *, "Error: second and third arguments of armasim() must be non-empty"
                                 eval_error = .true.; f = [bad_value]
                              else if (n1 < 1) then
                                 print *, "Error: armasim() length must be > 0"
                                 eval_error = .true.; f = [bad_value]
                              else
                                 if (n_args == 4) then
                                    tok = adjustl(labels(4))
                                    ltok = lower_str(tok)
                                    eqpos = index(tok, "=")
                                    if (eqpos > 0) then
                                       rval = adjustl(tok(eqpos + 1:))
                                       if (index(ltok, "noise") /= 1) then
                                          print *, "Error: unknown named argument in armasim()"
                                          eval_error = .true.; f = [bad_value]
                                       else
                                          noise_v = evaluate(rval)
                                          if (eval_error .or. size(noise_v) < 1) then
                                             print *, "Error: noise must be non-empty"
                                             eval_error = .true.; f = [bad_value]
                                          else
                                             have_noise = .true.
                                          end if
                                       end if
                                    else
                                       noise_v = evaluate(tok)
                                       if (eval_error .or. size(noise_v) < 1) then
                                          print *, "Error: fourth argument of armasim() must be non-empty"
                                          eval_error = .true.; f = [bad_value]
                                       else
                                          have_noise = .true.
                                       end if
                                    end if
                                 end if
                                 if (.not. eval_error) then
                                    if (have_noise) then
                                       if (size(noise_v) < n1) then
                                          print *, "Error: armasim() noise must have size >= n"
                                          eval_error = .true.; f = [bad_value]
                                       else
                                          f = armasim(n1, phi_v, theta_v, noise_v)
                                       end if
                                    else
                                       f = armasim(n1, phi_v, theta_v)
                                    end if
                                 end if
                              end if
                           end if
                        end if
                        pos = pend + 1
                        if (pos > lenstr) then
                           curr_char = char(0)
                        else
                           curr_char = expr(pos:pos)
                           pos = pos + 1
                        end if
                     end block

                  case ("arfimasim")
                     block
                        integer :: n_sim, burn_sim, m_sim, eqpos
                        logical :: have_burn, have_m, have_phi, have_theta, have_noise
                        real(kind=dp) :: d_sim
                        character(len=:), allocatable :: tok, ltok, rval
                        real(kind=dp), allocatable :: tmp(:), phi_sim(:), theta_sim(:), noise_sim(:)

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

                        have_burn = .false.; have_m = .false.; have_phi = .false.; have_theta = .false.; have_noise = .false.
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
                               else if (.not. have_noise) then
                                  noise_sim = evaluate(tok)
                                  if (eval_error) then
                                     f = [bad_value]; return
                                  end if
                                  if (size(noise_sim) < 1) then
                                     print *, "Error: positional noise argument must be non-empty"
                                     eval_error = .true.; f = [bad_value]; return
                                  end if
                                  have_noise = .true.
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
                            else if (index(ltok, "noise") == 1) then
                               if (have_noise) then
                                  print *, "Error: duplicate noise= argument"
                                  eval_error = .true.; f = [bad_value]; return
                               end if
                               rval = adjustl(tok(eqpos + 1:))
                               noise_sim = evaluate(rval)
                               if (eval_error) then
                                  f = [bad_value]; return
                               end if
                               if (size(noise_sim) < 1) then
                                  print *, "Error: noise must be non-empty"
                                  eval_error = .true.; f = [bad_value]; return
                               end if
                               have_noise = .true.
                            else
                               print *, "Error: arfimasim() optional args are phi/theta, burn/m, and noise"
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
                        if (have_burn .and. have_m .and. have_noise) then
                           f = arfimasim(n_sim, d_sim, phi_sim, theta_sim, burn=burn_sim, m=m_sim, noise=noise_sim)
                        else if (have_burn .and. have_m) then
                           f = arfimasim(n_sim, d_sim, phi_sim, theta_sim, burn=burn_sim, m=m_sim)
                        else if (have_burn .and. have_noise) then
                           f = arfimasim(n_sim, d_sim, phi_sim, theta_sim, burn=burn_sim, noise=noise_sim)
                        else if (have_m .and. have_noise) then
                           f = arfimasim(n_sim, d_sim, phi_sim, theta_sim, m=m_sim, noise=noise_sim)
                        else if (have_burn) then
                           f = arfimasim(n_sim, d_sim, phi_sim, theta_sim, burn=burn_sim)
                        else if (have_m) then
                           f = arfimasim(n_sim, d_sim, phi_sim, theta_sim, m=m_sim)
                        else if (have_noise) then
                           f = arfimasim(n_sim, d_sim, phi_sim, theta_sim, noise=noise_sim)
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

                  case ("armasimfit")
                     block
                        integer :: n_sim, iter_arg, eqpos
                        logical :: have_pvec, have_qvec, have_iter
                        character(len=:), allocatable :: tok, ltok, rval
                        real(kind=dp), allocatable :: tmp(:), ar_v(:), ma_v(:), p_ord(:), q_ord(:)

                        call split_by_comma(expr(pstart:pend - 1), n_args, labels)
                        if (n_args < 3) then
                           print *, "Error: function needs three arguments"
                           eval_error = .true.; f = [bad_value]; return
                        end if

                        tmp = evaluate(labels(1))
                        if (eval_error) then
                           f = [bad_value]; return
                        end if
                        if (size(tmp) /= 1) then
                           print *, "Error: first argument of armasimfit() must be scalar"
                           eval_error = .true.; f = [bad_value]; return
                        end if
                        n_sim = nint(tmp(1))
                        if (n_sim < 1) then
                           print *, "Error: armasimfit() requires n > 0"
                           eval_error = .true.; f = [bad_value]; return
                        end if

                        ar_v = evaluate(labels(2))
                        if (eval_error) then
                           f = [bad_value]; return
                        end if
                        if (size(ar_v) < 1) then
                           print *, "Error: armasimfit() requires non-empty AR coefficients"
                           eval_error = .true.; f = [bad_value]; return
                        end if

                        ma_v = evaluate(labels(3))
                        if (eval_error) then
                           f = [bad_value]; return
                        end if
                        if (size(ma_v) < 1) then
                           print *, "Error: armasimfit() requires non-empty MA coefficients"
                           eval_error = .true.; f = [bad_value]; return
                        end if

                        have_pvec = .false.
                        have_qvec = .false.
                        have_iter = .false.
                        iter_arg = 5

                        do i_arg = 4, n_args
                           tok = adjustl(labels(i_arg))
                           ltok = lower_str(tok)
                           eqpos = index(tok, "=")
                           if (eqpos > 0) then
                              rval = adjustl(tok(eqpos + 1:))
                              if (index(ltok, "pvec") == 1) then
                                 tmp = evaluate(rval)
                                 if (eval_error) then
                                    f = [bad_value]; return
                                 end if
                                 if (size(tmp) < 1) then
                                    print *, "Error: pvec must be non-empty"
                                    eval_error = .true.; f = [bad_value]; return
                                 end if
                                 if (allocated(p_ord)) deallocate (p_ord)
                                 allocate (p_ord(size(tmp)))
                                 p_ord = tmp
                                 have_pvec = .true.
                              else if (index(ltok, "qvec") == 1) then
                                 tmp = evaluate(rval)
                                 if (eval_error) then
                                    f = [bad_value]; return
                                 end if
                                 if (size(tmp) < 1) then
                                    print *, "Error: qvec must be non-empty"
                                    eval_error = .true.; f = [bad_value]; return
                                 end if
                                 if (allocated(q_ord)) deallocate (q_ord)
                                 allocate (q_ord(size(tmp)))
                                 q_ord = tmp
                                 have_qvec = .true.
                              else if (index(ltok, "iter") == 1) then
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
                              else
                                 print *, "Error: unknown named argument in armasimfit()"
                                 eval_error = .true.; f = [bad_value]; return
                              end if
                           else if (.not. have_pvec) then
                              tmp = evaluate(tok)
                              if (eval_error) then
                                 f = [bad_value]; return
                              end if
                              if (size(tmp) < 1) then
                                 print *, "Error: pvec must be non-empty"
                                 eval_error = .true.; f = [bad_value]; return
                              end if
                              if (allocated(p_ord)) deallocate (p_ord)
                              allocate (p_ord(size(tmp)))
                              p_ord = tmp
                              have_pvec = .true.
                           else if (.not. have_qvec) then
                              tmp = evaluate(tok)
                              if (eval_error) then
                                 f = [bad_value]; return
                              end if
                              if (size(tmp) < 1) then
                                 print *, "Error: qvec must be non-empty"
                                 eval_error = .true.; f = [bad_value]; return
                              end if
                              if (allocated(q_ord)) deallocate (q_ord)
                              allocate (q_ord(size(tmp)))
                              q_ord = tmp
                              have_qvec = .true.
                           else if (.not. have_iter) then
                              tmp = evaluate(tok)
                              if (eval_error) then
                                 f = [bad_value]; return
                              end if
                              if (size(tmp) /= 1) then
                                 print *, "Error: iter must be scalar"
                                 eval_error = .true.; f = [bad_value]; return
                              end if
                              iter_arg = nint(tmp(1))
                              have_iter = .true.
                           else
                              print *, "Error: armasimfit() takes at most 6 arguments"
                              eval_error = .true.; f = [bad_value]; return
                           end if
                        end do

                        if (iter_arg < 1) then
                           print *, "Error: iter must be >= 1"
                           eval_error = .true.; f = [bad_value]; return
                        end if

                        if (have_pvec .and. have_qvec) then
                           call armasimfit(n_sim, ar_v, ma_v, pvec=p_ord, qvec=q_ord, niter=iter_arg)
                        else if (have_pvec) then
                           call armasimfit(n_sim, ar_v, ma_v, pvec=p_ord, niter=iter_arg)
                        else if (have_qvec) then
                           call armasimfit(n_sim, ar_v, ma_v, qvec=q_ord, niter=iter_arg)
                        else
                           call armasimfit(n_sim, ar_v, ma_v, niter=iter_arg)
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

                  case ("araic")
                     block
                        integer :: pmax, iter_arg, eqpos
                        logical :: have_iter, have_pmax
                        character(len=:), allocatable :: tok, ltok, rval
                        real(kind=dp), allocatable :: tmp(:)
                        call split_by_comma(expr(pstart:pend - 1), n_args, labels)
                        pmax = 5
                        have_pmax = .false.
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
                              else if (.not. have_pmax) then
                                 tmp = evaluate(tok)
                                 if (eval_error) then
                                    f = [bad_value]; return
                                 end if
                                 if (size(tmp) /= 1) then
                                    print *, "Error: second argument of araic() must be scalar"
                                    eval_error = .true.; f = [bad_value]; return
                                 end if
                                 pmax = nint(tmp(1))
                                 have_pmax = .true.
                              else
                                 print *, "Error: araic() takes one max-order arg plus iter=..."
                                 eval_error = .true.; f = [bad_value]; return
                              end if
                           end do
                        end if
                        if (pmax < 0) then
                           print *, "Error: araic() max AR order must be >= 0"
                           eval_error = .true.; f = [bad_value]; return
                        end if
                        if (have_iter .and. iter_arg < 1) then
                           print *, "Error: iter must be >= 1"
                           eval_error = .true.; f = [bad_value]; return
                        end if
                        if (have_iter) then
                           call araic(arg1, pmax, niter=iter_arg)
                        else
                           call araic(arg1, pmax)
                        end if
                        suppress_result = .true.
                        f = [real(kind=dp) ::]
                     end block

                  case ("maaic")
                     block
                        integer :: qmax, iter_arg, eqpos
                        logical :: have_iter, have_qmax
                        character(len=:), allocatable :: tok, ltok, rval
                        real(kind=dp), allocatable :: tmp(:)
                        call split_by_comma(expr(pstart:pend - 1), n_args, labels)
                        qmax = 5
                        have_qmax = .false.
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
                              else if (.not. have_qmax) then
                                 tmp = evaluate(tok)
                                 if (eval_error) then
                                    f = [bad_value]; return
                                 end if
                                 if (size(tmp) /= 1) then
                                    print *, "Error: second argument of maaic() must be scalar"
                                    eval_error = .true.; f = [bad_value]; return
                                 end if
                                 qmax = nint(tmp(1))
                                 have_qmax = .true.
                              else
                                 print *, "Error: maaic() takes one max-order arg plus iter=..."
                                 eval_error = .true.; f = [bad_value]; return
                              end if
                           end do
                        end if
                        if (qmax < 0) then
                           print *, "Error: maaic() max MA order must be >= 0"
                           eval_error = .true.; f = [bad_value]; return
                        end if
                        if (have_iter .and. iter_arg < 1) then
                           print *, "Error: iter must be >= 1"
                           eval_error = .true.; f = [bad_value]; return
                        end if
                        if (have_iter) then
                           call maaic(arg1, qmax, niter=iter_arg)
                        else
                           call maaic(arg1, qmax)
                        end if
                        suppress_result = .true.
                        f = [real(kind=dp) ::]
                     end block

                  case ("poly1reg")
                     block
                        logical :: use_intcp
                        integer :: intcp_i
                        integer :: deg, eqpos
                        character(len=:), allocatable :: tok, ltok, rval
                        real(kind=dp), allocatable :: tmp(:)
                        if (.not. have_second) then
                           print *, "Error: function needs three arguments"
                           eval_error = .true.; f = [bad_value]
                           return
                        end if
                        call skip_spaces()
                        if (curr_char /= ",") then
                           print *, "Error: function needs three arguments"
                           eval_error = .true.; f = [bad_value]
                           return
                        end if
                        call next_char()
                        call skip_spaces()
                        arg3 = parse_expression()
                        if (eval_error .or. size(arg3) /= 1) then
                           print *, "Error: third argument must be scalar"
                           eval_error = .true.; f = [bad_value]
                           return
                        end if
                        deg = nint(arg3(1))
                        use_intcp = .true.
                        call skip_spaces()
                        if (curr_char == ",") then
                           call next_char()
                           call skip_spaces()
                           call split_by_comma(expr(pstart:pend - 1), n_args, labels)
                           if (n_args >= 4) then
                              tok = adjustl(labels(4))
                              ltok = lower_str(tok)
                              if (index(ltok, "intcp") == 1) then
                                 eqpos = index(tok, "=")
                                 if (eqpos > 0) then
                                    rval = adjustl(tok(eqpos + 1:))
                                    tmp = evaluate(rval)
                                 else
                                    tmp = evaluate(tok)
                                 end if
                                 if (eval_error .or. size(tmp) /= 1) then
                                    print *, "Error: intcp must be scalar"
                                    eval_error = .true.; f = [bad_value]
                                    return
                                 end if
                                 use_intcp = (tmp(1) /= 0.0_dp)
                              else
                                 tmp = evaluate(tok)
                                 if (eval_error .or. size(tmp) /= 1) then
                                    print *, "Error: fourth argument must be scalar"
                                    eval_error = .true.; f = [bad_value]
                                    return
                                 end if
                                 use_intcp = (tmp(1) /= 0.0_dp)
                              end if
                           end if
                        end if
                        if (use_intcp) then
                           intcp_i = 1
                        else
                           intcp_i = 0
                        end if
                        call poly1reg(arg1, arg2, deg, intcp=intcp_i)
                        call skip_spaces()
                        if (curr_char == ")") call next_char()
                        suppress_result = .true.
                        f = [real(kind=dp) ::]
                     end block

                  case ("splinereg")
                     block
                        integer :: kk, deg, intcp_i, plot_i, anon_idx, eqpos
                        integer, allocatable :: degv(:)
                        character(len=:), allocatable :: tok, ltok, rval
                        real(kind=dp), allocatable :: tmp(:)
                        logical :: have_degree_vec, do_points
                        call split_by_comma(expr(pstart:pend - 1), n_args, labels)
                        pos = pend + 1
                        if (pos > lenstr) then
                           curr_char = char(0)
                        else
                           curr_char = expr(pos:pos); pos = pos + 1
                        end if
                        if (n_args < 3 .or. .not. have_second) then
                           print *, "Error: splinereg() needs at least three arguments"
                           eval_error = .true.; f = [bad_value]
                           return
                        end if
                        tok = adjustl(labels(3))
                        tmp = evaluate(tok)
                        if (eval_error .or. size(tmp) /= 1) then
                           print *, "Error: third argument k must be scalar"
                           eval_error = .true.; f = [bad_value]
                           return
                        end if
                        kk = nint(tmp(1))
                        if (kk < 0) then
                           print *, "Error: splinereg() requires k >= 0"
                           eval_error = .true.; f = [bad_value]
                           return
                        end if
                        deg = 3
                        have_degree_vec = .false.
                        intcp_i = 1
                        plot_i = 1
                        do_points = .false.
                        anon_idx = 0
                        do i_arg = 4, n_args
                           tok = adjustl(labels(i_arg))
                           ltok = lower_str(tok)
                           eqpos = index(tok, "=")
                           if (eqpos > 0) then
                              rval = adjustl(tok(eqpos + 1:))
                              tmp = evaluate(rval)
                              if (index(ltok, "degree") == 1) then
                                 if (eval_error .or. size(tmp) < 1) then
                                    print *, "Error: degree must be non-empty"
                                    eval_error = .true.; f = [bad_value]
                                    return
                                 end if
                                 if (size(tmp) == 1) then
                                    deg = nint(tmp(1))
                                    have_degree_vec = .false.
                                 else
                                    degv = nint(tmp)
                                    have_degree_vec = .true.
                                 end if
                              else if (index(ltok, "intcp") == 1) then
                                 if (eval_error .or. size(tmp) /= 1) then
                                    print *, "Error: intcp must be scalar"
                                    eval_error = .true.; f = [bad_value]
                                    return
                                 end if
                                 intcp_i = merge(1, 0, tmp(1) /= 0.0_dp)
                              else if (index(ltok, "plot") == 1) then
                                 if (eval_error .or. size(tmp) /= 1) then
                                    print *, "Error: plot must be scalar"
                                    eval_error = .true.; f = [bad_value]
                                    return
                                 end if
                                 plot_i = merge(1, 0, tmp(1) /= 0.0_dp)
                              else if (index(ltok, "points") == 1) then
                                 if (eval_error .or. size(tmp) /= 1) then
                                    print *, "Error: points must be scalar"
                                    eval_error = .true.; f = [bad_value]
                                    return
                                 end if
                                 do_points = (tmp(1) /= 0.0_dp)
                              else
                                 print *, "Error: unknown optional argument in splinereg()"
                                 eval_error = .true.; f = [bad_value]
                                 return
                              end if
                           else
                              tmp = evaluate(tok)
                              anon_idx = anon_idx + 1
                              select case (anon_idx)
                              case (1)
                                 if (eval_error .or. size(tmp) < 1) then
                                    print *, "Error: degree must be non-empty"
                                    eval_error = .true.; f = [bad_value]
                                    return
                                 end if
                                 if (size(tmp) == 1) then
                                    deg = nint(tmp(1))
                                    have_degree_vec = .false.
                                 else
                                    degv = nint(tmp)
                                    have_degree_vec = .true.
                                 end if
                              case (2)
                                 if (eval_error .or. size(tmp) /= 1) then
                                    print *, "Error: intcp must be scalar"
                                    eval_error = .true.; f = [bad_value]
                                    return
                                 end if
                                 intcp_i = merge(1, 0, tmp(1) /= 0.0_dp)
                              case (3)
                                 if (eval_error .or. size(tmp) /= 1) then
                                    print *, "Error: plot must be scalar"
                                    eval_error = .true.; f = [bad_value]
                                    return
                                 end if
                                 plot_i = merge(1, 0, tmp(1) /= 0.0_dp)
                              case (4)
                                 if (eval_error .or. size(tmp) /= 1) then
                                    print *, "Error: points must be scalar"
                                    eval_error = .true.; f = [bad_value]
                                    return
                                 end if
                                 do_points = (tmp(1) /= 0.0_dp)
                              case default
                                 print *, "Error: too many optional arguments for splinereg()"
                                 eval_error = .true.; f = [bad_value]
                                 return
                              end select
                           end if
                        end do
                        if (have_degree_vec) then
                           if (any(degv < 0)) then
                              print *, "Error: splinereg() requires degree >= 0"
                              eval_error = .true.; f = [bad_value]
                              return
                           end if
                           f = splinereg(arg1, arg2, kk, degree=degv, intcp=intcp_i, plot=plot_i, points=do_points)
                        else
                           if (deg < 0) then
                              print *, "Error: splinereg() requires degree >= 0"
                              eval_error = .true.; f = [bad_value]
                              return
                           end if
                           f = splinereg(arg1, arg2, kk, degree=deg, intcp=intcp_i, plot=plot_i, points=do_points)
                        end if
                     end block

                  case ("naturalspline")
                     block
                        integer :: kk, intcp_i, plot_i, anon_idx, eqpos
                        integer, allocatable :: kv(:)
                        character(len=:), allocatable :: tok, ltok, rval
                        real(kind=dp), allocatable :: tmp(:)
                        logical :: have_k, have_kvec, do_points
                        call split_by_comma(expr(pstart:pend - 1), n_args, labels)
                        pos = pend + 1
                        if (pos > lenstr) then
                           curr_char = char(0)
                        else
                           curr_char = expr(pos:pos); pos = pos + 1
                        end if
                        if (n_args < 2 .or. .not. have_second) then
                           print *, "Error: naturalspline() needs at least two arguments"
                           eval_error = .true.; f = [bad_value]
                           return
                        end if
                        have_k = .false.
                        have_kvec = .false.
                        intcp_i = 1
                        plot_i = 1
                        do_points = .false.
                        anon_idx = 0
                        do i_arg = 3, n_args
                           tok = adjustl(labels(i_arg))
                           ltok = lower_str(tok)
                           eqpos = index(tok, "=")
                           if (eqpos > 0) then
                              rval = adjustl(tok(eqpos + 1:))
                              tmp = evaluate(rval)
                              if (eval_error .or. size(tmp) < 1) then
                                 print *, "Error: invalid optional argument in naturalspline()"
                                 eval_error = .true.; f = [bad_value]
                                 return
                              end if
                              if (index(ltok, "k") == 1) then
                                 have_k = .true.
                                 have_kvec = (size(tmp) > 1)
                                 if (have_kvec) then
                                    kv = nint(tmp)
                                 else
                                    kk = nint(tmp(1))
                                 end if
                              else if (index(ltok, "intcp") == 1) then
                                 if (size(tmp) /= 1) then
                                    print *, "Error: intcp must be scalar"
                                    eval_error = .true.; f = [bad_value]
                                    return
                                 end if
                                 intcp_i = merge(1, 0, tmp(1) /= 0.0_dp)
                              else if (index(ltok, "plot") == 1) then
                                 if (size(tmp) /= 1) then
                                    print *, "Error: plot must be scalar"
                                    eval_error = .true.; f = [bad_value]
                                    return
                                 end if
                                 plot_i = merge(1, 0, tmp(1) /= 0.0_dp)
                              else if (index(ltok, "points") == 1) then
                                 if (size(tmp) /= 1) then
                                    print *, "Error: points must be scalar"
                                    eval_error = .true.; f = [bad_value]
                                    return
                                 end if
                                 do_points = (tmp(1) /= 0.0_dp)
                              else
                                 print *, "Error: unknown optional argument in naturalspline()"
                                 eval_error = .true.; f = [bad_value]
                                 return
                              end if
                           else
                              tmp = evaluate(tok)
                              if (eval_error .or. size(tmp) < 1) then
                                 print *, "Error: invalid positional argument in naturalspline()"
                                 eval_error = .true.; f = [bad_value]
                                 return
                              end if
                              if (.not. have_k) then
                                 have_k = .true.
                                 have_kvec = (size(tmp) > 1)
                                 if (have_kvec) then
                                    kv = nint(tmp)
                                 else
                                    kk = nint(tmp(1))
                                 end if
                              else
                                 if (size(tmp) /= 1) then
                                    print *, "Error: optional arguments must be scalar"
                                    eval_error = .true.; f = [bad_value]
                                    return
                                 end if
                                 anon_idx = anon_idx + 1
                                 select case (anon_idx)
                                 case (1)
                                    intcp_i = merge(1, 0, tmp(1) /= 0.0_dp)
                                 case (2)
                                    plot_i = merge(1, 0, tmp(1) /= 0.0_dp)
                                 case (3)
                                    do_points = (tmp(1) /= 0.0_dp)
                                 case default
                                    print *, "Error: too many optional arguments for naturalspline()"
                                    eval_error = .true.; f = [bad_value]
                                    return
                                 end select
                              end if
                           end if
                        end do
                        if (have_k) then
                           if (have_kvec) then
                              if (any(kv < 0)) then
                                 print *, "Error: naturalspline() requires k >= 0"
                                 eval_error = .true.; f = [bad_value]
                                 return
                              end if
                              f = naturalspline(arg1, arg2, kv, intcp=intcp_i, plot=plot_i, points=do_points)
                           else
                              if (kk < 0) then
                                 print *, "Error: naturalspline() requires k >= 0"
                                 eval_error = .true.; f = [bad_value]
                                 return
                              end if
                              f = naturalspline(arg1, arg2, kk, intcp=intcp_i, plot=plot_i, points=do_points)
                           end if
                        else
                           f = naturalspline(arg1, arg2, intcp=intcp_i, plot=plot_i, points=do_points)
                        end if
                     end block

                  case ("cpsim")
                     block
                        integer :: nobs, seedi, eqpos, pos_idx, ploti, verbosei
                        logical :: have_mu, have_sd, have_seed, have_noise
                        real(kind=dp), allocatable :: cpv(:), muv(:), sdv(:), noisev(:), tmp(:)
                        character(len=:), allocatable :: tok, ltok, rval
                        call split_by_comma(expr(pstart:pend - 1), n_args, labels)
                        if (n_args < 2) then
                           print *, "Error: cpsim() needs at least n and cp"
                           eval_error = .true.; f = [bad_value]
                           return
                        end if
                        tmp = evaluate(adjustl(labels(1)))
                        if (eval_error .or. size(tmp) /= 1) then
                           print *, "Error: first argument n must be scalar"
                           eval_error = .true.; f = [bad_value]
                           return
                        end if
                        nobs = nint(tmp(1))
                        cpv = evaluate(adjustl(labels(2)))
                        if (eval_error .or. size(cpv) < 1) then
                           print *, "Error: second argument cp must be non-empty"
                           eval_error = .true.; f = [bad_value]
                           return
                        end if
                        have_mu = .false.; have_sd = .false.; have_seed = .false.; have_noise = .false.
                        ploti = 0
                        verbosei = 0
                        pos_idx = 0
                        do i_arg = 3, n_args
                           tok = adjustl(labels(i_arg))
                           eqpos = index(tok, "=")
                           if (eqpos > 0) then
                              ltok = lower_str(adjustl(tok(:eqpos - 1)))
                              rval = adjustl(tok(eqpos + 1:))
                              tmp = evaluate(rval)
                              if (eval_error .or. size(tmp) < 1) then
                                 print *, "Error: invalid named argument in cpsim()"
                                 eval_error = .true.; f = [bad_value]
                                 return
                              end if
                              if (trim(ltok) == "mu") then
                                 muv = tmp; have_mu = .true.
                              else if (trim(ltok) == "sd") then
                                 sdv = tmp; have_sd = .true.
                              else if (trim(ltok) == "seed") then
                                 if (size(tmp) /= 1) then
                                    print *, "Error: seed must be scalar"
                                    eval_error = .true.; f = [bad_value]
                                    return
                                 end if
                                 seedi = nint(tmp(1)); have_seed = .true.
                              else if (trim(ltok) == "plot") then
                                 if (size(tmp) /= 1) then
                                    print *, "Error: plot must be scalar"
                                    eval_error = .true.; f = [bad_value]
                                    return
                                 end if
                                  ploti = merge(1, 0, tmp(1) /= 0.0_dp)
                               else if (trim(ltok) == "verbose") then
                                  if (size(tmp) /= 1) then
                                     print *, "Error: verbose must be scalar"
                                     eval_error = .true.; f = [bad_value]
                                     return
                                  end if
                                  verbosei = merge(1, 0, tmp(1) /= 0.0_dp)
                               else if (trim(ltok) == "noise") then
                                  noisev = tmp; have_noise = .true.
                               else
                                  print *, "Error: unknown named argument in cpsim()"
                                  eval_error = .true.; f = [bad_value]
                                  return
                               end if
                           else
                              tmp = evaluate(tok)
                              if (eval_error .or. size(tmp) < 1) then
                                 print *, "Error: invalid positional argument in cpsim()"
                                 eval_error = .true.; f = [bad_value]
                                 return
                              end if
                              pos_idx = pos_idx + 1
                              select case (pos_idx)
                              case (1)
                                 muv = tmp; have_mu = .true.
                              case (2)
                                 sdv = tmp; have_sd = .true.
                              case (3)
                                 if (size(tmp) /= 1) then
                                    print *, "Error: seed must be scalar"
                                    eval_error = .true.; f = [bad_value]
                                    return
                                 end if
                                 seedi = nint(tmp(1)); have_seed = .true.
                              case (4)
                                 if (size(tmp) /= 1) then
                                    print *, "Error: plot must be scalar"
                                    eval_error = .true.; f = [bad_value]
                                    return
                                 end if
                                  ploti = merge(1, 0, tmp(1) /= 0.0_dp)
                               case (5)
                                  if (size(tmp) /= 1) then
                                     print *, "Error: verbose must be scalar"
                                     eval_error = .true.; f = [bad_value]
                                     return
                                  end if
                                  verbosei = merge(1, 0, tmp(1) /= 0.0_dp)
                               case (6)
                                  noisev = tmp; have_noise = .true.
                               case default
                                  print *, "Error: too many arguments for cpsim()"
                                  eval_error = .true.; f = [bad_value]
                                  return
                               end select
                           end if
                        end do
                        if (.not. have_mu) muv = [0.0_dp]
                        if (.not. have_sd) sdv = [1.0_dp]
                        if (have_noise) then
                           if (size(noisev) < nobs) then
                              print *, "Error: cpsim() noise must have size >= n"
                              eval_error = .true.; f = [bad_value]
                              return
                           end if
                        end if
                        if (have_seed .and. have_noise) then
                           f = cpsim(nobs, cpv, mu=muv, sd=sdv, seed=seedi, plot=ploti, verbose=verbosei, noise=noisev)
                        else if (have_seed) then
                           f = cpsim(nobs, cpv, mu=muv, sd=sdv, seed=seedi, plot=ploti, verbose=verbosei)
                        else if (have_noise) then
                           f = cpsim(nobs, cpv, mu=muv, sd=sdv, plot=ploti, verbose=verbosei, noise=noisev)
                        else
                           f = cpsim(nobs, cpv, mu=muv, sd=sdv, plot=ploti, verbose=verbosei)
                        end if
                     end block

                  case ("cpfit")
                     block
                        integer :: mcp, mseg, ploti, verbosei, eqpos, pos_idx
                        character(len=16) :: mode_s
                        character(len=:), allocatable :: tok, ltok, rval, sval
                        real(kind=dp), allocatable :: tmp(:)
                        mode_s = "mean"
                        mcp = 1
                        mseg = 10
                        ploti = 1
                        verbosei = 1
                        call split_by_comma(expr(pstart:pend - 1), n_args, labels)
                        pos_idx = 0
                        do i_arg = 2, n_args
                           tok = adjustl(labels(i_arg))
                           ltok = lower_str(tok)
                           eqpos = index(tok, "=")
                           if (eqpos > 0) then
                              rval = adjustl(tok(eqpos + 1:))
                              if (index(ltok, "mode") == 1) then
                                 sval = trim(rval)
                                 if (len_trim(sval) >= 2) then
                                    if (sval(1:1) == "'" .or. sval(1:1) == '"') sval = sval(2:)
                                    if (sval(len_trim(sval):len_trim(sval)) == "'" .or. sval(len_trim(sval):len_trim(sval)) == '"') then
                                       sval = sval(:len_trim(sval) - 1)
                                    end if
                                 end if
                                 mode_s = lower_str(trim(sval))
                              else
                                 tmp = evaluate(rval)
                                 if (eval_error .or. size(tmp) /= 1) then
                                    print *, "Error: optional arguments must be scalar"
                                    eval_error = .true.; f = [bad_value]
                                    return
                                 end if
                                 if (index(ltok, "max_cp") == 1) then
                                    mcp = nint(tmp(1))
                                 else if (index(ltok, "minseg") == 1) then
                                    mseg = nint(tmp(1))
                                 else if (index(ltok, "plot") == 1) then
                                    ploti = merge(1, 0, tmp(1) /= 0.0_dp)
                                 else if (index(ltok, "verbose") == 1) then
                                    verbosei = merge(1, 0, tmp(1) /= 0.0_dp)
                                 else
                                    print *, "Error: unknown named argument in cpfit()"
                                    eval_error = .true.; f = [bad_value]
                                    return
                                 end if
                              end if
                           else
                              pos_idx = pos_idx + 1
                              if (pos_idx == 1) then
                                 sval = trim(tok)
                                 if (len_trim(sval) >= 2) then
                                    if (sval(1:1) == "'" .or. sval(1:1) == '"') sval = sval(2:)
                                    if (sval(len_trim(sval):len_trim(sval)) == "'" .or. sval(len_trim(sval):len_trim(sval)) == '"') then
                                       sval = sval(:len_trim(sval) - 1)
                                    end if
                                 end if
                                 if (len_trim(sval) > 0 .and. .not. is_numeral(sval(1:1))) then
                                    mode_s = lower_str(trim(sval))
                                 else
                                    tmp = evaluate(tok)
                                    if (eval_error .or. size(tmp) /= 1) then
                                       print *, "Error: invalid optional argument in cpfit()"
                                       eval_error = .true.; f = [bad_value]
                                       return
                                    end if
                                    mcp = nint(tmp(1))
                                    pos_idx = 2
                                 end if
                              else
                                 tmp = evaluate(tok)
                                 if (eval_error .or. size(tmp) /= 1) then
                                    print *, "Error: optional arguments must be scalar"
                                    eval_error = .true.; f = [bad_value]
                                    return
                                 end if
                                 select case (pos_idx)
                                 case (2)
                                    mcp = nint(tmp(1))
                                 case (3)
                                    mseg = nint(tmp(1))
                                 case (4)
                                    ploti = merge(1, 0, tmp(1) /= 0.0_dp)
                                 case (5)
                                    verbosei = merge(1, 0, tmp(1) /= 0.0_dp)
                                 case default
                                    print *, "Error: too many arguments for cpfit()"
                                    eval_error = .true.; f = [bad_value]
                                    return
                                 end select
                              end if
                           end if
                        end do
                        f = cpfit(arg1, mode=trim(mode_s), max_cp=max(0, mcp), minseg=max(2, mseg), plot=ploti, verbose=verbosei)
                     end block

                  case ("cpfitaic", "cpfit_aic")
                     block
                        integer :: mcp, mseg, ploti, plotici, verbosei, eqpos, pos_idx
                        logical :: have_crit_pos
                        character(len=16) :: mode_s, crit_s
                        character(len=:), allocatable :: tok, ltok, rval, sval
                        real(kind=dp), allocatable :: tmp(:)
                        mode_s = "mean"
                        crit_s = "aic"
                        mcp = 5
                        mseg = 10
                        ploti = 1
                        plotici = 0
                        verbosei = 1
                        have_crit_pos = .false.
                        call split_by_comma(expr(pstart:pend - 1), n_args, labels)
                        pos_idx = 0
                        do i_arg = 2, n_args
                           tok = adjustl(labels(i_arg))
                           ltok = lower_str(tok)
                           eqpos = index(tok, "=")
                           if (eqpos > 0) then
                              rval = adjustl(tok(eqpos + 1:))
                              if (index(ltok, "mode") == 1) then
                                 sval = trim(rval)
                                 if (len_trim(sval) >= 2) then
                                    if (sval(1:1) == "'" .or. sval(1:1) == '"') sval = sval(2:)
                                    if (sval(len_trim(sval):len_trim(sval)) == "'" .or. sval(len_trim(sval):len_trim(sval)) == '"') then
                                       sval = sval(:len_trim(sval) - 1)
                                    end if
                                 end if
                                 mode_s = lower_str(trim(sval))
                              else if (index(ltok, "criterion") == 1) then
                                 sval = trim(rval)
                                 if (len_trim(sval) >= 2) then
                                    if (sval(1:1) == "'" .or. sval(1:1) == '"') sval = sval(2:)
                                    if (sval(len_trim(sval):len_trim(sval)) == "'" .or. sval(len_trim(sval):len_trim(sval)) == '"') then
                                       sval = sval(:len_trim(sval) - 1)
                                    end if
                                 end if
                                 crit_s = lower_str(trim(sval))
                              else
                                 tmp = evaluate(rval)
                                 if (eval_error .or. size(tmp) /= 1) then
                                    print *, "Error: optional arguments must be scalar"
                                    eval_error = .true.; f = [bad_value]
                                    return
                                 end if
                                 if (index(ltok, "max_cp") == 1) then
                                    mcp = nint(tmp(1))
                                 else if (index(ltok, "minseg") == 1) then
                                    mseg = nint(tmp(1))
                                 else if (index(ltok, "plot_ic") == 1) then
                                    plotici = merge(1, 0, tmp(1) /= 0.0_dp)
                                 else if (index(ltok, "plot") == 1) then
                                    ploti = merge(1, 0, tmp(1) /= 0.0_dp)
                                 else if (index(ltok, "verbose") == 1) then
                                    verbosei = merge(1, 0, tmp(1) /= 0.0_dp)
                                 else
                                    print *, "Error: unknown named argument in cpfitaic()"
                                    eval_error = .true.; f = [bad_value]
                                    return
                                 end if
                              end if
                           else
                              pos_idx = pos_idx + 1
                              if (pos_idx == 1) then
                                 sval = trim(tok)
                                 if (len_trim(sval) >= 2) then
                                    if (sval(1:1) == "'" .or. sval(1:1) == '"') sval = sval(2:)
                                    if (sval(len_trim(sval):len_trim(sval)) == "'" .or. sval(len_trim(sval):len_trim(sval)) == '"') then
                                       sval = sval(:len_trim(sval) - 1)
                                    end if
                                 end if
                                 if (len_trim(sval) > 0 .and. .not. is_numeral(sval(1:1))) then
                                    mode_s = lower_str(trim(sval))
                                 else
                                    tmp = evaluate(tok)
                                    if (eval_error .or. size(tmp) /= 1) then
                                       print *, "Error: invalid optional argument in cpfitaic()"
                                       eval_error = .true.; f = [bad_value]
                                       return
                                    end if
                                    mcp = nint(tmp(1))
                                    pos_idx = 2
                                 end if
                              else if (pos_idx == 4) then
                                 sval = trim(tok)
                                 if (len_trim(sval) >= 2) then
                                    if (sval(1:1) == "'" .or. sval(1:1) == '"') sval = sval(2:)
                                    if (sval(len_trim(sval):len_trim(sval)) == "'" .or. sval(len_trim(sval):len_trim(sval)) == '"') then
                                       sval = sval(:len_trim(sval) - 1)
                                    end if
                                 end if
                                 if (len_trim(sval) > 0 .and. .not. is_numeral(sval(1:1)) .and. &
                                     lower_str(trim(sval)) /= ".true." .and. lower_str(trim(sval)) /= ".false." .and. &
                                     lower_str(trim(sval)) /= "true" .and. lower_str(trim(sval)) /= "false") then
                                    crit_s = lower_str(trim(sval))
                                    have_crit_pos = .true.
                                 else
                                    tmp = evaluate(tok)
                                    if (eval_error .or. size(tmp) /= 1) then
                                       print *, "Error: optional arguments must be scalar"
                                       eval_error = .true.; f = [bad_value]
                                       return
                                    end if
                                    ploti = merge(1, 0, tmp(1) /= 0.0_dp)
                                    have_crit_pos = .false.
                                 end if
                              else
                                 tmp = evaluate(tok)
                                 if (eval_error .or. size(tmp) /= 1) then
                                    print *, "Error: optional arguments must be scalar"
                                    eval_error = .true.; f = [bad_value]
                                    return
                                 end if
                                 select case (pos_idx)
                                 case (2)
                                    mcp = nint(tmp(1))
                                 case (3)
                                    mseg = nint(tmp(1))
                                 case (4)
                                    ploti = merge(1, 0, tmp(1) /= 0.0_dp)
                                 case (5)
                                    if (have_crit_pos) then
                                       ploti = merge(1, 0, tmp(1) /= 0.0_dp)
                                    else
                                       plotici = merge(1, 0, tmp(1) /= 0.0_dp)
                                    end if
                                 case (6)
                                    if (have_crit_pos) then
                                       plotici = merge(1, 0, tmp(1) /= 0.0_dp)
                                    else
                                       verbosei = merge(1, 0, tmp(1) /= 0.0_dp)
                                    end if
                                 case (7)
                                    verbosei = merge(1, 0, tmp(1) /= 0.0_dp)
                                 case default
                                    print *, "Error: too many arguments for cpfitaic()"
                                    eval_error = .true.; f = [bad_value]
                                    return
                                 end select
                              end if
                           end if
                        end do
                        f = cpfitaic(arg1, mode=trim(mode_s), max_cp=max(0, mcp), minseg=max(2, mseg), criterion=trim(crit_s), &
                                     plot=ploti, plot_ic=plotici, verbose=verbosei)
                     end block

                  case ("distaicscan")
                     block
                        logical :: do_verbose
                        logical :: do_nct
                        integer :: verbose_i
                        character(len=:), allocatable :: labels(:), tok, aexpr
                        character(len=len_name) :: aname
                        logical :: is_named, ok_named
                        integer :: n_args, i_arg, peq
                        do_verbose = .true.
                        do_nct = .false.
                        call split_by_comma(expr(pstart:pend - 1), n_args, labels)
                        if (n_args < 1) then
                           print *, "Error: distaicscan() requires a data argument"
                           eval_error = .true.; f = [bad_value]
                           return
                        end if
                        arg1 = evaluate(adjustl(labels(1)))
                        if (eval_error .or. size(arg1) < 1) then
                           eval_error = .true.; f = [bad_value]
                           return
                        end if
                        do i_arg = 2, n_args
                           tok = adjustl(labels(i_arg))
                           call parse_call_actual(tok, is_named, aname, aexpr, ok_named)
                           if (.not. is_named) then
                              peq = index(tok, "=")
                              if (peq == 0) peq = top_level_keyword_eq_pos(tok)
                              if (peq > 0) then
                                 is_named = .true.
                                 ok_named = .true.
                                 aname = adjustl(trim(tok(1:peq - 1)))
                                 aexpr = adjustl(trim(tok(peq + 1:)))
                              end if
                           end if
                           if (.not. ok_named) then
                              eval_error = .true.; f = [bad_value]
                              return
                           end if
                           if (is_named) then
                              select case (trim(aname))
                              case ("verbose")
                                 arg2 = evaluate(adjustl(aexpr))
                                 if (eval_error .or. size(arg2) /= 1) then
                                    print *, "Error: verbose must be scalar"
                                    eval_error = .true.; f = [bad_value]
                                    return
                                 end if
                                 do_verbose = (arg2(1) /= 0.0_dp)
                              case ("nct")
                                 arg2 = evaluate(adjustl(aexpr))
                                 if (eval_error .or. size(arg2) /= 1) then
                                    print *, "Error: nct must be scalar logical"
                                    eval_error = .true.; f = [bad_value]
                                    return
                                 end if
                                 do_nct = (arg2(1) /= 0.0_dp)
                              case default
                                 print *, "Error: unknown argument for distaicscan: ", trim(aname)
                                 eval_error = .true.; f = [bad_value]
                                 return
                              end select
                           else
                              if (i_arg == 2) then
                                 arg2 = evaluate(adjustl(tok))
                                 if (eval_error .or. size(arg2) /= 1) then
                                    print *, "Error: verbose must be scalar"
                                    eval_error = .true.; f = [bad_value]
                                    return
                                 end if
                                 do_verbose = (arg2(1) /= 0.0_dp)
                              else if (i_arg == 3) then
                                 arg2 = evaluate(adjustl(tok))
                                 if (eval_error .or. size(arg2) /= 1) then
                                    print *, "Error: nct must be scalar logical"
                                    eval_error = .true.; f = [bad_value]
                                    return
                                 end if
                                 do_nct = (arg2(1) /= 0.0_dp)
                              else
                                 print *, "Error: unexpected positional argument for distaicscan"
                                 eval_error = .true.; f = [bad_value]
                                 return
                              end if
                           end if
                        end do
                        if (do_verbose) then
                           verbose_i = 1
                        else
                           verbose_i = 0
                        end if
                        call distaicscan(arg1, verbose=verbose_i, nct=do_nct)
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
                        if (n_args < 2) then
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

                  case ("dist_regress")
                     block
                        logical :: use_intcp
                        integer :: n_pred, eqpos
                        character(len=:), allocatable :: tok, ltok, rval, dist_s
                       real(kind=dp), allocatable :: tmp(:), df_vals(:), beta_vals(:)
                       logical :: have_df, have_beta

                        call split_by_comma(expr(pstart:pend - 1), n_args, labels)
                        pos = pend + 1
                        if (pos > lenstr) then
                           curr_char = char(0)
                        else
                           curr_char = expr(pos:pos); pos = pos + 1
                        end if
                        if (n_args < 2) then
                           print *, "Error: function needs two arguments"
                           eval_error = .true.; f = [bad_value]
                           return
                        end if

                        dist_s = adjustl(trim(labels(1)))
                        if (len_trim(dist_s) >= 2) then
                           if ((dist_s(1:1) == '"' .and. dist_s(len_trim(dist_s):len_trim(dist_s)) == '"') .or. &
                               (dist_s(1:1) == "'" .and. dist_s(len_trim(dist_s):len_trim(dist_s)) == "'")) then
                              dist_s = dist_s(2:len_trim(dist_s) - 1)
                           end if
                        end if
                        dist_s = lower_str(adjustl(trim(dist_s)))
                        if (dist_s /= str_normal .and. dist_s /= str_student_t &
                            .and. dist_s /= str_laplace .and. dist_s /= str_ged &
                            .and. dist_s /= str_sech) then
                           print *, "Error: dist_regress() dist must be one of " // &
                             str_normal // " " // str_student_t // &
                             " " // str_laplace // " " // str_ged // " " // &
                             str_sech
                           eval_error = .true.; f = [bad_value]
                           return
                        end if
                        arg1 = evaluate(adjustl(labels(2)))
                        if (eval_error .or. size(arg1) < 1) then
                           eval_error = .true.; f = [bad_value]
                           return
                        end if

                        use_intcp = .true.
                        have_df = .false.
                        have_beta = .false.
                       allocate (beta_vals(0))
                        allocate (args(max(1, n_args - 2)))
                        n_pred = 0
                        do i_arg = 3, n_args
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
                           else if (index(ltok, "df") == 1) then
                              eqpos = index(tok, "=")
                              if (eqpos == 0) then
                                 print *, "Error: df must be given as df=..."
                                 eval_error = .true.; f = [bad_value]; return
                              end if
                              rval = adjustl(tok(eqpos + 1:))
                              tmp = evaluate(rval)
                              if (eval_error) then
                                 f = [bad_value]; return
                              end if
                              if (size(tmp) < 1) then
                                 print *, "Error: df must be non-empty"
                                 eval_error = .true.; f = [bad_value]; return
                              end if
                              df_vals = tmp
                              have_df = .true.
                           else if (index(ltok, "beta") == 1) then
                              eqpos = index(tok, "=")
                              if (eqpos == 0) then
                                 print *, "Error: beta must be given as beta=..."
                                 eval_error = .true.; f = [bad_value]; return
                              end if
                              rval = adjustl(tok(eqpos + 1:))
                              tmp = evaluate(rval)
                              if (eval_error) then
                                 f = [bad_value]; return
                              end if
                              if (size(tmp) < 1) then
                                 print *, "Error: beta must be non-empty"
                                 eval_error = .true.; f = [bad_value]; return
                              end if
                              beta_vals = tmp
                              have_beta = .true.
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

                        n1 = size(arg1)
                        if (n1 < 2) then
                           print *, "Error: function array arguments must have sizes > 1"
                           eval_error = .true.; f = [bad_value]; return
                        end if
                        do i_arg = 1, n_pred
                           if (size(args(i_arg)%v) /= n1) then
                              print "(a,i0,1x,i0,a)", "Error: function array arguments have sizes ", &
                                 n1, size(args(i_arg)%v), " must be equal"
                              eval_error = .true.; f = [bad_value]; return
                           end if
                        end do

                        if (n_pred == 0) then
                           if (have_df) then
                              if (have_beta) then
                                 call dist_regress(trim(dist_s), arg1, intcp=use_intcp, df=df_vals, beta=beta_vals)
                              else
                                 call dist_regress(trim(dist_s), arg1, intcp=use_intcp, df=df_vals)
                              end if
                           else
                              if (have_beta) then
                                 call dist_regress(trim(dist_s), arg1, intcp=use_intcp, beta=beta_vals)
                              else
                                 call dist_regress(trim(dist_s), arg1, intcp=use_intcp)
                              end if
                           end if
                        else if (n_pred == 1) then
                           if (have_df) then
                              if (have_beta) then
                                 call dist_regress(trim(dist_s), arg1, args(1)%v, intcp=use_intcp, df=df_vals, beta=beta_vals)
                              else
                                 call dist_regress(trim(dist_s), arg1, args(1)%v, intcp=use_intcp, df=df_vals)
                              end if
                           else
                              if (have_beta) then
                                 call dist_regress(trim(dist_s), arg1, args(1)%v, intcp=use_intcp, beta=beta_vals)
                              else
                                 call dist_regress(trim(dist_s), arg1, args(1)%v, intcp=use_intcp)
                              end if
                           end if
                        else
                           allocate (xmat(n1, n_pred))
                           do i_arg = 1, n_pred
                              xmat(:, i_arg) = args(i_arg)%v
                           end do
                           if (have_df) then
                              if (have_beta) then
                                 call dist_regress(trim(dist_s), arg1, xmat, intcp=use_intcp, df=df_vals, beta=beta_vals)
                              else
                                 call dist_regress(trim(dist_s), arg1, xmat, intcp=use_intcp, df=df_vals)
                              end if
                           else
                              if (have_beta) then
                                 call dist_regress(trim(dist_s), arg1, xmat, intcp=use_intcp, beta=beta_vals)
                              else
                                 call dist_regress(trim(dist_s), arg1, xmat, intcp=use_intcp)
                              end if
                           end if
                        end if

                        suppress_result = .true.
                        f = [real(kind=dp) ::]
                     end block

                  case ("huber_regress", "bisquare_regress")
                     block
                        logical :: use_intcp
                        integer :: n_pred, eqpos
                        character(len=:), allocatable :: tok, ltok, rval
                        real(kind=dp), allocatable :: tmp(:)
                        real(kind=dp) :: cval
                        logical :: have_c

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
                        have_c = .false.
                        cval = 1.345_dp
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
                           else if (index(ltok, "c") == 1) then
                              eqpos = index(tok, "=")
                              if (eqpos == 0) then
                                 print *, "Error: c must be given as c=..."
                                 eval_error = .true.; f = [bad_value]; return
                              end if
                              rval = adjustl(tok(eqpos + 1:))
                              tmp = evaluate(rval)
                              if (eval_error) then
                                 f = [bad_value]; return
                              end if
                              if (size(tmp) /= 1) then
                                 print *, "Error: c must be scalar"
                                 eval_error = .true.; f = [bad_value]; return
                              end if
                              cval = tmp(1)
                              have_c = .true.
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
                        if (n_pred /= 1) then
                           print "(a,a,a)", "Error: ", trim(id), "() currently supports one predictor"
                           eval_error = .true.; f = [bad_value]; return
                        end if

                        n1 = size(arg1)
                        if (n1 < 2) then
                           print *, "Error: function array arguments must have sizes > 1"
                           eval_error = .true.; f = [bad_value]; return
                        end if
                        if (size(args(1)%v) /= n1) then
                           print "(a,i0,1x,i0,a)", "Error: function array arguments have sizes ", &
                              n1, size(args(1)%v), " must be equal"
                           eval_error = .true.; f = [bad_value]; return
                        end if

                        if (trim(id) == "huber_regress") then
                           if (have_c) then
                              call huber_regress(arg1, args(1)%v, c=cval, intcp=use_intcp)
                           else
                              call huber_regress(arg1, args(1)%v, intcp=use_intcp)
                           end if
                        else
                           if (have_c) then
                              call bisquare_regress(arg1, args(1)%v, c=cval, intcp=use_intcp)
                           else
                              call bisquare_regress(arg1, args(1)%v, intcp=use_intcp)
                           end if
                        end if
                        suppress_result = .true.
                        f = [real(kind=dp) ::]
                     end block

                  case ("arfit")
                     block
                        logical :: have_k1, have_k2, have_acf, have_lb, have_kvec
                        integer :: acf_lags, lb_lags, eqpos, j
                        integer, allocatable :: kvec(:)
                        character(len=:), allocatable :: tok, ltok, rval, sval
                        character(len=16) :: method_s
                        real(kind=dp), allocatable :: tmp(:)

                        call split_by_comma(expr(pstart:pend - 1), n_args, labels)
                        if (n_args < 2) then
                           print *, "Error: function needs two arguments"
                           eval_error = .true.; f = [bad_value]; return
                        end if
                        have_k1 = .false.
                        have_k2 = .false.
                        have_kvec = .false.
                        have_acf = .false.
                        have_lb = .false.
                        acf_lags = 0
                        lb_lags = 0
                        method_s = "ls"
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
                           else if (index(ltok, "method") == 1) then
                              eqpos = index(tok, "=")
                              if (eqpos == 0) then
                                 print *, "Error: method must be given as method=..."
                                 eval_error = .true.; f = [bad_value]; return
                              end if
                              sval = trim(tok(eqpos + 1:))
                              if (len_trim(sval) >= 2) then
                                 if (sval(1:1) == "'" .or. sval(1:1) == '"') sval = sval(2:)
                                 if (sval(len_trim(sval):len_trim(sval)) == "'" .or. sval(len_trim(sval):len_trim(sval)) == '"') then
                                    sval = sval(:len_trim(sval) - 1)
                                 end if
                              end if
                              method_s = lower_str(trim(sval))
                           else if (.not. have_k1) then
                              tmp = evaluate(tok)
                              if (eval_error) then
                                 f = [bad_value]; return
                              end if
                              if (size(tmp) < 1) then
                                 print *, "Error: second argument of arfit() must be non-empty"
                                 eval_error = .true.; f = [bad_value]; return
                              end if
                              if (size(tmp) == 1) then
                                 n1 = nint(tmp(1))
                              else
                                 allocate (kvec(size(tmp)))
                                 kvec = nint(tmp)
                                 n1 = kvec(1)
                                 have_kvec = .true.
                              end if
                              have_k1 = .true.
                            else if (.not. have_k2) then
                              if (have_kvec) then
                                 print *, "Error: arfit() with vector order does not allow a third positional argument"
                                 eval_error = .true.; f = [bad_value]; return
                              end if
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
                        if ((.not. have_kvec .and. (n1 < 0 .or. (have_k2 .and. n2 < 0))) .or. acf_lags < 0 .or. lb_lags < 0) then
                           print *, "Error: arfit() lag order must be >= 0"
                           eval_error = .true.; f = [bad_value]; return
                        end if
                        if (have_kvec) then
                           if (any(kvec < 0)) then
                              print *, "Error: arfit() lag order must be >= 0"
                              eval_error = .true.; f = [bad_value]
                           else
                              do j = 1, size(kvec)
                                 if (have_acf .and. have_lb) then
                                    call arfit(arg1, kvec(j), nacf=acf_lags, nlb=lb_lags, method=trim(method_s), header=(j == 1))
                                 else if (have_acf) then
                                    call arfit(arg1, kvec(j), nacf=acf_lags, method=trim(method_s), header=(j == 1))
                                 else if (have_lb) then
                                    call arfit(arg1, kvec(j), nlb=lb_lags, method=trim(method_s), header=(j == 1))
                                  else
                                    call arfit(arg1, kvec(j), method=trim(method_s), header=(j == 1))
                                  end if
                               end do
                            end if
                         else
                            if (have_k2) then
                               if (have_acf .and. have_lb) then
                                 call arfit(arg1, n1, n2, nacf=acf_lags, nlb=lb_lags, method=trim(method_s))
                               else if (have_acf) then
                                 call arfit(arg1, n1, n2, nacf=acf_lags, method=trim(method_s))
                               else if (have_lb) then
                                 call arfit(arg1, n1, n2, nlb=lb_lags, method=trim(method_s))
                               else
                                 call arfit(arg1, n1, n2, method=trim(method_s))
                               end if
                            else
                               if (have_acf .and. have_lb) then
                                 call arfit(arg1, n1, nacf=acf_lags, nlb=lb_lags, method=trim(method_s))
                               else if (have_acf) then
                                 call arfit(arg1, n1, nacf=acf_lags, method=trim(method_s))
                               else if (have_lb) then
                                 call arfit(arg1, n1, nlb=lb_lags, method=trim(method_s))
                               else
                                 call arfit(arg1, n1, method=trim(method_s))
                               end if
                            end if
                         end if
                        if (allocated(kvec)) deallocate (kvec)
                        suppress_result = .true.
                        f = [real(kind=dp) ::]
                     end block

                  case ("arsimfit")
                     block
                        logical :: have_k2, have_acf, have_lb, have_kvec, parse_k_arg
                        integer :: acf_lags, lb_lags, eqpos, j, n_sim
                        integer :: first_opt_arg
                        integer, allocatable :: kvec(:)
                        character(len=:), allocatable :: tok, ltok, rval, sval
                        character(len=16) :: method_s
                        real(kind=dp), allocatable :: tmp(:), phi(:), xsim(:)

                        call split_by_comma(expr(pstart:pend - 1), n_args, labels)
                        if (n_args < 2) then
                           print *, "Error: function needs two arguments"
                           eval_error = .true.; f = [bad_value]; return
                        end if

                        tmp = evaluate(labels(1))
                        if (eval_error) then
                           f = [bad_value]; return
                        end if
                        if (size(tmp) /= 1) then
                           print *, "Error: first argument of arsimfit() must be scalar"
                           eval_error = .true.; f = [bad_value]; return
                        end if
                        n_sim = nint(tmp(1))
                        if (n_sim < 1) then
                           print *, "Error: arsimfit() requires n > 0"
                           eval_error = .true.; f = [bad_value]; return
                        end if

                        phi = evaluate(labels(2))
                        if (eval_error) then
                           f = [bad_value]; return
                        end if
                        if (size(phi) < 1) then
                           print *, "Error: arsimfit() requires non-empty AR coefficients"
                           eval_error = .true.; f = [bad_value]; return
                        end if

                        have_k2 = .false.
                        have_kvec = .false.
                        have_acf = .false.
                        have_lb = .false.
                        acf_lags = 0
                        lb_lags = 0
                        method_s = "ls"
                        n1 = size(phi)
                        first_opt_arg = 3
                        parse_k_arg = .false.
                        if (n_args >= 3) then
                           tok = adjustl(labels(3))
                           eqpos = index(tok, "=")
                           if (eqpos > 0) then
                              ltok = lower_str(adjustl(tok(:eqpos - 1)))
                              if (.not. (index(ltok, "acf") == 1 .or. index(ltok, "lb") == 1 .or. index(ltok, "method") == 1)) then
                                 parse_k_arg = .true.
                              end if
                           else
                              parse_k_arg = .true.
                           end if
                        end if
                        if (parse_k_arg) then
                           tmp = evaluate(labels(3))
                           if (eval_error) then
                              f = [bad_value]; return
                           end if
                           if (size(tmp) < 1) then
                              print *, "Error: third argument of arsimfit() must be non-empty"
                              eval_error = .true.; f = [bad_value]; return
                           end if
                           if (size(tmp) == 1) then
                              n1 = nint(tmp(1))
                           else
                              allocate (kvec(size(tmp)))
                              kvec = nint(tmp)
                              n1 = kvec(1)
                              have_kvec = .true.
                           end if
                           first_opt_arg = 4
                        end if

                        do i_arg = first_opt_arg, n_args
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
                           else if (index(ltok, "method") == 1) then
                              eqpos = index(tok, "=")
                              if (eqpos == 0) then
                                 print *, "Error: method must be given as method=..."
                                 eval_error = .true.; f = [bad_value]; return
                              end if
                              sval = trim(tok(eqpos + 1:))
                              if (len_trim(sval) >= 2) then
                                 if (sval(1:1) == "'" .or. sval(1:1) == '"') sval = sval(2:)
                                 if (sval(len_trim(sval):len_trim(sval)) == "'" .or. sval(len_trim(sval):len_trim(sval)) == '"') then
                                    sval = sval(:len_trim(sval) - 1)
                                 end if
                              end if
                              method_s = lower_str(trim(sval))
                           else if (.not. have_k2) then
                              if (have_kvec) then
                                 print *, "Error: arsimfit() with vector order does not allow a fourth positional argument"
                                 eval_error = .true.; f = [bad_value]; return
                              end if
                              tmp = evaluate(tok)
                              if (eval_error) then
                                 f = [bad_value]; return
                              end if
                              if (size(tmp) /= 1) then
                                 print *, "Error: fourth argument of arsimfit() must be scalar"
                                 eval_error = .true.; f = [bad_value]; return
                              end if
                              n2 = nint(tmp(1))
                              have_k2 = .true.
                           else
                              print *, "Error: arsimfit() takes at most 4 arguments plus acf=..."
                              eval_error = .true.; f = [bad_value]; return
                           end if
                        end do

                        if ((.not. have_kvec .and. (n1 < 0 .or. (have_k2 .and. n2 < 0))) .or. acf_lags < 0 .or. lb_lags < 0) then
                           print *, "Error: arsimfit() lag order must be >= 0"
                           eval_error = .true.; f = [bad_value]; return
                        end if
                        if (have_kvec) then
                           if (any(kvec < 0)) then
                              print *, "Error: arsimfit() lag order must be >= 0"
                              eval_error = .true.; f = [bad_value]
                           else
                              xsim = arsim(n_sim, phi)
                              do j = 1, size(kvec)
                                 if (have_acf .and. have_lb) then
                                    call arfit(xsim, kvec(j), nacf=acf_lags, nlb=lb_lags, method=trim(method_s), header=(j == 1), true_phi=phi)
                                 else if (have_acf) then
                                    call arfit(xsim, kvec(j), nacf=acf_lags, method=trim(method_s), header=(j == 1), true_phi=phi)
                                 else if (have_lb) then
                                    call arfit(xsim, kvec(j), nlb=lb_lags, method=trim(method_s), header=(j == 1), true_phi=phi)
                                 else
                                    call arfit(xsim, kvec(j), method=trim(method_s), header=(j == 1), true_phi=phi)
                                 end if
                              end do
                           end if
                        else
                           if (have_k2) then
                              if (have_acf .and. have_lb) then
                                 call arsimfit(n_sim, phi, n1, n2, nacf=acf_lags, nlb=lb_lags, method=trim(method_s))
                              else if (have_acf) then
                                 call arsimfit(n_sim, phi, n1, n2, nacf=acf_lags, method=trim(method_s))
                              else if (have_lb) then
                                 call arsimfit(n_sim, phi, n1, n2, nlb=lb_lags, method=trim(method_s))
                              else
                                 call arsimfit(n_sim, phi, n1, n2, method=trim(method_s))
                              end if
                           else
                              if (have_acf .and. have_lb) then
                                 call arsimfit(n_sim, phi, n1, nacf=acf_lags, nlb=lb_lags, method=trim(method_s))
                              else if (have_acf) then
                                 call arsimfit(n_sim, phi, n1, nacf=acf_lags, method=trim(method_s))
                              else if (have_lb) then
                                 call arsimfit(n_sim, phi, n1, nlb=lb_lags, method=trim(method_s))
                              else
                                 call arsimfit(n_sim, phi, n1, method=trim(method_s))
                              end if
                           end if
                        end if

                        if (allocated(kvec)) deallocate (kvec)
                        suppress_result = .true.
                        f = [real(kind=dp) ::]
                     end block

                  case ("masimfit")
                     block
                        logical :: have_k2, have_acf, have_lb, have_iter, have_kvec, parse_k_arg
                        integer :: acf_lags, lb_lags, iter_arg, eqpos, n_sim
                        integer :: first_opt_arg
                        integer, allocatable :: kvec(:)
                        character(len=:), allocatable :: tok, ltok, rval
                        real(kind=dp), allocatable :: tmp(:), theta(:)

                        call split_by_comma(expr(pstart:pend - 1), n_args, labels)
                        if (n_args < 2) then
                           print *, "Error: function needs two arguments"
                           eval_error = .true.; f = [bad_value]; return
                        end if

                        tmp = evaluate(labels(1))
                        if (eval_error) then
                           f = [bad_value]; return
                        end if
                        if (size(tmp) /= 1) then
                           print *, "Error: first argument of masimfit() must be scalar"
                           eval_error = .true.; f = [bad_value]; return
                        end if
                        n_sim = nint(tmp(1))
                        if (n_sim < 1) then
                           print *, "Error: masimfit() requires n > 0"
                           eval_error = .true.; f = [bad_value]; return
                        end if

                        theta = evaluate(labels(2))
                        if (eval_error) then
                           f = [bad_value]; return
                        end if
                        if (size(theta) < 1) then
                           print *, "Error: masimfit() requires non-empty MA coefficients"
                           eval_error = .true.; f = [bad_value]; return
                        end if

                        have_k2 = .false.
                        have_kvec = .false.
                        have_acf = .false.
                        have_lb = .false.
                        have_iter = .false.
                        acf_lags = 0
                        lb_lags = 0
                        iter_arg = 5
                        n1 = size(theta)
                        first_opt_arg = 3
                        parse_k_arg = .false.
                        if (n_args >= 3) then
                           tok = adjustl(labels(3))
                           eqpos = index(tok, "=")
                           if (eqpos > 0) then
                              ltok = lower_str(adjustl(tok(:eqpos - 1)))
                              if (.not. (index(ltok, "acf") == 1 .or. index(ltok, "lb") == 1 .or. index(ltok, "iter") == 1)) then
                                 parse_k_arg = .true.
                              end if
                           else
                              parse_k_arg = .true.
                           end if
                        end if
                        if (parse_k_arg) then
                           tmp = evaluate(labels(3))
                           if (eval_error) then
                              f = [bad_value]; return
                           end if
                           if (size(tmp) < 1) then
                              print *, "Error: third argument of masimfit() must be non-empty"
                              eval_error = .true.; f = [bad_value]; return
                           end if
                           if (size(tmp) == 1) then
                              n1 = nint(tmp(1))
                           else
                              allocate (kvec(size(tmp)))
                              kvec = nint(tmp)
                              n1 = kvec(1)
                              have_kvec = .true.
                           end if
                           first_opt_arg = 4
                        end if

                        do i_arg = first_opt_arg, n_args
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
                           else if (.not. have_k2) then
                              if (have_kvec) then
                                 print *, "Error: masimfit() with vector order does not allow a fourth positional argument"
                                 eval_error = .true.; f = [bad_value]; return
                              end if
                              tmp = evaluate(tok)
                              if (eval_error) then
                                 f = [bad_value]; return
                              end if
                              if (size(tmp) /= 1) then
                                 print *, "Error: fourth argument of masimfit() must be scalar"
                                 eval_error = .true.; f = [bad_value]; return
                              end if
                              n2 = nint(tmp(1))
                              have_k2 = .true.
                           else
                              print *, "Error: masimfit() takes at most 4 arguments plus acf=..."
                              eval_error = .true.; f = [bad_value]; return
                           end if
                        end do

                        if ((.not. have_kvec .and. (n1 < 0 .or. (have_k2 .and. n2 < 0))) .or. acf_lags < 0 .or. lb_lags < 0) then
                           print *, "Error: masimfit() lag order must be >= 0"
                           eval_error = .true.; f = [bad_value]; return
                        end if
                        if (have_iter .and. iter_arg < 1) then
                           print *, "Error: iter must be >= 1"
                           eval_error = .true.; f = [bad_value]; return
                        end if
                        if (have_kvec) then
                           if (any(kvec < 0)) then
                              print *, "Error: masimfit() lag order must be >= 0"
                              eval_error = .true.; f = [bad_value]
                           else
                              if (have_acf .and. have_lb .and. have_iter) then
                                 call masimfit(n_sim, theta, kvec, nacf=acf_lags, nlb=lb_lags, niter=iter_arg)
                              else if (have_acf .and. have_lb) then
                                 call masimfit(n_sim, theta, kvec, nacf=acf_lags, nlb=lb_lags)
                              else if (have_acf .and. have_iter) then
                                 call masimfit(n_sim, theta, kvec, nacf=acf_lags, niter=iter_arg)
                              else if (have_lb .and. have_iter) then
                                 call masimfit(n_sim, theta, kvec, nlb=lb_lags, niter=iter_arg)
                              else if (have_acf) then
                                 call masimfit(n_sim, theta, kvec, nacf=acf_lags)
                              else if (have_lb) then
                                 call masimfit(n_sim, theta, kvec, nlb=lb_lags)
                              else if (have_iter) then
                                 call masimfit(n_sim, theta, kvec, niter=iter_arg)
                              else
                                 call masimfit(n_sim, theta, kvec)
                              end if
                           end if
                        else
                           if (have_k2) then
                              if (have_acf .and. have_lb .and. have_iter) then
                                 call masimfit(n_sim, theta, n1, n2, nacf=acf_lags, nlb=lb_lags, niter=iter_arg)
                              else if (have_acf .and. have_lb) then
                                 call masimfit(n_sim, theta, n1, n2, nacf=acf_lags, nlb=lb_lags)
                              else if (have_acf .and. have_iter) then
                                 call masimfit(n_sim, theta, n1, n2, nacf=acf_lags, niter=iter_arg)
                              else if (have_lb .and. have_iter) then
                                 call masimfit(n_sim, theta, n1, n2, nlb=lb_lags, niter=iter_arg)
                              else if (have_acf) then
                                 call masimfit(n_sim, theta, n1, n2, nacf=acf_lags)
                              else if (have_lb) then
                                 call masimfit(n_sim, theta, n1, n2, nlb=lb_lags)
                              else if (have_iter) then
                                 call masimfit(n_sim, theta, n1, n2, niter=iter_arg)
                              else
                                 call masimfit(n_sim, theta, n1, n2)
                              end if
                           else
                              if (have_acf .and. have_lb .and. have_iter) then
                                 call masimfit(n_sim, theta, n1, nacf=acf_lags, nlb=lb_lags, niter=iter_arg)
                              else if (have_acf .and. have_lb) then
                                 call masimfit(n_sim, theta, n1, nacf=acf_lags, nlb=lb_lags)
                              else if (have_acf .and. have_iter) then
                                 call masimfit(n_sim, theta, n1, nacf=acf_lags, niter=iter_arg)
                              else if (have_lb .and. have_iter) then
                                 call masimfit(n_sim, theta, n1, nlb=lb_lags, niter=iter_arg)
                              else if (have_acf) then
                                 call masimfit(n_sim, theta, n1, nacf=acf_lags)
                              else if (have_lb) then
                                 call masimfit(n_sim, theta, n1, nlb=lb_lags)
                              else if (have_iter) then
                                 call masimfit(n_sim, theta, n1, niter=iter_arg)
                              else
                                 call masimfit(n_sim, theta, n1)
                              end if
                           end if
                        end if

                        if (allocated(kvec)) deallocate (kvec)
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

                  case ("lowess")
                     block
                        logical :: have_span, do_plot, do_points
                        integer :: eqpos, nit
                        real(kind=dp), allocatable :: spanv(:), tmp(:)
                        character(len=:), allocatable :: tok, ltok, rval
                        if (.not. have_second) then
                           print *, "Error: function needs two arguments"
                           eval_error = .true.; f = [bad_value]
                           return
                        end if
                        if (size(arg1) /= size(arg2)) then
                           print "(a,i0,1x,i0,a)", "Error: lowess() argument sizes ", &
                              size(arg1), size(arg2), " must be equal"
                           eval_error = .true.; f = [bad_value]
                           return
                        end if
                        if (size(arg1) < 2) then
                           print *, "Error: lowess() needs size >= 2"
                           eval_error = .true.; f = [bad_value]
                           return
                        end if
                        call split_by_comma(expr(pstart:pend - 1), n_args, labels)
                        have_span = .false.
                        nit = 2
                        do_plot = .true.
                        do_points = .true.
                        do i_arg = 3, n_args
                           tok = adjustl(labels(i_arg))
                           ltok = lower_str(tok)
                           eqpos = index(tok, "=")
                           if (eqpos > 0) then
                              rval = adjustl(tok(eqpos + 1:))
                              tmp = evaluate(rval)
                              if (eval_error .or. size(tmp) < 1) then
                                 print *, "Error: invalid optional argument in lowess()"
                                 eval_error = .true.; f = [bad_value]
                                 return
                              end if
                              if (index(ltok, "span") == 1) then
                                 spanv = tmp
                                 have_span = .true.
                              else if (index(ltok, "it") == 1) then
                                 if (size(tmp) /= 1) then
                                    print *, "Error: it must be scalar"
                                    eval_error = .true.; f = [bad_value]
                                    return
                                 end if
                                 nit = max(0, nint(tmp(1)))
                              else if (index(ltok, "plot") == 1) then
                                 if (size(tmp) /= 1) then
                                    print *, "Error: plot must be scalar"
                                    eval_error = .true.; f = [bad_value]
                                    return
                                 end if
                                 do_plot = (tmp(1) /= 0.0_dp)
                              else if (index(ltok, "points") == 1) then
                                 if (size(tmp) /= 1) then
                                    print *, "Error: points must be scalar"
                                    eval_error = .true.; f = [bad_value]
                                    return
                                 end if
                                 do_points = (tmp(1) /= 0.0_dp)
                              else
                                 print *, "Error: unknown optional argument in lowess()"
                                 eval_error = .true.; f = [bad_value]
                                 return
                              end if
                           else
                              tmp = evaluate(tok)
                              if (eval_error .or. size(tmp) < 1) then
                                 print *, "Error: invalid positional argument in lowess()"
                                 eval_error = .true.; f = [bad_value]
                                 return
                              end if
                              if (.not. have_span) then
                                 spanv = tmp
                                 have_span = .true.
                              else if (size(tmp) == 1) then
                                 if (i_arg == 4) then
                                    nit = max(0, nint(tmp(1)))
                                 else if (i_arg == 5) then
                                    do_plot = (tmp(1) /= 0.0_dp)
                                 else if (i_arg == 6) then
                                    do_points = (tmp(1) /= 0.0_dp)
                                 else
                                    print *, "Error: too many arguments for lowess()"
                                    eval_error = .true.; f = [bad_value]
                                    return
                                 end if
                              else
                                 print *, "Error: too many arguments for lowess()"
                                 eval_error = .true.; f = [bad_value]
                                 return
                              end if
                           end if
                        end do
                        if (have_span) then
                           if (any(spanv <= 0.0_dp) .or. any(spanv > 1.0_dp)) then
                              print *, "Error: lowess() span must satisfy 0 < span <= 1"
                              eval_error = .true.; f = [bad_value]
                              return
                           end if
                        end if

                        if (.not. have_span) then
                           f = lowess(arg1, arg2, it=nit, plot=do_plot, points=do_points)
                        else if (size(spanv) == 1) then
                           f = lowess(arg1, arg2, spanv(1), it=nit, plot=do_plot, points=do_points)
                        else
                           f = lowess(arg1, arg2, spanv, it=nit, plot=do_plot, points=do_points)
                        end if
                     end block

                  case ("lowesscv")
                     block
                        logical :: have_span, do_plot, do_points
                        integer :: eqpos, nit
                        real(kind=dp), allocatable :: spanv(:), tmp(:)
                        character(len=:), allocatable :: tok, ltok, rval
                        if (.not. have_second) then
                           print *, "Error: function needs two arguments"
                           eval_error = .true.; f = [bad_value]
                           return
                        end if
                        if (size(arg1) /= size(arg2)) then
                           print "(a,i0,1x,i0,a)", "Error: lowesscv() argument sizes ", &
                              size(arg1), size(arg2), " must be equal"
                           eval_error = .true.; f = [bad_value]
                           return
                        end if
                        if (size(arg1) < 3) then
                           print *, "Error: lowesscv() needs size >= 3"
                           eval_error = .true.; f = [bad_value]
                           return
                        end if
                        call split_by_comma(expr(pstart:pend - 1), n_args, labels)
                        have_span = .false.
                        nit = 2
                        do_plot = .true.
                        do_points = .true.
                        do i_arg = 3, n_args
                           tok = adjustl(labels(i_arg))
                           ltok = lower_str(tok)
                           eqpos = index(tok, "=")
                           if (eqpos > 0) then
                              rval = adjustl(tok(eqpos + 1:))
                              tmp = evaluate(rval)
                              if (eval_error .or. size(tmp) < 1) then
                                 print *, "Error: invalid optional argument in lowesscv()"
                                 eval_error = .true.; f = [bad_value]
                                 return
                              end if
                              if (index(ltok, "span") == 1) then
                                 spanv = tmp
                                 have_span = .true.
                              else if (index(ltok, "it") == 1) then
                                 if (size(tmp) /= 1) then
                                    print *, "Error: it must be scalar"
                                    eval_error = .true.; f = [bad_value]
                                    return
                                 end if
                                 nit = max(0, nint(tmp(1)))
                              else if (index(ltok, "plot") == 1) then
                                 if (size(tmp) /= 1) then
                                    print *, "Error: plot must be scalar"
                                    eval_error = .true.; f = [bad_value]
                                    return
                                 end if
                                 do_plot = (tmp(1) /= 0.0_dp)
                              else if (index(ltok, "points") == 1) then
                                 if (size(tmp) /= 1) then
                                    print *, "Error: points must be scalar"
                                    eval_error = .true.; f = [bad_value]
                                    return
                                 end if
                                 do_points = (tmp(1) /= 0.0_dp)
                              else
                                 print *, "Error: unknown optional argument in lowesscv()"
                                 eval_error = .true.; f = [bad_value]
                                 return
                              end if
                           else
                              tmp = evaluate(tok)
                              if (eval_error .or. size(tmp) < 1) then
                                 print *, "Error: invalid positional argument in lowesscv()"
                                 eval_error = .true.; f = [bad_value]
                                 return
                              end if
                              if (.not. have_span) then
                                 spanv = tmp
                                 have_span = .true.
                              else if (size(tmp) == 1) then
                                 if (i_arg == 4) then
                                    nit = max(0, nint(tmp(1)))
                                 else if (i_arg == 5) then
                                    do_plot = (tmp(1) /= 0.0_dp)
                                 else if (i_arg == 6) then
                                    do_points = (tmp(1) /= 0.0_dp)
                                 else
                                    print *, "Error: too many arguments for lowesscv()"
                                    eval_error = .true.; f = [bad_value]
                                    return
                                 end if
                              else
                                 print *, "Error: too many arguments for lowesscv()"
                                 eval_error = .true.; f = [bad_value]
                                 return
                              end if
                           end if
                        end do
                        if (have_span) then
                           if (any(spanv <= 0.0_dp) .or. any(spanv > 1.0_dp)) then
                              print *, "Error: lowesscv() span must satisfy 0 < span <= 1"
                              eval_error = .true.; f = [bad_value]
                              return
                           end if
                        end if

                        if (.not. have_span) then
                           f = lowesscv(arg1, arg2, it=nit, plot=do_plot, points=do_points)
                        else if (size(spanv) == 1) then
                           f = lowesscv(arg1, arg2, spanv(1), it=nit, plot=do_plot, points=do_points)
                        else
                           f = lowesscv(arg1, arg2, spanv, it=nit, plot=do_plot, points=do_points)
                        end if
                     end block

                  case ("knnreg")
                     block
                        logical :: have_k, do_plot, do_points
                        integer :: eqpos, ord
                        real(kind=dp), allocatable :: kv_r(:), tmp(:)
                        integer, allocatable :: kv(:)
                        character(len=:), allocatable :: tok, ltok, rval
                        if (.not. have_second) then
                           print *, "Error: function needs two arguments"
                           eval_error = .true.; f = [bad_value]
                           return
                        end if
                        if (size(arg1) /= size(arg2)) then
                           print "(a,i0,1x,i0,a)", "Error: knnreg() argument sizes ", &
                              size(arg1), size(arg2), " must be equal"
                           eval_error = .true.; f = [bad_value]
                           return
                        end if
                        if (size(arg1) < 2) then
                           print *, "Error: knnreg() needs size >= 2"
                           eval_error = .true.; f = [bad_value]
                           return
                        end if
                        call split_by_comma(expr(pstart:pend - 1), n_args, labels)
                        have_k = .false.
                        ord = 0
                        do_plot = .true.
                        do_points = .true.
                        do i_arg = 3, n_args
                           tok = adjustl(labels(i_arg))
                           ltok = lower_str(tok)
                           eqpos = index(tok, "=")
                           if (eqpos > 0) then
                              rval = adjustl(tok(eqpos + 1:))
                              tmp = evaluate(rval)
                              if (eval_error .or. size(tmp) < 1) then
                                 print *, "Error: invalid optional argument in knnreg()"
                                 eval_error = .true.; f = [bad_value]
                                 return
                              end if
                              if (index(ltok, "k") == 1) then
                                 kv_r = tmp
                                 kv = nint(kv_r)
                                 if (any(kv < 1)) then
                                    print *, "Error: k values must be >= 1"
                                    eval_error = .true.; f = [bad_value]
                                    return
                                 end if
                                 have_k = .true.
                              else if (index(ltok, "order") == 1) then
                                 if (size(tmp) /= 1) then
                                    print *, "Error: order must be scalar"
                                    eval_error = .true.; f = [bad_value]
                                    return
                                 end if
                                 ord = max(0, nint(tmp(1)))
                              else if (index(ltok, "plot") == 1) then
                                 if (size(tmp) /= 1) then
                                    print *, "Error: plot must be scalar"
                                    eval_error = .true.; f = [bad_value]
                                    return
                                 end if
                                 do_plot = (tmp(1) /= 0.0_dp)
                              else if (index(ltok, "points") == 1) then
                                 if (size(tmp) /= 1) then
                                    print *, "Error: points must be scalar"
                                    eval_error = .true.; f = [bad_value]
                                    return
                                 end if
                                 do_points = (tmp(1) /= 0.0_dp)
                              else
                                 print *, "Error: unknown optional argument in knnreg()"
                                 eval_error = .true.; f = [bad_value]
                                 return
                              end if
                           else
                              tmp = evaluate(tok)
                              if (eval_error .or. size(tmp) < 1) then
                                 print *, "Error: invalid positional argument in knnreg()"
                                 eval_error = .true.; f = [bad_value]
                                 return
                              end if
                              if (.not. have_k) then
                                 kv_r = tmp
                                 kv = nint(kv_r)
                                 if (any(kv < 1)) then
                                    print *, "Error: k values must be >= 1"
                                    eval_error = .true.; f = [bad_value]
                                    return
                                 end if
                                 have_k = .true.
                              else if (i_arg == 4 .and. size(tmp) == 1) then
                                 ord = max(0, nint(tmp(1)))
                              else if (i_arg == 5 .and. size(tmp) == 1) then
                                 do_plot = (tmp(1) /= 0.0_dp)
                              else if (i_arg == 6 .and. size(tmp) == 1) then
                                 do_points = (tmp(1) /= 0.0_dp)
                              else
                                 print *, "Error: too many arguments for knnreg()"
                                 eval_error = .true.; f = [bad_value]
                                 return
                              end if
                           end if
                        end do

                        if (.not. have_k) then
                           f = knnreg(arg1, arg2, order=ord, plot=do_plot, points=do_points)
                        else if (size(kv) == 1) then
                           f = knnreg(arg1, arg2, kv(1), order=ord, plot=do_plot, points=do_points)
                        else
                           f = knnreg(arg1, arg2, kv, order=ord, plot=do_plot, points=do_points)
                        end if
                     end block

                  case ("knnregcv")
                     block
                        logical :: have_k, do_plot, do_points
                        integer :: eqpos, ord
                        real(kind=dp), allocatable :: kv_r(:), tmp(:)
                        integer, allocatable :: kv(:)
                        character(len=:), allocatable :: tok, ltok, rval
                        if (.not. have_second) then
                           print *, "Error: function needs two arguments"
                           eval_error = .true.; f = [bad_value]
                           return
                        end if
                        if (size(arg1) /= size(arg2)) then
                           print "(a,i0,1x,i0,a)", "Error: knnregcv() argument sizes ", &
                              size(arg1), size(arg2), " must be equal"
                           eval_error = .true.; f = [bad_value]
                           return
                        end if
                        if (size(arg1) < 3) then
                           print *, "Error: knnregcv() needs size >= 3"
                           eval_error = .true.; f = [bad_value]
                           return
                        end if
                        call split_by_comma(expr(pstart:pend - 1), n_args, labels)
                        have_k = .false.
                        ord = 0
                        do_plot = .true.
                        do_points = .true.
                        do i_arg = 3, n_args
                           tok = adjustl(labels(i_arg))
                           ltok = lower_str(tok)
                           eqpos = index(tok, "=")
                           if (eqpos > 0) then
                              rval = adjustl(tok(eqpos + 1:))
                              tmp = evaluate(rval)
                              if (eval_error .or. size(tmp) < 1) then
                                 print *, "Error: invalid optional argument in knnregcv()"
                                 eval_error = .true.; f = [bad_value]
                                 return
                              end if
                              if (index(ltok, "k") == 1) then
                                 kv_r = tmp
                                 kv = nint(kv_r)
                                 if (any(kv < 1)) then
                                    print *, "Error: k values must be >= 1"
                                    eval_error = .true.; f = [bad_value]
                                    return
                                 end if
                                 have_k = .true.
                              else if (index(ltok, "order") == 1) then
                                 if (size(tmp) /= 1) then
                                    print *, "Error: order must be scalar"
                                    eval_error = .true.; f = [bad_value]
                                    return
                                 end if
                                 ord = max(0, nint(tmp(1)))
                              else if (index(ltok, "plot") == 1) then
                                 if (size(tmp) /= 1) then
                                    print *, "Error: plot must be scalar"
                                    eval_error = .true.; f = [bad_value]
                                    return
                                 end if
                                 do_plot = (tmp(1) /= 0.0_dp)
                              else if (index(ltok, "points") == 1) then
                                 if (size(tmp) /= 1) then
                                    print *, "Error: points must be scalar"
                                    eval_error = .true.; f = [bad_value]
                                    return
                                 end if
                                 do_points = (tmp(1) /= 0.0_dp)
                              else
                                 print *, "Error: unknown optional argument in knnregcv()"
                                 eval_error = .true.; f = [bad_value]
                                 return
                              end if
                           else
                              tmp = evaluate(tok)
                              if (eval_error .or. size(tmp) < 1) then
                                 print *, "Error: invalid positional argument in knnregcv()"
                                 eval_error = .true.; f = [bad_value]
                                 return
                              end if
                              if (.not. have_k) then
                                 kv_r = tmp
                                 kv = nint(kv_r)
                                 if (any(kv < 1)) then
                                    print *, "Error: k values must be >= 1"
                                    eval_error = .true.; f = [bad_value]
                                    return
                                 end if
                                 have_k = .true.
                              else if (i_arg == 4 .and. size(tmp) == 1) then
                                 ord = max(0, nint(tmp(1)))
                              else if (i_arg == 5 .and. size(tmp) == 1) then
                                 do_plot = (tmp(1) /= 0.0_dp)
                              else if (i_arg == 6 .and. size(tmp) == 1) then
                                 do_points = (tmp(1) /= 0.0_dp)
                              else
                                 print *, "Error: too many arguments for knnregcv()"
                                 eval_error = .true.; f = [bad_value]
                                 return
                              end if
                           end if
                        end do

                        if (.not. have_k) then
                           f = knnregcv(arg1, arg2, order=ord, plot=do_plot, points=do_points)
                        else if (size(kv) == 1) then
                           f = knnregcv(arg1, arg2, kv(1), order=ord, plot=do_plot, points=do_points)
                        else
                           f = knnregcv(arg1, arg2, kv, order=ord, plot=do_plot, points=do_points)
                        end if
                     end block

                  case ("kernelreg")
                     block
                        logical :: have_bw, have_order, do_points
                        integer :: eqpos
                        real(kind=dp), allocatable :: bwv(:), ordv_r(:), tmp(:)
                        integer, allocatable :: ordv(:)
                        character(len=:), allocatable :: tok, ltok, rval
                        if (.not. have_second) then
                           print *, "Error: function needs two arguments"
                           eval_error = .true.; f = [bad_value]
                           return
                        end if
                        if (size(arg1) /= size(arg2)) then
                           print "(a,i0,1x,i0,a)", "Error: kernelreg() argument sizes ", &
                              size(arg1), size(arg2), " must be equal"
                           eval_error = .true.; f = [bad_value]
                           return
                        end if
                        if (size(arg1) < 2) then
                           print *, "Error: kernelreg() needs size >= 2"
                           eval_error = .true.; f = [bad_value]
                           return
                        end if
                        call split_by_comma(expr(pstart:pend - 1), n_args, labels)
                        have_bw = .false.
                        have_order = .false.
                        do_points = .false.
                        do i_arg = 3, n_args
                           tok = adjustl(labels(i_arg))
                           ltok = lower_str(tok)
                           eqpos = index(tok, "=")
                           if (eqpos > 0) then
                              rval = adjustl(tok(eqpos + 1:))
                              tmp = evaluate(rval)
                              if (eval_error .or. size(tmp) < 1) then
                                 print *, "Error: invalid optional argument in kernelreg()"
                                 eval_error = .true.; f = [bad_value]
                                 return
                              end if
                              if (index(ltok, "order") == 1) then
                                 ordv_r = tmp
                                 ordv = nint(ordv_r)
                                 if (any(ordv < 0)) then
                                    print *, "Error: order values must be >= 0"
                                    eval_error = .true.; f = [bad_value]
                                    return
                                 end if
                                 have_order = .true.
                              else if (index(ltok, "points") == 1) then
                                 if (size(tmp) /= 1) then
                                    print *, "Error: points must be scalar"
                                    eval_error = .true.; f = [bad_value]
                                    return
                                 end if
                                 do_points = (tmp(1) /= 0.0_dp)
                              else if (index(ltok, "bw") == 1) then
                                 bwv = tmp
                                 have_bw = .true.
                              else
                                 print *, "Error: unknown optional argument in kernelreg()"
                                 eval_error = .true.; f = [bad_value]
                                 return
                              end if
                           else
                              tmp = evaluate(tok)
                              if (eval_error .or. size(tmp) < 1) then
                                 print *, "Error: invalid positional argument in kernelreg()"
                                 eval_error = .true.; f = [bad_value]
                                 return
                              end if
                              if (.not. have_bw) then
                                 bwv = tmp
                                 have_bw = .true.
                              else if (.not. have_order) then
                                 ordv_r = tmp
                                 ordv = nint(ordv_r)
                                 if (any(ordv < 0)) then
                                    print *, "Error: order values must be >= 0"
                                    eval_error = .true.; f = [bad_value]
                                    return
                                 end if
                                 have_order = .true.
                              else if (size(tmp) == 1) then
                                 do_points = (tmp(1) /= 0.0_dp)
                              else
                                 print *, "Error: too many arguments for kernelreg()"
                                 eval_error = .true.; f = [bad_value]
                                 return
                              end if
                           end if
                        end do

                        if (.not. have_bw) then
                           if (have_order .and. size(ordv) > 1) then
                              print *, "Error: vector order requires explicit bw"
                              eval_error = .true.; f = [bad_value]
                           else if (have_order) then
                              f = kernelreg(arg1, arg2, order=ordv(1), points=do_points)
                           else
                              f = kernelreg(arg1, arg2, points=do_points)
                           end if
                        else if (.not. have_order) then
                           if (size(bwv) == 1) then
                              f = kernelreg(arg1, arg2, bwv(1), points=do_points)
                           else
                              f = kernelreg(arg1, arg2, bwv, points=do_points)
                           end if
                        else
                           if (size(ordv) == 1) then
                              if (size(bwv) == 1) then
                                 f = kernelreg(arg1, arg2, bwv(1), ordv(1), points=do_points)
                              else
                                 f = kernelreg(arg1, arg2, bwv, ordv(1), points=do_points)
                              end if
                           else
                              if (size(bwv) == 1) then
                                 f = kernelreg(arg1, arg2, bwv(1), ordv, points=do_points)
                              else
                                 f = kernelreg(arg1, arg2, bwv, ordv, points=do_points)
                              end if
                           end if
                        end if
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

                  case ("kde")
                     if (.not. have_second) then
                        f = kde(arg1)
                     else if (size(arg2) /= 1) then
                        print *, "Error: second argument of kde() must be scalar"
                        eval_error = .true.; f = [bad_value]
                     else
                        nsize = nint(arg2(1))
                        if (nsize < 20) nsize = 20
                        f = kde(arg1, nsize)
                     end if

                  case ("runif", "rnorm", "rsech", "zeros", "ones") ! one-arg
                     if (have_second) then
                        print *, "Error: function ", trim(id), " takes one argument"
                        eval_error = .true.; f = [bad_value]
                     else
                        nsize = nint(arg1(1))
                        select case (id)
                        case ("runif"); f = runif(nsize)
                        case ("rnorm"); f = random_normal(nsize)
                        case ("rsech"); f = rsech(nsize)
                        case ("zeros"); f = zeros(nsize)
                        case ("ones"); f = ones(nsize)
                        end select
                    end if

                  case ("arange", "irange")
                     block
                        real(kind=dp), allocatable :: a1(:), a2(:), a3(:)
                        integer :: n_args_local
                        call split_by_comma(expr(pstart:pend - 1), n_args_local, labels)
                        if (n_args_local < 1 .or. n_args_local > 3) then
                           print *, "Error: function ", trim(id), " takes one, two, or three arguments"
                           eval_error = .true.; f = [bad_value]
                        else
                           a1 = evaluate(labels(1))
                           if (eval_error .or. size(a1) /= 1) then
                              print *, "Error: first argument of ", trim(id), "() must be scalar"
                              eval_error = .true.; f = [bad_value]
                           else if (n_args_local == 1) then
                              if (trim(id) == "arange") then
                                 f = arange(nint(a1(1)))
                              else
                                 f = real(irange(nint(a1(1))), kind=dp)
                              end if
                           else
                              a2 = evaluate(labels(2))
                              if (eval_error .or. size(a2) /= 1) then
                                 print *, "Error: second argument of ", trim(id), "() must be scalar"
                                 eval_error = .true.; f = [bad_value]
                              else if (n_args_local == 2) then
                                 if (trim(id) == "arange") then
                                    f = arange(a1(1), a2(1))
                                 else
                                    f = real(irange(nint(a1(1)), nint(a2(1))), kind=dp)
                                 end if
                              else
                                 a3 = evaluate(labels(3))
                                 if (eval_error .or. size(a3) /= 1) then
                                    print *, "Error: third argument of ", trim(id), "() must be scalar"
                                    eval_error = .true.; f = [bad_value]
                                 else
                                    if (trim(id) == "arange") then
                                       f = arange(a1(1), a2(1), a3(1))
                                    else
                                       f = real(irange(nint(a1(1)), nint(a2(1)), nint(a3(1))), kind=dp)
                                    end if
                                 end if
                              end if
                           end if
                        end if
                     end block

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

                  case ("polyroots")
                     if (have_second) then
                        print "(a)", "Error: function '"//trim(id)//"' takes one argument"
                        eval_error = .true.; f = [bad_value]
                     else
                        f = polyroots(arg1)
                     end if

                  case ("adf_stat")
                     block
                        integer :: n_args_local
                        real(kind=dp), allocatable :: t1(:), t2(:)
                        call split_by_comma(expr(pstart:pend - 1), n_args_local, labels)
                        if (n_args_local == 1 .and. len_trim(adjustl(labels(1))) == 0) n_args_local = 0
                        if (n_args_local < 1 .or. n_args_local > 2) then
                           print *, "Error: function adf_stat takes one or two arguments"
                           eval_error = .true.; f = [bad_value]
                        else
                           t1 = evaluate(labels(1))
                           if (eval_error) then
                              f = [bad_value]
                           else if (n_args_local == 1) then
                              f = [adf_stat(t1)]
                           else
                              t2 = evaluate(labels(2))
                              if (eval_error .or. size(t2) /= 1) then
                                 print *, "Error: second argument of adf_stat() must be scalar"
                                 eval_error = .true.; f = [bad_value]
                              else
                                 f = [adf_stat(t1, max(0, nint(t2(1))))]
                              end if
                           end if
                        end if
                     end block
                     pos = pend + 1
                     if (pos > lenstr) then
                        curr_char = char(0)
                     else
                        curr_char = expr(pos:pos); pos = pos + 1
                     end if

                  case ("phillips_perron_stat")
                     block
                        integer :: n_args_local
                        real(kind=dp), allocatable :: t1(:), t2(:)
                        call split_by_comma(expr(pstart:pend - 1), n_args_local, labels)
                        if (n_args_local == 1 .and. len_trim(adjustl(labels(1))) == 0) n_args_local = 0
                        if (n_args_local < 1 .or. n_args_local > 2) then
                           print *, "Error: function phillips_perron_stat takes one or two arguments"
                           eval_error = .true.; f = [bad_value]
                        else
                           t1 = evaluate(labels(1))
                           if (eval_error) then
                              f = [bad_value]
                           else if (n_args_local == 1) then
                              f = [phillips_perron_stat(t1)]
                           else
                              t2 = evaluate(labels(2))
                              if (eval_error .or. size(t2) /= 1) then
                                 print *, "Error: second argument of phillips_perron_stat() must be scalar"
                                 eval_error = .true.; f = [bad_value]
                              else
                                 f = [phillips_perron_stat(t1, max(0, nint(t2(1))))]
                              end if
                           end if
                        end if
                     end block
                     pos = pend + 1
                     if (pos > lenstr) then
                        curr_char = char(0)
                     else
                        curr_char = expr(pos:pos); pos = pos + 1
                     end if

                  case ("adf")
                     block
                        integer :: n_args_local
                        real(kind=dp), allocatable :: t1(:), t2(:)
                        call split_by_comma(expr(pstart:pend - 1), n_args_local, labels)
                        if (n_args_local == 1 .and. len_trim(adjustl(labels(1))) == 0) n_args_local = 0
                        if (n_args_local < 1 .or. n_args_local > 2) then
                           print *, "Error: function adf takes one or two arguments"
                           eval_error = .true.; f = [bad_value]
                        else
                           t1 = evaluate(labels(1))
                           if (eval_error) then
                              f = [bad_value]
                           else if (n_args_local == 1) then
                              call adf(t1)
                              suppress_result = .true.
                              f = [real(kind=dp) ::]
                           else
                              t2 = evaluate(labels(2))
                              if (eval_error .or. size(t2) /= 1) then
                                 print *, "Error: second argument of adf() must be scalar"
                                 eval_error = .true.; f = [bad_value]
                              else
                                 call adf(t1, max(0, nint(t2(1))))
                                 suppress_result = .true.
                                 f = [real(kind=dp) ::]
                              end if
                           end if
                        end if
                     end block
                     pos = pend + 1
                     if (pos > lenstr) then
                        curr_char = char(0)
                     else
                        curr_char = expr(pos:pos); pos = pos + 1
                     end if

                  case ("phillips_perron")
                     block
                        integer :: n_args_local
                        real(kind=dp), allocatable :: t1(:), t2(:)
                        call split_by_comma(expr(pstart:pend - 1), n_args_local, labels)
                        if (n_args_local == 1 .and. len_trim(adjustl(labels(1))) == 0) n_args_local = 0
                        if (n_args_local < 1 .or. n_args_local > 2) then
                           print *, "Error: function phillips_perron takes one or two arguments"
                           eval_error = .true.; f = [bad_value]
                        else
                           t1 = evaluate(labels(1))
                           if (eval_error) then
                              f = [bad_value]
                           else if (n_args_local == 1) then
                              call phillips_perron(t1)
                              suppress_result = .true.
                              f = [real(kind=dp) ::]
                           else
                              t2 = evaluate(labels(2))
                              if (eval_error .or. size(t2) /= 1) then
                                 print *, "Error: second argument of phillips_perron() must be scalar"
                                 eval_error = .true.; f = [bad_value]
                              else
                                 call phillips_perron(t1, max(0, nint(t2(1))))
                                 suppress_result = .true.
                                 f = [real(kind=dp) ::]
                              end if
                           end if
                        end if
                     end block
                     pos = pend + 1
                     if (pos > lenstr) then
                        curr_char = char(0)
                     else
                        curr_char = expr(pos:pos); pos = pos + 1
                     end if

                  case ("abs", "acos", "acosh", "asin", "asinh", "atan", "atanh", "cos", "cosh", &
                        "exp", "log", "log10", "sin", "sinh", "sqrt", "tan", "tanh", "size", &
                        "norm1", "norm2", "minloc", "maxloc", "count", "mean", "geomean", "iqr", "iqr_scale", &
                        "harmean", "sd", "cumsum", &
                        "cummin", "cummax", "cummean", "cumprod", "diff", "sort", "indexx", "rank", &
                        "unique", "stdz", "reverse", "median", "mssk", "jb_test", "fit_norm", "fit_exp", "fit_gamma", "fit_lnorm", "fit_chisq", "fit_f", "fit_beta", "fit_logis", "fit_sech", "fit_laplace", "fit_cauchy", "fit_ged", "fit_hyperb", "dsech", "psech", "qsech", "bessel_j0", "bessel_j1", &
                        "bessel_y0", "bessel_y1", "gamma", "log_gamma", "cosd", "sind", "tand", &
                        "acosd", "asind", "atand", "spacing", "skew", "kurt", "print_stats")
                     if (have_second) then
                        print "(a)", "Error: function '"//trim(id)//"' takes one argument"
                        eval_error = .true.; f = [bad_value]
                     else
                        if (index("size sum product norm1 norm2 minval maxval minloc "// &
                                  "maxloc count mean geomean iqr iqr_scale harmean sd median print_stats skew kurt", &
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
                        f = [real(kind=dp) ::]
                     else if (size(arg1) /= size(arg2)) then
                        print *, "Error: plot() arguments must have same size"
                        eval_error = .true.
                        f = [bad_value]
                     else
                        call plot(arg1, arg2, title=plot_to_label(line_cp))         ! <-- actual drawing
                        f = [real(kind=dp) ::] ! return “nothing”
                     end if

                  case default ! subscript  x(i)
                     if (user_func_index(trim(id)) > 0) then
                        f = call_user_function(trim(id), expr(pstart:pend - 1))
                     else if (have_second) then
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
                                 f = vvar(idxv)
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

      logical function starts_decimal_literal()
         starts_decimal_literal = .false.
         if (curr_char /= ".") return
         if (pos > lenstr) return
         starts_decimal_literal = is_numeral(expr(pos:pos))
      end function starts_decimal_literal

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
      p_rpar = scan(lhs, ")", back=.true.)
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
            if (.not. eval_error) call mark_sub_arg_assigned(trim(name))
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
      logical :: print_array_as_int, run_then, had_error, in_quote, comment_only, consumed_loop_line, ok_for, prev_exec, ok_plotout
      character(len=1) :: quote_char
      character(len=*), parameter :: fmt_real_array = '("[",*(i0,:,", "))'
      character(len=:), allocatable :: lhs, rhs, rhs_tail
      integer :: p_lpar, p_rpar, depth, len_adj, comment_pos, i_c
      integer :: n_names
      character(len=:), allocatable :: cond_txt, then_txt, low_adj
      line_eval = line
      if (len(line_eval) >= 3) then
         if (iachar(line_eval(1:1)) == 239 .and. iachar(line_eval(2:2)) == 187 .and. &
             iachar(line_eval(3:3)) == 191) then
            line_eval = line_eval(4:)
         end if
      end if
      in_quote = .false.
      quote_char = " "
      comment_pos = 0
      comment_only = .false.
      do i_c = 1, len_trim(line)
         if (line(i_c:i_c) /= " ") then
            if (line(i_c:i_c) == comment_char) comment_only = .true.
            exit
         end if
      end do
      do i_c = 1, len_trim(line_eval)
         if (.not. in_quote) then
            if (line_eval(i_c:i_c) == '"' .or. line_eval(i_c:i_c) == "'") then
               in_quote = .true.
               quote_char = line_eval(i_c:i_c)
            end if
         else
            if (line_eval(i_c:i_c) == quote_char) then
               in_quote = .false.
               quote_char = " "
            end if
         end if
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
      if (sub_collecting) then
         if (len_trim(line_eval) > 0) then
            if (is_end_subroutine_line(line_eval)) then
               block
                  integer :: parsed_intents(max_sub_args)
                  character(len=32768) :: exec_body
                  logical :: ok_intent
                  parsed_intents = sub_collect_intents
                  call strip_sub_intent_lines(sub_collect_body, sub_collect_args, sub_collect_nargs, parsed_intents, exec_body, ok_intent)
                  if (.not. ok_intent) then
                     eval_error = .true.
                  else
                     do i = 1, sub_collect_nargs
                        if (sub_collect_has_default(i)) then
                           if (parsed_intents(i) /= 1) then
                              print *, "Error: default values are only allowed for intent(in) subroutine arguments: '", trim(sub_collect_args(i)), "'"
                              eval_error = .true.
                              exit
                           end if
                        end if
                     end do
                     if (.not. eval_error) then
                        call set_user_subroutine(sub_collect_name, sub_collect_nargs, sub_collect_args, parsed_intents, sub_collect_has_default, sub_collect_defaults, trim(exec_body))
                     end if
                  end if
               end block
               sub_collecting = .false.
               sub_collect_name = ""
               sub_collect_nargs = 0
               sub_collect_args = ""
               sub_collect_intents = 0
               sub_collect_has_default = .false.
               sub_collect_defaults = ""
               sub_collect_body = ""
               goto 9000
            end if
            if (len_trim(sub_collect_body) + len_trim(line_eval) + 1 > len(sub_collect_body)) then
               print *, "Error: subroutine body too large"
               eval_error = .true.
               sub_collecting = .false.
               sub_collect_name = ""
               sub_collect_nargs = 0
               sub_collect_args = ""
               sub_collect_intents = 0
               sub_collect_has_default = .false.
               sub_collect_defaults = ""
               sub_collect_body = ""
               goto 9000
            end if
            sub_collect_body = trim(sub_collect_body)//trim(line_eval)//new_line("a")
         end if
         goto 9000
      end if
      if (func_collecting) then
         if (len_trim(line_eval) > 0) then
            if (is_end_function_line(lower_str(adj_line))) then
               call set_user_function(func_collect_name, func_collect_nargs, func_collect_args, func_collect_has_default, func_collect_defaults, trim(func_collect_body))
               func_collecting = .false.
               func_collect_name = ""
               func_collect_nargs = 0
               func_collect_args = ""
               func_collect_has_default = .false.
               func_collect_defaults = ""
               func_collect_body = ""
               goto 9000
            end if
            if (len_trim(func_collect_body) + len_trim(line_eval) + 1 > len(func_collect_body)) then
               print *, "Error: function body too large"
               eval_error = .true.
               func_collecting = .false.
               func_collect_name = ""
               func_collect_nargs = 0
               func_collect_args = ""
               func_collect_has_default = .false.
               func_collect_defaults = ""
               func_collect_body = ""
               goto 9000
            end if
            func_collect_body = trim(func_collect_body)//trim(line_eval)//new_line("a")
         end if
         goto 9000
      end if
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
         if ((adj_line(1:1) == '"' .and. adj_line(len_adj:len_adj) == '"') .or. &
             (adj_line(1:1) == "'" .and. adj_line(len_adj:len_adj) == "'")) then
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
      block
         character(len=len_name) :: ffname
         character(len=len_name) :: fargs(max_func_args)
         logical :: fhas_default(max_func_args)
         character(len=len_default_expr) :: fdefaults(max_func_args)
         integer :: fnargs
         logical :: fok
         call parse_function_header(adj_line, ffname, fnargs, fargs, fhas_default, fdefaults, fok)
         if (fok) then
            func_collecting = .true.
            func_collect_name = trim(ffname)
            func_collect_nargs = fnargs
            func_collect_args = ""
            func_collect_has_default = .false.
            func_collect_defaults = ""
            if (fnargs > 0) func_collect_args(1:fnargs) = fargs(1:fnargs)
            if (fnargs > 0) then
               func_collect_has_default(1:fnargs) = fhas_default(1:fnargs)
               func_collect_defaults(1:fnargs) = fdefaults(1:fnargs)
            end if
            func_collect_body = ""
            goto 9000
         end if
      end block
      block
         character(len=len_name) :: ssname
         character(len=len_name) :: sargs(max_sub_args)
         logical :: shas_default(max_sub_args)
         character(len=len_default_expr) :: sdefaults(max_sub_args)
         integer :: snargs, sintents(max_sub_args)
         logical :: sok
         call parse_subroutine_header(adj_line, ssname, snargs, sargs, sintents, shas_default, sdefaults, sok)
         if (sok) then
            sub_collecting = .true.
            sub_collect_name = trim(ssname)
            sub_collect_nargs = snargs
            sub_collect_args = ""
            sub_collect_intents = 0
            sub_collect_has_default = .false.
            sub_collect_defaults = ""
            if (snargs > 0) then
               sub_collect_args(1:snargs) = sargs(1:snargs)
               sub_collect_intents(1:snargs) = sintents(1:snargs)
               sub_collect_has_default(1:snargs) = shas_default(1:snargs)
               sub_collect_defaults(1:snargs) = sdefaults(1:snargs)
            end if
            sub_collect_body = ""
            goto 9000
         end if
      end block
      if (is_else_if_line(adj_line) .or. is_else_line(low_adj) .or. is_end_if_line(low_adj)) then
         print *, "Error: IF/ELSE branch without matching block IF"
         had_error = .true.
         goto 9000
      end if
      if (is_end_subroutine_line(low_adj)) then
         print *, "Error: END SUBROUTINE without matching SUBROUTINE"
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
      if (len_trim(trimmed_line) >= 11 .and. lower_str(trimmed_line(1:11)) == "set plotout" &
          .and. (len_trim(trimmed_line) == 11 .or. trimmed_line(12:12) == " ")) then
         tail = adjustl(trimmed_line(12:))
         if (len_trim(tail) == 0) then
            print *, "plotout =", trim(get_plotout())
         else
            call set_plotout(trim(tail), ok_plotout)
            if (ok_plotout) then
               print *, "plotout =", trim(get_plotout())
            else
               print *, "Error: set plotout <screen|png|pdf|svg|eps>"
               had_error = .true.
            end if
         end if
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

         block
            character(len=:), allocatable :: pcall, lowcall, sname, argtxt
            integer :: p1, p2
            pcall = adjustl(trim(part_eval))
            lowcall = lower_str(pcall)
            if (index(lowcall, "call ") == 1) then
               p1 = index(pcall, "(")
               p2 = index(pcall, ")")
               if (p1 <= 6 .or. p2 <= p1 .or. len_trim(pcall(p2 + 1:)) > 0) then
                  print *, "Error: bad CALL syntax"
                  had_error = .true.
                  cycle
               end if
               sname = adjustl(trim(pcall(6:p1 - 1)))
               if (.not. is_alnum_string(sname)) then
                  print *, "Error: bad subroutine name in CALL"
                  had_error = .true.
                  cycle
               end if
               argtxt = trim(pcall(p1 + 1:p2 - 1))
               call call_user_subroutine(trim(sname), argtxt)
               if (eval_error) then
                  had_error = .true.
               end if
               cycle
            end if
         end block

         call split_by_comma(part_eval, n_names, names)
         if (n_names > 1) then
            if (.not. suppress(k) .and. .not. in_user_function) then
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

         if (index(part_eval, '"') > 0 .and. index(part_eval, "(") == 0 .and. index(part_eval, "[") == 0) then
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
                  if (.not. suppress(k) .and. .not. in_user_function) then
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
         if (.not. allocated(r)) then
            print *, "Error: internal evaluator returned no value"
            had_error = .true.
            cycle
         end if
         const_assign = .false.
         if (suppress_result) then
            suppress_result = .false.
            cycle
         end if
         if (index(trim(parts(k)), "print_stats") == 1) cycle

         ! ---------- echo only when the segment is *not* suppressed ----------
         if (.not. suppress(k) .and. .not. in_user_function) then
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
         integer :: newlen, oldlen

         if (allocated(parts)) then
            oldlen = len(parts(1))
         else
            oldlen = 0
         end if
         newlen = max(len_trim(txt), oldlen)

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
      integer :: i, start, len_line, newlen, nlen_tail, oldlen

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
         if (allocated(parts)) then
            oldlen = len(parts(1))
         else
            oldlen = 0
         end if
         newlen = max(nlen_tail - start + 1, oldlen)
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
         integer :: newlen, oldlen

         if (allocated(parts)) then
            oldlen = len(parts(1))
         else
            oldlen = 0
         end if
         newlen = max(len_trim(txt), oldlen)

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
         if (eval_error) then
            in_loop_execute = .false.
            loop_exec_base_depth = prev_base_depth
            return
         end if
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
