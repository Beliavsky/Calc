module plot_mod
  use kind_mod, only: dp
  use util_mod, only: arange
  implicit none
  private
  public :: plot, use_windows, plot_to_label, set_plotout, get_plotout
  logical, parameter :: noplot = .false.

  !── Named executables for each platform
  character(len=*), parameter :: gnuplot_cmd_win  = "wgnuplot"
  character(len=*), parameter :: gnuplot_cmd_unix = "gnuplot"

  !── Toggle this in your program before calling plot()
  logical :: use_windows = .true.
  integer, save :: plot_counter = 0
  character(len=16), save :: plotout_mode = "screen"

  interface plot
    module procedure plot_1d, plot_2d, plot_y
  end interface

contains

  pure function sanitize_tag(s, fallback) result(tag)
    character(len=*), intent(in) :: s, fallback
    character(len=:), allocatable :: tag
    character(len=:), allocatable :: t
    integer :: i, c
    logical :: prev_us
    t = adjustl(trim(s))
    if (len_trim(t) == 0) t = trim(fallback)
    do i = 1, len(t)
      c = iachar(t(i:i))
      if (c >= iachar('A') .and. c <= iachar('Z')) then
        t(i:i) = achar(c + 32)
      else if (.not. ((c >= iachar('a') .and. c <= iachar('z')) .or. (c >= iachar('0') .and. c <= iachar('9')))) then
        t(i:i) = "_"
      end if
    end do
    tag = ""
    prev_us = .false.
    do i = 1, len_trim(t)
      if (t(i:i) == "_") then
        if (.not. prev_us) tag = tag//"_"
        prev_us = .true.
      else
        tag = tag//t(i:i)
        prev_us = .false.
      end if
    end do
    if (len_trim(tag) == 0) tag = trim(fallback)
    if (tag(1:1) == "_") tag = tag(2:)
    if (len_trim(tag) > 0) then
      if (tag(len_trim(tag):len_trim(tag)) == "_") tag = tag(:len_trim(tag)-1)
    end if
    if (len_trim(tag) == 0) tag = trim(fallback)
  end function sanitize_tag

  subroutine next_plot_files(tag, fn_data, fn_script, fn_out)
    character(len=*), intent(in) :: tag
    character(len=:), allocatable, intent(out) :: fn_data, fn_script
    character(len=:), allocatable, intent(out) :: fn_out
    character(len=16) :: num
    character(len=:), allocatable :: stem
    plot_counter = plot_counter + 1
    write(num, "(I4.4)") plot_counter
    stem = trim(tag)//"_"//trim(num)
    fn_data = trim(stem)//".dat"
    fn_script = trim(stem)//".gp"
    select case (trim(plotout_mode))
    case ("png")
      fn_out = trim(stem)//".png"
    case ("pdf")
      fn_out = trim(stem)//".pdf"
    case ("svg")
      fn_out = trim(stem)//".svg"
    case ("eps")
      fn_out = trim(stem)//".eps"
    case default
      fn_out = ""
    end select
  end subroutine next_plot_files

  subroutine set_plotout(mode, ok)
    character(len=*), intent(in) :: mode
    logical, intent(out) :: ok
    character(len=:), allocatable :: m
    integer :: i, c
    m = adjustl(trim(mode))
    do i = 1, len_trim(m)
      c = iachar(m(i:i))
      if (c >= iachar('A') .and. c <= iachar('Z')) m(i:i) = achar(c + 32)
    end do
    select case (trim(m))
    case ("screen", "png", "pdf", "svg", "eps")
      plotout_mode = trim(m)
      ok = .true.
    case default
      ok = .false.
    end select
  end subroutine set_plotout

  function get_plotout() result(mode)
    character(len=16) :: mode
    mode = plotout_mode
  end function get_plotout

  subroutine plot_y(y, title, xlabel, ylabel, style, data_file, script_file)
    ! Plot a single series y(:) versus x(:)
    real(kind=dp), intent(in)                ::  y(:)
    character(len=*), intent(in), optional   :: title, xlabel, ylabel, style
    character(len=*), intent(in), optional   :: data_file, script_file
    call plot_1d(arange(size(y)), y, title, xlabel, ylabel, style, data_file, &
                 script_file)
  end subroutine plot_y

  subroutine plot_1d(x, y, title, xlabel, ylabel, style, data_file, script_file, points_y)
    ! Plot a single series y(:) versus x(:)
    real(kind=dp), intent(in)               :: x(:), y(:)
    character(len=*), intent(in), optional   :: title, xlabel, ylabel, style
    character(len=*), intent(in), optional   :: data_file, script_file
    real(kind=dp), intent(in), optional      :: points_y(:)

    character(len=:), allocatable :: fn_data, fn_script, st, fn_data_def, fn_script_def, fn_out, tag
    character(len=512)            :: cmd
    integer                       :: i, n, unit_data, unit_script
    logical                       :: with_points
    character(len=*), parameter   :: fmt = "(F12.6,1x,F12.6)"
    character(len=*), parameter   :: fmtp = "(F12.6,1x,F12.6,1x,F12.6)"
    if (noplot) then
       print*,"in plot_1d, not plotting" ! debug
       return
    end if
    n = size(x)

    !── defaults
    if (present(title)) then
      tag = sanitize_tag(title, "plot1d")
    else
      tag = "plot1d"
    end if
    if (present(data_file)) fn_data = trim(data_file)
    if (present(script_file)) fn_script = trim(script_file)
    if (.not. present(data_file) .or. .not. present(script_file)) then
      call next_plot_files(tag, fn_data_def, fn_script_def, fn_out)
      if (.not. present(data_file)) fn_data = fn_data_def
      if (.not. present(script_file)) fn_script = fn_script_def
    else
      call next_plot_files(tag, fn_data_def, fn_script_def, fn_out)
    end if

    if (present(style)) then
      st = trim(style)
    else
      st = "lines"
    end if

    with_points = present(points_y) .and. size(points_y) == n

    !── write data
    open(newunit=unit_data, file=fn_data,   status="replace", action="write")
      do i = 1, n
        if (with_points) then
          write(unit_data, fmtp) x(i), y(i), points_y(i)
        else
          write(unit_data, fmt) x(i), y(i)
        end if
      end do
    close(unit_data)

    !── write gnuplot script
    open(newunit=unit_script, file=fn_script, status="replace", action="write")
      if (present(title)) then
        write(unit_script,"(A)") "set title '"//trim(title)//"'"
      end if
      if (present(xlabel)) then
        write(unit_script,"(A)") "set xlabel '"//trim(xlabel)//"'"
      end if
      if (present(ylabel)) then
        write(unit_script,"(A)") "set ylabel '"//trim(ylabel)//"'"
      end if
      write(unit_script,"(A)") "set grid"
      select case (trim(plotout_mode))
      case ("png")
        write(unit_script,"(A)") "set terminal pngcairo size 1000,700"
        write(unit_script,"(A)") "set output '"//trim(fn_out)//"'"
      case ("pdf")
        write(unit_script,"(A)") "set terminal pdfcairo"
        write(unit_script,"(A)") "set output '"//trim(fn_out)//"'"
      case ("svg")
        write(unit_script,"(A)") "set terminal svg size 1000,700"
        write(unit_script,"(A)") "set output '"//trim(fn_out)//"'"
      case ("eps")
        write(unit_script,"(A)") "set terminal postscript eps enhanced color"
        write(unit_script,"(A)") "set output '"//trim(fn_out)//"'"
      end select
      if (with_points) then
        write(unit_script,"(A)") "plot '"//trim(fn_data)//"' using 1:2 with "//trim(st) // " title 'fit', " // &
                                  "'"//trim(fn_data)//"' using 1:3 with points pt 7 ps 0.6 title 'data'"
      else
        write(unit_script,"(A)") "plot '"//trim(fn_data)//"' using 1:2 with "//trim(st) // " notitle"
      end if
      if (trim(plotout_mode) == "screen") then
        write(unit_script,"(A)") "pause -1"
      end if
    close(unit_script)

    !── invoke gnuplot based on platform toggle
    if (use_windows .and. trim(plotout_mode) == "screen") then
      cmd = 'cmd /c start "" ' // gnuplot_cmd_win  // " " // trim(fn_script)
      call execute_command_line(cmd, wait = .false.)
    else
      cmd = trim(gnuplot_cmd_unix) // " " // trim(fn_script)
      call execute_command_line(cmd)
    end if

  end subroutine plot_1d


  subroutine plot_2d(x, y, title, xlabel, ylabel, style, data_file, script_file, legend_labels, points_y)
    ! Plot multiple series (columns of y(:,j)) versus x(:)
    real(kind=dp), intent(in)               :: x(:), y(:, :)
    character(len=*), intent(in), optional   :: title, xlabel, ylabel, style
    character(len=*), intent(in), optional   :: data_file, script_file
    character(len=*), intent(in), optional   :: legend_labels(:)
    real(kind=dp), intent(in), optional      :: points_y(:)

    character(len=:), allocatable :: fn_data, fn_script, st, plot_cmd, fn_data_def, fn_script_def, fn_out, tag
    character(len=512)            :: cmd
    integer                       :: i, n, ns, unit_data, unit_script
    character(len=10)             :: col_max
    character(len=*), parameter   :: fmt1 = "(F12.6,1x)"
    character(len=*), parameter   :: fmty = "(1x,*(F12.6,1x))"
    character(len=*), parameter   :: fmtp = "(1x,*(F12.6,1x),F12.6)"
    logical                       :: with_points
    if (noplot) then
       print*,"in plot_2d, not plotting" ! debug
       return
    end if
    n  = size(x)
    ns = size(y,2)

    !── defaults
    if (present(title)) then
      tag = sanitize_tag(title, "plot2d")
    else
      tag = "plot2d"
    end if
    if (present(data_file)) fn_data = trim(data_file)
    if (present(script_file)) fn_script = trim(script_file)
    if (.not. present(data_file) .or. .not. present(script_file)) then
      call next_plot_files(tag, fn_data_def, fn_script_def, fn_out)
      if (.not. present(data_file)) fn_data = fn_data_def
      if (.not. present(script_file)) fn_script = fn_script_def
    else
      call next_plot_files(tag, fn_data_def, fn_script_def, fn_out)
    end if

    if (present(style)) then
      st = trim(style)
    else
      st = "lines"
    end if

    with_points = present(points_y) .and. size(points_y) == n

    !── write data
    open(newunit=unit_data, file=fn_data,   status="replace", action="write")
      do i = 1, n
        write(unit_data, fmt1, advance="no") x(i)
        if (with_points) then
          write(unit_data, fmtp) y(i,1:ns), points_y(i)
        else
          write(unit_data, fmty) y(i,1:ns)
        end if
      end do
    close(unit_data)

    !── write gnuplot script
    open(newunit=unit_script, file=fn_script, status="replace", action="write")
      if (present(title)) then
        write(unit_script,"(A)") "set title '"//trim(title)//"'"
      end if
      if (present(xlabel)) then
        write(unit_script,"(A)") "set xlabel '"//trim(xlabel)//"'"
      end if
      if (present(ylabel)) then
        write(unit_script,"(A)") "set ylabel '"//trim(ylabel)//"'"
      end if
      write(unit_script,"(A)") "set grid"
      select case (trim(plotout_mode))
      case ("png")
        write(unit_script,"(A)") "set terminal pngcairo size 1000,700"
        write(unit_script,"(A)") "set output '"//trim(fn_out)//"'"
      case ("pdf")
        write(unit_script,"(A)") "set terminal pdfcairo"
        write(unit_script,"(A)") "set output '"//trim(fn_out)//"'"
      case ("svg")
        write(unit_script,"(A)") "set terminal svg size 1000,700"
        write(unit_script,"(A)") "set output '"//trim(fn_out)//"'"
      case ("eps")
        write(unit_script,"(A)") "set terminal postscript eps enhanced color"
        write(unit_script,"(A)") "set output '"//trim(fn_out)//"'"
      end select

      if (present(legend_labels) .and. size(legend_labels) == ns) then
        plot_cmd = "plot "
        do i = 1, ns
          if (i > 1) plot_cmd = trim(plot_cmd)//", "
          write(col_max,"(I0)") i + 1
          plot_cmd = trim(plot_cmd)//"'"//trim(fn_data)//"' using 1:"//trim(col_max)// &
                     " with "//trim(st)//" title '"//trim(legend_labels(i))//"'"
        end do
        if (with_points) then
          write(col_max,"(I0)") ns + 2
          plot_cmd = trim(plot_cmd)//", '"//trim(fn_data)//"' using 1:"//trim(col_max)// &
                     " with points pt 7 ps 0.6 title 'data'"
        end if
        write(unit_script,"(A)") trim(plot_cmd)
      else
        write(col_max,"(I0)") ns + 1
        plot_cmd = "plot for [col=2:"//trim(col_max)//"] '"//trim(fn_data)//"' using 1:col with "//trim(st)
        if (with_points) then
          write(col_max,"(I0)") ns + 2
          plot_cmd = trim(plot_cmd)//", '"//trim(fn_data)//"' using 1:"//trim(col_max)// &
                     " with points pt 7 ps 0.6 title 'data'"
        end if
        write(unit_script,"(A)") trim(plot_cmd)
      end if
      if (trim(plotout_mode) == "screen") then
        write(unit_script,"(A)") "pause -1"
      end if
    close(unit_script)

    !── invoke gnuplot based on platform toggle
    if (use_windows .and. trim(plotout_mode) == "screen") then
      cmd = 'cmd /c start "" ' // gnuplot_cmd_win  // " " // trim(fn_script)
      call execute_command_line(cmd, wait = .false.)
    else
      cmd = trim(gnuplot_cmd_unix) // " " // trim(fn_script)
      call execute_command_line(cmd)
    end if

  end subroutine plot_2d

pure function plot_to_label(s, print_x) result(label)
  ! converts a string such as 'plot(x,sin(x))' or 'plot(sin(x))' to 'sin(x)'
  character(len=*), intent(in)      :: s
  logical         , intent(in), optional :: print_x
  character(len=:), allocatable     :: label
  character(len=:), allocatable     :: inner, first, second
  integer                           :: i, i1, i2, commapos
  logical                           :: print_x_
  if (present(print_x)) then
     print_x_ = print_x
  else
     print_x_ = .false.
  end if
  ! Look for “plot(”
  i1 = index(s, "plot(")
  if (i1 == 0) then
    ! not a plot call → return trimmed input
    label = trim(s)
    return
  end if

  ! Find the last “)” in the string
  i2 = 0
  do i = len(s), 1, -1
    if (s(i:i) == ')') then
      i2 = i
      exit
    end if
  end do
  if (i2 <= i1 + 4) then
    ! malformed → just return trimmed input
    label = trim(s)
    return
  end if

  ! Extract the text between the parentheses
  inner = s(i1+5:i2-1)

  ! Look for a comma separating two arguments
  commapos = index(inner, ",")

  if (commapos > 0) then
    ! two arguments → second vs first
    second = trim(adjustl(inner(commapos+1:)))
    if (print_x_) then
       first  = trim(inner(:commapos-1))
       label  = second // " vs. " // first
    else
       label = second
    end if
  else
    ! single argument → just that
    label = trim(inner)
  end if

end function plot_to_label


end module plot_mod
