module plot_mod
  use kind_mod, only: dp
  use util_mod, only: arange
  implicit none
  private
  public :: plot, use_windows

  !── Named executables for each platform
  character(len=*), parameter :: gnuplot_cmd_win  = "wgnuplot"
  character(len=*), parameter :: gnuplot_cmd_unix = "gnuplot"

  !── Toggle this in your program before calling plot()
  logical :: use_windows = .true.

  interface plot
    module procedure plot_1d, plot_2d, plot_y
  end interface

contains

  subroutine plot_y(y, title, xlabel, ylabel, style, data_file, script_file)
    ! Plot a single series y(:) versus x(:)
    real(kind=dp), intent(in)                ::  y(:)
    character(len=*), intent(in), optional   :: title, xlabel, ylabel, style
    character(len=*), intent(in), optional   :: data_file, script_file
    call plot_1d(arange(size(y)), y, title, xlabel, ylabel, style, data_file, &
                 script_file)
  end subroutine plot_y

  subroutine plot_1d(x, y, title, xlabel, ylabel, style, data_file, script_file)
    ! Plot a single series y(:) versus x(:)
    real(kind=dp), intent(in)               :: x(:), y(:)
    character(len=*), intent(in), optional   :: title, xlabel, ylabel, style
    character(len=*), intent(in), optional   :: data_file, script_file

    character(len=:), allocatable :: fn_data, fn_script, st
    character(len=512)            :: cmd
    integer                       :: i, n, unit_data, unit_script
    character(len=*), parameter   :: fmt = "(F12.6,1x,F12.6)"

    n = size(x)

    !── defaults
    if (present(data_file)) then
      fn_data = trim(data_file)
    else
      fn_data = "plot1d.dat"
    end if

    if (present(script_file)) then
      fn_script = trim(script_file)
    else
      fn_script = "plot1d.gp"
    end if

    if (present(style)) then
      st = trim(style)
    else
      st = "lines"
    end if

    !── write data
    open(newunit=unit_data, file=fn_data,   status="replace", action="write")
      do i = 1, n
        write(unit_data, fmt) x(i), y(i)
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
      write(unit_script,"(A)") "plot '"//trim(fn_data)//"' using 1:2 with "//trim(st) // " notitle"
      write(unit_script,"(A)") "pause -1"
    close(unit_script)

    !── invoke gnuplot based on platform toggle
    if (use_windows) then
      cmd = 'cmd /c start "" ' // gnuplot_cmd_win  // " " // trim(fn_script)
      call execute_command_line(cmd, wait = .false.)
    else
      cmd = trim(gnuplot_cmd_unix) // " " // trim(fn_script)
      call execute_command_line(cmd)
    end if

  end subroutine plot_1d


  subroutine plot_2d(x, y, title, xlabel, ylabel, style, data_file, script_file)
    ! Plot multiple series (columns of y(:,j)) versus x(:)
    real(kind=dp), intent(in)               :: x(:), y(:, :)
    character(len=*), intent(in), optional   :: title, xlabel, ylabel, style
    character(len=*), intent(in), optional   :: data_file, script_file

    character(len=:), allocatable :: fn_data, fn_script, st
    character(len=512)            :: cmd
    integer                       :: i, n, ns, unit_data, unit_script
    character(len=10)             :: col_max
    character(len=*), parameter   :: fmt1 = "(F12.6,1x)"
    character(len=*), parameter   :: fmty = "(1x,*(F12.6,1x))"

    n  = size(x)
    ns = size(y,2)

    !── defaults
    if (present(data_file)) then
      fn_data = trim(data_file)
    else
      fn_data = "plot2d.dat"
    end if

    if (present(script_file)) then
      fn_script = trim(script_file)
    else
      fn_script = "plot2d.gp"
    end if

    if (present(style)) then
      st = trim(style)
    else
      st = "lines"
    end if

    !── write data
    open(newunit=unit_data, file=fn_data,   status="replace", action="write")
      do i = 1, n
        write(unit_data, fmt1, advance="no") x(i)
        write(unit_data, fmty             ) y(i,1:ns)
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

      write(col_max,"(I0)") ns + 1
      write(unit_script,"(A)") &
           "plot for [col=2:"//trim(col_max)//"] '"//trim(fn_data)//"' using 1:col with "//trim(st)
      write(unit_script,"(A)") "pause -1"
    close(unit_script)

    !── invoke gnuplot based on platform toggle
    if (use_windows) then
      cmd = 'cmd /c start "" ' // gnuplot_cmd_win  // " " // trim(fn_script)
      call execute_command_line(cmd, wait = .false.)
    else
      cmd = trim(gnuplot_cmd_unix) // " " // trim(fn_script)
      call execute_command_line(cmd)
    end if

  end subroutine plot_2d

end module plot_mod
