program xinterpret
  use interpret_mod, only: eval_print, tunit, code_transcript_file, &
                           vars, write_code, echo_code, get_prompt_depth
  use util_mod, only: replace, windows
  use kind_mod, only: dp
  use plot_mod, only: use_windows, set_noplot
  implicit none
  integer :: i, iostat_err, varu, narg, indent
  character (len=1000) :: line, arg
  logical, parameter :: write_vars_at_end = .false., &
                        time_code=.false., run_sample_code=.false.
  character (len=*), parameter :: vars_file = "temp_vars.txt"
  real(kind=dp) :: t1, t2
  logical :: no_plot, have_script
  character(len=1000) :: script_file

  use_windows = windows()
  narg = command_argument_count()
  no_plot = .false.
  have_script = .false.
  script_file = ""

  do i = 1, narg
    call get_command_argument(i, arg)
    arg = trim(adjustl(arg))
    if (arg == "--noplot") then
      no_plot = .true.
    else if (len_trim(arg) > 0 .and. arg(1:1) == "-") then
      print *, "Error: unknown option: ", trim(arg)
      stop
    else if (.not. have_script) then
      script_file = arg
      have_script = .true.
    else
      print *, "Error: only one script file may be provided"
      stop
    end if
  end do

  call set_noplot(no_plot)
  if (have_script) then
    call eval_print("run(" // '"' // trim(script_file) // '"' // ")")
    stop
  end if
  write_code = .true.
  if (write_code) open (newunit=tunit, file=code_transcript_file, action="write", status="replace")
  if (run_sample_code) then
     call eval_print("n = 20")
     call eval_print("y = [1, 2, 3]")
     call eval_print("z = n * y")
     call eval_print("w = [10 20 30] + y")
     call eval_print("z ^ 2")
     call eval_print("x = runif(n)")
     call eval_print("sum(x)")
     call eval_print("[sum(x) minval(x) maxval(x)]")
     call eval_print([character (len=999) :: "r = 10", "r^2", "r^3"])
     call eval_print("v = 10*arange(10)")
     call eval_print("v([2 3 4])")
     call eval_print("v(3:9:2)")
     call eval_print("rnorm(5)")
     call eval_print("rnorm(10^3)")
     call eval_print("?vars")
  end if
  echo_code = .false.
  do
     indent = 3*max(0, get_prompt_depth())
     write (*,"(a)", advance="no") repeat(" ", indent)//"> "
     read (*,"(a)", iostat=iostat_err) line
     if (iostat_err /= 0) exit
     if (line == "exit" .or. line == "exit()" .or. line == "quit" .or. line == "quit()" .or. line == "q" .or. line == "q()") exit
!     line = replace(line, '"', "'")
     line = replace(line, "**", "^")
     if (time_code) call cpu_time(t1)
     call eval_print(trim(line))
     if (time_code) then
        call cpu_time(t2)
        print "(a, f0.4)", "time: ",t2-t1
     end if
  end do
  if (write_vars_at_end) then
     open (newunit=varu, file=vars_file, action="write", status="replace")
     do i=1,size(vars)
        if (allocated(vars(i)%val)) write (varu, "(a,*(1x, f0.6))") &
           trim(vars(i)%name), vars(i)%val
     end do
  end if
  if (write_code) print "(a)", "wrote code to " // trim(code_transcript_file)
end program xinterpret
