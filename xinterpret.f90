program xinterpret
  use interpret_mod, only: eval_print, tunit, code_transcript_file, vars
  implicit none
  integer :: i, iostat_err, varu
  character (len=1000) :: line
  logical, parameter :: write_vars_at_end = .true.
  character (len=*), parameter :: vars_file = "temp_vars.txt"
  open (newunit=tunit, file=code_transcript_file, action="write", status="replace")
  call eval_print("n = 10")
  call eval_print("y = [1, 2, 3]")
  call eval_print("z = n * y")
  call eval_print("w = [10 20 30] + y")
  call eval_print("z ^ 2")
  call eval_print("x = runif(n)")
  call eval_print("sum(x)")
  call eval_print("[sum(x) minval(x) maxval(x)]")
  do
     write (*,"('> ')", advance="no")
     read (*,"(a)", iostat=iostat_err) line
     if (iostat_err /= 0) exit
     if (line == "exit" .or. line == "exit()" .or. line == "quit" .or. line == "quit()" .or. line == "q" .or. line == "q()") exit
     call eval_print(trim(line))
  end do
  if (write_vars_at_end) then
     open (newunit=varu, file=vars_file, action="write", status="replace")
     do i=1,size(vars)
        if (allocated(vars(i)%val)) write (varu, "(a,*(1x, f0.6))") &
           trim(vars(i)%name), vars(i)%val
     end do
  end if
end program xinterpret

