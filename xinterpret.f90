program xinterpret
  use interpret_mod, only: eval_print, tunit, code_transcript_file
  implicit none
  integer :: iostat_err
  character (len=1000) :: line
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
end program xinterpret

