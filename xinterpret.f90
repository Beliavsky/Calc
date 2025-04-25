program xinterpret
  use interpret_mod
  implicit none
  character (len=1000) :: line
  call eval_print("x = 3")
  call eval_print("y = [1, 2, 3]")
  call eval_print("z = y * x")
  call eval_print("w = [10 20 30] + y")
  call eval_print("z ^ 2")
  do
     write (*,"('> ')", advance="no")
     read (*,"(a)") line
     if (line == "exit") exit
     call eval_print(trim(line))
  end do
end program xinterpret

