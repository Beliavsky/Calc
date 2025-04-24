program test_evaluator
use calc_mod, only: eval_print
implicit none
call eval_print("10 + (5 * (1 + (2*3)))")
call eval_print("1 + 2*3")
call eval_print("3^2 + 4^2")
call eval_print("(3^2 + 4^2)^0.5")
call eval_print("(3^2 + 4^2)^(-0.5)")
end program test_evaluator
