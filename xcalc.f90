program test_evaluator
use calc_mod, only: eval_print
implicit none
call eval_print("10 + (5 * (1 + (2*3)))")
call eval_print("1 + 2*3")
end program test_evaluator
