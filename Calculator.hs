import Calculate
import DerivativeSolver
import IntegralSolver

printTerm (Var c) = do
	 (putChar c)
printTerm (Num i) = do
      (putStr (show i))
printTerm (Func op arg1 arg2) = do
	 { (putChar '(')
	 ; (printTerm arg1)
     ; (putChar op)
     ; (printTerm arg2) 
	 ; (putChar ')') }	
	  