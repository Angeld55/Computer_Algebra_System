module DerivativeSolver (module Calculate,derivative) where
import  Calculate

derivative :: Term ->Term
derivative t = calculate(derivative1 t)


derivative1 :: Term -> Term

derivative1 (Var 'x') = (Num 1)
derivative1 (Var a) = (Num 0)
derivative1 (Num a) = (Num 0)

derivative1(Func '*' (Var v) (Num n)) = derivative1(Func '*' (Num n) (Var v))

derivative1 (Func '*' (Num a) (Var 'x')) = (Num a)



derivative1 (Func '^' (Var 'x') (Num n))
  |  n==2 = (Func '*' (Num 2) (Var 'x'))
  |  otherwise = (Func '*' (Num n) (Func '^' (Var 'x') (Num (n-1))))  
  
derivative1 (Func '+' x y) = (Func '+' (derivative x) (derivative y)) 

derivative1 (Func '-' x y) = (Func '-' (derivative x) (derivative y)) 

derivative1 (Func '*' x y) = (Func '+' (Func '*' (derivative x) (y)) (Func '*' (x) (derivative y)))   

derivative1 (Func '/' x y) = (Func '/' (Func '-' (Func '*' (derivative x) (y)) (Func '*' (x) (derivative y))) (Func '^' y (Num 2)))