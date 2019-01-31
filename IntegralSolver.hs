module IntegralSolver (module Calculate,integral,defIntegral) where
import  Calculate


integral :: Term -> Term
integral t = calculate(integral1 t)

integral1 :: Term -> Term

integral1 (Var 'x') = (Func '/' (Func '^' (Var 'x') (Num 2)) (Num 2))

integral1 (Var t) = (Func '*' (Var t) (Var 'x'))

integral1 (Num n) = (Func '*' (Num n) (Var 'x'))
  
integral1 (Func '^' (Var 'x') (Num n)) = (Func '/' (Func '^' (Var 'x') (Num (n+1)))(Num (n+1)))

integral1 (Func '*' (Num n)(Func '^' (Var 'x') (Num m))) = (Func '*' (Func '/' (Num n) (Num (m+1))) (Func '^' (Var 'x') (Num (m+1))) )
integral1 (Func '*' (Num n) t) = (Func '*' (Num n) (integral1 t))

integral1 (Func '+' t1 t2) = (Func '+' (integral1 t1) ( integral1 t2))

integral1 (Func '-' t1 t2) = (Func '-' (integral1 t1) ( integral1 t2))  


defIntegral :: Term -> Int -> Int -> Term
defIntegral t a b = calculate (Func '-' (evaluate(integral t) b)  (evaluate(integral t) a))


