module Calculate(calculate,evaluate, Term(..)) where

data Term = Var Char | Num Int | Func Char Term Term deriving(Eq,Show)  

calculate :: Term -> Term

--basic inductive basis
calculate (Num i) = (Num i)
calculate (Var a) = (Var a)

--number operations
calculate (Func '+' (Num a) (Num b)) = (Num (a+b))
calculate (Func '-' (Num a) (Num b)) = (Num (a-b))
calculate (Func '*' (Num a) (Num b)) = (Num (a*b))
calculate (Func '^' (Num a) (Num b)) = (Num (a^b))

calculate (Func '/' (Num a) (Num b))
  |  (mod a b) == 0 = (Num (div a b))
  |  otherwise = (Func '/' (Num (div a (gcd a b))) (Num (div b  (gcd a b))))
  
calculate (Func '+' t (Num 0) ) = t
calculate (Func '+'  (Num 0) t ) = t
calculate (Func '-' t (Num 0) ) = t
calculate (Func '-'  (Num 0) t ) = t
calculate (Func '*'  t (Num 0) ) = (Num 0)
calculate (Func '*'  (Num 0) t) = (Num 0)
calculate (Func '*'  t (Num 1)) = t
calculate (Func '*'  (Num 1) t) = t
calculate (Func '/'  t (Num 1)) = t
calculate (Func '^'  t (Num 0) ) = (Num 1)
--  * is commutative  x*3 = 3x
calculate(Func '*' (Var v) (Num n)) = calculate(Func '*' (Num n) (Var v))

-- x+x = 2x
calculate (Func '+'  (Var a) (Var b))  
  | a == b = (Func '*' (Num 2) (Var a))
  | otherwise = (Func '+'  (Var a) (Var b))

-- x + 4x = 5x
calculate (Func '+'  (Var a) (Func '*' (Num b) (Var c) ) )  
  | a == c = (Func '*' (Num (b+1)) (Var a))
  | otherwise = (Func '+'  (Var a) (Func '*' (Num b) (Var c))) 	 

-- 5x + 6x= 11x  
calculate (Func '+' (Func '*' (Num a) (Var b) ) (Func '*' (Num c) (Var d) )) 
  | b == d = (Func '*' (Num (a+c)) (Var b))
  | otherwise = (Func '+' (Func '*' (Num a) (Var b)) (Func '*' (Num c) (Var d) ))
  
  
calculate (Func '+' (Func '*' (Num b) (Var c)) (Var a) )  = (calculate (Func '+' (Var a) (Func '*' (Num b) (Var c)))) 
 
 
-- x*x= x^2 
calculate (Func '*'  (Var a) (Var b) )  
  | a == b = (Func '^' (Var a) (Num 2))
  | otherwise = (Func '*'  (Var a) (Var b) )  

-- x^4*x= x^5   
calculate (Func '*'  (Func '^' (Var a) (Num b) ) (Var c) )  
  | a == c = (Func '^' (Var a) (Num (b + 1)))
  | otherwise = (Func '*'  (Func '^' (Var a) (Num b) ) (Var c) )
 
-- x^3 * x^4 = x^7 
calculate (Func '*'  (Func '^' (Var a) (Num b) ) (Func '^' (Var c) (Num d) ) )  
  | a == c = (Func '^' (Var a) (Num (b + d)))
  | otherwise = (Func '*'  (Func '^' (Var a) (Num b) ) (Func '^' (Var c) (Num d) ) )  
 
 
 
calculate (Func op t1 t2)
  | (ismodifed t1) || (ismodifed t2) = calculate (Func op (calculate t1) (calculate t2))
  | otherwise = (Func op t1 t2)  
  
 -- if the term is modified after calculation(was there something to calculate)  
ismodifed :: Term -> Bool
ismodifed t = ((calculate t) /= t) 

evaluate :: Term -> Int ->Term
evaluate t i = (calculate(interpret1 t i))

interpret1 :: Term -> Int -> Term
interpret1 (Var 'x') i = (Num i)
interpret1 (Var v) i = (Var v)
interpret1 (Num n) i = (Num n)
interpret1 (Func op t1 t2) i = (Func op (interpret1 t1 i) (interpret1 t2 i))