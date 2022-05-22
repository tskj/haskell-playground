data Symbol = X deriving Show

data Expression = Constant Int | 
                  Variable Symbol | 
                  Add Expression Expression | 
                  Sub Expression Expression |
                  Mul Expression Expression | 
                  Div Expression Expression | 
                  Exp Expression Expression |
                  Log Expression Expression 
                  deriving Show

zero = Constant 0
one = Constant 1
negOne = Constant (-1)

e = Constant 3
x = Variable X

derivative (Constant n) = zero
derivative (Variable x) = one
derivative (Add left right) = Add (derivative left) (derivative right)
derivative (Sub left right) = Sub (derivative left) (derivative right)
derivative (Mul left right) = Add (Mul (derivative left) right)
                                  (Mul left (derivative right))
derivative (Div numerator denominator) = derivative (Mul numerator (Exp denominator negOne))
derivative (Log base arg) = Div (derivative arg) arg
derivative (Exp base arg) = Mul (derivative (Mul arg (Log e base)))
                                (Exp base arg)

main :: IO ()
main = do
  print $ derivative (Exp e x)