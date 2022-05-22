x |> f = f x

zero = Const 0.0
one = Const 1.0

data Symbol = X 
    deriving (Eq, Show)

data Op = Pol Float | Exp | Log | Sin | Cos
    deriving (Eq, Show)

data Expression 
    = Add Expression Expression 
    | Sub Expression Expression 
    | Mul Expression Expression
    | Div Expression Expression
    | Unary Op Expression
    | Const Float
    | Var Symbol
    deriving (Eq, Show)

d x (Const n) = zero
d x (Var u) =
    if x == Var u then
        one
    else
        zero
d x (Add a b) = Add (d x a) (d x b)
d x (Sub a b) = Sub (d x a) (d x b)
d x (Mul a b) = Add (Mul (d x a) b) (Mul a (d x b))
d x (Div a b) = d x (Mul a (Unary (Pol (-1.0)) b))
d x (Unary f u) =
    if x == u then
        case f of
            Pol 0.0 -> 
                zero
            Pol n -> 
                Mul (Const n) (Unary (Pol (n - 1)) x)
            Exp ->
                Unary Exp x
            Log ->
                Div one x
            Sin ->
                Unary Cos x
            Cos ->
                Sub zero (Unary Sin x)
    else
        Mul (d u (Unary f u)) (d x u)

simplify (Add a b) =
    case (simplify a, simplify b) of
        (Const 0.0, b') ->
            simplify b'
        (a', Const 0.0) ->
            simplify a'
        (Const a', Const b') ->
            Const (a' + b')
        (a', b') ->
            Add a' b'
simplify (Sub a b) = Sub (simplify a) (simplify b)
simplify (Mul a b) =
    case (simplify a, simplify b) of
        (Const 0.0, b') ->
            zero
        (a', Const 0.0) ->
            zero
        (Const 1.0, b') ->
            simplify b'
        (a', Const 1.0) ->
            simplify a'
        (Const a', Const b') ->
            Const (a' * b')
        (a', b') ->
            Mul a' b'
simplify (Div a (Const 1.0)) = a |> simplify
simplify (Div a b) = Div (simplify a) (simplify b)
simplify (Unary (Pol 0.0) _) = one |> simplify
simplify (Unary (Pol 1.0) a) = a |> simplify
simplify (Unary op ex) = Unary op (simplify ex)
simplify x = x

-- f(x) = 2 * x^3
f = Mul (Const 2.0) (Unary (Pol 3.0) (Var X))
-- g(x) = (x + 1)^2
g = Unary (Pol 2.0) (Add (Var X) one) 
-- h(x) = e^(x^2 + x)
h = Unary Exp (Add (Unary (Pol 2.0) (Var X)) (Var X))

main :: IO ()
main = do
    print $ simplify $ d (Var X) f
    print $ simplify $ d (Var X) g
    print $ simplify $ d (Var X) h