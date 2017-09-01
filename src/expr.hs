-- Integer Expression AST/Evaluator: Haskell --

{--
  define an algebraic type representing the abstract syntax tree
  for our simple expression language:
  an Expr is either a Const (containing an integer value),
  or a Plus (of two Exprs),
  or a Sub (of two Exprs),
  or a Mul (of two Exprs),
  or a Div (of two Exprs)
--}
data Expr = Const Int
          | Plus Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr

{--
  to evaluate an Expr, we pattern-match against its top-level structure
  and produce the associated value of a Const, or evaluate recursively otherwise
--}
eval :: Expr -> Int
eval (Const n) = n
eval (Plus e1 e2) = (eval e1) + (eval e2)
eval (Sub e1 e2) = (eval e1) - (eval e2)
eval (Mul e1 e2) = (eval e1) * (eval e2)
eval (Div e1 e2) = (eval e1) `div` (eval e2)

-- evaluate a simple expression represented as a literal Expr
main :: IO ()
main = let e = Mul (Const 3) (Plus (Const 5) (Const 2)) in
    putStrLn $ show $ eval e
