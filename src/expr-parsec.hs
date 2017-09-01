-- Integer Expression AST/Evaluator and Parser: Haskell --
-- this builds on the contents of expr.hs to add a parser

-- we'll use the Parsec parser combinator library
-- (https://www.stackage.org/package/parsec)
import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)

-- add 'deriving Show' to let us display AST values interactively
data Expr = Const Int
          | Plus Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          deriving Show

eval :: Expr -> Int
eval (Const n) = n
eval (Plus e1 e2) = (eval e1) + (eval e2)
eval (Sub e1 e2) = (eval e1) - (eval e2)
eval (Mul e1 e2) = (eval e1) * (eval e2)
eval (Div e1 e2) = (eval e1) `div` (eval e2)

-- we'll build a lexer from the empty language defintion;
-- this'll give us a record full of Parsec functions to recognize
-- things like whitespace-separated tokens or literal integers
lexer = P.makeTokenParser emptyDef

-- a combinator: take a parser and match that with optional whitespace around it
lexeme = P.lexeme lexer

-- a combinator: take a parser and match that surrounded by '(' ')' parens
parens = P.parens lexer

-- recognize a literal integer
integer = P.integer lexer

-- to parse a constExpr, recognize a literal intger and pass that result
--   to the Const constructor (<$> means 'fmap')
constExpr = (Const . fromInteger) <$> integer

-- monadic 'do' notation (this is what LINQ is emulating in the C# example)
plusExpr = parens (do
    lexeme $ char '+'
    lhs <- lexeme expr
    rhs <- lexeme expr
    return (Plus lhs rhs))

subExpr = parens (do
    lexeme $ char '-'
    lhs <- lexeme expr
    rhs <- lexeme expr
    return (Sub lhs rhs))

mulExpr = parens (do
    lexeme $ char '*'
    lhs <- lexeme expr
    rhs <- lexeme expr
    return (Mul lhs rhs))

divExpr = parens (do
    lexeme $ char '/'
    lhs <- lexeme expr
    rhs <- lexeme expr
    return (Div lhs rhs))

-- in Parsec, we need the 'try' combinator to allow backtracking
-- without consuming partial input if an alternative fails: remember that
-- many of our expression share a common prefix of '('
expr = constExpr
   <|> try plusExpr
   <|> try subExpr
   <|> try mulExpr
   <|> try divExpr

-- Parsec uses the Either result type to represent a parse result:
--   either the succeesful value (Right) or an error (Left)
parseExpr :: String -> Either ParseError Expr
parseExpr = parse expr ""

main :: IO ()
main = case parseExpr "(* 3 (+ 5 2))" of
    Left err -> putStrLn $ show $ err
    Right e -> putStrLn $ show $ eval e
