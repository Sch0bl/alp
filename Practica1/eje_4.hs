import Parsing
import Data.Char
import Control.Monad
import Control.Applicative hiding (many)

data Expr = Num Int | BinOp Op Expr Expr deriving Show
data Op = Add | Mul | Min | Div deriving Show

-- expr -> term('+' expr | '-' expr | void) Tiene un porblema con el - ya que 
-- asocia a derecha, para esto cambiaremos la gramática para que asocia a izqui_
-- erda.

expr :: Parser Expr
expr = 
    do t <- term
       (do char '+'
           e <- expr
           return (BinOp Add t e)
           <|> 
           (do char '-'
               e <- expr
               return (BinOp Min t e)
               <|> 
               return t))

term :: Parser Expr
term = 
    (do f <- factor
        (do char '*'
            t <- term
            return (BinOp Mul f t)
            <|>
            (do char '/'
                t <- term
                return (BinOp Div f t)
                <|>
                return f)))

-- where digit -> '0' | '1' | '2' | ... | '9'
factor :: Parser Expr
factor = do d <- digit
            return (Num (digitToInt d))
            <|>
            do char '('
               e <- expr
               char ')'
               return e
               
eval :: String -> Expr
eval str = fst (head (parse expr str))

transform :: Parser a -> Parser a
transform par = par 
                <|>
                do char '('
                   n <- transform par 
                   char ')'
                   return n 


