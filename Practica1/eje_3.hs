import Parsing
import Data.Char
import Control.Monad
import Control.Applicative hiding (many)

-- expr -> term('+' expr | '-' expr | void) Tiene un porblema con el - ya que 
-- asocia a derecha, para esto cambiaremos la gramática para que asocia a izqui_
-- erda.

expr :: Parser Int
expr = 
    do t <- term
       (do char '+'
           e <- expr
           return (t+e)
           <|> 
           (do char '-'
               e <- expr
               return (t-e)
               <|> 
               return t))

term :: Parser Int
term = 
    (do f <- factor
        (do char '*'
            t <- term
            return (f*t)
            <|>
            (do char '/'
                t <- term
                return (div f t)
                <|>
                return f)))

-- where digit -> '0' | '1' | '2' | ... | '9'
factor :: Parser Int
factor = do d <- digit
            return (digitToInt d)
            <|>
            do char '('
               e <- expr
               char ')'
               return e
               
eval :: String -> Int
eval str = fst (head (parse expr str))

transform :: Parser a -> Parser a
transform par = par 
                <|>
                do char '('
                   n <- transform par 
                   char ')'
                   return n 


