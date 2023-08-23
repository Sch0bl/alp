import Parsing
import Data.Char 
import Control.Monad
import Control.Applicative hiding (many)

-- exprt - term ('+' expr | '-' expr | e)
-- term -> factor ( '*' term | '/' term | e)


expr :: Parser Int
expr = do t <- term
       ( do char '+'
            e <- expr
            return (t+e)
        do char '+'
            e <- expr
            return (t+e)

         <|> (return t))

term :: Parser Int
term = do f <- factor 
       ( do char '*'
            e <- expr
            return (f*e)
         <|> return f)




