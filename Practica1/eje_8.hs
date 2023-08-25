import Parsing
import Data.Char
import Control.Monad
import Control.Applicative hiding (many)

-- expr -> term('+' expr | '-' expr | void) Tiene un porblema con el - ya que 
-- asocia a derecha, para esto cambiaremos la gramática para que asocia a izqui_
-- erda.

-- expr -> expr('+' term | '-' term) | term
-- Sin embargo, si queremos parsear con esta gramática, vamos a tener el proble-
-- ma llamado "Recursión izquierda", por lo que modificaremos la gramática una
-- vez mas.

-- expr -> termexpr'
-- expr' -> ('+' term | '-' term)expr' | e
expr :: Parser Int
expr = do t <- term 
          (do f <- expr'
              return (f t)
              <|>
              return t)

expr' :: Parser (Int -> Int)
expr' = do char '+'
           t <- term
           (do f <- expr'
               return (f . (\n -> n + t))
               <|>
               return (\n -> n + t))
           <|>
           do char '-'
              t <- term
              (do f <- expr'
                  return (f . (\n -> n - t))
                  <|>
                  return (\n -> n - t)) 

-- El cambio de gramática con term es análogo a exp.

term :: Parser Int
term = do fac <- factor
          (do f <- term'
              return (f fac)
              <|>
              return fac)

term' :: Parser (Int -> Int)
term' = do char '*'
           fac <- factor
           (do f <- term'
               return (f . (\n -> n * fac))
               <|>
               return (\n -> n * fac))
           <|>
           do char '/'
              fac <- factor
              (do f <- term'
                  return (f . (\n -> div n fac))
                  <|>
                  return (\n -> div n fac))




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


