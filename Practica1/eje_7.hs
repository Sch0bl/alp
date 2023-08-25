import Parsing
import Data.Char
import Control.Monad
import Control.Applicative hiding (many)

data Hasktype = DInt | DChar | DFloat | Fun Hasktype Hasktype deriving Show

tparser :: Parser Hasktype
tparser = do string "Int" 
             return DInt
             <|>
             do string "Char"
                return DChar
                <|> do string "Float"
                       return DFloat

fun :: Parser Hasktype
fun = do a1 <- tparser
         (do string " -> "
             t <- fun
             return (Fun a1 t)
             <|>
             return a1)
