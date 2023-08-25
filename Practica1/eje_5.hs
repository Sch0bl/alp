import Parsing
import Data.Char
import Control.Monad
import Control.Applicative hiding (many)

data Basetype = DInt | DChar | DFloat deriving Show
type Hasktype = [Basetype]  

-- Primero vamos a representar la gramática
--
-- Int -> Char -> Float
-- [DInt, DChar, DFloat]
--

arrowSep :: Parser ()
arrowSep = do string " -> "
              return ()

bparser :: Parser Basetype
bparser = do string "Int" 
             return DInt
             <|>
             do string "Char"
                return DChar
                <|> do string "Float"
                       return DFloat

htparser :: Parser Hasktype
htparser = (sepBy bparser arrowSep)


