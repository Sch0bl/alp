import Parsing
import Data.Char
import Control.Monad
import Control.Applicative hiding (many)

list0 :: Parser [Int]
list0 = do l1 <- list'
           symbol "++" 
           l2 <- list0
           return (l1 ++ l2)
        <|> 
        do l <- list'
           return l

list' :: Parser [Int]
list' = do char '['
           v <- val
           char ']'
           return v
        <|>
        do e <- int
           char ':'
           l <- list'
           return (e:l)
        <|> 
        do symbol "[]"
           return []

val :: Parser [Int]
val = do n <- int
         char ','
         v <- val
         return (n:v)
      <|>
      do n <- int
         return [n]


