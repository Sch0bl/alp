import Parsing
import Data.Char
import Control.Monad
import Control.Applicative hiding (many)

type HetLists = ([Int],String)

data CharInt = C Char | I Int
type HeteroList = [CharInt]

charSep :: Char -> Parser ()
charSep c = do many (char c)
               return ()

heter :: Parser ([Int], String)
heter = do heter1
           <|>
           return ([],[])

heter1 :: Parser ([Int], String)
heter1 = do char ','
            x <- heter1
            return x
            <|>
            do char '\''
               c <- letter
               char '\''
               (ds,cs) <- heter
               return (ds, (c:cs))
               <|>
               do d <- digit 
                  (ds,cs) <- heter
                  return (((digitToInt d):ds),cs)
                  
                     

heterogeneus :: Parser ([Int], String)
heterogeneus = do char '['
                  l <-  heter 
                  char ']'
                  return l 

              

