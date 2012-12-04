module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)
newtype T = Program [Statement.T] deriving Show
instance Parse T where
  parse = (iter Statement.parse) >-> Program
  toString (Program a) = foldr1 (++) (map Statement.toString a)
             
exec :: T -> [Integer] -> [Integer]
exec (Program a) b = Statement.exec a Dictionary.empty b