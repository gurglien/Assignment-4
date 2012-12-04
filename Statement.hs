module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T |   
		Skip |
		Begin [Statement] |
		If Expr.T Statement Statement |
		While Expr.T Statement |
		Read Expr.T |
		Write Expr.T
    deriving Show




assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss --done
buildAss (v, e) = Assignment v e --done
skip = accept "skip" #- require ";">->buildSkip --done
buildSkip _ = Skip  --vi
begin = accept "begin" -# iter parse #- require "end" >-> buildBegin
buildBegin a = Begin a 
ifStmt = (accept "if" -# Expr.parse #- require "then" # parse #- require "else") # parse >-> buildIf
buildIf ((a,b), c)= If a b c 
while = accept "while" -# Expr.parse #- require "do" # parse >-> buildWhile
buildWhile (v,e) = While v e 
readStmt =  accept "read" -# Expr.parse #- require ";" >-> buildRead
buildRead b = Read b
write = accept "write" -# Expr.parse #- require ";" >-> buildWrite
buildWrite b = Write b 

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input

instance Parse Statement where
  parse = assignment ! skip ! readStmt ! write ! ifStmt ! while ! begin
  toString = show
