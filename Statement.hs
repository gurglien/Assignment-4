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
		Read String |
		Write Expr.T
    deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e 

skip = accept "skip" #- require ";">->buildSkip
buildSkip _ = Skip


begin = accept "begin" -# iter parse #- require "end" >-> buildBegin
buildBegin a = Begin a 

ifStmt = (accept "if" -# Expr.parse #- require "then" # parse #- require "else") # parse >-> buildIf
buildIf ((a,b), c)= If a b c 

while = accept "while" -# Expr.parse #- require "do" # parse >-> buildWhile
buildWhile (v,e) = While v e 

readStmt =  accept "read" -# word #- require ";" >-> buildRead
buildRead b = Read b

write = accept "write" -# Expr.parse #- require ";" >-> buildWrite
buildWrite b = Write b 

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec (If cond thenStmts elseStmts: stmts) dict input = 
	if (Expr.value cond dict)>0 
	then exec (thenStmts: stmts) dict input
	else exec (elseStmts: stmts) dict input
exec (While expr stmt : stmts) dict input = 
	if(Expr.value expr dict) > 0
	then exec (stmt:(While expr stmt : stmts)) dict input
	else exec stmts dict input
exec (Assignment name expr: stmts) dict input = exec stmts (Dictionary.insert (name, Expr.value expr dict) dict) input
exec (Skip : stmts) dict input = exec stmts dict input
exec (Write expr : stmts) dict input = Expr.value expr dict : exec stmts dict input 
exec (Read expr : stmts) dict (i:input) = exec stmts (Dictionary.insert (expr, i) dict) input
exec (Begin list : stmts) dict input = exec (list ++stmts) dict input
exec [] _ _ = []


instance Parse Statement where
  parse = assignment ! skip ! readStmt ! write ! ifStmt ! while ! begin
	toString (While expr stmts) = "while " ++ Expr.toString expr ++ " do\n" ++ (toString stmts)
	toString (Skip) = "skip;\n"
	toString (Assignment name expr) = "Assignment " ++ name ++ " := " ++ Expr.toString expr ++ ";\n"
	toString (Write expr) = "Write " ++ Expr.toString expr ++ ";\n"
	toString (Begin list) = "Begin " ++ map toString list
	toString (Read expr) = "Read " ++ Expr.toString expr ++ ";\n"
	toString (If cond thenStmt elseStmt) = "if " ++ Expr.toString cond ++ " then\n"  ++ (toString thenStmt) ++ "\nelse\n" ++ (toString elseStmt)
