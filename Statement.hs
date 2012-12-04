module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T |   
		Skip |
		Begin [Statement] String |
		If Expr.T Statement Statement |
		While Expr.T Statement |
		Read Variable |
		Write Expr.T
    deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input

instance Parse Statement where
  parse = error "Statement.parse not implemented"
  toString = error "Statement.toString not implemented"


-- program ::= statements
-- statement ::= variable ':=' expr ';'
  -- | 'skip' ';'
  -- | 'begin' statements 'end'
  -- | 'if' expr 'then' statement 'else' statemen
  -- | 'while' expr 'do' statement
  -- | 'read' variable ';'
  -- | 'write' expr ';'
--  statements ::= {statement}
--  variable ::= letter {letter}