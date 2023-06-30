module Diqcode where
import Diqtypes
import Diqxpressions
data Var = Id :#: Type 
  deriving (Show)

data Funcao = Id :->: ([Var], Type) 

  deriving (Show)

data Programa = Prog [Funcao] [(Id, [Var], Bloco)] [Var] Bloco 
  deriving (Show)

type Bloco = [Comando]

data Comando
  = If ExprL Bloco Bloco
  | While ExprL Bloco
  |For Comando ExprL Comando Bloco
  |DoWhile Bloco ExprL
  | Atrib Id Expr
  | Leitura Id
  | Imp Expr
  | Ret (Maybe Expr)
  | Proc Id [Expr]
  deriving (Show)
