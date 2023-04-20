{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

module Diqtables where

import Diqtypes
import Diqxpressions
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec.Token qualified as T

data Var = Id :#: Type deriving (Show)

data Funcao = Id :->: ([Var], Type) deriving (Show)

data Programa = Prog [Funcao] [(Id, [Var], Bloco)] [Var] Bloco deriving (Show)

type Bloco = [Comando]

data Comando
  = If ExprL Bloco Bloco
  | While ExprL Bloco
  | Atrib Id Expr
  | Leitura Id
  | Imp Expr
  | Ret Expr
  deriving (Show)

lingDef =
  emptyDef
    { T.commentStart = "{-",
      T.commentEnd = "-}",
      T.commentLine = "--",
      T.reservedOpNames =
        [ "+",
          "-",
          "/",
          "*",
          "==",
          ">=",
          "<=",
          ">",
          "<",
          "/="
        ]
    }
tabela   = [[prefix "-" Neg]
            , [binario "*" (:*:) AssocLeft, binario "/" (:/:) AssocLeft ]
            , [binario "+" (:+:) AssocLeft, binario "-" (:-:)   AssocLeft ]
           ]
binario  name fun assoc = Infix (do{reservedOp name; return fun }) assoc
prefix   name fun       = Prefix (do{reservedOp name; return fun })

lexico = T.makeTokenParser lingDef

natural = T.natural lexico

symbol = T.symbol lexico

parens = T.parens lexico
fator = parens expr
       <|> do{n <- natural; return (Const (CInt n))}
       <?> "simple expression"
 
reservedOp = T.reservedOp lexico


expr = buildExpressionParser tabela fator
       <?> "expression"   
op =
  do reservedOp "=="; return (:==:)
    <|> do reservedOp ">="; return (:>=:)
    <|> do reservedOp "<="; return (:<=:)
    <|> do reservedOp ">"; return (:>:)
    <|> do reservedOp "<"; return (:<:)
    <|> do reservedOp "/="; return (:/=:)

partida = do {e <- expr; eof; return e}

parserE e = runParser partida [] "Expressoes" e

parserExpr s = case parserE s of
                     Left er -> print er
                     Right v -> (print v)
                     
main = do putStr "Expressão:"
          e <- getLine 
          parserExpr e