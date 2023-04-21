{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Redundant return" #-}

module Diqmain where
import Diqcode
import Diqtypes
import Diqxpressions
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec.Token qualified as T
import GHC.Generics (Associativity(NotAssociative))


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
          "/=",
          "&&",
          "||",
          "~",
          "#"
        ],
        T.identStart      = letter <|> char '_',
        T.identLetter     = alphaNum <|> char '_',
        T.reservedNames = 
          [
            "TInt", "TDouble", "TString", "TVoid"
          ]
    }

lexico = T.makeTokenParser lingDef

intOrDouble = T.naturalOrFloat lexico

symbol = T.symbol lexico

parens = T.parens lexico

reservedOp = T.reservedOp lexico

identifier = T.identifier lexico

literalString = T.stringLiteral lexico

comment = T.whiteSpace lexico


-- *********************************************************************
-- Expr
-- *********************************************************************


binario name fun = Infix (do {reservedOp name; return fun })

prefix   name fun       = Prefix (do {reservedOp name; return fun })


tabela   = [[prefix "-" Neg]
            , [binario "*" (:*:) AssocLeft, binario "/" (:/:) AssocLeft ]
            , [binario "+" (:+:) AssocLeft, binario "-" (:-:)   AssocLeft ]
           ]


constT = try (do {n <- intOrDouble; case n of
                                  Left num -> return (Const (CInt num))
                                  Right num -> return (Const (CDouble num)) })
         <|> try (do {lit <- literalString; return (Lit lit)})
         <|> try (do {id <- identifier; exs <- many1 expr; return (Chamada id exs)})
         <|> try (do {id <- identifier; return (IdVar id)})


fator = parens expr
       <|> do {c <- constT; return c}
       <?> "simple expression"


expr = buildExpressionParser tabela fator
       <?> "expression" 




-- *********************************************************************
-- ExprR
-- *********************************************************************


-- Note that we don't need to make a table, nor use buildExpressionParser for ExprR,
-- because all operators have the same precedence level

op =
  do reservedOp "=="; return (:==:)
    <|> do reservedOp ">="; return (:>=:)
    <|> do reservedOp "<="; return (:<=:)
    <|> do reservedOp ">"; return (:>:)
    <|> do reservedOp "<"; return (:<:)
    <|> do reservedOp "/="; return (:/=:)


exprR =  do {e1 <- expr; o <- op; e2 <- expr; return (o e1 e2)}


-- *********************************************************************
-- ExprL
-- *********************************************************************


tabelaL = [[prefix "~" Not]
            ,[binarioL "&&" (:&:) AssocLeft]
            ,[binarioL "||" (:|:) AssocLeft]
          ]


binarioL name fun = Infix (do {reservedOp name; return fun })

prefixL   name fun       = Prefix (do {reservedOp name; return fun })


fatorL = parens exprL
        <|> do {e <- exprR; return (Rel e)}


exprL = buildExpressionParser tabelaL fatorL


-- *********************************************************************
-- Parser Start
-- *********************************************************************


partida = do comment ; try (do {e <- exprL; eof; return (show e)})
                        <|> do {e <- expr; eof; return (show e)}
           

parserE  = do runParser partida [] "Expressoes"   


parserExpr s = case parserE s of
                     Left er -> print er
                     Right v -> print v


main = do 
          e <- readFile "arq.txt"
          parserExpr e
