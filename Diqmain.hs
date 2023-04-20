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
          "~"
        ],
        T.identStart      = letter <|> char '_',
        T.identLetter     = alphaNum <|> char '_'
    }
tabela   = [[prefix "-" Neg]
            , [binario "*" (:*:) AssocLeft, binario "/" (:/:) AssocLeft ]
            , [binario "+" (:+:) AssocLeft, binario "-" (:-:)   AssocLeft ]
           ]
tabelaL = [[prefix "~" Not]
            ,[binarioL "&&" (:&:) AssocLeft]
            ,[binarioL "||" (:|:) AssocLeft]
          ]
binario name fun = Infix (do {reservedOp name; return fun })
prefix   name fun       = Prefix (do {reservedOp name; return fun })

binarioL name fun = Infix (do {reservedOp name; return fun })
prefixL   name fun       = Prefix (do {reservedOp name; return fun })

lexico = T.makeTokenParser lingDef
number = T.naturalOrFloat lexico
symbol = T.symbol lexico
parens = T.parens lexico
reservedOp = T.reservedOp lexico
identificador = T.identifier lexico
literal = T.stringLiteral lexico

constT = do {n <- number; case n of
                                  Left num -> return (Const (CInt num))
                                  Right num -> return (Const (CDouble num)) }
         <|> do {lit <- literal; return (Lit lit)}
{-
Note que fator agora chama somente constT que tratara tanto de Int e Double quanto de Literais Strings
-}
fator = parens expr
       <|> do {c <- constT; return c}
       <?> "simple expression"
  
fatorL = parens exprL
        <|> do {e <- exprR; return (Rel e)}


expr = buildExpressionParser tabela fator
       <?> "expression" 

exprR = do {e1 <- expr; o <- op; e2 <- expr; return (o e1 e2);}

exprL = buildExpressionParser tabelaL fatorL
{-para o operador relacional nao precisamos fazer tabela nem o fator, pois cada operador tem a mesma precedencia-}
op =
  do reservedOp "=="; return (:==:)
    <|> do reservedOp ">="; return (:>=:)
    <|> do reservedOp "<="; return (:<=:)
    <|> do reservedOp ">"; return (:>:)
    <|> do reservedOp "<"; return (:<:)
    <|> do reservedOp "/="; return (:/=:)


partida = do {e <- exprL; eof; return e}

parserE  = runParser partida [] "Expressoes"

parserExpr s = case parserE s of
                     Left er -> print er
                     Right v -> print v

main = do putStr "Expressao:"
          e <- getLine
          parserExpr e