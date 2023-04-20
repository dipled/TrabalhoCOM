
module Diqmain where
import Diqcode
import Diqtypes
import Diqxpressions
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec.Token qualified as T
import qualified Data.Ord as T

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
binario name fun = Infix (do {reservedOp name; return fun })
prefix   name fun       = Prefix (do {reservedOp name; return fun })

lexico = T.makeTokenParser lingDef

--O natural foi comentado para não precisarmos
--natural = T.natural lexico
number = T.naturalOrFloat lexico
symbol = T.symbol lexico

parens = T.parens lexico
fator = parens expr
    -- <|> do {n <- natural; return (Const (CInt n))}
    --Comentado em virtude do number que assume tanto int quanto double
       <|> do {n <- number; case n of
                                  Left num -> return (Const (CInt num))
                                  Right num -> return (Const (CDouble num))}
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