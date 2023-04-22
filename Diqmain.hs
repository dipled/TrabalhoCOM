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
import Text.Parsec.Token (GenLanguageDef(identLetter))


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
          "#",
          "->",
          ":="
        ],
        T.identStart      = letter <|> char '_',
        T.identLetter     = alphaNum <|> char '_',
        T.reservedNames = 
          [
            "int", "double", "string", "void",
            "read", "print", "return", "if", "while", "main"
          ]
    }

lexico = T.makeTokenParser lingDef

intOrDouble = T.naturalOrFloat lexico

symbol = T.symbol lexico

parens = T.parens lexico

reservedOp = T.reservedOp lexico

identifier = T.identifier lexico

literalString = T.stringLiteral lexico

reserved = T.reserved lexico

comment = T.whiteSpace lexico

braces = T.braces lexico

angles = T.angles lexico

brackets = T.brackets lexico
-- *********************************************************************
-- Expr
-- *********************************************************************


binario name fun = Infix (do {reservedOp name; return fun })

prefix   name fun = Prefix (do {reservedOp name; return fun })


tabela   = [[prefix "-" Neg]
            , [binario "*" (:*:) AssocLeft, binario "/" (:/:) AssocLeft ]
            , [binario "+" (:+:) AssocLeft, binario "-" (:-:)   AssocLeft ]
           ]


constT = try (do {n <- intOrDouble; case n of
                                  Left num -> return (Const (CInt num))
                                  Right num -> return (Const (CDouble num))})
         <|> try (do {lit <- literalString; return (Lit lit)})
         <|> try (do {id <- identifier; exs <- many1 expr; return (Chamada id exs)}) --esse many1 pode vir a virar many futuramente
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


exprR = parens exprR 
        <|>do {e1 <- expr; o <- op; e2 <- expr; return (o e1 e2)}


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
-- Var declaration and func
-- *********************************************************************


typeAssert = 
  do reserved "int"; return TInt
  <|> do reserved "double"; return TDouble
  <|> do reserved "string"; return TString
  <|> do reserved "void"; return TVoid


varDecOp = do {reservedOp "#"; return (:#:)}


varDec = 
  do
    id <- identifier
    o <- varDecOp
    t <- typeAssert
    return (o id t)


funOp = do{reservedOp "->"; return (:->:)}

funDec = 
  do
    id <- identifier
    o <- funOp 
    vars <- parens (many varDec)
    t <-typeAssert
    return (o id (vars, t))


-- *********************************************************************
-- Commands
-- *********************************************************************

atribOp = do{reservedOp ":="; return Atrib}

atrib = 
  do
    id <- identifier
    o <- atribOp
    e <- expr
    return (o id e)


readCmd = do reserved "read" ; return Leitura

readSomething = 
  do
    rd <- readCmd
    id <- identifier
    return (rd id)

retCmd = do reserved "return"; return Ret

returnSomething =
  do
    rt <- retCmd
    e <- expr
    return (rt e)


impCmd = do reserved "print"; return Imp

printSomething =
  do
    pt <- impCmd
    e <- expr
    return (pt e)

ifCmd = do reserved "if"; return If

ifBlock =
  do
    cmd <- ifCmd
    e <- parens exprL
    blk <- braces cmdBlock
    blk2 <- braces cmdBlock
    return (cmd e blk blk2)

whileCmd = do reserved "while"; return While

whileBlock =
  do
    cmd <- whileCmd
    e <- parens exprL
    blk <- braces cmdBlock
    return (cmd e blk)

cmd = try (do {atrib})
      <|> try (do {readSomething})
      <|> try (do {printSomething})
      <|> try (do {returnSomething})
      <|> try (do {ifBlock})
      <|> try (do {whileBlock})


cmdBlock = 
  do
    blk <- many1 cmd
    return blk


-- *********************************************************************
-- Program :33
-- *********************************************************************
funBlock = 
  do
    id <- identifier
    vars <- parens (many varDec)
    blk <- braces cmdBlock
    return (id, vars, blk)

prog = 
  do
    funDecs <- brackets(many funDec)
    funBlks <- brackets(many funBlock)
    vars <- brackets(many varDec)
    reserved "main"
    mainBlk <- braces cmdBlock
    return (Prog funDecs funBlks vars mainBlk)



-- *********************************************************************
-- Parser Start
-- *********************************************************************


partida = do comment ;  
                         try(do{r <- prog; eof; return r})
parserE  = do runParser partida [] "Expressoes"   


parserExpr s = case parserE s of
                     Left er -> print er
                     Right v -> print v


main = do 
          e <- readFile "arq.txt"
          parserExpr e
