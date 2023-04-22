{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use concat" #-}
module Diqmain where

import Diqcode
import Diqtypes
import Diqxpressions
import GHC.Generics (Associativity (NotAssociative))
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec.Token (GenLanguageDef (identLetter))
import Text.Parsec.Token qualified as T

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
          "="
        ],
      T.identStart = letter <|> char '_',
      T.identLetter = alphaNum <|> char '_',
      T.reservedNames =
        [ "int",
          "double",
          "string",
          "void",
          "read",
          "print",
          "return",
          "if",
          "else",
          "while",
          ";"
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

comma = T.comma lexico

-- Expr

binario name fun = Infix (do reservedOp name; return fun)

prefix name fun = Prefix (do reservedOp name; return fun)

tabela =
  [ [prefix "-" Neg],
    [binario "*" (:*:) AssocLeft, binario "/" (:/:) AssocLeft],
    [binario "+" (:+:) AssocLeft, binario "-" (:-:) AssocLeft]
  ]

constT =
  try
    ( do
        n <- intOrDouble
        case n of
          Left num -> return (Const (CInt num))
          Right num -> return (Const (CDouble num))
    )
    <|> try (do lit <- literalString; return (Lit lit))
    <|> try (funCall)
    <|> try (do id <- identifier; return (IdVar id))

fator =
  parens expr
    <|> do constT >>= \c -> return c
    <?> "simple expression"

expr =
  buildExpressionParser tabela fator
    <?> "expression"

-- ExprR

-- Note that we don't need to make a table, nor use buildExpressionParser for ExprR,
-- because all operators have the same precedence level

op =
  do reservedOp "==" >> return (:==:)
    <|> do reservedOp ">=" >> return (:>=:)
    <|> do reservedOp "<=" >> return (:<=:)
    <|> do reservedOp ">" >> return (:>:)
    <|> do reservedOp "<" >> return (:<:)
    <|> do reservedOp "/=" >> return (:/=:)

exprR =
  parens exprR
    <|> do e1 <- expr; o <- op; e2 <- expr; return (o e1 e2)

-- ExprL

tabelaL =
  [ [prefix "~" Not],
    [binarioL "&&" (:&:) AssocLeft],
    [binarioL "||" (:|:) AssocLeft]
  ]

binarioL name fun = Infix (do reservedOp name >> return fun)

prefixL name fun = Prefix (do reservedOp name >> return fun)

fatorL =
  parens exprL
    <|> do exprR >>= \e -> return (Rel e)

exprL = buildExpressionParser tabelaL fatorL

-- Var declaration and func

typeAssert =
  do reserved "int" >> return TInt
    <|> do reserved "double" >> return TDouble
    <|> do reserved "string" >> return TString
    <|> do reserved "void" >> return TVoid



-- Commands
listExpr = try (do comma; e <- expr; return e)
           <|> do e <- expr; return e
funCall = do id <- identifier; exs <- parens(many listExpr); return (Chamada id exs)
cmdEnd = do reserved ";"

atribOp = try (do reservedOp "=" >> return Atrib)
atrib =
  do
    id <- identifier
    o <- atribOp
    e <- expr
    cmdEnd
    return (o id e)

readCmd = do reserved "read" >> return Leitura

readSomething =
  do
    rd <- readCmd
    id <- parens identifier
    cmdEnd
    return (rd id)

retCmd = do reserved "return" >> return Ret

returnSomething =
  do
    rt <- retCmd
    e <- expr
    cmdEnd
    return (rt e)

impCmd = do reserved "print" >> return Imp

printSomething =
  do
    pt <- impCmd
    e <- parens expr
    cmdEnd
    return (pt e)

ifCmd = do reserved "if" >> return If

ifBlock =
  do
    cmd <- ifCmd
    e <- parens exprL
    blk <- braces cmdBlock
    let actualBlk = snd blk
    reserved ("else")
    blk2 <- braces cmdBlock
    let actualBlk2 = snd blk2
    return (cmd e actualBlk actualBlk2)

whileCmd = do reserved "while" >> return While

whileBlock =
  do
    cmd <- whileCmd
    e <- parens exprL
    blk <- braces cmdBlock
    let actualBlk = snd blk

    return (cmd e actualBlk)

cmd =
  try (atrib)
    <|> try (readSomething)
    <|> try (printSomething)
    <|> try (returnSomething)
    <|> try (ifBlock)
    <|> try (whileBlock)
    <|> try (do r <-funCall; cmdEnd; return (Fun r))

parseIds = try (do comma ; id <- identifier; return id)
           <|> do id <- identifier ; return id
varDec = 
  do
    t <- typeAssert
    ids <- many1 parseIds
    cmdEnd
    let ret = map (argConstruct t) ids
    return ret

argConstruct t id = (id :#: t)

cmdBlock =
  do
    vars <- many varDec
    let v = concat vars
    blk <- many cmd
    return (v, blk)

-- Program :33

argDec =
  do
    t <- typeAssert
    id <- identifier
    return (id :#: t)

args = try (do comma; arg <- argDec; return arg)
       <|> do arg <- argDec; return arg
funBlock =
  do
    t <- typeAssert
    id <- identifier
    as <- parens (many args)
    blk <- braces cmdBlock
    let localVars = fst blk
    let actualBlk = snd blk
    return (id :->: (as, t),id,localVars,actualBlk)

first (a,b,c,d) = a
third (a,b,c,d) = c
resto (a,b,c,d) = (b,c,d)
prog =
  do
    funBlks <- many funBlock
    let decs = map first funBlks
        blks = map resto funBlks
        varsList = map third funBlks
    mainBlk <- braces cmdBlock
    let actualMain = snd mainBlk
    let localVars = fst mainBlk
    return (Prog decs blks localVars actualMain)

-- Parser Start

partida = do comment; try (do r <- prog; eof; return r)

parserE = runParser partida [] "Expressoes"

parserExpr s = case parserE s of
  Left er -> print er
  Right v -> print v

main = do readFile "arq.txt" >>= \code -> parserExpr code