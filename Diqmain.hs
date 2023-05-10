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
          "!",
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
          "while"
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

semi = T.semi lexico

-- Expr

binario name fun = Infix (do reservedOp name; return fun)

prefix name fun = Prefix (do reservedOp name; return fun)

tabela =
  [ [prefix "-" Neg],
    [binario "*" (:*:) AssocLeft, binario "/" (:/:) AssocLeft],
    [binario "+" (:+:) AssocLeft, binario "-" (:-:) AssocLeft]
  ]

constT =
  ( do
      n <- intOrDouble
      case n of
        Left num -> return (Const (CInt num))
        Right num -> return (Const (CDouble num))
  )
    <|> (do literalString >>= \lit -> return (Lit lit))
    <|> try (do id <- identifier; exs <- parens (many listExpr); return (Chamada id exs))
    <|> (do identifier >>= \id -> return (IdVar id))

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
    <|> 
    do 
      e1 <- expr
      o <- op
      e2 <- expr
      return (o e1 e2) {-Note que nesse caso, o "o" vem antes das duas
                         expressoes, pois ele eh um construtor do tipo algebrico
                         que recebe duas Expr e retorna uma ExprR -}

-- ExprL

tabelaL =
  [ [prefix "!" Not],
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
listExpr =
  do e <- expr; return e
    <|> (do comma; e <- expr; return e)

atrib =
  do
    id <- identifier
    reservedOp "="
    e <- expr
    return (Atrib id e)

readSomething =
  do
    reserved "read"
    id <- parens identifier
    return (Leitura id)

returnSomething =
  try (do reserved "return" >> expr >>= \e -> return (Ret (Just e)))
    <|> do reserved "return" >> return (Ret (Nothing))

printSomething =
  do
    reserved "print"
    e <- parens expr
    return (Imp e)

elseBlock =
  do
    reserved "else"
    blk2 <- braces cmdBlock
    let actualBlk2 = snd blk2
    return (actualBlk2)
    <|> do return []

ifBlock =
  do
    reserved "if"
    e <- parens exprL
    blk <- braces cmdBlock
    let actualBlk = snd blk
    ret <- elseBlock
    return (If e actualBlk ret)

whileBlock =
  do
    reserved "while"
    e <- parens exprL
    blk <- braces cmdBlock
    let actualBlk = snd blk
    return (While e actualBlk)

cmd =
  try (do atrib >>= \r -> semi >> return r)
    <|> (do readSomething >>= \r -> semi >> return r)
    <|> (do printSomething >>= \r -> semi >> return r)
    <|> (do returnSomething >>= \r -> semi >> return r)
    <|> (do ifBlock >>= \r -> return r)
    <|> (do whileBlock >>= \r -> return r)
    <|> (do id <- identifier; exs <- parens (many listExpr); semi; return (Proc id exs))

parseIds = sepBy1 identifier comma

varDec =
  do
    t <- typeAssert
    ids <- parseIds
    semi
    let ret = (map (argConstruct t) ids)
    return ret

argConstruct t id = (id :#: t)

cmdBlock =
  do
    vars <- many varDec
    let varList = concat vars
    cmds <- many cmd
    return (varList, cmds)

-- Program :33

argDec =
  do
    t <- typeAssert
    id <- identifier
    return (id :#: t)

args = sepBy argDec comma

funBlock =
  do
    t <- typeAssert
    id <- identifier
    as <- parens (args)
    blk <- braces cmdBlock
    let localVars = fst blk
        cmds = snd blk
    return (id :->: (as, t), id, localVars, cmds)

first4 (a, b, c, d) = a

resto4 (a, b, c, d) = (b, c, d)

prog =
  do
    funBlks <- many funBlock
    mainBlk <- braces cmdBlock

    let funDecs = map first4 funBlks
        funScopes = map resto4 funBlks
        mainVars = fst mainBlk
        actualMain = snd mainBlk
    return (Prog funDecs funScopes mainVars actualMain)

-- Parser Start

partida = do comment; r <- prog; eof; return r

parserE = runParser partida [] "Expressoes"

parserExpr s = case parserE s of
  Left er -> print er
  Right v -> print v

main =
  do
    e <- readFile "arq.diq"
    parserExpr e
