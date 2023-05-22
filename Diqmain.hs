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
        ["+", "-", "/", "*", "==", ">=", "<=", ">", "<", "/=", "&&", "||", "!", "="],
      T.identStart = letter <|> char '_',
      T.identLetter = alphaNum <|> char '_',
      T.reservedNames =
        ["int", "double", "string", "void", "read", "print", "return", "if", "else", "while"]
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
    <|> try (do id <- identifier; exs <- parens (listExpr); return (Chamada id exs))
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
    <|> do e1 <- expr; o <- op; e2 <- expr; return (o e1 e2 {-Note que nesse caso, o "o" vem antes das duas
                                                             expressoes, pois ele eh um construtor do tipo algebrico
                                                             que recebe duas Expr e retorna uma ExprR -})

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

returnType =
  do reserved "int" >> return TInt
    <|> do reserved "double" >> return TDouble
    <|> do reserved "string" >> return TString
    <|> do reserved "void" >> return TVoid

-- Commands
listExpr = sepBy expr comma

atrib = do id <- identifier; reservedOp "="; e <- expr; return (Atrib id e)

readSomething = do reserved "read"; id <- parens identifier; return (Leitura id)

returnSomething =
  try (do reserved "return" >> expr >>= \e -> return (Ret (Just e)))
    <|> do reserved "return" >> return (Ret (Nothing))

printSomething = do reserved "print"; e <- parens expr; return (Imp e)

elseBlock =
  do reserved "else"; blk2 <- braces cmdBlock; return (blk2)
    <|> do return []

ifBlock = do reserved "if"; e <- parens exprL; blk <- braces cmdBlock; ret <- elseBlock; return (If e blk ret)

whileBlock =
  do
    reserved "while"
    e <- parens exprL
    blk <- braces cmdBlock
    return (While e blk)

cmd =
  try (do atrib >>= \r -> semi >> return r)
    <|> (do readSomething >>= \r -> semi >> return r)
    <|> (do printSomething >>= \r -> semi >> return r)
    <|> (do returnSomething >>= \r -> semi >> return r)
    <|> (do ifBlock >>= \r -> return r)
    <|> (do whileBlock >>= \r -> return r)
    <|> (do id <- identifier; exs <- parens (listExpr); semi; return (Proc id exs))

parseIds = sepBy1 identifier comma

varDec = do t <- typeAssert; ids <- parseIds; semi; return (map (argConstruct t) ids)

argConstruct t id = (id :#: t)

varBlock = do vars <- many varDec; return (concat vars)

cmdBlock = do cmds <- many cmd; return cmds

completeBlock = do vars <- varBlock; cmds <- cmdBlock; return (vars, cmds)

-- Program :33
argDec = do t <- typeAssert; id <- identifier; return (id :#: t)

args = sepBy argDec comma

funBlock =
  do
    t <- returnType
    id <- identifier
    as <- parens (args)
    blk <- braces completeBlock
    let localVars = fst blk
        cmds = snd blk
    return (id :->: (as ++ localVars, t), (id, as ++ localVars, cmds))

prog =
  do
    funBlks <- many funBlock
    mainBlk <- braces completeBlock
    let funDecs = map fst funBlks
        funScopes = map snd funBlks
        mainVars = fst mainBlk
        actualMain = snd mainBlk
    return (Prog funDecs funScopes mainVars actualMain)

-- Parser Start
partida = do r <- prog; eof; return r

parserE = runParser partida [] "Expressoes"

parserExpr s = case parserE s of
  Left er -> print er
  Right v -> print v

main =
  do
    e <- readFile "teste1.j--"
    parserExpr e
