module Diqxpressions where
import Diqtypes

data Expr = Expr :+: Expr | Expr :-: Expr | Expr :*: Expr | Expr :/: Expr | Neg Expr | Const TCons | IdVar String | Chamada Id [Expr] | Lit String
    deriving Show


data ExprR = Expr :==: Expr | Expr :/=: Expr | Expr :<: Expr | Expr :>: Expr | Expr :<=: Expr | Expr :>=: Expr 
    deriving Show


data ExprL = ExprL :&: ExprL | ExprL :|: ExprL | Not ExprL | Rel ExprR
    deriving Show