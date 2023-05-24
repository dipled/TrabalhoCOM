import Diqparser
import Diqcode
import Diqtypes
import Diqxpressions
-- verProg (Prog tf fs tv b) = 
--     do
verExpr tab (Const(CInt i)) = return (TInt,(Const (CInt i)))
verExpr tab (Const (CDouble d)) = return (TDouble,(Const (CDouble d)))
verExpr tab (IdVar s) =
 
--verExpr tab (e1 :+: e2) =
--    do
--        (t1,e1') <- verExpr tab e1
--        (t2,e2') <- verExpr tab e2

        