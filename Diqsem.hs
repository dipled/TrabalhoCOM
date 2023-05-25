import Diqparser
import Diqcode
import Diqtypes
import Diqxpressions
import Diqmonad
-- verProg (Prog tf fs tv b) = 
--     do
-- verExpr tab (Const(CInt i)) = (TInt,(Const (CInt i)))

-- verExpr tab (Const (CDouble d)) = (TDouble,(Const (CDouble d)))
  
-- verExpr tab (Lit s) = (TString,(Lit s))
-- -- verExpr [] (IdVar s) = erro "Variavel nao encontrada no escopo" (IdVar s)
-- verExpr ((id :#: t):xs) (IdVar s)
--     |s == id = (t, (IdVar s))
--     |otherwise = verExpr xs (IdVar s)

-- verExpr tab (e1 :+: e2) =
--    do
--        let (t1,e1') = verExpr tab e1
--        let (t2,e2') = verExpr tab e2
--        case (t1, t2) of
--         (TInt, TInt) -> (TInt, e1' :+: e2')
--         (TDouble, TInt) -> (TDouble, e1' :+: e2')
--         (TInt, TDouble) -> (TDouble, e1' :+: e2')
        
verExpr tab ( MS (s,(Const(CInt i)))) = MS(s,(TInt,(Const (CInt i))))
verExpr tab (MS(s,(Const (CDouble d)))) = MS(s,(TDouble,(Const (CDouble d))))

verExpr tab (MS(s,(Lit v))) = MS(s,(TString,(Lit v)))

verExpr [] (MS(s,(IdVar v))) = erro (s++"Variavel nao encontrada") (TInt,IdVar v)

verExpr ((id :#: t):xs) (MS(s,(IdVar v)))
    |s == id = MS(s,(t, (IdVar s)))
    |otherwise = verExpr xs (MS(s,(IdVar v)))


verExpr tab (MS(s,(e1 :+: e2))) =
   do
       let (MS(s1,(t1,e1'))) = verExpr tab (MS(s,e1))
       let (MS(s2,(t2,e2'))) = verExpr tab (MS(s,e2))
       case (t1, t2) of
        (TInt, TInt) -> MS(s1++s2,(TInt, e1' :+: e2'))
        (TDouble, TInt) -> MS(s1++s2,(TDouble, e1' :+: e2'))
        (TInt, TDouble) -> MS(s1++s2,(TDouble, e1' :+: e2'))
 
        


        