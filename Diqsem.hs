import Diqparser
import Diqcode
import Diqtypes
import Diqxpressions
import Diqmonad
-- verProg (Prog tf fs tv b) = 
--     do
verExpr tab (Const(CInt i)) = (TInt,(Const (CInt i)))

verExpr tab (Const (CDouble d)) = (TDouble,(Const (CDouble d)))

verExpr tab (Lit s) = (TString,(Lit s))

--verExpr [] (IdVar s) = erro "Variavel nao encontrada no escopo"
verExpr ((id :#: t):xs) (IdVar s)
    |s == id = (t, (IdVar s))
    |otherwise = verExpr xs (IdVar s)
-- verExpr tab (MS ("",(Const(CInt i)))) = MS("",(TInt,(Const (CInt i))))
-- verExpr tab (MS("",(Const (CDouble d)))) = MS("",(TDouble,(Const (CDouble d))))

-- verExpr tab (MS("",(Lit s))) = MS("",(TString,(Lit s)))

-- verExpr [] (MS("",(IdVar s))) = erro "Variavel nao encontrada" (TInt,IdVar s)
-- verExpr ((id :#: t):xs) (MS("",(IdVar s)))
--     |s == id = MS("",(t, (IdVar s)))
--     |otherwise = verExpr xs (MS("",(IdVar s)))
 
verExpr tab (e1 :+: e2) =
   do
       let (t1,e1') = verExpr tab e1
       let (t2,e2') = verExpr tab e2
       case (t1, t2) of
        (TInt, TInt) -> (TInt, e1' :+: e2')
        (TDouble, TInt) -> (TDouble, e1' :+: e2')
        (TInt, TDouble) -> (TDouble, e1' :+: e2')
        
        


        