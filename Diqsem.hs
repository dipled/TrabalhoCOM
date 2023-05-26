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

verExpr tab (MS(s,(Const(CInt i)))) = MS(s,(TInt,(Const (CInt i))))
verExpr tab (MS(s,(Const (CDouble d)))) = MS(s,(TDouble,(Const (CDouble d))))

verExpr tab (MS(s,(Lit v))) = MS(s,(TString,(Lit v)))

verExpr ((fn :->: ([],ft)):ys) (MS(s,(IdVar v))) = erro (s++"Variavel nao encontrada") (TInt,IdVar v)

verExpr ((fn :->: ((id :#: t):xs,ft)):ys) (MS(s,(IdVar v)))
    |v == id = MS(s,(t, (IdVar v)))
    |otherwise = verExpr ((fn :->: (xs,ft)):ys) (MS(s,(IdVar v)))

verExpr [] (MS(s,(Chamada fid args))) = erro (s++"Variavel nao encontrada") (TInt,(Chamada fid args)) 
verExpr ((fn :->: ((id :#: t):xs,ft)):ys) (MS(s,(Chamada fid args))) 
    |fid == fn = (MS(s,(ft,Chamada fid args)))
    |otherwise = verExpr ys (MS(s,(Chamada fid args)))
verExpr tab (MS(s,(Neg e))) = 
    do
        let MS(s1,(t,e')) = verExpr tab (MS(s,e))
        case t of
            (TInt) -> MS(s1,(t,Neg e))
            (TDouble) -> MS(s1,(t,Neg e))
            _ -> erro(s1++"Erro de tipo na tentativa de Negar a expressao") (t,Neg e)

verExpr tab (MS(s,((e1 :+: e2)))) =
    do
        let MS(s1,(t1,e1')) = verExpr tab (MS(s,e1))
            MS(s2,(t2,e2')) = verExpr tab (MS(s,e2))
        case (t1, t2) of
            (TInt, TInt) -> MS(s1++s2,(TInt, e1' :+: e2'))
            (TDouble, TInt) -> MS(s1++s2,(TDouble, e1' :+: IntDouble e2'))
            (TInt, TDouble) -> MS(s1++s2,(TDouble, IntDouble e1' :+: e2'))
            (TString, _) -> erro(s1++s2++"Tipo String nao compativel com operacao")(TString, e1' :+: e2')
            (_, TString) -> erro(s1++s2++"Tipo String nao compativel com operacao")(TString, e1' :+: e2')
            

verExpr tab (MS(s,((e1 :-: e2)))) =
    do
        let MS(s1,(t1,e1')) = verExpr tab (MS(s,e1))
            MS(s2,(t2,e2')) = verExpr tab (MS(s,e2))
        case (t1, t2) of
            (TInt, TInt) -> MS(s1++s2,(TInt, e1' :-: e2'))
            (TDouble, TInt) -> MS(s1++s2,(TDouble, e1' :-: IntDouble e2'))
            (TInt, TDouble) -> MS(s1++s2,(TDouble, IntDouble e1' :-: e2'))
            (TString, _) -> erro(s1++s2++"Tipo String nao compativel com operacao")(TString, e1' :-: e2') 
            (_, TString) -> erro(s1++s2++"Tipo String nao compativel com operacao")(TString, e1' :-: e2')

verExpr tab (MS(s,((e1 :*: e2)))) =
    do
        let MS(s1,(t1,e1')) = verExpr tab (MS(s,e1))
            MS(s2,(t2,e2')) = verExpr tab (MS(s,e2))
        case (t1, t2) of
            (TInt, TInt) -> MS(s1++s2,(TInt, e1' :*: e2'))
            (TDouble, TInt) -> MS(s1++s2,(TDouble, e1' :*: IntDouble e2'))
            (TInt, TDouble) -> MS(s1++s2,(TDouble, IntDouble e1' :*: e2'))
            (TString, _) -> erro(s1++s2++"Tipo String nao compativel com operacao")(TString, e1' :*: e2') 
            (_, TString) -> erro(s1++s2++"Tipo String nao compativel com operacao")(TString, e1' :*: e2')

verExpr tab (MS(s,((e1 :/: e2)))) =
    do
        let MS(s1,(t1,e1')) = verExpr tab (MS(s,e1))
            MS(s2,(t2,e2')) = verExpr tab (MS(s,e2))
        case (t1, t2) of
            (TInt, TInt) -> MS(s1++s2,(TInt, e1' :/: e2'))
            (TDouble, TInt) -> MS(s1++s2,(TDouble, e1' :/: IntDouble e2'))
            (TInt, TDouble) -> MS(s1++s2,(TDouble, IntDouble e1' :/: e2'))
            (TString, _) -> erro(s1++s2++"Tipo String nao compativel com operacao")(TString, e1' :/: e2') 
            (_, TString) -> erro(s1++s2++"Tipo String nao compativel com operacao")(TString, e1' :/: e2')