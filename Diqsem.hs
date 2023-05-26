import Diqparser
import Diqcode
import Diqtypes
import Diqxpressions
import Diqmonad

verFunType [] (Chamada fid args) = erro("Funcao nao encontrada\n")(TVoid,(Chamada fid args))
verFunType ((fid' :->: (vars, ft)):fs) (Chamada fid args) =
    if fid' == fid then MS("",(ft,Chamada fid args))
    else verFunType fs (Chamada fid args)


verExpr tab (Const(CInt i)) = MS("",(TInt,(Const (CInt i))))
verExpr tab (Const (CDouble d)) = MS("",(TDouble,(Const (CDouble d))))

verExpr tab ((Lit v)) = MS("",(TString,(Lit v)))

verExpr tab ((e1 :+: e2)) =
    do
        let MS(s1,(t1,e1')) = verExpr tab e1
            MS(s2,(t2,e2')) = verExpr tab e2
        case (t1, t2) of
            (TInt, TInt) -> MS(s1++s2,(TInt, e1' :+: e2'))
            (TDouble, TInt) -> MS(s1++s2,(TDouble, e1' :+: IntDouble e2'))
            (TInt, TDouble) -> MS(s1++s2,(TDouble, IntDouble e1' :+: e2'))
            (TDouble, TDouble) -> MS(s1++s2,(TDouble, e1' :+: e2'))
            (TString, _) -> erro("Tipo String nao compativel com operacao\n"++s1++s2)(TString, e1' :+: e2')
            (_, TString) -> erro("Tipo String nao compativel com operacao\n"++s1++s2)(TString, e1' :+: e2')
            (TVoid, _) -> erro("Tipo Void nao compativel com operacao\n"++s1++s2)(TVoid, e1' :+: e2')
            (_, TVoid) -> erro("Tipo Void nao compativel com operacao\n"++s1++s2)(TVoid, e1' :+: e2')
verExpr tab ((e1 :-: e2)) =
    do
        let MS(s1,(t1,e1')) = verExpr tab e1
            MS(s2,(t2,e2')) = verExpr tab e2
        case (t1, t2) of
            (TInt, TInt) -> MS(s1++s2,(TInt, e1' :-: e2'))
            (TDouble, TInt) -> MS(s1++s2,(TDouble, e1' :-: IntDouble e2'))
            (TInt, TDouble) -> MS(s1++s2,(TDouble, IntDouble e1' :-: e2'))
            (TDouble, TDouble) -> MS(s1++s2,(TDouble, e1' :-: e2'))
            (TString, _) -> erro("Tipo String nao compativel com operacao\n"++s1++s2)(TString, e1' :-: e2')
            (_, TString) -> erro("Tipo String nao compativel com operacao\n"++s1++s2)(TString, e1' :-: e2')
            (TVoid, _) -> erro("Tipo Void nao compativel com operacao\n"++s1++s2)(TVoid, e1' :-: e2')
            (_, TVoid) -> erro("Tipo Void nao compativel com operacao\n"++s1++s2)(TVoid, e1' :-: e2')

verExpr tab ((e1 :/: e2)) =
    do
        let MS(s1,(t1,e1')) = verExpr tab e1
            MS(s2,(t2,e2')) = verExpr tab e2
        case (t1, t2) of
            (TInt, TInt) -> MS(s1++s2,(TInt, e1' :/: e2'))
            (TDouble, TInt) -> MS(s1++s2,(TDouble, e1' :/: IntDouble e2'))
            (TInt, TDouble) -> MS(s1++s2,(TDouble, IntDouble e1' :/: e2'))
            (TDouble, TDouble) -> MS(s1++s2,(TDouble, e1' :/: e2'))
            (TString, _) -> erro("Tipo String nao compativel com operacao\n"++s1++s2)(TString, e1' :/: e2')
            (_, TString) -> erro("Tipo String nao compativel com operacao\n"++s1++s2)(TString, e1' :/: e2')
            (TVoid, _) -> erro("Tipo Void nao compativel com operacao\n"++s1++s2)(TVoid, e1' :/: e2')
            (_, TVoid) -> erro("Tipo Void nao compativel com operacao\n"++s1++s2)(TVoid, e1' :/: e2')

verExpr tab ((e1 :*: e2)) =
    do
        let MS(s1,(t1,e1')) = verExpr tab e1
            MS(s2,(t2,e2')) = verExpr tab e2
        case (t1, t2) of
            (TInt, TInt) -> MS(s1++s2,(TInt, e1' :*: e2'))
            (TDouble, TInt) -> MS(s1++s2,(TDouble, e1' :*: IntDouble e2'))
            (TInt, TDouble) -> MS(s1++s2,(TDouble, IntDouble e1' :*: e2'))
            (TDouble, TDouble) -> MS(s1++s2,(TDouble, e1' :*: e2'))
            (TString, _) -> erro("Tipo String nao compativel com operacao\n"++s1++s2)(TString, e1' :*: e2')
            (_, TString) -> erro("Tipo String nao compativel com operacao\n"++s1++s2)(TString, e1' :*: e2')
            (TVoid, _) -> erro("Tipo Void nao compativel com operacao\n"++s1++s2)(TVoid, e1' :*: e2')
            (_, TVoid) -> erro("Tipo Void nao compativel com operacao\n"++s1++s2)(TVoid, e1' :*: e2')

verExpr (fns,[]) (IdVar v) = erro ("Variavel " ++ v ++ " nao encontrada\n") (TVoid,IdVar v)
verExpr (fns, ((id:#:t):vs)) ((IdVar v))
    |v == id = MS("",(t, (IdVar v)))
    |otherwise = verExpr (fns,vs) (IdVar v)


verExpr tab ((Neg e)) = 
    do
        let MS(s1,(t,e')) = verExpr tab e
        case t of
            (TInt) -> MS(s1,(t,Neg e))
            (TDouble) -> MS(s1,(t,Neg e))
            _ -> erro("Erro de tipo na tentativa de Negar a expressao. Tipo invalido\n"++s1) (t,Neg e)


verCmd tab (Atrib id e) = 
    do 
        let MS(s1, (t1,e1)) = verExpr tab (IdVar id)
            MS(s2, (t2,e2)) = verExpr tab e
        case (t1,t2) of 
            (TInt, TDouble) -> adv("Atribuicao de Double para Int \n"++s1++s2)(Atrib id (DoubleInt e2))
            (TDouble, TInt) -> adv("Atribuicao de Int para Double \n"++s1++s2)(Atrib id (IntDouble e2))
            (TString,TString) -> (MS(s1++s2,(Atrib id e2)))
            (TString, _) -> erro("Atribuicao invalida\n"++s1++s2)(Atrib id e2)
            (_,TString) -> erro("Atribuicao invalida\n"++s1++s2)(Atrib id e2)
            (TVoid,_ ) -> erro("Atribuicao invalida\n"++s1++s2)(Atrib id e2)
            (_,TVoid) -> erro("Atribuicao invalida\n"++s1++s2)(Atrib id e2)
            (TDouble, TDouble) -> (MS(s1++s2,(Atrib id e2)))
            (TInt,TInt) -> (MS(s1++s2,(Atrib id e2)))

