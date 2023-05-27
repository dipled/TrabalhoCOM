import Diqparser
import Diqcode
import Diqtypes
import Diqxpressions
import Diqmonad

verFunType [] (Chamada fid args) = erro("Funcao nao encontrada\n")(TVoid,(Chamada fid args))
verFunType ((fid' :->: (vars, ft)):fs) (Chamada fid args) =
    if fid' == fid then MS("",(ft,Chamada fid args))
    else verFunType fs (Chamada fid args)
verArg [] [] = ([],[])
verArg _ [] = ("Quantidade insuficiente de argumentos",[])
verArg [] _ = ("Quantidade excessiva de argumentos",[])
verArg ((id:#:tp):vs) ((MS (s,(t,e))):as) = 
    
    do
        case (tp, t) of
            (TInt, TDouble) -> ("Atribuicao de Double para Int \n"++s ++ fst (verArg vs as), DoubleInt e: snd (verArg vs as))
            (TDouble, TInt) -> ("Atribuicao de Int para Double \n"++s ++ fst (verArg vs as),IntDouble e: snd (verArg vs as))
            (TString,TString) -> (""++s++fst (verArg vs as),e:snd (verArg vs as))
            (TString, _) -> ("Atribuicao invalida\n"++s++ fst (verArg vs as), e: snd (verArg vs as))
            (_,TString) -> ("Atribuicao invalida\n"++s++ fst (verArg vs as), e: snd (verArg vs as))
            (TVoid,_ ) -> ("Atribuicao invalida\n"++s++ fst (verArg vs as), e: snd (verArg vs as))
            (_,TVoid) -> ("Atribuicao invalida\n"++ s++fst (verArg vs as), e: snd (verArg vs as))
            (TDouble, TDouble) -> (""++s++ fst (verArg vs as),e:snd (verArg vs as))
            (TInt,TInt) -> (""++s++ fst (verArg vs as),e:snd (verArg vs as))

verExpr ([],[])  (Chamada id args) = erro("Funcao nao encontrada")(TVoid,Chamada id args)
verExpr (((fid:->:(idealArgs, ft)):fs), vars) (Chamada id args) =
    if(id /= fid) then verExpr (fs,vars) (Chamada id args)
    else
    do
        let argos = map (verExpr (((fid:->:(idealArgs, ft)):fs), vars)) args
            MS(s,(t,e)) = verFunType ((fid:->:(idealArgs, ft)):fs) (Chamada id args)
            argosVerified = verArg idealArgs argos
        MS((fst argosVerified),(t,Chamada id (snd argosVerified)))
            

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

