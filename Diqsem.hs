import Diqparser
import Diqcode
import Diqtypes
import Diqxpressions
import Diqmonad


verFunTypeAndGetArgs [] fid = ("Erro: Funcao nao encontrada "++fid++"\n",TVoid, fid,[])
verFunTypeAndGetArgs ((fid' :->: (vars, ft)):fs) fid  =
    if fid' == fid then ("",ft,fid,vars)
    else verFunTypeAndGetArgs fs fid
verArg (fid,[]) [] = ([],[])
verArg (fid,_) [] = ("Erro:Quantidade insuficiente de argumentos na chamada da funcao "++fid++"\n",[])
verArg (fid,[]) _ = ("Erro:Quantidade excessiva de argumentos na chamada da funcao "++fid++"\n",[])
verArg (fid,(id:#:tp):vs) ((MS (s,(t,e))):as) = 
    do
        case (tp, t) of
            (TInt, TDouble) -> ("Adv:Atribuicao de Double para Int na chamada da funcao "++fid++"\n"++s ++ fst (verArg (fid,vs) as), DoubleInt e: snd (verArg (fid,vs) as))
            (TDouble, TInt) -> ("Adv:Atribuicao de Int para Double na chamada da funcao "++fid++"\n"++s ++ fst (verArg (fid,vs) as),IntDouble e: snd (verArg (fid,vs) as))
            (TString,TString) -> (""++s++fst (verArg (fid,vs) as),e:snd (verArg (fid,vs) as))
            (TString, _) -> ("Erro:Atribuicao invalida na chamada da funcao "++fid++"\n"++s++ fst (verArg (fid,vs) as), e: snd (verArg (fid,vs) as))
            (_,TString) -> ("Erro:Atribuicao invalida na chamada da funcao "++fid++"\n"++s++ fst (verArg (fid,vs) as), e: snd (verArg (fid,vs) as))
            (TVoid,_ ) -> ("Erro:Atribuicao invalida na chamada da funcao "++fid++"\n"++s++ fst (verArg (fid,vs) as), e: snd (verArg (fid,vs) as))
            (_,TVoid) -> ("Erro:Atribuicao invalida na chamada da funcao "++fid++"\n"++ s++fst (verArg (fid,vs) as), e: snd (verArg (fid,vs) as))
            (TDouble, TDouble) -> (""++s++ fst (verArg (fid,vs) as),e:snd (verArg (fid,vs) as))
            (TInt,TInt) -> (""++s++ fst (verArg (fid,vs) as),e:snd (verArg (fid,vs) as))

verExpr (funs, (fi,vars)) (Chamada id args) =
    do
        let argos = map (verExpr (funs, (fid,vars))) args
            (s,t,fid,expectedArgs) = verFunTypeAndGetArgs funs id
            argosVerified = verArg (fid,expectedArgs) argos
        MS((fst argosVerified)++s,(t,Chamada id (snd argosVerified)))
            

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

verExpr (fns,(fid,[])) (IdVar v) = erro ("Variavel " ++ v ++ " nao encontrada\n") (TVoid,IdVar v)
verExpr (fns, (fid,(id:#:t):vs)) ((IdVar v))
    |v == id = MS("",(t, (IdVar v)))
    |otherwise = verExpr (fns,(fid,vs)) (IdVar v)


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
            (TInt, TDouble) -> adv("Atribuicao de Double para Int na variavel "++id++"\n"++s1++s2)(Atrib id (DoubleInt e2))
            (TDouble, TInt) -> adv("Atribuicao de Int para Double na variavel "++id++"\n"++s1++s2)(Atrib id (IntDouble e2))
            (TString,TString) -> (MS(s1++s2,(Atrib id e2)))
            (TString, _) -> erro("Atribuicao invalida na variavel "++id++"\n"++s1++s2)(Atrib id e2)
            (_,TString) -> erro("Atribuicao invalida na variavel "++id++"\n"++s1++s2)(Atrib id e2)
            (TVoid,_ ) -> erro("Atribuicao invalida na variavel "++id++"\n"++s1++s2)(Atrib id e2)
            (_,TVoid) -> erro("Atribuicao invalida na variavel "++id++"\n"++s1++s2)(Atrib id e2)
            (TDouble, TDouble) -> (MS(s1++s2,(Atrib id e2)))
            (TInt,TInt) -> (MS(s1++s2,(Atrib id e2)))
verCmd (funs,(fid,vars)) (Ret e) = 
    case e of
        Nothing -> MS("",Ret Nothing)
        Just ex -> do    
                        let (s,t,fi,expectedArgs) = verFunTypeAndGetArgs funs fid
                            MS(se,(et,exp)) = verExpr (funs,(fid,vars)) ex
                        case (t, et) of 
                                (TInt, TInt) -> MS(s ++ se, Ret (Just exp))
                                (TDouble, TDouble) -> MS(s ++ se, Ret (Just exp))
                                (TString, TString) -> MS(s ++ se, Ret (Just exp))
                                (TVoid, _) -> erro("Tipo incompativel com retorno de funcao "++fid++"\n"++s++se)(Ret (Just exp))
                                (_,TVoid) -> erro("Tipo incompativel com retorno de funcao "++fid++"\n"++s++se)(Ret (Just exp))
                                (_, TString) -> erro("Tipo incompativel com retorno de funcao "++fid++"\n"++s++se)(Ret (Just exp))
                                (TString,_) -> erro("Tipo incompativel com retorno de funcao "++fid++"\n"++s++se)(Ret (Just exp))
                                (TInt, TDouble) -> adv("Tipo Double no retorno esperado Int de funcao "++fid++"\n"++s++se)(Ret (Just (DoubleInt exp)))
                                (TDouble, TInt) -> adv("Tipo Int no retorno esperado Double de funcao "++fid++"\n"++s++se)(Ret (Just (IntDouble exp)))
                            

