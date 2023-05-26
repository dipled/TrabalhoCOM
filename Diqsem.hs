import Diqparser
import Diqcode
import Diqtypes
import Diqxpressions
import Diqmonad

makeArg arg = MS("",arg)
verArgs [] [] = ""
verArgs _ [] = "Erro: Numero de argumentos errados\n"
verArgs ((id:#:t):xs) ((MS(s,(tp,e))):es) =
    do
        if tp == t then s++verArgs xs es
        else s++"Erro: Tipos nao correspondentes na chamada da funcao\n" ++ verArgs xs es
verFunType ([],_) (MS(s,(Chamada fid args))) = erro (s++"Funcao " ++ fid ++  " nao encontrada\n") (TVoid,(Chamada fid args)) 
verFunType (((fi:->:(as,ft)):ys),vars) (MS(s,(Chamada fid args))) =
    if fi == fid then (MS(s,(ft,Chamada fid args)))
    else verExpr (ys,vars) (MS(s,(Chamada fid args)))

verExpr tab (MS(s,(Const(CInt i)))) = MS(s,(TInt,(Const (CInt i))))
verExpr tab (MS(s,(Const (CDouble d)))) = MS(s,(TDouble,(Const (CDouble d))))

verExpr tab (MS(s,(Lit v))) = MS(s,(TString,(Lit v)))

verExpr (fns,[]) (MS(s,(IdVar v))) = erro ("Variavel " ++ v ++ " nao encontrada\n"++s) (TVoid,IdVar v)
verExpr (fns, ((id:#:t):vs)) (MS(s,(IdVar v)))
    |v == id = MS(s,(t, (IdVar v)))
    |otherwise = verExpr (fns,vs) (MS(s,(IdVar v)))



verExpr ([],_) (MS(s,(Chamada fid args))) = erro (s++"Funcao " ++ fid ++  " nao encontrada\n") (TVoid,(Chamada fid args)) 
verExpr (((fi:->:(as,ft)):ys),vars) (MS(s,(Chamada fid args))) =
    do
        let (MS(s1,(t,Chamada fid1 args1))) = verFunType (((fi:->:(as,ft)):ys),vars) (MS(s,(Chamada fid args)))
        let arguments = map (makeArg) args
        let actualArgs = map (verExpr  (((fi:->:(as,ft)):ys),vars)) arguments
        let sr = verArgs as actualArgs
        (MS(s++sr,(ft,Chamada fid args)))

                    

verExpr tab (MS(s,(Neg e))) = 
    do
        let MS(s1,(t,e')) = verExpr tab (MS("",e))
        case t of
            (TInt) -> MS(s1,(t,Neg e))
            (TDouble) -> MS(s1,(t,Neg e))
            _ -> erro("Erro de tipo na tentativa de Negar a expressao\n"++s) (t,Neg e)

verExpr tab (MS(s,((e1 :+: e2)))) =
    do
        let MS(s1,(t1,e1')) = verExpr tab (MS("",e1))
            MS(s2,(t2,e2')) = verExpr tab (MS("",e2))
        case (t1, t2) of
            (TInt, TInt) -> MS(s++s1++s2,(TInt, e1' :+: e2'))
            (TDouble, TInt) -> MS(s++s1++s2,(TDouble, e1' :+: IntDouble e2'))
            (TInt, TDouble) -> MS(s++s1++s2,(TDouble, IntDouble e1' :+: e2'))
            (TDouble, TDouble) -> MS(s++s1++s2,(TDouble, e1' :+: e2'))
            (TString, _) -> erro("Tipo String nao compativel com operacao\n"++s++s1++s2)(TString, e1' :+: e2')
            (_, TString) -> erro("Tipo String nao compativel com operacao\n"++s++s1++s2)(TString, e1' :+: e2')
            (TVoid, _) -> erro("Tipo Void nao compativel com operacao\n"++s++s1++s2)(TVoid, e1' :+: e2')
            (_, TVoid) -> erro("Tipo Void nao compativel com operacao\n"++s++s1++s2)(TVoid, e1' :+: e2')
            

verExpr tab (MS(s,((e1 :-: e2)))) =
    do
        let MS(s1,(t1,e1')) = verExpr tab (MS("",e1))
            MS(s2,(t2,e2')) = verExpr tab (MS("",e2))
        case (t1, t2) of
            (TInt, TInt) -> MS(s++s1++s2,(TInt, e1' :-: e2'))
            (TDouble, TInt) -> MS(s++s1++s2,(TDouble, e1' :-: IntDouble e2'))
            (TInt, TDouble) -> MS(s++s1++s2,(TDouble, IntDouble e1' :-: e2'))
            (TDouble, TDouble) -> MS(s++s1++s2,(TDouble, e1' :-: e2'))
            (TString, _) -> erro("Tipo String nao compativel com operacao\n"++s++s1++s2)(TString, e1' :-: e2') 
            (_, TString) -> erro("Tipo String nao compativel com operacao\n"++s++s1++s2)(TString, e1' :-: e2')
            (TVoid, _) -> erro("Tipo Void nao compativel com operacao\n"++s++s1++s2)(TVoid, e1' :-: e2')
            (_, TVoid) -> erro("Tipo Void nao compativel com operacao\n"++s++s1++s2)(TVoid, e1' :-: e2')

verExpr tab (MS(s,((e1 :*: e2)))) =
    do
        let MS(s1,(t1,e1')) = verExpr tab (MS("",e1))
            MS(s2,(t2,e2')) = verExpr tab (MS("",e2))
        case (t1, t2) of
            (TInt, TInt) -> MS(s++s1++s2,(TInt, e1' :*: e2'))
            (TDouble, TInt) -> MS(s++s1++s2,(TDouble, e1' :*: IntDouble e2'))
            (TInt, TDouble) -> MS(s++s1++s2,(TDouble, IntDouble e1' :*: e2'))
            (TDouble, TDouble) -> MS(s++s1++s2,(TDouble, e1' :*: e2'))
            (TString, _) -> erro(s++s1++s2++"Tipo String nao compativel com operacao\n")(TString, e1' :*: e2') 
            (_, TString) -> erro(s++s1++s2++"Tipo String nao compativel com operacao\n")(TString, e1' :*: e2')
            (TVoid, _) -> erro(s++s1++s2++"Tipo Void nao compativel com operacao\n")(TVoid, e1' :*: e2')
            (_, TVoid) -> erro(s++s1++s2++"Tipo Void nao compativel com operacao\n")(TVoid, e1' :*: e2')

verExpr tab (MS(s,((e1 :/: e2)))) =
    do
        let MS(s1,(t1,e1')) = verExpr tab (MS("",e1))
            MS(s2,(t2,e2')) = verExpr tab (MS("",e2))
        case (t1, t2) of
            (TInt, TInt) -> MS(s++s1++s2,(TInt, e1' :/: e2'))
            (TDouble, TInt) -> MS(s++s1++s2,(TDouble, e1' :/: IntDouble e2'))
            (TInt, TDouble) -> MS(s++s1++s2,(TDouble, IntDouble e1' :/: e2'))
            (TDouble, TDouble) -> MS(s++s1++s2,(TDouble, e1' :/: e2'))
            (TString, _) -> erro(s++s1++s2++"Tipo String nao compativel com operacao\n")(TString, e1' :/: e2') 
            (_, TString) -> erro(s++s1++s2++"Tipo String nao compativel com operacao\n")(TString, e1' :/: e2')
            (TVoid, _) -> erro(s++s1++s2++"Tipo Void nao compativel com operacao\n")(TVoid, e1' :/: e2')
            (_, TVoid) -> erro(s++s1++s2++"Tipo Void nao compativel com operacao\n")(TVoid, e1' :/: e2')


verCmd tab (MS(s, (Atrib id e))) = 
    do 
        let MS(s1, (t1,e1)) = verExpr tab (MS("", IdVar id))
            MS(s2, (t2,e2)) = verExpr tab (MS("", e))
        case (t1,t2) of 
            (TInt, TDouble) -> adv("Atribuicao de Double para Int \n"++s++s1++s2)(Atrib id (DoubleInt e2))
            (TDouble, TInt) -> adv("Atribuicao de Int para Double \n"++s++s1++s2)(Atrib id (IntDouble e2))
            (TString,TString) -> (MS(s++s1++s2,(Atrib id e2)))
            (TString, _) -> erro("Atribuicao invalida\n"++s++s1++s2)(Atrib id e2)
            (_,TString) -> erro("Atribuicao invalida\n"++s++s1++s2)(Atrib id e2)
            (TVoid,_ ) -> erro("Atribuicao invalida\n"++s++s1++s2)(Atrib id e2)
            (_,TVoid) -> erro("Atribuicao invalida\n"++s++s1++s2)(Atrib id e2)
            (TDouble, TDouble) -> (MS(s++s1++s2,(Atrib id e2)))
            (TInt,TInt) -> (MS(s++s1++s2,(Atrib id e2)))
