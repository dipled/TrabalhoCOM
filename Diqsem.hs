import Diqparser
import Diqcode
import Diqtypes
import Diqxpressions
import Diqmonad


verFunTypeAndGetArgs [] fid = erro("Funcao nao encontrada "++fid++"\n")(TVoid, (fid,[]))
verFunTypeAndGetArgs ((fid' :->: (vars, ft)):fs) fid  =
    if fid' == fid then pure ((ft,(fid,vars)))
    else verFunTypeAndGetArgs fs fid

verExprLOp (funs,(fid,vars)) o e1 e2 =
    do
        (t1,e1') <- verExpr (funs,(fid,vars)) e1
        (t2,e2') <- verExpr (funs,(fid,vars)) e2
        case (t1,t2) of
            (TInt, TInt) -> pure(Rel (o e1' e2'))
            (TDouble, TInt) -> pure (Rel (o e1' (IntDouble e2')))
            (TInt, TDouble) -> pure (Rel (o (IntDouble e1') e2'))
            (TDouble, TDouble) -> pure  (Rel (o e1' e2'))
            (TString,TString) -> pure (Rel (o e1' e2'))
            (TString, _) -> erro("Tipo String nao compativel com operacao na funcao "++fid++"\n")(Rel (o e1' e2'))
            (_, TString) -> erro("Tipo String nao compativel com operacao na funcao "++fid++"\n")(Rel (o e1' e2'))
            (TVoid, _) -> erro("Tipo Void nao compativel com operacao na funcao "++fid++"\n")(Rel (o e1' e2'))
            (_, TVoid) -> erro("Tipo Void nao compativel com operacao na funcao "++fid++"\n")(Rel (o e1' e2'))

verExprOp (funs,(fid,vars))  o e1 e2 = 
    do
        (t1, e1') <- verExpr (funs,(fid,vars)) e1
        (t2,e2') <- verExpr (funs,(fid,vars)) e2
        case (t1, t2) of
            (TInt, TInt) -> pure (TInt, o e1' e2')
            (TDouble, TInt) -> pure (TDouble, o e1' (IntDouble e2'))
            (TInt, TDouble) -> pure (TDouble, o (IntDouble e1') e2')
            (TDouble, TDouble) -> pure (TDouble, o e1' e2')
            (TString, _) -> erro("Tipo String nao compativel com operacao na funcao "++fid++"\n")(TString,  o e1' e2')
            (_, TString) -> erro("Tipo String nao compativel com operacao na funcao "++fid++"\n")(TString, o e1' e2')
            (TVoid, _) -> erro("Tipo Void nao compativel com operacao na funcao "++fid++"\n")(TVoid, o e1' e2')
            (_, TVoid) -> erro("Tipo Void nao compativel com operacao na funcao "++fid++"\n")(TVoid, o e1' e2')

        
verArg (fid,[]) [] = pure []
verArg (fid,_) [] = erro("Quantidade insuficiente de argumentos na chamada da funcao "++fid++"\n")[]
verArg (fid,[]) _ = erro("Quantidade excessiva de argumentos na chamada da funcao "++fid++"\n")[]
verArg (fid,(id:#:tp):vs) ((t,e):as) = 
    do
        case (tp, t) of
            (TInt, TDouble) -> (:) <$> (adv("Atribuicao de Double para Int na chamada da funcao "++fid++"\n") (DoubleInt e)) <*> (verArg (fid,vs) as)
            (TDouble, TInt) ->(:) <$> (adv("Atribuicao de Int para Double na chamada da funcao "++fid++"\n")(IntDouble e)) <*> verArg (fid,vs) as
            (TString,TString) ->(:) <$> (pure e) <*> verArg (fid,vs) as
            (TString, _) ->(:) <$> (erro("Atribuicao invalida na chamada da funcao "++fid++"\n")e) <*> verArg (fid,vs) as
            (_,TString) ->(:) <$> (erro("Atribuicao invalida na chamada da funcao "++fid++"\n") e) <*> verArg (fid,vs) as
            (TVoid,_ ) ->(:) <$> (erro("Atribuicao invalida na chamada da funcao "++fid++"\n") e) <*> verArg (fid,vs) as
            (_,TVoid) ->(:) <$> (erro("Atribuicao invalida na chamada da funcao "++fid++"\n")e) <*> verArg (fid,vs) as
            (TDouble, TDouble) ->(:) <$> (pure e) <*> verArg (fid,vs) as
            (TInt,TInt) ->(:) <$> (pure e) <*> verArg (fid,vs) as

verExpr (funs, (fi,vars)) (Chamada id args) =
    do
        argos <- mapM (verExpr (funs, (fi,vars))) args
        (t,(fid,expectedArgs)) <- verFunTypeAndGetArgs funs id
        argosVerified <- verArg (fid,expectedArgs) argos
        pure(t,Chamada id argosVerified)

verExpr tab (Const(CInt i)) = pure(TInt,(Const (CInt i)))
verExpr tab (Const (CDouble d)) = pure(TDouble,(Const (CDouble d)))
verExpr tab ((Lit v)) = pure (TString,(Lit v))

verExpr tab (e1 :+: e2) = verExprOp tab (:+:) e1 e2
verExpr tab (e1 :-: e2) = verExprOp tab (:-:) e1 e2
verExpr tab (e1 :*: e2) = verExprOp tab (:*:) e1 e2
verExpr tab (e1 :/: e2) = verExprOp tab (:/:) e1 e2

verExpr (fns,(fid,[])) (IdVar v) = erro ("Variavel " ++ v ++ " nao encontrada na funcao "++fid++"\n") (TVoid,IdVar v)
verExpr (fns, (fid,(id:#:t):vs)) ((IdVar v))
    |v == id = pure (t, (IdVar v))
    |otherwise = verExpr (fns,(fid,vs)) (IdVar v)


verExpr (funs,(fid,vars)) ((Neg e)) = 
    do
        (t,e') <- verExpr (funs,(fid,vars)) e
        case t of
            (TInt) ->pure ((t,Neg e))
            (TDouble) -> pure ((t,Neg e))
            _ -> erro("Erro de tipo na tentativa de Negar a expressao na funcao "++fid++". Tipo invalido\n") (t,Neg e)

verCmd (funs, (fi,vars)) (Proc id args) =
    do
        argos <- mapM (verExpr (funs, (fi,vars))) args
        (t,(fid,expectedArgs)) <- verFunTypeAndGetArgs funs id
        argosVerified <- verArg (fid,expectedArgs) argos
        pure(Proc id argosVerified)

verCmd (funs,(fid,vars)) (Atrib id e) = 
    do 
        (t1,e1) <- verExpr (funs,(fid,vars)) (IdVar id)
        (t2,e2) <- verExpr (funs,(fid,vars)) e
        case (t1,t2) of 
            (TInt, TDouble) -> adv("Atribuicao de Double para Int na variavel "++id++" na funcao " ++ fid ++ "\n")(Atrib id (DoubleInt e2))
            (TDouble, TInt) -> adv("Atribuicao de Int para Double na variavel "++id++" na funcao " ++ fid ++ "\n")(Atrib id (IntDouble e2))
            (TString,TString) -> pure (Atrib id e2)
            (TString, _) -> erro("Atribuicao invalida na variavel "++id++" na funcao " ++ fid ++ "\n")(Atrib id e2)
            (_,TString) -> erro("Atribuicao invalida na variavel "++id++" na funcao " ++ fid ++ "\n")(Atrib id e2)
            (TVoid,_ ) -> erro("Atribuicao invalida na variavel "++id++" na funcao " ++ fid ++ "\n")(Atrib id e2)
            (_,TVoid) -> erro("Atribuicao invalida na variavel "++id++" na funcao " ++ fid ++ "\n")(Atrib id e2)
            (TDouble, TDouble) -> pure (Atrib id e2)
            (TInt,TInt) -> pure (Atrib id e2)
verCmd (funs,(fid,vars)) (Ret e) = 
    
    case e of
        Nothing -> do
                    (t,(fid,expectedArgs)) <- verFunTypeAndGetArgs funs fid
                    if t == TVoid then pure(Ret Nothing)
                    else
                        erro("Tipo Void incompativel com retorno da funcao "++fid++"\n")(Ret Nothing)
        Just ex -> do    
                    (t,(fid,expectedArgs)) <- verFunTypeAndGetArgs funs fid
                    (et,exp) <- verExpr (funs,(fid,vars)) ex
                    case (t, et) of 
                            (TInt, TInt) -> pure (Ret (Just exp))
                            (TDouble, TDouble) ->  pure (Ret (Just exp))
                            (TString, TString) ->  pure (Ret (Just exp))
                            (TVoid, _) -> erro("Tipo incompativel com retorno de funcao "++fid++"\n")(Ret (Just exp))
                            (_,TVoid) -> erro("Tipo incompativel com retorno de funcao "++fid++"\n")(Ret (Just exp))
                            (_, TString) -> erro("Tipo incompativel com retorno de funcao "++fid++"\n")(Ret (Just exp))
                            (TString,_) -> erro("Tipo incompativel com retorno de funcao "++fid++"\n")(Ret (Just exp))
                            (TInt, TDouble) -> adv("Tipo Double no retorno esperado Int de funcao "++fid++"\n")(Ret (Just (DoubleInt exp)))
                            (TDouble, TInt) -> adv("Tipo Int no retorno esperado Double de funcao "++fid++"\n")(Ret (Just (IntDouble exp)))
verCmd (funs,(fid,vars)) (Imp e) = 
    do 
        (t,er) <- verExpr (funs,(fid,vars)) e
        pure (Imp(er))

verCmd tab (Leitura id) = 
    do
        (t,d) <- verExpr tab (IdVar id)
        case d of
            IdVar w -> pure (Leitura w)
        

verCmd tab (While el blk) = 
    do
        el' <- verExprL tab el 
        retBlk <- mapM (verCmd tab) blk
        pure ((While el' retBlk))

verCmd tab (If el blk blk2) = 
    do
        el' <- verExprL tab el 
        retBlk <- mapM (verCmd tab) blk
        retBlk2 <- mapM (verCmd tab) blk2
        pure (If el' retBlk retBlk2)
verCmd tab (DoWhile blk el) = 
    do
        el' <- verExprL tab el
        retBlk <- mapM (verCmd tab) blk
        pure ((DoWhile retBlk el'))

verCmd tab (For a1 el a2 blk) = 
    do
        a1' <- verCmd tab a1
        a2' <- verCmd tab a2
        el' <- verExprL tab el
        retBlk <- mapM (verCmd tab) blk
        pure ((For a1' el' a2' retBlk))

verExprL tab (Rel (e1 :==: e2)) = verExprLOp tab (:==:) e1 e2
verExprL tab (Rel (e1 :/=: e2)) = verExprLOp tab (:/=:) e1 e2
verExprL tab (Rel (e1 :>=: e2)) = verExprLOp tab (:>=:) e1 e2
verExprL tab (Rel (e1 :<=: e2)) = verExprLOp tab (:<=:) e1 e2
verExprL tab (Rel (e1 :>: e2)) = verExprLOp tab (:>:) e1 e2
verExprL tab (Rel (e1 :<: e2)) = verExprLOp tab (:<:) e1 e2

verExprL (funs,(fid,vars)) (e1 :&: e2) =
    do
        e1' <- verExprL (funs,(fid,vars)) e1
        e2' <- verExprL (funs,(fid,vars)) e2
        pure (e1' :&: e2')

verExprL (funs,(fid,vars)) (e1 :|: e2) =
    do
        e1' <- verExprL (funs,(fid,vars)) e1
        e2' <- verExprL (funs,(fid,vars)) e2
        pure (e1' :|: e2')

verExprL (funs,(fid,vars)) (Not e1) =
    do
        e1' <- verExprL (funs,(fid,vars)) e1
        pure (Not e1')


verBlk fns (fi, vrs, blk) = 
    do
        verDuplicateVars fi vrs (tail vrs)
        cmds <-  mapM (verCmd (fns,(fi,vrs))) blk
        pure(fi,vrs,cmds)

verDuplicateVar fid id [] = pure()
verDuplicateVar fid id ((id':#:t'):vs') = 
    if id == id' then erro("variavel "++id++" duplicada na funcao "++fid++"\n")() >> verDuplicateVar fid id vs'
    else verDuplicateVar fid id vs'

verDuplicateVars fid [] _ = pure()
verDuplicateVars fid ((id:#:t):vs) vs' = (verDuplicateVar fid id vs') >> (verDuplicateVars fid vs (tail vs'))

verDuplicateFun fid [] = pure()
verDuplicateFun fid ((id:->:r):fs) = 
    if fid == id then erro("Funcao "++fid++" duplicada\n")()
    else verDuplicateFun fid fs

verDuplicateFuns [] _ = pure()
verDuplicateFuns ((fid:->:s):fz) fs = (verDuplicateFun fid fs) >> (verDuplicateFuns fz (tail fs))

verProg (Prog fns ls mainVar mainBlk) = 
    do
        verDuplicateFuns fns (tail fns)
        fx <- mapM (verBlk fns) ls
        (fi,vrs,cmds) <- verBlk fns ("main",mainVar,mainBlk)
        pure (Prog fns fx mainVar cmds)

main =
  do
    e <- readFile "teste1.j--"
    let syntaxTree = parserE e
    case syntaxTree of 
        Left v -> print v
        Right x -> do 
                    printMS(verProg x)