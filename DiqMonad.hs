data ErrorM a = MS (String, a)
    deriving Show

instance Functor ErrorM where
    fmap f (MS (s,a)) = MS(s,f a)

instance Applicative ErrorM where
    pure x = MS ("",x)
    MS(s1,f) <*> MS (s2,x) = MS(s1<>s2,f x)

instance Monad ErrorM where
    return x = MS ("", x)
    MS (s,a) >>= f = let MS (s',b) = f a in MS (s++s',b)

erro s = MS ("Erro:"++s,())
adv s = MS("Adv:"++s,())