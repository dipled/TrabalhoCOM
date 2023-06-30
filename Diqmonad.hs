module Diqmonad where
data ErrorM a = MS (String, a)
    deriving Show

instance Functor ErrorM where
    fmap f (MS (s,a)) = MS(s,f a)

instance Applicative ErrorM where
    pure x = MS ("",x)
    MS(s1,f) <*> MS (s2,x) = MS(s1<>s2,f x) -- <> is a Semigroup operator. In the case of Strings, <> acts as ++ 

instance Monad ErrorM where
    MS(s,a) >>= f = let MS (s',b) = f a in MS (s++s',b)

erro s a= MS ("Erro:"++s,a)
adv s a = MS("Adv:"++s,a)
printMS (MS(s,a)) = putStrLn s >> print a
