module Diqtypes where

type Id = String

data Type = TDouble | TInt | TString | TVoid
    deriving Show

data TCons = CDouble Double | CInt Integer
    deriving Show