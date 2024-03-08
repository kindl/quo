module Platte where

import Data.Data(Data, Typeable, cast, gfoldl)
import Data.Functor.Identity(runIdentity)
import Control.Applicative(Const(Const), getConst)
import Data.Maybe(fromMaybe)


-- Cast has the disadvantage of needing a Typeable f constraint, but is easier to read than GADT syntax
descendBiM :: (Typeable f, Applicative f, Typeable a, Data s) => (a -> f a) -> s -> f s
descendBiM f = fromMaybe (tinplate f) (cast f)

tinplate f = gfoldl (\w s -> w <*> descendBiM f s) pure

-- Narrow down the type
descendM :: (Typeable f, Data a, Applicative f) => (a -> f a) -> a -> f a
descendM = tinplate

descend f x = runIdentity (descendM (return . f) x)

descendBi f x = runIdentity (descendBiM (return . f) x)


transformM f = g
    where g x = f =<< descendM g x

transform f x = runIdentity (transformM (return . f) x)

transformBiM f = descendBiM (transformM f)

transformBi f x = runIdentity (transformBiM (return . f) x)


rewrite f = transform (\x -> maybe x (rewrite f) (f x))

rewriteBi f = descendBi (rewrite f)

rewriteM f = transformM (\x -> maybe (return x) (rewriteM f) =<< f x)

rewriteBiM f = descendBiM (rewriteM f)


universe x = getConst (g x)
    where g y = Const (singleton y) <*> descendM g y

children x = getConst (descendM (Const . singleton) x)

para f x = f x (map (para f) (children x))

universeBi x = getConst (descendBiM g x)
    where g y = Const (singleton y) <*> descendM g y

singleton x = [x]
