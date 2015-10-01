{-|
Module      : Types.Parametric
Description : Parameterize a functor with 0 or more arguments.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Types.Parametric where

import Data.Proxy

type family ParametricType (f :: [*]) (y :: *) :: * where
    ParametricType (t ': ts) y = t -> ParametricType ts y
    ParametricType '[] y = y

data Parametric (fs :: [*]) (m :: * -> *) (t :: *) where
    Base :: m t -> Parametric fs m t
    Lift :: (f -> (Parametric fs m t)) -> Parametric (f ': fs) m t

instance Functor m => Functor (Parametric fs m) where
    fmap f term = case term of
        Base g -> Base (fmap f g)
        Lift g -> Lift ((fmap . fmap) f g)

instance Applicative m => Applicative (Parametric fs m) where
    pure = Base . pure
    f <*> x = case (f, x) of
        (Base f', Base x') -> Base (f' <*> x')
        (Base f', Lift x') -> Lift (\x -> Base f' <*> x' x)
        (Lift f', Base x') -> Lift (\x -> f' x <*> Base x')
        (Lift f', Lift x') -> Lift (\x -> f' x <*> x' x)

class RunParametric fs m t where
    runParametric :: Proxy fs -> Parametric fs m t -> ParametricType fs (m t)

instance RunParametric '[] m t where
    runParametric _ term = case term of
        Base x -> x

instance RunParametric fs m t => RunParametric (f ': fs) m t where
    runParametric _ term = case term of
        Base x -> const (runParametric (Proxy :: Proxy fs) (Base x))
        Lift g -> runParametric (Proxy :: Proxy fs) . g

ex1 :: Parametric '[Int, Bool] IO ()
ex1 = Lift (\i -> Lift (\j -> Base (print i >> print j)))

type family BundleParameters (fs :: [*]) :: * where
    BundleParameters '[] = ()
    BundleParameters '[f1] = f1
    BundleParameters '[f1, f2] = (f1, f2)
    BundleParameters '[f1, f2, f3] = (f1, f2, f3)
    BundleParameters '[f1, f2, f3, f4] = (f1, f2, f3, f4)
    BundleParameters '[f1, f2, f3, f4, f5] = (f1, f2, f3, f4, f5)
    BundleParameters '[f1, f2, f3, f4, f5, f6] = (f1, f2, f3, f4, f5, f6)
    BundleParameters '[f1, f2, f3, f4, f5, f6, f7] = (f1, f2, f3, f4, f5, f6, f7)
    BundleParameters '[f1, f2, f3, f4, f5, f6, f7, f8] = (f1, f2, f3, f4, f5, f6, f7, f8)
    BundleParameters '[f1, f2, f3, f4, f5, f6, f7, f8, f9] = (f1, f2, f3, f4, f5, f6, f7, f8, f9)
    BundleParameters '[f1, f2, f3, f4, f5, f6, f7, f8, f9, f10] = (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10)

class RunParametricBundle fs m t where
    runParametricBundle :: Proxy fs -> Parametric fs m t -> BundleParameters fs -> m t

instance RunParametricBundle '[] m t where
    runParametricBundle _ term _ = case term of
        Base x -> x

instance RunParametricBundle '[] m t => RunParametricBundle '[f1] m t where
    runParametricBundle _ term f1 = case term of
        Base x -> x
        Lift f -> runParametricBundle (Proxy :: Proxy '[]) (f f1) ()

instance RunParametricBundle '[f2] m t => RunParametricBundle '[f1, f2] m t where
    runParametricBundle _ term (f1, f2) = case term of
        Base x -> x
        Lift f -> runParametricBundle (Proxy :: Proxy '[f2]) (f f1) f2

instance RunParametricBundle '[f2, f3] m t => RunParametricBundle '[f1, f2, f3] m t where
    runParametricBundle _ term (f1, f2, f3) = case term of
        Base x -> x
        Lift f -> runParametricBundle (Proxy :: Proxy '[f2, f3]) (f f1) (f2, f3)

instance RunParametricBundle '[f2, f3, f4] m t => RunParametricBundle '[f1, f2, f3, f4] m t where
    runParametricBundle _ term (f1, f2, f3, f4) = case term of
        Base x -> x
        Lift f -> runParametricBundle (Proxy :: Proxy '[f2, f3, f4]) (f f1) (f2, f3, f4)

instance RunParametricBundle '[f2, f3, f4, f5] m t => RunParametricBundle '[f1, f2, f3, f4, f5] m t where
    runParametricBundle _ term (f1, f2, f3, f4, f5) = case term of
        Base x -> x
        Lift f -> runParametricBundle (Proxy :: Proxy '[f2, f3, f4, f5]) (f f1) (f2, f3, f4, f5)

instance RunParametricBundle '[f2, f3, f4, f5, f6] m t => RunParametricBundle '[f1, f2, f3, f4, f5, f6] m t where
    runParametricBundle _ term (f1, f2, f3, f4, f5, f6) = case term of
        Base x -> x
        Lift f -> runParametricBundle (Proxy :: Proxy '[f2, f3, f4, f5, f6]) (f f1) (f2, f3, f4, f5, f6)

instance RunParametricBundle '[f2, f3, f4, f5, f6, f7] m t => RunParametricBundle '[f1, f2, f3, f4, f5, f6, f7] m t where
    runParametricBundle _ term (f1, f2, f3, f4, f5, f6, f7) = case term of
        Base x -> x
        Lift f -> runParametricBundle (Proxy :: Proxy '[f2, f3, f4, f5, f6, f7]) (f f1) (f2, f3, f4, f5, f6, f7)

instance RunParametricBundle '[f2, f3, f4, f5, f6, f7, f8] m t => RunParametricBundle '[f1, f2, f3, f4, f5, f6, f7, f8] m t where
    runParametricBundle _ term (f1, f2, f3, f4, f5, f6, f7, f8) = case term of
        Base x -> x
        Lift f -> runParametricBundle (Proxy :: Proxy '[f2, f3, f4, f5, f6, f7, f8]) (f f1) (f2, f3, f4, f5, f6, f7, f8)

instance RunParametricBundle '[f2, f3, f4, f5, f6, f7, f8, f9] m t => RunParametricBundle '[f1, f2, f3, f4, f5, f6, f7, f8, f9] m t where
    runParametricBundle _ term (f1, f2, f3, f4, f5, f6, f7, f8, f9) = case term of
        Base x -> x
        Lift f -> runParametricBundle (Proxy :: Proxy '[f2, f3, f4, f5, f6, f7, f8, f9]) (f f1) (f2, f3, f4, f5, f6, f7, f8, f9)

instance RunParametricBundle '[f2, f3, f4, f5, f6, f7, f8, f9, f10] m t => RunParametricBundle '[f1, f2, f3, f4, f5, f6, f7, f8, f9, f10] m t where
    runParametricBundle _ term (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10) = case term of
        Base x -> x
        Lift f -> runParametricBundle (Proxy :: Proxy '[f2, f3, f4, f5, f6, f7, f8, f9, f10]) (f f1) (f2, f3, f4, f5, f6, f7, f8, f9, f10)
