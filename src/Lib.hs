{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}

module Lib where

import Prelude hiding (lookup, pi)

import Data.Singletons (Sing, SingI(..), SingKind(..), KProxy(..), SomeSing(..))
import GHC.TypeLits ()


-- comment

data Doggy = Tucker | Rover deriving Show


-- | STucker and SRover are values that introduce indexed types. This is a glue
-- between terms under types (Tucker and Rover), and terms under kinds ('Tucker
-- and 'Rover).
-- data SDoggy :: Doggy → * where
--   STucker :: SDoggy Tucker
--   SRover  :: SDoggy Rover

-- However, GHC provides a Sing type family under which we should create our
-- singleton sets; this allows us to use some built-in machinery, like sing
-- which allows one to infer the right singleton value for a known type.
data instance Sing (d :: Doggy) where
    STucker :: Sing 'Tucker
    SRover :: Sing 'Rover

instance SingI 'Tucker where
    sing :: Sing 'Tucker
    sing = STucker

instance SingI 'Rover where
    sing :: Sing 'Rover
    sing = SRover

instance SingKind ('KProxy :: KProxy Doggy) where

    type DemoteRep ('KProxy :: KProxy Doggy) = Doggy

    -- fromSing :: Sing (a :: Doggy) -> DemoteRep (KProxy :: KProxy Doggy)
    fromSing :: Sing (a :: Doggy) -> Doggy
    fromSing STucker = Tucker
    fromSing SRover = Rover

    -- toSing :: DemoteRep (KProxy :: KProxy Doggy) -> SomeSing (KProxy :: KProxy Doggy)
    toSing :: Doggy -> SomeSing ('KProxy :: KProxy Doggy)
    toSing Tucker = SomeSing STucker
    toSing Rover = SomeSing SRover

type Pi = Sing

pi :: forall (a :: k)
    . SingKind ('KProxy :: KProxy k)
   => Sing a
   -> DemoteRep ('KProxy :: KProxy k)
pi = fromSing




-- lala :: SDoggy a -> Bool
-- lala STucker = True
-- lala SRover = False

-- lala2 :: SDoggy 'Tucker -> Bool
-- lala2 STucker = True

-- lala2 :: Doggy -> Bool
-- lala STucker = True
-- lala SRover = False

someFunc :: IO ()
someFunc = putStrLn "someFunc"
