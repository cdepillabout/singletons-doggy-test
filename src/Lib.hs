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

import Prelude hiding (lookup)

import Data.Singletons (Sing, SingI(..))
import GHC.TypeLits ()


-- comment

data Doggy = Tucker | Rover


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

instance SingI Doggy where
    sing :: Sing Doggy
    sing = undefined



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
