{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Database where

import Prelude hiding ( tail, id )
import Data.Singletons.Prelude hiding ( Lookup, sLookup )
import Data.Singletons.SuppressUnusedWarnings
import Data.Singletons.TH
import Control.Monad
import Data.List hiding ( tail )


$(singletons [d|
   -- Basic Nat type
   data Nat = Zero | Succ Nat deriving (Eq, Ord)
       |])

-- Conversions to any from Integers
fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Succ n) = fromNat n + 1

toNat :: Integer -> Nat
toNat 0         = Zero
toNat n | n > 0 = Succ (toNat (n - 1))
toNat _         = error "Converting negative to Nat"

-- Display and read Nats using decimal digits
instance Show Nat where
    show = show . fromNat
instance Read Nat where
    readsPrec n s = map (\(a,rest) -> (toNat a,rest)) $ readsPrec n s

$(singletons [d|
  -- Our "U"niverse of types. These types can be stored in our database.
  data U = BOOL
         | STRING
         | NAT
         | VEC U Nat deriving (Read, Eq, Show)

  -- A re-definition of Char as an algebraic data type.
  -- This is necessary to allow for promotion and type-level Strings.
  data AChar = CA | CB | CC | CD | CE | CF | CG | CH | CI
             | CJ | CK | CL | CM | CN | CO | CP | CQ | CR
             | CS | CT | CU | CV | CW | CX | CY | CZ
    deriving (Read, Show, Eq)

  -- A named attribute in our database
  data Attribute = Attr [AChar] U

  -- A schema is an ordered list of named attributes
  data Schema = Sch [Attribute]

  -- append two schemas
  append :: Schema -> Schema -> Schema
  append (Sch s1) (Sch s2) = Sch (s1 ++ s2)

  -- predicate to check that a schema is free of a certain attribute
  attrNotIn :: Attribute -> Schema -> Bool
  attrNotIn _ (Sch []) = True
  attrNotIn (Attr name u) (Sch ((Attr name' _) : t)) =
    (name /= name') && (attrNotIn (Attr name u) (Sch t))

  -- predicate to check that two schemas are disjoint
  disjoint :: Schema -> Schema -> Bool
  disjoint (Sch []) _ = True
  disjoint (Sch (h : t)) s = (attrNotIn h s) && (disjoint (Sch t) s)

  -- predicate to check if a name occurs in a schema
  occurs :: [AChar] -> Schema -> Bool
  occurs _ (Sch []) = False
  occurs name (Sch ((Attr name' _) : attrs)) =
    name == name' || occurs name (Sch attrs)

  -- looks up an element type from a schema
  lookup :: [AChar] -> Schema -> U
  lookup _ (Sch []) = undefined
  lookup name (Sch ((Attr name' u) : attrs)) =
    if name == name' then u else lookup name (Sch attrs)
  |])

-- The El type family gives us the type associated with a constructor
-- of U:
type family El (u :: U) :: *
type instance El BOOL = Bool
type instance El STRING = String
type instance El NAT  = Nat
type instance El (VEC u n) = Vec (El u) n

-- Length-indexed vectors
data Vec :: * -> Nat -> * where
    VNil :: Vec a 'Zero
    VCons :: a -> Vec a n -> Vec a ('Succ n)

-- Read instances are keyed by the index of the vector to aid in parsing
instance Read (Vec a Zero) where
    readsPrec _ s = [(VNil, s)]

instance (Read a, Read (Vec a n)) => Read (Vec a (Succ n)) where
    readsPrec n s = do
        (a, rest) <- readsPrec n s
        (tail, restrest) <- readsPrec n rest
        return (VCons a tail, restrest)

-- Because the Read instances are keyed by the length of the vector,
-- it is not obvious to the compiler that all Vecs have a Read instance.
-- We must make a short inductive proof of this fact.

-- First, we define a datatype to store the resulting instance, keyed
-- by the parameters to Vec:
data VecReadInstance a n where
  VecReadInstance :: Read (Vec a n) => VecReadInstance a n

-- Then, we make a function that produces an instance of Read for a
-- Vec, given the datatype it is over and its length, both encoded
-- using singleton types:
-- vecReadInstance :: Read (El u) => SU u -> SNat n -> VecReadInstance (El u) n
-- vecReadInstance _ SZero = VecReadInstance
-- vecReadInstance u (SSucc n) = case vecReadInstance u n of
--   VecReadInstance -> VecReadInstance
vecReadInstance :: Read a => proxy a -> SNat n -> VecReadInstance a n
vecReadInstance _ SZero = VecReadInstance
vecReadInstance proxy (SSucc n) = case vecReadInstance proxy n of
  VecReadInstance -> VecReadInstance



-- The Show instance can be straightforwardly defined:
instance Show a => Show (Vec a n) where
    show VNil = ""
    show (VCons h t) = show h ++ " " ++ show t


-- We need to be able to Read and Show elements of our database, so
-- we must know that any type of the form (El u) for some (u :: U)
-- has a Read and Show instance. Because we can't declare this instance
-- directly (as, in general, declaring an instance of a type family
-- would be unsound), we provide inductive proofs that these instances
-- exist:
-- data ElUReadInstance u where
--     ElUReadInstance :: Read (El u) => ElUReadInstance u

-- elUReadInstance :: Sing u -> ElUReadInstance u
-- elUReadInstance SBOOL = ElUReadInstance
-- elUReadInstance SSTRING = ElUReadInstance
-- elUReadInstance SNAT  = ElUReadInstance
-- elUReadInstance (SVEC u n) =
--     case elUReadInstance u of
--         ElUReadInstance ->
--             case vecReadInstance u n of
--                 VecReadInstance -> ElUReadInstance
