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
import Control.Monad.Except  ( throwError )
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

-- lala :: forall a b . ((a :== b) ~ 'True) => SNat a -> SNat b -> Bool
lala :: forall (a :: Nat) (b :: Nat) . ((a :== b) ~ 'True) => Sing a -> Sing b -> Bool
lala SZero SZero = True
lala _ _ = False

what :: Nat -> SomeSing ('KProxy :: KProxy Nat)
what = toSing

useWhat :: Nat -> String
useWhat nat = case what nat of
    SomeSing (s :: Sing (a :: Nat)) ->
        case fromSing s of
            Zero -> "got zero"
            Succ _ -> "got succ"

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
vecReadInstance :: Read (El u) => SU u -> SNat n -> VecReadInstance (El u) n
vecReadInstance _ SZero = VecReadInstance
vecReadInstance u (SSucc n) = case vecReadInstance u n of
  VecReadInstance -> VecReadInstance
-- vecReadInstance :: Read a => proxy a -> SNat n -> VecReadInstance a n
-- vecReadInstance _ SZero = VecReadInstance
-- vecReadInstance proxy (SSucc n) = case vecReadInstance proxy n of
--   VecReadInstance -> VecReadInstance



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
data ElUReadInstance u where
    ElUReadInstance :: Read (El u) => ElUReadInstance u

elUReadInstance :: forall (x :: U) . Sing x -> ElUReadInstance x
elUReadInstance SBOOL = ElUReadInstance
elUReadInstance SSTRING = ElUReadInstance
elUReadInstance SNAT  = ElUReadInstance
elUReadInstance (SVEC (u :: Sing u) (n :: Sing n)) =
    case elUReadInstance u of
        (ElUReadInstance :: ElUReadInstance u) ->
            case vecReadInstance u n of
                (VecReadInstance :: VecReadInstance (El u) n) -> ElUReadInstance

data ElUShowInstance u where
      ElUShowInstance :: Show (El u) => ElUShowInstance u

elUShowInstance :: Sing u -> ElUShowInstance u
elUShowInstance SBOOL = ElUShowInstance
elUShowInstance SSTRING = ElUShowInstance
elUShowInstance SNAT  = ElUShowInstance
elUShowInstance (SVEC u _) = case elUShowInstance u of
    ElUShowInstance -> ElUShowInstance

showAttrProof :: Sing (Attr nm u) -> ElUShowInstance u
showAttrProof (SAttr _ u) = elUShowInstance u


-- A Row is one row of our database table, keyed by its schema.
data Row :: Schema -> * where
    EmptyRow :: [Int] -> Row (Sch '[]) -- the Ints are the unique id of the row
    ConsRow :: El u -> Row (Sch s) -> Row (Sch ((Attr name u) ': s))


-- We build Show instances for a Row element by element:
instance Show (Row (Sch '[])) where
    show (EmptyRow n) = "(id=" ++ show n ++ ")"

instance (Show (El u), Show (Row (Sch attrs))) =>
    Show (Row (Sch ((Attr name u) ': attrs))) where
        show (ConsRow h t) =
            case t of
                EmptyRow n -> show h ++ " (id=" ++ show n ++ ")"
                _ -> show h ++ ", " ++ show t

-- A Handle in our system is an abstract handle to a loaded table.
-- The constructor is not exported. In our simplistic case, we
-- just store the list of rows. A more sophisticated implementation
-- could store some identifier to the connection to an external database.
data Handle :: Schema -> * where
    Handle :: [Row s] -> Handle s



-- The following functions parse our very simple flat file database format.

-- The file, with a name ending in ".dat", consists of a sequence of lines,
-- where each line contains one entry in the table. There is no row separator;
-- if a row contains n pieces of data, that row is represented in n lines in
-- the file.

-- A schema is stored in a file of the same name, except ending in ".schema".
-- Each line in the file is a constructor of U indicating the type of the
-- corresponding row element.

-- Use Either for error handling in parsing functions
type ErrorM = Either String

-- This function is relatively uninteresting except for its use of
-- pattern matching to introduce the instances of Read and Show for
-- elements
-- >>> readRow
--       0
--       (SSch
--         (SCons
--           (SAttr (SCons SCA SNil) SBOOL)
--           (SCons
--             (SAttr (SCons SCA SNil) SNAT)
--             SNil
--           )
--         )
--       )
--       ["True", "4"]
--          :: ErrorM
--               ( Row ('Sch '[ 'Attr '[ CA ] 'BOOL
--                            , 'Attr '[ CA ] 'NAT ]
--                     )
--               , [String]
--               )
readRow :: forall (s :: Schema) . Int -> SSchema s -> [String] -> ErrorM (Row s, [String])
readRow id' (SSch SNil) strs =
  return (EmptyRow [id'], strs)
readRow _ (SSch (SCons _ _)) [] =
  throwError "Ran out of data while processing row"
readRow id' (SSch (SCons (SAttr _ (u :: SU (x :: U))) at)) (sh:st) = do
  (rowTail, strTail :: [String]) <- readRow id' (SSch at) st
  case elUReadInstance u of
    ElUReadInstance ->
      let (results :: [(El x, String)]) = readsPrec 0 sh in
      if null results
        then throwError $ "No parse of " ++ sh ++ " as a " ++ show (fromSing u)
        else
          let (item :: El x) = fst $ head results in
          case elUShowInstance u of
            ElUShowInstance -> return (ConsRow item rowTail, strTail)
