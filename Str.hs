{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Str where

import Chr
import Data.Type.Bool
import Data.Type.Equality

-- newtype Str = String [Chr]

data Str :: [Chr] -> * where
  EmptyStr :: Str '[]
  SStr     :: SChar c -> Str cs -> Str (c:cs)

type family CompareStr (a :: [Chr]) (b :: [Chr]) :: Ordering where
  CompareStr '[]    '[]     = 'EQ
  CompareStr '[]    (y:ys)  = 'LT
  CompareStr (x:xs) '[]     = 'GT
  CompareStr (x:xs) (y:ys)  =
    (If (CompareChr x y == 'EQ)
      (CompareStr xs ys)
      (If (CompareChr x y == 'LT)
        'LT
        'GT
      )
    )

data OWOTO :: [Chr] -> [Chr] -> * where
  LE :: (CompareStr x y ~ 'LT) => OWOTO x y
  EE :: (CompareStr x x ~ 'EQ) => OWOTO x x
  GE :: (CompareStr x y ~ 'GT) => OWOTO x y

owoto :: Str m -> Str n -> OWOTO m n
owoto EmptyStr    EmptyStr    = EE
owoto EmptyStr    (SStr d ds) = LE
owoto (SStr c cs) EmptyStr    = GE
owoto (SStr c cs) (SStr d ds) = case owoto c d of
  LE -> LE
  GE -> GE
  EE -> EE
