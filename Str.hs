{-# LANGUAGE DataKinds #-}

{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE UndecidableInstances #-}

module Str where

import Chr
import Data.Type.Bool
import Data.Type.Equality

newtype Str = String [Chr]

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
