{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE IncoherentInstances   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Binary trees without constraints. The functions insert, member, max and delete
-- | assume the trees are binary search, but this constraint is not included in
-- | the types of the trees.

module Extern.ITree where

import           Data.Kind
import           Extern.Node

data Tree :: Type where
  EmptyTree :: Tree
  ForkTree  :: Tree -> n -> Tree -> Tree

data ITree :: Tree -> Type where
  EmptyITree :: ITree 'EmptyTree
  ForkITree  :: Show a => ITree l -> Node n a -> ITree r -> ITree ('ForkTree l (Node n a) r)

instance Show (ITree t) where
  show EmptyITree         = "E"
  show (ForkITree l n@(Node _) r)  = "F " ++ go l ++ " " ++ show n ++ " " ++ go r
    where
      go :: ITree t' -> String
      go EmptyITree         = "E"
      go (ForkITree l' n'@(Node _) r')  = "(F " ++ go l' ++ " " ++ show n' ++ " " ++ go r' ++ ")"
