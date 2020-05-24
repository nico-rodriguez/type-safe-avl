{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}

module Data.Tree.ITree (
  Tree(EmptyTree,ForkTree),
  ITree(EmptyITree,ForkITree))
where

import           Data.Kind      (Type)
import           Data.Tree.Node (Node (Node))
import           Prelude        (Show (show), String, (++))

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
