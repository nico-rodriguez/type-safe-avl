{-|
Module      : W
Description : 
Copyright   : (c) Nicolás Rodríguez, 2021
License     : GPL-3
Maintainer  : Nicolás Rodríguez
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE ExplicitNamespaces  #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

{-# LANGUAGE Safe                #-}

{-# OPTIONS_GHC -Wno-incomplete-patterns     #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Data.Tree.AVL.Unsafe (
  emptyAVL,
  insertAVL,
  lookupAVL,
  deleteAVL
) where

import           Data.Kind     (Type)
import           Prelude       (Int, Maybe (Just, Nothing),
                                Ordering (EQ, GT, LT), Show,
                                compare, max, ($), (+), (-))


data Node :: Type where
  Node :: Show a => Int -> a -> Node
deriving stock instance Show Node

data AVL :: Type where
  E :: AVL
  F :: AVL -> Node -> AVL -> AVL
  deriving stock Show

data AlmostAVL :: Type where
  FF :: AVL -> Node -> AVL -> AlmostAVL
  deriving stock Show

emptyAVL :: AVL
emptyAVL = E

-- | Get the height of a tree.
height :: AVL -> Int
height E                  = 0
height (F l _ r) = 1 + max (height l) (height r)


-- | Data type that represents the state of unbalance of the sub trees:
-- | - LeftUnbalanced: height(left sub tree) = height(right sub tree) + 2.
-- | - RightUnbalanced: height(right sub tree) = height(leftt sub tree) + 2.
-- | - NotUnbalanced: tree is not unbalanced.
data US = LeftUnbalanced | RightUnbalanced | NotUnbalanced

-- | Check from two natural numbers,
-- | that represent the heights of some left and right sub trees,
-- | if the tree is balanced or if some of those sub trees is unbalanced.
unbalancedState :: Int -> Int -> US
unbalancedState 0 0   = NotUnbalanced
unbalancedState 1 0   = NotUnbalanced
unbalancedState 0 1   = NotUnbalanced
unbalancedState 2 0   = LeftUnbalanced
unbalancedState 0 2   = RightUnbalanced
unbalancedState h1 h2 = unbalancedState (h1-1) (h2-1)


-- | Data type that represents the state of balance of the sub trees in a balanced tree:
-- | - LeftHeavy: height(left sub tree) = height(right sub tree) + 1.
-- | - RightHeavy: height(right sub tree) = height(leftt sub tree) + 1.
-- | - Balanced: height(left sub tree) = height(right sub tree).
data BS = LeftHeavy | RightHeavy | Balanced

-- | Check from two natural numbers,
-- | that represent the heights of some left and right sub trees,
-- | if some of those sub trees have height larger than the other.
balancedState :: Int -> Int -> BS
balancedState 0 0   = Balanced
balancedState 1 0   = LeftHeavy
balancedState 0 1   = RightHeavy
balancedState h1 h2 = balancedState (h1-1) (h2-1)


-- | Balance a tree.
balance :: AlmostAVL -> AVL
balance t@(FF l _ r) = balance' t (unbalancedState (height l) (height r))

balance' :: AlmostAVL -> US -> AVL
balance' (FF l n r)             NotUnbalanced   = F l n r
balance' t@(FF (F ll _ lr) _ _) LeftUnbalanced  =
  rotate t LeftUnbalanced $ balancedState (height ll) (height lr)
balance' t@(FF _ _ (F rl _ rr)) RightUnbalanced =
  rotate t RightUnbalanced $ balancedState (height rl) (height rr)


-- | Apply a rotation to a tree.
rotate :: AlmostAVL -> US -> BS -> AVL
-- | Left-Left case (Right rotation)
rotate (FF (F ll lnode lr) node r) LeftUnbalanced LeftHeavy = F ll lnode (F lr node r)
rotate (FF (F ll lnode lr) node r) LeftUnbalanced Balanced  = F ll lnode (F lr node r)
-- | Right-Right case (Left rotation)
rotate (FF l node (F rl rnode rr)) RightUnbalanced RightHeavy = F (F l node rl) rnode rr
rotate (FF l node (F rl rnode rr)) RightUnbalanced Balanced   = F (F l node rl) rnode rr
-- | Left-Right case (First left rotation, then right rotation)
rotate (FF (F ll lnode (F lrl lrnode lrr)) node r) LeftUnbalanced RightHeavy =
  F (F ll lnode lrl) lrnode (F lrr node r)
-- | Right-Left case (First right rotation, then left rotation)
rotate (FF l node (F (F rll rlnode rlr) rnode rr)) RightUnbalanced LeftHeavy =
  F (F l node rll) rlnode (F rlr rnode rr)


-- | Insert a new key and value.
-- | If the key is already present in the tree, update the value.
insertAVL :: Show a => Int -> a -> AVL -> AVL
insertAVL x  v  E                    = F E (Node x v) E
insertAVL x' v' t@(F _ (Node x _) _) = insertAVL' (Node x' v') t (compare x' x)

insertAVL' :: Node -> AVL -> Ordering -> AVL
insertAVL' node (F l _ r) EQ = F l node r
insertAVL' n' (F E n r) LT = balance (FF (F E n' E) n r)
insertAVL' n'@(Node x _) (F l@(F _ (Node ln _) _) n r) LT =
  balance $ FF (insertAVL' n' l (compare x ln)) n r
insertAVL' n' (F l n E) GT = balance (FF l n (F E n' E))
insertAVL' n'@(Node x _) (F l n r@(F _ (Node rn _) _)) GT =
  balance $ FF l n (insertAVL' n' r (compare x rn))


-- | Lookup the given key in the tree.
-- | It returns Nothing if tree is empty
-- | or if it doesn't have the key.
lookupAVL :: Int -> AVL -> Maybe Node
lookupAVL _ E                    = Nothing
lookupAVL x t@(F _ (Node n _) _) = lookupAVL' x t (compare x n)

lookupAVL' :: Int -> AVL -> Ordering -> Maybe Node
lookupAVL' _ E                             _  = Nothing
lookupAVL' _ (F _ node _)                  EQ = Just node
lookupAVL' _ (F E _ _)                     LT = Nothing
lookupAVL' _ (F _ _ E)                     GT = Nothing
lookupAVL' x (F l@(F _ (Node ln _) _) _ _) LT = lookupAVL' x l (compare x ln)
lookupAVL' x (F _ _ r@(F _ (Node rn _) _)) GT = lookupAVL' x r (compare x rn)


-- | Delete the node with the maximum key value.
maxKeyDelete :: AVL -> AVL
maxKeyDelete E                = E
maxKeyDelete (F l _ E)        = l
maxKeyDelete (F l node r@F{}) =
  balance $ FF l node (maxKeyDelete r)


-- | Get the node with maximum key value.
-- | It returns Nothing if tree is empty.
maxNode :: AVL -> Maybe Node
maxNode E                       = Nothing
maxNode (F _ n E)               = Just n
maxNode (F _ (Node _ _) r@F{})  = maxNode r


-- | Delete the node with the given key.
-- | If the key is not in the tree, return the same tree.
deleteAVL :: Int -> AVL -> AVL
deleteAVL _ E                    = E
deleteAVL x t@(F _ (Node n _) _) = deleteAVL' x t (compare x n)

deleteAVL' :: Int -> AVL -> Ordering -> AVL
deleteAVL' _ (F E     _ E)     EQ = E
deleteAVL' _ (F E     _ r@F{}) EQ = r
deleteAVL' _ (F l@F{} _ E)     EQ = l
deleteAVL' _ (F l@F{} _ r@F{}) EQ =
  balance $ FF (maxKeyDelete l) mNode r
    where Just mNode = maxNode l
deleteAVL' _ t@(F E _ _) LT = t
deleteAVL' x (F l@(F _ (Node ln _) _) node r) LT =
  balance $ FF (deleteAVL' x l (compare x ln)) node r
deleteAVL' _ t@(F _ _ E) GT = t
deleteAVL' x (F l node r@(F _ (Node rn _) _)) GT =
  balance $ FF l node (deleteAVL' x r (compare x rn))
