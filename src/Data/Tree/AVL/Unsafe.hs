{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ExplicitNamespaces  #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

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
import           Unsafe.Coerce (unsafeCoerce)


data Node :: Type where
  Node :: Show a => Int -> a -> Node
deriving instance Show Node

data AVL :: Type where
  E :: AVL
  F :: AVL -> Node -> AVL -> AVL
  deriving Show

emptyAVL :: AVL
emptyAVL = E

-- | Get the height of a tree.
height :: AVL -> Int
height E                  = 0
height (F l (Node _ _) r) = 1 + max (height l) (height r)


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
balance :: AVL -> AVL
balance E = E
balance t@(F l (Node _ _) r) = balance' t (unbalancedState (height l) (height r))

balance' :: AVL -> US -> AVL
balance' t@(F _ (Node _ _) _) NotUnbalanced = t
balance' t@(F (F ll (Node _ _) lr) (Node _ _) _) LeftUnbalanced  =
  rotate t LeftUnbalanced (balancedState (height ll) (height lr))
balance' t@(F _ (Node _ _) (F rl (Node _ _) rr)) RightUnbalanced =
  rotate t RightUnbalanced (balancedState (height rl) (height rr))


-- | Apply a rotation to a tree.
rotate :: AVL -> US -> BS -> AVL
-- | Left-Left case (Right rotation)
rotate (F (F ll lnode lr) xnode r) LeftUnbalanced LeftHeavy = F ll lnode (F lr xnode r)
rotate (F (F ll lnode lr) xnode r) LeftUnbalanced Balanced  = F ll lnode (F lr xnode r)
-- | Right-Right case (Left rotation)
rotate (F l xnode (F rl rnode rr)) RightUnbalanced RightHeavy = F (F l xnode rl) rnode rr
rotate (F l xnode (F rl rnode rr)) RightUnbalanced Balanced   = F (F l xnode rl) rnode rr
-- | Left-Right case (First left rotation, then right rotation)
rotate (F (F ll lnode (F lrl lrnode lrr)) xnode r) LeftUnbalanced RightHeavy =
    F (F ll lnode lrl) lrnode (F lrr xnode r)
-- | Right-Left case (First right rotation, then left rotation)
rotate (F l xnode (F (F rll rlnode rlr) rnode rr)) RightUnbalanced LeftHeavy =
    F (F l xnode rll) rlnode (F rlr rnode rr)


-- | Insert a new key and value.
-- | If the key is already present in the tree, update the value.
insertAVL :: Show a => Int -> a -> AVL -> AVL
insertAVL x v E                    = F E (Node x v) E
insertAVL x' v' t@(F _ (Node x _) _) = insertAVL' (Node x' v') t (compare x' x)

insertAVL' :: Node -> AVL -> Ordering -> AVL
insertAVL' (Node x v') (F l (Node _ _) r) EQ = F l (Node x v') r
insertAVL' n' (F E n r) LT = balance (F (F E n' E) n r)
insertAVL' n'@(Node x _) (F l@(F _ (Node ln _) _) n r) _ =
  balance (F (insertAVL' n' l (compare x ln)) n r)
insertAVL' n'@(Node _ _) (F l n E) GT = balance (F l n (F E n' E))
insertAVL' n'@(Node x _) (F l n r@(F _ (Node rn _) _)) GT =
  balance (F l n (insertAVL' n' r (compare x rn)))


-- | Lookup the given key in the tree.
-- | It returns Nothing if tree is empty
-- | or if it doesn't have the key.
lookupAVL :: Int -> AVL -> Maybe a
lookupAVL _ E                    = Nothing
lookupAVL x t@(F _ (Node n _) _) = lookupAVL' x t (compare x n)

lookupAVL' :: Int -> AVL -> Ordering -> Maybe a
lookupAVL' _ E                             _  = Nothing
lookupAVL' _ (F _ (Node _ v) _)            EQ = unsafeCoerce $ Just v
lookupAVL' x (F l@(F _ (Node ln _) _) _ _) LT = lookupAVL' x l (compare x ln)
lookupAVL' x (F _ _ r@(F _ (Node rn _) _)) GT = lookupAVL' x r (compare x rn)


-- | Delete the node with the maximum key value.
maxKeyDelete :: AVL -> AVL
maxKeyDelete E                  = E
maxKeyDelete (F l (Node _ _) E) = l
maxKeyDelete (F l node r@F{})   =
  balance $ F l node (maxKeyDelete r)


-- | Get the node with maximum key value.
-- | It returns Nothing if tree is empty.
maxNode :: AVL -> Maybe Node
maxNode E                      = Nothing
maxNode (F _ n@(Node _ _) E)   = Just n
maxNode (F _ (Node _ _) r@F{}) = maxNode r


-- | Delete the node with the given key.
-- | If the key is not in the tree, return the same tree.
deleteAVL :: Int -> AVL -> AVL
deleteAVL _ E                    = E
deleteAVL x t@(F _ (Node n _) _) = deleteAVL' x t (compare x n)

deleteAVL' :: Int -> AVL -> Ordering -> AVL
deleteAVL' _ (F E (Node _ _) E)         EQ = E
deleteAVL' _ (F E (Node _ _) r@F{})     EQ = r
deleteAVL' _ (F l@F{} (Node _ _) E)     EQ = l
deleteAVL' _ (F l@F{} (Node _ _) r@F{}) EQ =
  balance $ F (maxKeyDelete l) mNode r
    where Just mNode = maxNode l
deleteAVL' _ t@(F E (Node _ _) _)             LT = t
deleteAVL' x (F l@(F _ (Node ln _) _) node r) LT =
  balance $ F (deleteAVL' x l (compare x ln)) node r
deleteAVL' _ t@(F _ (Node _ _) E)             GT = t
deleteAVL' x (F l node r@(F _ (Node rn _) _)) GT =
  balance $ F l node (deleteAVL' x r (compare x rn))
