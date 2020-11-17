{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# LANGUAGE Safe                  #-}

module Data.Tree.AVL.Intern.Insert (
  Insertable(Insert,insert)
) where

import           Data.Kind                        (Type)
import           Data.Proxy                       (Proxy (Proxy))
import           Data.Tree.AVL.Intern.Balance     (Balanceable (Balance, balance),
                                                   ProofGtNBalance (proofGtNBalance),
                                                   ProofLtNBalance (proofLtNBalance))
import           Data.Tree.AVL.Intern.Constructor (AVL (EmptyAVL, ForkAVL),
                                                   AlmostAVL (AlmostAVL))
import           Data.Tree.BST.Invariants         (GtN, LtN)
import           Data.Tree.ITree                  (Tree (EmptyTree, ForkTree))
import           Data.Tree.Node                   (Node (Node))
import           Data.Type.Equality               ((:~:) (Refl), gcastWith)
import           GHC.TypeLits                     (CmpNat, Nat)
import           Prelude                          (Bool (True),
                                                   Ordering (EQ, GT, LT), Show,
                                                   ($))


-- | Prove that inserting a node with key 'x' (lower than 'n') and element value 'a'
-- | in an AVL 't' which verifies 'LtN t n ~ 'True' preserves the LtN invariant,
-- | given that the comparison between 'x' and the root key of the tree equals 'o'.
-- | The 'o' parameter guides the proof.
class ProofLtNInsert' (x :: Nat) (a :: Type) (t :: Tree) (n :: Nat) (o :: Ordering) where
  proofLtNInsert' :: (CmpNat x n ~ 'LT, LtN t n ~ 'True) =>
    Node x a -> AVL t -> Proxy n -> Proxy o -> LtN (Insert x a t) n :~: 'True
instance (CmpNat x n1 ~ 'EQ) =>
  ProofLtNInsert' x a ('ForkTree l (Node n1 a1) r) n 'EQ where
  proofLtNInsert' _ ForkAVL{} _ _ = Refl
instance (CmpNat x n1 ~ 'LT, ProofLtNBalance ('ForkTree ('ForkTree 'EmptyTree (Node x a) 'EmptyTree) (Node n1 a1) r) n, Show a) =>
  ProofLtNInsert' x a ('ForkTree 'EmptyTree (Node n1 a1) r) n 'LT where
  proofLtNInsert' node (ForkAVL _ node' r) pn _ =
    gcastWith (proofLtNBalance (AlmostAVL (ForkAVL EmptyAVL node EmptyAVL) node' r) pn) Refl
instance (CmpNat x n1 ~ 'LT, l ~ 'ForkTree ll (Node ln lna) lr, o ~ CmpNat x ln, LtN l n ~ 'True, ProofLtNInsert' x a l n1 o,
  ProofLtNInsert' x a l n o, ProofLtNBalance ('ForkTree (Insert' x a l o) (Node n1 a1) r) n, Insertable x a l) =>
  ProofLtNInsert' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n1 a1) r) n 'LT where
  proofLtNInsert' node (ForkAVL l node' r) pn _ =
    gcastWith (proofLtNInsert' node l pn (Proxy::Proxy o)) $
    gcastWith (proofLtNInsert' node l (Proxy::Proxy n1) (Proxy::Proxy o)) $
    gcastWith (proofLtNBalance (AlmostAVL l' node' r) pn) Refl
      where
        l' = insert node l
instance (CmpNat x n1 ~ 'GT, ProofLtNBalance ('ForkTree l (Node n1 a1) ('ForkTree 'EmptyTree (Node x a) 'EmptyTree)) n, Show a) =>
  ProofLtNInsert' x a ('ForkTree l (Node n1 a1) 'EmptyTree) n 'GT where
  proofLtNInsert' node (ForkAVL l node' _) pn _ =
    gcastWith (proofLtNBalance (AlmostAVL l node' (ForkAVL EmptyAVL node EmptyAVL)) pn) Refl
instance (CmpNat x n1 ~ 'GT, r ~ 'ForkTree rl (Node rn rna) rr, o ~ CmpNat x rn, ProofLtNInsert' x a r n o, LtN r n ~ 'True, Insertable x a r,
  ProofLtNBalance ('ForkTree l (Node n1 a1) (Insert' x a ('ForkTree rl (Node rn rna) rr) o)) n, ProofGtNInsert' x a r n1 o) =>
  ProofLtNInsert' x a ('ForkTree l (Node n1 a1) ('ForkTree rl (Node rn rna) rr)) n 'GT where
  proofLtNInsert' node (ForkAVL l node' r) pn _ =
    gcastWith (proofLtNInsert' node r pn (Proxy::Proxy o)) $
    gcastWith (proofGtNInsert' node r (Proxy::Proxy n1) (Proxy::Proxy o)) $
    gcastWith (proofLtNBalance (AlmostAVL l node' r') pn) Refl
      where
        r' = insert node r

-- | Prove that inserting a node with key 'x' (greater than 'n') and element value 'a'
-- | in an AVL 't' which verifies 'GtN t n ~ 'True' preserves the GtN invariant,
-- | given that the comparison between 'x' and the root key of the tree equals 'o'.
-- | The 'o' parameter guides the proof.
class ProofGtNInsert' (x :: Nat) (a :: Type) (t :: Tree) (n :: Nat) (o :: Ordering) where
  proofGtNInsert' :: (CmpNat x n ~ 'GT, GtN t n ~ 'True) =>
    Node x a -> AVL t -> Proxy n -> Proxy o -> GtN (Insert x a t) n :~: 'True
instance (CmpNat x n1 ~ 'EQ) =>
  ProofGtNInsert' x a ('ForkTree l (Node n1 a1) r) n 'EQ where
  proofGtNInsert' _ ForkAVL{} _ _ = Refl
instance (CmpNat x n1 ~ 'LT, ProofGtNBalance ('ForkTree ('ForkTree 'EmptyTree (Node x a) 'EmptyTree) (Node n1 a1) r) n, Show a) =>
  ProofGtNInsert' x a ('ForkTree 'EmptyTree (Node n1 a1) r) n 'LT where
  proofGtNInsert' node (ForkAVL _ node' r) pn _ =
    gcastWith (proofGtNBalance (AlmostAVL (ForkAVL EmptyAVL node EmptyAVL) node' r) pn) Refl
instance (CmpNat x n1 ~ 'LT, l ~ 'ForkTree ll (Node ln lna) lr, o ~ CmpNat x ln, ProofGtNInsert' x a l n o, GtN l n ~ 'True, Insertable x a l,
  ProofGtNBalance ('ForkTree (Insert' x a ('ForkTree ll (Node ln lna) lr) o) (Node n1 a1) r) n, ProofLtNInsert' x a l n1 o) =>
  ProofGtNInsert' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n1 a1) r) n 'LT where
  proofGtNInsert' node (ForkAVL l node' r) pn _ =
    gcastWith (proofGtNInsert' node l pn (Proxy::Proxy o)) $
    gcastWith (proofLtNInsert' node l (Proxy::Proxy n1) (Proxy::Proxy o)) $
    gcastWith (proofGtNBalance (AlmostAVL l' node' r) pn) Refl
      where
        l' = insert node l
instance (CmpNat x n1 ~ 'GT, ProofGtNBalance ('ForkTree l (Node n1 a1) ('ForkTree 'EmptyTree (Node x a) 'EmptyTree)) n, Show a) =>
  ProofGtNInsert' x a ('ForkTree l (Node n1 a1) 'EmptyTree) n 'GT where
  proofGtNInsert' node (ForkAVL l node' _) pn _ =
    gcastWith (proofGtNBalance (AlmostAVL l node' (ForkAVL EmptyAVL node EmptyAVL)) pn) Refl
instance (CmpNat x n1 ~ 'GT, r ~ 'ForkTree rl (Node rn rna) rr, o ~ CmpNat x rn, ProofGtNInsert' x a r n o, GtN r n ~ 'True, Insertable x a r,
  ProofGtNBalance ('ForkTree l (Node n1 a1) (Insert' x a ('ForkTree rl (Node rn rna) rr) o)) n, ProofGtNInsert' x a r n1 o) =>
  ProofGtNInsert' x a ('ForkTree l (Node n1 a1) ('ForkTree rl (Node rn rna) rr)) n 'GT where
  proofGtNInsert' node (ForkAVL l node' r) pn _ =
    gcastWith (proofGtNInsert' node r pn (Proxy::Proxy o)) $
    gcastWith (proofGtNInsert' node r (Proxy::Proxy n1) (Proxy::Proxy o)) $
    gcastWith (proofGtNBalance (AlmostAVL l node' r') pn) Refl
      where
        r' = insert node r


-- | This class provides the functionality to insert a node with key 'x' and value type 'a'
-- | in an AVL 't'.
-- | The insertion is defined at the value level and the type level.
-- | The returned tree verifies the BST/AVL invariant.
class Insertable (x :: Nat) (a :: Type) (t :: Tree) where
  type Insert (x :: Nat) (a :: Type) (t :: Tree) :: Tree
  insert :: Node x a -> AVL t -> AVL (Insert x a t)
instance Show a => Insertable x a 'EmptyTree where
  type Insert x a 'EmptyTree = 'ForkTree 'EmptyTree (Node x a) 'EmptyTree
  insert (Node a) EmptyAVL         = ForkAVL EmptyAVL (Node a::Node x a) EmptyAVL
instance (Insertable' x a ('ForkTree l (Node n a1) r) (CmpNat x n)) =>
  Insertable x a ('ForkTree l (Node n a1) r) where
  type Insert x a ('ForkTree l (Node n a1) r) = Insert' x a ('ForkTree l (Node n a1) r) (CmpNat x n)
  insert n t = insert' n t (Proxy::Proxy (CmpNat x n))

-- | This class provides the functionality to insert a node with key 'x' and value type 'a'
-- | in a non empty AVL 't'.
-- | It's only used by the 'Insertable' class and it has one extra parameter 'o',
-- | which is the type level comparison of 'x' with the key value of the root node.
-- | The 'o' parameter guides the insertion.
class Insertable' (x :: Nat) (a :: Type) (t :: Tree) (o :: Ordering) where
  type Insert' (x :: Nat) (a :: Type) (t :: Tree) (o :: Ordering) :: Tree
  insert' :: Node x a -> AVL t -> Proxy o -> AVL (Insert' x a t o)
instance (Show a) =>
  Insertable' x a ('ForkTree l (Node n a1) r) 'EQ where
  type Insert' x a ('ForkTree l (Node n a1) r) 'EQ = 'ForkTree l (Node n a) r
  insert' (Node a) (ForkAVL l (Node _) r) _ = ForkAVL l (Node a::Node n a) r
instance (Show a, CmpNat x n ~ 'LT,
  Balanceable ('ForkTree ('ForkTree 'EmptyTree (Node x a) 'EmptyTree) (Node n a1) r)) =>
  Insertable' x a ('ForkTree 'EmptyTree (Node n a1) r) 'LT where
  type Insert' x a ('ForkTree 'EmptyTree (Node n a1) r) 'LT = Balance ('ForkTree ('ForkTree 'EmptyTree (Node x a) 'EmptyTree) (Node n a1) r)
  insert' (Node a) (ForkAVL EmptyAVL n r) _ = balance (AlmostAVL (ForkAVL EmptyAVL (Node a::Node x a) EmptyAVL) n r)
instance (CmpNat x n ~ 'LT, l ~ 'ForkTree ll (Node ln lna) lr, Insertable' x a l (CmpNat x ln),
  Balanceable ('ForkTree (Insert' x a l (CmpNat x ln)) (Node n a1) r),
  ProofLtNInsert' x a l n (CmpNat x ln)) =>
  Insertable' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n a1) r) 'LT where
  type Insert' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n a1) r) 'LT =
    Balance ('ForkTree (Insert' x a ('ForkTree ll (Node ln lna) lr) (CmpNat x ln)) (Node n a1) r)
  insert' nx (ForkAVL l@ForkAVL{} n r) _ =
    gcastWith (proofLtNInsert' nx l (Proxy::Proxy n) (Proxy::Proxy (CmpNat x ln))) $
    balance $
    AlmostAVL (insert' nx l (Proxy::Proxy (CmpNat x ln))) n r
instance (Show a, CmpNat x n ~ 'GT,
  Balanceable ('ForkTree l (Node n a1) ('ForkTree 'EmptyTree (Node x a) 'EmptyTree))) =>
  Insertable' x a ('ForkTree l (Node n a1) 'EmptyTree) 'GT where
  type Insert' x a ('ForkTree l (Node n a1) 'EmptyTree) 'GT = Balance ('ForkTree l (Node n a1) ('ForkTree 'EmptyTree (Node x a) 'EmptyTree))
  insert' (Node a) (ForkAVL l n EmptyAVL) _ = balance (AlmostAVL l n (ForkAVL EmptyAVL (Node a::Node x a) EmptyAVL))
instance (CmpNat x n ~ 'GT, r ~ 'ForkTree rl (Node rn rna) rr, Insertable' x a r (CmpNat x rn),
  Balanceable ('ForkTree l (Node n a1) (Insert' x a r (CmpNat x rn))),
  ProofGtNInsert' x a r n (CmpNat x rn)) =>
  Insertable' x a ('ForkTree l (Node n a1) ('ForkTree rl (Node rn rna) rr)) 'GT where
  type Insert' x a ('ForkTree l (Node n a1) ('ForkTree rl (Node rn rna) rr)) 'GT =
    Balance ('ForkTree l (Node n a1) (Insert' x a ('ForkTree rl (Node rn rna) rr) (CmpNat x rn)))
  insert' nx (ForkAVL l n r@ForkAVL{}) _ =
    gcastWith (proofGtNInsert' nx r (Proxy::Proxy n) (Proxy::Proxy (CmpNat x rn))) $
    balance $
    AlmostAVL l n (insert' nx r (Proxy::Proxy (CmpNat x rn)))
