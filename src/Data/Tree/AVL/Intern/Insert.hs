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
import           Data.Tree.AVL.Intern.Constructors (AVL (EmptyAVL, ForkAVL),
                                                   AlmostAVL (AlmostAVL))
import           Data.Tree.BST.Invariants         (GtN, LtN)
import           Data.Tree.ITree                  (Tree (EmptyTree, ForkTree))
import           Data.Tree.Node                   (Node, mkNode, getValue)
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
  proofLtNInsert' _ _ _ _ = Refl
instance (CmpNat x n1 ~ 'LT,
  ProofLtNBalance ('ForkTree ('ForkTree 'EmptyTree (Node x a) 'EmptyTree) (Node n1 a1) r) n, Show a) =>
  ProofLtNInsert' x a ('ForkTree 'EmptyTree (Node n1 a1) r) n 'LT where
  proofLtNInsert' node (ForkAVL _ node' r) pn _ =
    gcastWith (proofLtNBalance (AlmostAVL (ForkAVL EmptyAVL node EmptyAVL) node' r) pn) Refl
instance (l ~ 'ForkTree ll (Node ln lna) lr, o ~ CmpNat x ln,
  CmpNat x n1 ~ 'LT, LtN l n ~ 'True,
  Insertable x a l,
  ProofLtNInsert' x a l n1 o, ProofLtNInsert' x a l n o, ProofLtNBalance ('ForkTree (Insert' x a l o) (Node n1 a1) r) n) =>
  ProofLtNInsert' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n1 a1) r) n 'LT where
  proofLtNInsert' node (ForkAVL l node' r) pn _ =
    gcastWith (proofLtNInsert' node l pn po) $
    gcastWith (proofLtNInsert' node l (Proxy::Proxy n1) po) $
    gcastWith (proofLtNBalance (AlmostAVL l' node' r) pn) Refl
      where
        po = Proxy::Proxy o
        l' = insert node l
instance (CmpNat x n1 ~ 'GT,
  Show a,
  ProofLtNBalance ('ForkTree l (Node n1 a1) ('ForkTree 'EmptyTree (Node x a) 'EmptyTree)) n) =>
  ProofLtNInsert' x a ('ForkTree l (Node n1 a1) 'EmptyTree) n 'GT where
  proofLtNInsert' node (ForkAVL l node' _) pn _ =
    gcastWith (proofLtNBalance (AlmostAVL l node' (ForkAVL EmptyAVL node EmptyAVL)) pn) Refl
instance (r ~ 'ForkTree rl (Node rn rna) rr, o ~ CmpNat x rn,
  CmpNat x n1 ~ 'GT, LtN r n ~ 'True,
  Insertable x a r,
  ProofLtNInsert' x a r n o, ProofLtNBalance ('ForkTree l (Node n1 a1) (Insert' x a r o)) n, ProofGtNInsert' x a r n1 o) =>
  ProofLtNInsert' x a ('ForkTree l (Node n1 a1) ('ForkTree rl (Node rn rna) rr)) n 'GT where
  proofLtNInsert' node (ForkAVL l node' r) pn _ =
    gcastWith (proofLtNInsert' node r pn po) $
    gcastWith (proofGtNInsert' node r (Proxy::Proxy n1) po) $
    gcastWith (proofLtNBalance (AlmostAVL l node' r') pn) Refl
      where
        po = Proxy::Proxy o
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
  proofGtNInsert' _ _ _ _ = Refl
instance (CmpNat x n1 ~ 'LT,
  Show a,
  ProofGtNBalance ('ForkTree ('ForkTree 'EmptyTree (Node x a) 'EmptyTree) (Node n1 a1) r) n) =>
  ProofGtNInsert' x a ('ForkTree 'EmptyTree (Node n1 a1) r) n 'LT where
  proofGtNInsert' node (ForkAVL _ node' r) pn _ =
    gcastWith (proofGtNBalance (AlmostAVL (ForkAVL EmptyAVL node EmptyAVL) node' r) pn) Refl
instance (l ~ 'ForkTree ll (Node ln lna) lr, o ~ CmpNat x ln,
  CmpNat x n1 ~ 'LT, GtN l n ~ 'True,
  Insertable x a l,
  ProofGtNInsert' x a l n o, ProofGtNBalance ('ForkTree (Insert' x a l o) (Node n1 a1) r) n, ProofLtNInsert' x a l n1 o) =>
  ProofGtNInsert' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n1 a1) r) n 'LT where
  proofGtNInsert' node (ForkAVL l node' r) pn _ =
    gcastWith (proofGtNInsert' node l pn po) $
    gcastWith (proofLtNInsert' node l (Proxy::Proxy n1) po) $
    gcastWith (proofGtNBalance (AlmostAVL l' node' r) pn) Refl
      where
        po = Proxy::Proxy o
        l' = insert node l
instance (CmpNat x n1 ~ 'GT,
  Show a,
  ProofGtNBalance ('ForkTree l (Node n1 a1) ('ForkTree 'EmptyTree (Node x a) 'EmptyTree)) n) =>
  ProofGtNInsert' x a ('ForkTree l (Node n1 a1) 'EmptyTree) n 'GT where
  proofGtNInsert' node (ForkAVL l node' _) pn _ =
    gcastWith (proofGtNBalance (AlmostAVL l node' (ForkAVL EmptyAVL node EmptyAVL)) pn) Refl
instance (r ~ 'ForkTree rl (Node rn rna) rr, o ~ CmpNat x rn,
  CmpNat x n1 ~ 'GT, GtN r n ~ 'True,
  Insertable x a r,
  ProofGtNInsert' x a r n o, ProofGtNBalance ('ForkTree l (Node n1 a1) (Insert' x a r o)) n, ProofGtNInsert' x a r n1 o) =>
  ProofGtNInsert' x a ('ForkTree l (Node n1 a1) ('ForkTree rl (Node rn rna) rr)) n 'GT where
  proofGtNInsert' node (ForkAVL l node' r) pn _ =
    gcastWith (proofGtNInsert' node r pn po) $
    gcastWith (proofGtNInsert' node r (Proxy::Proxy n1) po) $
    gcastWith (proofGtNBalance (AlmostAVL l node' r') pn) Refl
      where
        po = Proxy::Proxy o
        r' = insert node r


-- | This class provides the functionality to insert a node with key 'x' and value type 'a'
-- | in an AVL 't'.
-- | The insertion is defined at the value level and the type level.
-- | The returned tree verifies the BST/AVL invariant.
class Insertable (x :: Nat) (a :: Type) (t :: Tree) where
  type Insert (x :: Nat) (a :: Type) (t :: Tree) :: Tree
  insert :: Node x a -> AVL t -> AVL (Insert x a t)
instance (Show a) =>
  Insertable x a 'EmptyTree where
  type Insert x a 'EmptyTree = 'ForkTree 'EmptyTree (Node x a) 'EmptyTree
  insert node _ = ForkAVL EmptyAVL node' EmptyAVL
    where
      node' = mkNode (Proxy::Proxy x) (getValue node)
instance (o ~ CmpNat x n,
  Insertable' x a ('ForkTree l (Node n a1) r) o) =>
  Insertable x a ('ForkTree l (Node n a1) r) where
  type Insert x a ('ForkTree l (Node n a1) r) = Insert' x a ('ForkTree l (Node n a1) r) (CmpNat x n)
  insert node t = insert' node t (Proxy::Proxy o)

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
  insert' node (ForkAVL l _ r) _ = ForkAVL l node' r
    where
      node' = mkNode (Proxy::Proxy n) (getValue node)
instance (CmpNat x n ~ 'LT,
  Show a, Balanceable ('ForkTree ('ForkTree 'EmptyTree (Node x a) 'EmptyTree) (Node n a1) r)) =>
  Insertable' x a ('ForkTree 'EmptyTree (Node n a1) r) 'LT where
  type Insert' x a ('ForkTree 'EmptyTree (Node n a1) r) 'LT = Balance ('ForkTree ('ForkTree 'EmptyTree (Node x a) 'EmptyTree) (Node n a1) r)
  insert' node (ForkAVL _ node' r) _ = balance (AlmostAVL (ForkAVL EmptyAVL node EmptyAVL) node' r)
instance (l ~ 'ForkTree ll (Node ln lna) lr, o ~ CmpNat x ln,
  CmpNat x n ~ 'LT,
  Insertable' x a l o, Balanceable ('ForkTree (Insert' x a l o) (Node n a1) r),
  ProofLtNInsert' x a l n o) =>
  Insertable' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n a1) r) 'LT where
  type Insert' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n a1) r) 'LT =
    Balance ('ForkTree (Insert' x a ('ForkTree ll (Node ln lna) lr) (CmpNat x ln)) (Node n a1) r)
  insert' node (ForkAVL l node' r) _ =
    gcastWith (proofLtNInsert' node l (Proxy::Proxy n) po) $
    balance $ AlmostAVL (insert' node l po) node' r
      where
        po = Proxy::Proxy o
instance (CmpNat x n ~ 'GT,
  Show a, Balanceable ('ForkTree l (Node n a1) ('ForkTree 'EmptyTree (Node x a) 'EmptyTree))) =>
  Insertable' x a ('ForkTree l (Node n a1) 'EmptyTree) 'GT where
  type Insert' x a ('ForkTree l (Node n a1) 'EmptyTree) 'GT = Balance ('ForkTree l (Node n a1) ('ForkTree 'EmptyTree (Node x a) 'EmptyTree))
  insert' node (ForkAVL l node' _) _ = balance (AlmostAVL l node' (ForkAVL EmptyAVL node EmptyAVL))
instance (r ~ 'ForkTree rl (Node rn rna) rr, o ~ CmpNat x rn,
  CmpNat x n ~ 'GT,
  Insertable' x a r o, Balanceable ('ForkTree l (Node n a1) (Insert' x a r o)),
  ProofGtNInsert' x a r n o) =>
  Insertable' x a ('ForkTree l (Node n a1) ('ForkTree rl (Node rn rna) rr)) 'GT where
  type Insert' x a ('ForkTree l (Node n a1) ('ForkTree rl (Node rn rna) rr)) 'GT =
    Balance ('ForkTree l (Node n a1) (Insert' x a ('ForkTree rl (Node rn rna) rr) (CmpNat x rn)))
  insert' node (ForkAVL l node' r) _ =
    gcastWith (proofGtNInsert' node r (Proxy::Proxy n) po) $
    balance $ AlmostAVL l node' (insert' node r po)
      where
        po = Proxy::Proxy o
