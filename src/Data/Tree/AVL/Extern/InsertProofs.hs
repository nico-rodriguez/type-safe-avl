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

module Data.Tree.AVL.Extern.InsertProofs (
  ProofIsAVLInsert(proofIsAVLInsert),
  ProofIsBSTInsert(proofIsBSTInsert)
) where

import           Data.Kind                          (Type)
import           Data.Proxy                         (Proxy (Proxy))
import           Data.Tree.AVL.Extern.BalanceProofs (ProofGtNBalance (proofGtNBalance),
                                                     ProofIsAVLBalance (proofIsAVLBalance),
                                                     ProofIsBSTBalance (proofIsBSTBalance),
                                                     ProofLtNBalance (proofLtNBalance))
import           Data.Tree.AVL.Extern.Insert        (Insert',
                                                     Insertable (Insert))
import           Data.Tree.AVL.Invariants           (IsAVLT(EmptyIsAVLT,ForkIsAVLT), IsAlmostAVLT(ForkIsAlmostAVLT))
import           Data.Tree.BST.Invariants           (GtN, IsBSTT(EmptyIsBSTT,ForkIsBSTT), LtN)
import           Data.Tree.ITree                    (Tree (EmptyTree, ForkTree))
import           Data.Tree.Node                     (Node)
import           Data.Type.Equality                 ((:~:) (Refl), gcastWith)
import           GHC.TypeNats                       (CmpNat, Nat)
import           Prelude                            (Bool (True), undefined,
                                                     Ordering (EQ, GT, LT), ($))


-- | Prove that inserting a node with key 'x' and element value 'a'
-- | in a BST tree preserves BST condition.
class ProofIsBSTInsert (x :: Nat) (a :: Type) (t :: Tree) where
  proofIsBSTInsert :: Node x a -> IsBSTT t -> IsBSTT (Insert x a t)
instance ProofIsBSTInsert x a 'EmptyTree where
  proofIsBSTInsert node _ = ForkIsBSTT EmptyIsBSTT node EmptyIsBSTT
instance ProofIsBSTInsert' x a ('ForkTree l (Node n a1) r) (CmpNat x n) =>
  ProofIsBSTInsert x a ('ForkTree l (Node n a1) r) where
  proofIsBSTInsert node tIsBST = proofIsBSTInsert' node tIsBST (Proxy::Proxy (CmpNat x n))

-- | Prove that inserting a node with key 'x' and element value 'a'
-- | in a BST tree preserves BST condition, given that the comparison between
-- | 'x' and the root key of the tree equals 'o'.
-- | The BST invariant was already check when proofIsBSTInsert was called before.
-- | The 'o' parameter guides the proof.
class ProofIsBSTInsert' (x :: Nat) (a :: Type) (t :: Tree) (o :: Ordering) where
  proofIsBSTInsert' :: Node x a -> IsBSTT t -> Proxy o -> IsBSTT (Insert' x a t o)
instance ProofIsBSTInsert' x a ('ForkTree l (Node n a1) r) 'EQ where
  proofIsBSTInsert' _ (ForkIsBSTT lIsBST _ rIsBST) _ = ForkIsBSTT lIsBST (undefined::Node n a) rIsBST
instance (CmpNat x n ~ 'LT, ProofIsBSTBalance ('ForkTree ('ForkTree 'EmptyTree (Node x a) 'EmptyTree) (Node n a1) r)) =>
  ProofIsBSTInsert' x a ('ForkTree 'EmptyTree (Node n a1) r) 'LT where
  proofIsBSTInsert' node (ForkIsBSTT _EmptyITree node' rIsBST) _ =
    proofIsBSTBalance $ ForkIsBSTT (ForkIsBSTT EmptyIsBSTT node EmptyIsBSTT) node' rIsBST
instance (l ~ 'ForkTree ll (Node ln lna) lr, CmpNat x n ~ 'LT, o ~ CmpNat x ln,
  ProofIsBSTInsert' x a l o, ProofLtNInsert' x a l n o,
  ProofIsBSTBalance ('ForkTree (Insert' x a l o) (Node n a1) r)) =>
  ProofIsBSTInsert' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n a1) r) 'LT where
  proofIsBSTInsert' node (ForkIsBSTT lIsBST node' rIsBST) _ =
    gcastWith (proofLtNInsert' node lIsBST (Proxy::Proxy n) (Proxy::Proxy o)) $
    proofIsBSTBalance $ ForkIsBSTT (proofIsBSTInsert node lIsBST) node' rIsBST
instance (CmpNat x n ~ 'GT, ProofIsBSTBalance ('ForkTree l (Node n a1) ('ForkTree 'EmptyTree (Node x a) 'EmptyTree))) =>
  ProofIsBSTInsert' x a ('ForkTree l (Node n a1) 'EmptyTree) 'GT where
  proofIsBSTInsert' node (ForkIsBSTT lIsBST node' _) _ =
    proofIsBSTBalance (ForkIsBSTT lIsBST node' (ForkIsBSTT EmptyIsBSTT node EmptyIsBSTT))
instance (r ~ 'ForkTree rl (Node rn rna) rr, CmpNat x n ~ 'GT, o ~ CmpNat x rn,
  ProofGtNInsert' x a r n o, ProofIsBSTInsert' x a r o,
  ProofIsBSTBalance ('ForkTree l (Node n a1) (Insert' x a ('ForkTree rl (Node rn rna) rr) o))) =>
  ProofIsBSTInsert' x a ('ForkTree l (Node n a1) ('ForkTree rl (Node rn rna) rr)) 'GT where
  proofIsBSTInsert' node (ForkIsBSTT lIsBST node' rIsBST) _ =
    gcastWith (proofGtNInsert' node rIsBST (Proxy::Proxy n) (Proxy::Proxy o)) $
    proofIsBSTBalance $ ForkIsBSTT lIsBST node' (proofIsBSTInsert node rIsBST)


-- | Prove that inserting a node with key 'x' (lower than 'n') and element value 'a'
-- | in a tree 't' which verifies 'LtN t n ~ 'True' preserves the LtN invariant,
-- | given that the comparison between 'x' and the root key of the tree equals 'o'.
-- | The 'o' parameter guides the proof.
class ProofLtNInsert' (x :: Nat) (a :: Type) (t :: Tree) (n :: Nat) (o :: Ordering) where
  proofLtNInsert' :: (CmpNat x n ~ 'LT, LtN t n ~ 'True) =>
    Node x a -> IsBSTT t -> Proxy n -> Proxy o -> LtN (Insert' x a t o) n :~: 'True
instance ProofLtNInsert' x a ('ForkTree l (Node n1 a1) r) n 'EQ where
  proofLtNInsert' _ _ _ _ = Refl
instance (ProofLtNBalance ('ForkTree ('ForkTree 'EmptyTree (Node x a) 'EmptyTree) (Node n1 a1) r) n,
  CmpNat x n1 ~ 'LT) =>
  ProofLtNInsert' x a ('ForkTree 'EmptyTree (Node n1 a1) r) n 'LT where
  proofLtNInsert' node (ForkIsBSTT _ node' rIsBST) pn _ =
    gcastWith (proofLtNBalance (ForkIsBSTT (ForkIsBSTT EmptyIsBSTT node EmptyIsBSTT) node' rIsBST) pn) Refl
instance (l ~ 'ForkTree ll (Node ln lna) lr, ProofLtNInsert' x a l n o, ProofLtNInsert' x a l n1 o,
  CmpNat x n1 ~ 'LT, o ~ CmpNat x ln, LtN l n ~ 'True, ProofIsBSTInsert x a l,
  ProofLtNBalance ('ForkTree (Insert' x a l o) (Node n1 a1) r) n) =>
  ProofLtNInsert' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n1 a1) r) n 'LT where
  proofLtNInsert' node (ForkIsBSTT lIsBST node' rIsBST) pn _ =
    gcastWith (proofLtNInsert' node lIsBST pn (Proxy::Proxy o)) $
    gcastWith (proofLtNInsert' node lIsBST (Proxy::Proxy n1) (Proxy::Proxy o)) $
    gcastWith (proofLtNBalance (ForkIsBSTT lIsBST' node' rIsBST) pn) Refl
      where
        lIsBST' = proofIsBSTInsert node lIsBST
instance (ProofLtNBalance ('ForkTree l (Node n1 a1) ('ForkTree 'EmptyTree (Node x a) 'EmptyTree)) n,
  CmpNat x n1 ~ 'GT) =>
  ProofLtNInsert' x a ('ForkTree l (Node n1 a1) 'EmptyTree) n 'GT where
  proofLtNInsert' node (ForkIsBSTT lIsBST node' _) pn _ =
    gcastWith (proofLtNBalance (ForkIsBSTT lIsBST node' (ForkIsBSTT EmptyIsBSTT node EmptyIsBSTT)) pn) Refl
instance (r ~ 'ForkTree rl (Node rn rna) rr, o ~ CmpNat x rn, ProofLtNInsert' x a r n o, LtN r n ~ 'True,
  ProofGtNInsert' x a r n1 o, ProofIsBSTInsert x a r,
  CmpNat x n1 ~ 'GT, ProofLtNBalance ('ForkTree l (Node n1 a1) (Insert' x a r o)) n, CmpNat n1 n ~ 'LT) =>
  ProofLtNInsert' x a ('ForkTree l (Node n1 a1) ('ForkTree rl (Node rn rna) rr)) n 'GT where
  proofLtNInsert' node (ForkIsBSTT lIsBST node' rIsBST) pn _ =
    gcastWith (proofLtNInsert' node rIsBST pn (Proxy::Proxy o)) $
    gcastWith (proofGtNInsert' node rIsBST (Proxy::Proxy n1) (Proxy::Proxy o)) $
    gcastWith (proofLtNBalance (ForkIsBSTT lIsBST node' rIsBST') pn) Refl
      where
        rIsBST' = proofIsBSTInsert node rIsBST


-- | Prove that inserting a node with key 'x' (greater than 'n') and element value 'a'
-- | in a tree 't' which verifies 'GtN t n ~ 'True' preserves the GtN invariant,
-- | given that the comparison between 'x' and the root key of the tree equals 'o'.
-- | The 'o' parameter guides the proof.
class ProofGtNInsert' (x :: Nat) (a :: Type) (t :: Tree) (n :: Nat) (o :: Ordering) where
  proofGtNInsert' :: (CmpNat x n ~ 'GT, GtN t n ~ 'True) =>
    Node x a -> IsBSTT t -> Proxy n -> Proxy o -> GtN (Insert' x a t o) n :~: 'True
instance ProofGtNInsert' x a ('ForkTree l (Node n1 a1) r) n 'EQ where
  proofGtNInsert' _ _ _ _ = Refl
instance (ProofGtNBalance ('ForkTree ('ForkTree 'EmptyTree (Node x a) 'EmptyTree) (Node n1 a1) r) n,
  CmpNat x n1 ~ 'LT) =>
  ProofGtNInsert' x a ('ForkTree 'EmptyTree (Node n1 a1) r) n 'LT where
  proofGtNInsert' node (ForkIsBSTT _ node' rIsBST) pn _ =
    gcastWith (proofGtNBalance (ForkIsBSTT (ForkIsBSTT EmptyIsBSTT node EmptyIsBSTT) node' rIsBST) pn) Refl
instance (l ~ 'ForkTree ll (Node ln lna) lr, o ~ CmpNat x ln, ProofGtNInsert' x a l n o, CmpNat x n1 ~ 'LT,
  ProofLtNInsert' x a l n1 o, ProofIsBSTInsert x a l,
  GtN l n ~ 'True, ProofGtNBalance ('ForkTree (Insert' x a l o) (Node n1 a1) r) n) =>
  ProofGtNInsert' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n1 a1) r) n 'LT where
  proofGtNInsert' node (ForkIsBSTT lIsBST node' rIsBST) pn _ =
    gcastWith (proofGtNInsert' node lIsBST pn (Proxy::Proxy o)) $
    gcastWith (proofLtNInsert' node lIsBST (Proxy::Proxy n1) (Proxy::Proxy o)) $
    gcastWith (proofGtNBalance (ForkIsBSTT lIsBST' node' rIsBST) pn) Refl
      where
        lIsBST' = proofIsBSTInsert node lIsBST
instance (ProofGtNBalance ('ForkTree l (Node n1 a1) ('ForkTree 'EmptyTree (Node x a) 'EmptyTree)) n,
  CmpNat x n1 ~ 'GT) =>
  ProofGtNInsert' x a ('ForkTree l (Node n1 a1) 'EmptyTree) n 'GT where
  proofGtNInsert' node (ForkIsBSTT lIsBST node' _) pn _ =
    gcastWith (proofGtNBalance (ForkIsBSTT lIsBST node' (ForkIsBSTT EmptyIsBSTT node EmptyIsBSTT)) pn) Refl
instance (r ~ 'ForkTree rl (Node rn rna) rr, o ~ CmpNat x rn, ProofGtNInsert' x a r n o, GtN r n ~ 'True,
  ProofGtNInsert' x a r n1 o, ProofIsBSTInsert x a r,
  CmpNat x n1 ~ 'GT, ProofGtNBalance ('ForkTree l (Node n1 a1) (Insert' x a r o)) n) =>
  ProofGtNInsert' x a ('ForkTree l (Node n1 a1) ('ForkTree rl (Node rn rna) rr)) n 'GT where
  proofGtNInsert' node (ForkIsBSTT lIsBST node' rIsBST) pn _ =
    gcastWith (proofGtNInsert' node rIsBST pn (Proxy::Proxy o)) $
    gcastWith (proofGtNInsert' node rIsBST (Proxy::Proxy n1) (Proxy::Proxy o)) $
    gcastWith (proofGtNBalance (ForkIsBSTT lIsBST node' rIsBST') pn) Refl
      where
        rIsBST' = proofIsBSTInsert node rIsBST


-- | Prove that inserting a node with key 'x' and element value 'a'
-- | in an AVL tree preserves the AVL condition.
class ProofIsAVLInsert (x :: Nat) (a :: Type) (t :: Tree) where
  proofIsAVLInsert :: Node x a -> IsAVLT t -> IsAVLT (Insert x a t)
instance ProofIsAVLInsert x a 'EmptyTree where
  proofIsAVLInsert node _ = ForkIsAVLT EmptyIsAVLT node EmptyIsAVLT
instance (o ~ CmpNat x n, ProofIsAVLInsert' x a ('ForkTree l (Node n a1) r) o) =>
  ProofIsAVLInsert x a ('ForkTree l (Node n a1) r) where
  proofIsAVLInsert node tIsAVL = proofIsAVLInsert' node tIsAVL (Proxy::Proxy o)

-- | Prove that inserting a node with key 'x' and element value 'a'
-- | in an AVL tree preserves the AVL condition, given that the comparison between
-- | 'x' and the root key of the tree equals 'o'.
-- | The AVL invariant was already check when proofIsBSTInsert was called before.
-- | The 'o' parameter guides the proof.
class ProofIsAVLInsert' (x :: Nat) (a :: Type) (t :: Tree) (o :: Ordering) where
  proofIsAVLInsert' :: Node x a -> IsAVLT t -> Proxy o -> IsAVLT (Insert' x a t o)
instance (x ~ n) => ProofIsAVLInsert' x a ('ForkTree l (Node n a1) r) 'EQ where
  proofIsAVLInsert' node (ForkIsAVLT lIsAVL _ rIsAVL) _ = ForkIsAVLT lIsAVL node rIsAVL
instance (ProofIsAVLBalance ('ForkTree ('ForkTree 'EmptyTree (Node x a) 'EmptyTree) (Node n a1) r)) =>
  ProofIsAVLInsert' x a ('ForkTree 'EmptyTree (Node n a1) r) 'LT where
  proofIsAVLInsert' node (ForkIsAVLT _ node' rIsAVL) _ =
    proofIsAVLBalance (ForkIsAlmostAVLT (ForkIsAVLT EmptyIsAVLT node EmptyIsAVLT) node' rIsAVL)
instance (l ~ 'ForkTree ll (Node ln lna) lr, ProofIsAVLInsert' x a l (CmpNat x ln),
  ProofIsAVLBalance ('ForkTree (Insert' x a l (CmpNat x ln)) (Node n a1) r)) =>
  ProofIsAVLInsert' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n a1) r) 'LT where
  proofIsAVLInsert' node (ForkIsAVLT lIsAVL node' rIsAVL) _ =
    proofIsAVLBalance $ ForkIsAlmostAVLT lIsAVL' node' rIsAVL
      where
        lIsAVL' = proofIsAVLInsert node lIsAVL
instance (ProofIsAVLBalance ('ForkTree l (Node n a1) ('ForkTree 'EmptyTree (Node x a) 'EmptyTree))) =>
  ProofIsAVLInsert' x a ('ForkTree l (Node n a1) 'EmptyTree) 'GT where
  proofIsAVLInsert' node (ForkIsAVLT lIsAVL node' _) _ =
    proofIsAVLBalance $ ForkIsAlmostAVLT lIsAVL node' (ForkIsAVLT EmptyIsAVLT node EmptyIsAVLT)
instance (r ~ 'ForkTree rl (Node rn rna) rr, ProofIsAVLInsert' x a r (CmpNat x rn),
  ProofIsAVLBalance ('ForkTree l (Node n a1) (Insert' x a ('ForkTree rl (Node rn rna) rr) (CmpNat x rn)))) =>
  ProofIsAVLInsert' x a ('ForkTree l (Node n a1) ('ForkTree rl (Node rn rna) rr)) 'GT where
  proofIsAVLInsert' node (ForkIsAVLT lIsAVL node' rIsAVL) _ =
    proofIsAVLBalance $ ForkIsAlmostAVLT lIsAVL node' rIsAVL'
      where
        rIsAVL' = proofIsAVLInsert node rIsAVL
