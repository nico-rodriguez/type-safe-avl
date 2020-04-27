# Project Title

One Paragraph of project description goes here

## Summary

- [Prerequisites](#prerequisites)
- [Installing](#installing)
- [Project Structure](#project-structure)
- [Interface](#interface)
- [Examples](#examples)
- [Benchmark](#benchmark)

## Prerequisites

```Shell
ghc (>= 8.4.3)  # GHC Haskell compiler
make (>=4.2.1)  # GNU Make
```

No external Haskell libraries are needed.

## Installing

To get a running copy of the project, simply clone this repository:

```Shell
git clone https://github.com/nico-rodriguez/balanced-binary-search-tree.git
```

## Project Structure

```Shell
balanced-binary-search-tree
│   README.md
│   Makefile
│   benchmark.sh
│   ITree.hs
│   Node.hs
│
└───Benchmark
│   │
│   └───Extern
│   │
│   └───FullExtern
│   │
│   └───Intern
│
└───Extern
│   │   AVL.hs
│   │   AVLOperations.hs
│   │   AVLProofs.hs
│   │   BST.hs
│   │   BSTOperations.hs
│   │   BSTProofs.hs
│   │   Examples.hs
│
└───FullExtern
│   │   AVL.hs
│   │   BST.hs
│   │   Examples.hs
│
└───Intern
    │   AVL.hs
    │   AVLOperations.hs
    │   BST.hs
    │   BSTOperations.hs
    │   Examples.hs
```

- `ITree.hs` implements the `Tree` and `ITree` data types.

- `Node.hs` implements the nodes of the trees.

- `Extern` contains the implementation of AVL trees and its operations for the externalist approach; likewise, `Intern` folder contains the implementation for the internalist approach. Notice that there isn't a `BSTProofs.hs` neither a `AVLProofs.hs` inside `Inter`. That's because the proofs and operations in the internalist approach are implemented together (in `BSTOperations.hs` and `AVLOperations.hs`).

- `FullExtern` contains the implementation of the full externalist approach. It provides functionality for performing operations over trees and checking the invariants at the end.

- `Extern`, `FullExtern` and `Intern` have an `Examples.hs` with usage examples of the BST/AVL operations.

- `Extern`, `FullExtern` and `Intern` have a `BST.hs` and `AVL.hs`. These are the main modules for the BST/AVL trees and their operations. In order to use BST/AVL trees, only these modules need to be imported (and only import them for one approach; for instance, do not import `AVL.hs` from both `Extern` and `Inter` since it would cause an error due to duplicated names).

## Interface

Both externalist and internalist approaches have a common interface for manipulating BST/AVL trees. The difference in their implementation is the approach taken when defining the structural invariants that represent the conditions for a tree to be BST/AVL.

### BST trees (some function constraints omitted)

- `emptyBST :: BST 'EmptyTree`, an empty BST tree.

- `insertBST :: Proxy x -> a -> BST t -> BST (Insert x a t)`, inserts a value of type `a` with key `x` in a BST of type `BST t`. If the tree already has a node with key `x`, the value is updated.

- `lookupBST :: (t ~ 'ForkTree l (Node n a1) r, Member x t ~ 'True) => Proxy x -> BST t -> a`, returns the value, of type `a`, that is associated to the key `x` in the BST tree of type `BST t`. The constraint `t ~ 'ForkTree l (Node n a1) r` checks at compile time if the tree is not empty; the constraint `Member x t ~ 'True` checks at compile time that the tree `t` has a node with key `x` (ensuring that the lookup will return some value).

- `deleteBST :: Proxy x -> BST t -> BST (Delete x t)`, deletes the node with key `x` in a BST of type `BST t`. If the tree doesn't have a node with key `x`, it just returns the original tree.

### AVL trees (some function constraints omitted)

- `emptyAVL :: AVL 'EmptyTree`.

- `insertAVL :: Proxy x -> a -> AVL t -> AVL (Insert x a t)`.

- `lookupAVL :: (t ~ 'ForkTree l (Node n a1) r, Member x t ~ 'True) => Proxy x -> AVL t -> a`.

- `deleteAVL :: Proxy x -> AVL t -> AVL (Delete x t)`.

Analogous comments hold for the AVL interface.

For the full externalist approach, the interface is

- `EmptyITree :: ITree 'EmptyTree`.

- `insert :: Node x a -> ITree t -> ITree (Insert x a t)`.

- `lookup :: (t ~ 'ForkTree l (Node n a1) r, Member x t ~ 'True) => Proxy x -> ITree t -> a`.

- `delete :: Proxy x -> ITree t ->  (Delete x t)`.

- `AVL :: (IsBST t ~ 'True, IsAVL t ~ 'True) => ITree t -> AVL t`

- `BST :: (IsBST t ~ 'True) => ITree t -> BST t`

- `proofIsBST :: ITree t -> IsBST t :~: 'True`

- `proofIsAVL :: ITree t -> IsAVL t :~: 'True`

## Examples

For more usage examples see the `Examples.hs` file for each approach.

### Extern

```haskell
import Proxy (Proxy(Proxy))
import Extern.BST (emptyBST,insertBST,lookupBST,deleteBST)
import Extern.AVL (emptyAVL,insertAVL,lookupAVL,deleteAVL)

bste = emptyBST

# Insert value 'f' with key 4
bst1 = insertBST (Proxy::Proxy 4) 'f' bste
# Insert value [1,2] with key 2
bst2 = insertBST (Proxy::Proxy 2) [1,2] bst1

# list = [1,2]
list = lookupBST (Proxy::Proxy 2) bst2
# Following line gives error at compile time because bst2 doesn't have key 3
# lookupBST (Proxy::Proxy 3) bst2

# Delete node with key 4
bst3 = deleteBST (Proxy::Proxy 4) bst 2
# Following line gives error at compile time because bst2 doesn't have key 1
# deleteBST (Proxy::Proxy 1) bst2
```

The previous example used BST tree. For using AVL trees just replace

```Shell
emptyBST -> emptyAVL
insertBST -> insertAVL
lookupBST -> lookupAVL
deleteBST -> deleteAVL
```

Notice that operations for BST may only be applied to BST trees, and operations for AVL trees may only be applied for AVL trees. For instance, this is not possible (gives error at compile time):

```haskell
insertAVL (Proxy::Proxy 5) bst2
```

because `bst2` is not an AVL tree.

### Full Extern

A full externalist approach means grouping the operations and only perform the check of the invariants at the end (instead of checking the invariants after each operation)

```haskell
import           Data.Proxy (Proxy (Proxy))
import           Data.Type.Equality   (gcastWith)
import           FullExtern.AVL (delete, ITree(EmptyITree), insert, lookup, AVL(AVL),
                                ProofIsAVL(proofIsAVL))
import           Node (mkNode)

-- Insert four values in a row and check the BST and AVL invariants at the end
avl = gcastWith (proofIsAVL t) $ gcastWith (proofIsBST t) $ AVL t
    where
        t = insert (mkNode (Proxy::Proxy 4) 'f') $ insert (mkNode (Proxy::Proxy 3) True) $ insert (mkNode (Proxy::Proxy 5) [1,2,3]) $ EmptyTree

-- For performing a lookup, it's necessary to take the ITree 't' out of the AVL constructor
l1' = case avl of
    AVL t -> lookup (Proxy::Proxy 3) t

-- | Error at compile time: key 1 is not in the tree avl
-- err = case avl of
--     AVL t -> lookup p1 t
-- For performing deletions, it's necessary to take the ITree 't' out of the AVL constructor
avlt2 = case avl of
AVL t -> gcastWith (proofIsAVL t') $ gcastWith (proofIsBST t') $ AVL t'
            where
                t' = delete (Proxy::Proxy 3) $ delete (Proxy::Proxy 4) $ delete (Proxy::Proxy 5) $ t
```

### Intern

For using the internalist approach, the code example for the externalist approach works, with the only difference in the import list:

```haskell
import Intern.BST (emptyBST,insertBST,lookupBST,deleteBST)
-- Instead of import Extern.BST (emptyBST,insertBST,lookupBST,deleteBST)

import Intern.AVL (emptyAVL,insertAVL,lookupAVL,deleteAVL)
-- Instead of import Extern.AVL (emptyAVL,insertAVL,lookupAVL,deleteAVL)
```

## Benchmark

### Break down

TODO: Explain which benchmarks were defined

### Running the benchmark

TODO: Explain how to run the automated benchmark
