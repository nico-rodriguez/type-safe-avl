# Type-safe BST and AVL trees

Implementation of type-safe BST and AVL trees, following four different approaches: unsafe, full externalist, externalist and internalist.

## Summary

- [Prerequisites](#prerequisites)
- [Installing](#installing)
- [Project Structure](#project-structure)
- [Interface](#interface)
- [Examples](#examples)
- [Benchmark](#benchmark)

## Prerequisites

```Shell
ghc (>= 8.10.2)     # GHC Haskell compiler
cabal (>=3.4.0.0)   # Haskell package manager
python3 (>=3.6.8)   # Python interpreter (only needed for running the benchmarks)
Some python package manager (e.g., pip)
```

No external Haskell libraries are needed.

## Installing

To get a running copy of the project, simply clone this repository:

```Shell
git clone https://github.com/nico-rodriguez/balanced-binary-search-tree.git
```

The project may be used locally (inside the folder with `cabal repl`) or it may be install for system wide use:

```Shell
cd balanced-binary-search-tree
cabal install --lib
```

For usage examples see the [Examples](#examples) section.

## Project Structure

```Shell
balanced-binary-search-tree
│   README.md
└───benchmark
|    │
│    └── ...
└───src/Data/Tree
    │   ITree.hs
    │   Node.hs
    └───AVL
    │   │   FullExtern.hs
    │   │   Extern.hs
    │   │   Intern.hs
    │   │   Unsafe.hs
    │   │   Invariants.hs
    │   └───FullExtern
    │   │   │   Examples.hs
    │   └───Extern
    │   │   │   Constructor.hs
    │   │   │   Balance.hs, BalanceProofs.hs
    │   │   │   Insert.hs, InsertProofs.hs
    │   │   │   Lookup.hs
    │   │   │   Delete.hs, DeleteProofs.hs
    │   │   │   Examples.hs
    │   └───Intern
    │   │   │   Constructor.hs
    │   │   │   Balance.hs
    │   │   │   Insert.hs
    │   │   │   Lookup.hs
    │   │   │   Delete.hs
    │   │   │   Examples.hs
    │   └───Unsafe
    │       │   Examples.hs
    └───BST
        │
        └── ...
```

- For details on the `benchmark` directory, see the [Benchmark](#benchmark) section.

- `ITree.hs` implements the `Tree` and `ITree` data types.

- `Node.hs` implements the nodes of the trees.

- Both `ITree.hs` and `Node.hs` are used in both BST and AVL type-safe trees.

- The structure of `Data/Tree/AVL` and `Data/Tree/BST` is similar.

- `Data/Tree/{BST,AVL}/Invariants.hs` implements the BST,AVL invariants, like what it means for a tree to be BST,AVL.

- `Data/Tree/AVL/Unsafe.hs` contains an unsafe implementation of AVL trees (notice there's also an unsafe implementation of BST in the `BST` directory). The code was extracted and refactored from that in `Data/Tree/AVL/Extern/{Balance,Insert,Lookup,Delete}.hs`, 'un-lifting' the type level computations to the value level.

- `FullExtern.hs` contains the implementation of the full externalist approach. It provides functionality for performing operations over trees and checking the invariants at the end.

- `Extern.hs` provides the implementation of BST/AVL trees and its operations for the externalist approach; likewise, `Intern` folder contains the implementation for the internalist approach. Notice that there isn't a `*Proofs.hs` inside `Intern`. That's because the proofs and operations in the internalist approach are implemented together (in `{Balance,Insert,Lookup,Delete}.hs`).

- All four approaches, `Unsafe`, `FullExtern`, `Extern`, and `Intern`, have an `Examples.hs` with usage examples of the BST/AVL operations.

- In order to use BST/AVL trees, only one of `Usafe.hs`, `FullExtern.hs`, `Extern.hs` or `Intern.hs` need to be imported. They all export the basic functionality for inserting, looking and deleting on the corresponding BST/AVL tree. See the [Interface](#interface) section for exploring the functions exported by each of them.

## Interface

Both externalist and internalist approaches have a common interface for manipulating BST/AVL trees. The difference in their implementation is the approach taken when defining the structural invariants that represent the conditions for a tree to be BST/AVL.

The interface for the unsafe and the full externalist approaches are only slightly different.

### AVL trees (some function constraints omitted)

For the unsafe approach, the interface is

- `emptyAVL :: AVL a`, an empty ITree.

- `insertAVL :: Show a => Int -> a -> AVL a -> AVL a`.

- `lookupAVL :: Int -> AVL a -> Maybe a`.

- `deleteAVL :: Int -> AVL a -> AVL a`.

For the full externalist approach, the interface is:

- `EmptyITree :: ITree 'EmptyTree`, an empty ITree.

- `insert :: Node x a -> ITree t -> ITree (Insert x a t)`, inserts a node in a ITree. If the tree already has a node with key `x`, the value is updated.

- `lookup :: (t ~ 'ForkTree l (Node n a1) r, Member x t ~ 'True) => Proxy x -> ITree t -> a`, lookup a key in a ITree.

- `delete :: Proxy x -> ITree t ->  (Delete x t)`, delete a node with a given key in a ITree. If the tree doesn't have a node with key `x`, it just returns the original tree.

- `AVL :: (IsBST t ~ 'True, IsAVL t ~ 'True) => ITree t -> AVL t`, constructor for type-safe AVL trees.

- `mkAVL :: (IsBSTC t, IsAVLC t) => ITree t -> AVL t`, constructor for type-safe AVL from an ITree, with automatic proof term construction.

For the externalist and internalist approaches, the interface is the same and is as follows:

- `emptyAVL :: AVL 'EmptyTree`, an empty AVL tree.

- `insertAVL :: Proxy x -> a -> AVL t -> AVL (Insert x a t)`, inserts a value of type `a` with key `x` in an AVL of type `AVL t`. If the tree already has a node with key `x`, the value is updated.

- `lookupAVL :: (t ~ 'ForkTree l (Node n a1) r, Member x t ~ 'True) => Proxy x -> AVL t -> a`, returns the value, of type `a`, that is associated to the key `x` in the AVL tree of type `AVL t`. The constraint `t ~ 'ForkTree l (Node n a1) r` checks at compile time if the tree is not empty; the constraint `Member x t ~ 'True` checks at compile time that the tree `t` has a node with key `x` (ensuring that the lookup will return some value).

- `deleteAVL :: Proxy x -> AVL t -> AVL (Delete x t)`, deletes the node with key `x` in an AVL of type `AVL t`. If the tree doesn't have a node with key `x`, it just returns the original tree.

### BST trees (some function constraints omitted)

The interfaces are analogous to those for AVL trees. Just replace "AVL" for "BST" in the functions names.

## Examples

For more usage examples see the `Examples.hs` file for each approach.

### Full Extern

A full externalist approach means grouping the operations and only perform the check of the invariants at the end:

```haskell
import           Data.Proxy               (Proxy (Proxy))
import           Data.Tree.AVL.FullExtern (AVL (AVL), mkAVL, ITree (EmptyITree),
                                           delete, insert, lookup)
import           Data.Tree.ITree          (ITree(EmptyITree,ForkITree))
import           Data.Tree.Node           (mkNode)
import           Prelude                  (Bool (False, True), Float, Int, ($))


-- | Proxies for the node keys
p0 = Proxy :: Proxy 0
p1 = Proxy :: Proxy 1
p2 = Proxy :: Proxy 2
p3 = Proxy :: Proxy 3
p4 = Proxy :: Proxy 4
p5 = Proxy :: Proxy 5
p6 = Proxy :: Proxy 6
p7 = Proxy :: Proxy 7

-- | Insert several values in a row and check the BST and AVL invariants at the end
avl = mkAVL t
  where
    t = insert (mkNode p4 'f') $ insert (mkNode p2 (4::Int)) $
        insert (mkNode p6 "lala") $ insert (mkNode p3 True) $
        insert (mkNode p5 ([1,2,3]::[Int])) $ insert (mkNode p0 (1.8::Float)) $
        insert (mkNode p7 [False]) EmptyITree

-- | For performing a lookup, it's necessary to take the ITree 't' out of the AVL constructor
l1' = case avl of
    AVL t -> lookup (Proxy::Proxy 3) t

-- | Compile time error: key 1 is not in the tree avl and left subtree at node with key 4 has height +2 greater than the right subtree
-- avlError = mkAVL $
--   ForkITree (ForkITree
--             (ForkITree
--               EmptyITree (mkNode p0 'a') EmptyITree)
--               (mkNode p7 4)
--               EmptyITree)
--             (mkNode p4 'f')
--             EmptyITree

-- | Delete several values in a row and check the BST and AVL invariants at the end
avlt2 = case avl of
AVL t -> gcastWith (proofIsAVL t') $ gcastWith (proofIsBST t') $ AVL t'
            where
                t' = delete (Proxy::Proxy 3) $ delete (Proxy::Proxy 4) $ delete (Proxy::Proxy 5) $ t
```

### Extern and Inter

In the externalist and internalist approaches, the invariants are checked after every operation performed over the tree.

Some examples for the externalist approach:

```haskell
import           Data.Proxy           (Proxy (Proxy))
import           Data.Tree.AVL.Extern (deleteAVL, emptyAVL, insertAVL,
                                       lookupAVL)
import           Prelude              (Bool (False, True), Float, Int, String)


-- | Proxies for the node keys
p2 = Proxy :: Proxy 2
p4 = Proxy :: Proxy 4
p7 = Proxy :: Proxy 7


-- Insert value 'f' with key 4
avlt1 = insertAVL p4 'f' emptyAVL
-- Insert value [1,2] with key 2
avlt2 = insertBST p2 [1,2] avlt1

-- list = [1,2]
list = lookupAVL p2 avlt2

-- | Error: key 7 is not in the tree avlt2
-- err = lookupAVL p7 avlt2

-- Delete node with key 4
avlt3 = deleteAVL p4 avlt2
-- Delete node with key 2. Returns the empty AVL
avlt4 = deleteAVL p2 avlt3
```

For using the internalist approach, the code example for the externalist approach works with the only difference in the import list:

```haskell
import Data.Tree.AVL.Intern (emptyAVL,insertAVL,lookupAVL,deleteAVL)
-- Instead of import Data.Tree.AVL.Extern (emptyAVL,insertAVL,lookupAVL,deleteAVL)
```

### BST trees

The previous examples used AVL trees. For using AVL trees just replace

```Shell
mkAVL     -> mkBST
emptyAVL  -> emptyBST
insertAVL -> insertBST
lookupAVL -> lookupBST
deleteAVL -> deleteBST
```

Notice that operations for BST may only be applied to BST trees, and operations for AVL trees may only be applied for AVL trees. For instance, this is not possible if `bst2` is not an AVL tree:

```haskell
deleteAVL (Proxy::Proxy 5) bst2
```

It gives an error at compile time.

## Benchmark

### Structure

```Shell
balanced-binary-search-tree
│   README.md
└───benchmark
     └───AVL
     │   └───FullExtern
     |   |   |   Benchmark.hs
     │   │   │
     |   |   └───Insert
     │   │   │
     |   |   └───Lookup
     │   │   │
     |   |   └───Delete
     │   └───Extern
     │   │   └── ...
     │   └───Intern
     │   │   └── ...
     │   └───Unsafe
     │       └── ...
     └───BST
     │
     └── ...

```

There are benchmarks for both BST and AVL trees for each approach. For instance, in the folder `benchmark/AVL/FullExtern`
there are three folders and one source file: `Insert`, `Lookup`, `Delete` and `Benchmark.hs`.

Inside each folder there are different source files for benchmarking each operation under several tree sizes; they're split in different files
in order to be able to measure not only the running times, but also the compile times.

The source files `Benchmark.hs` performs all of the running time benchmarks defined inside the folders `Insert`, `Lookup` and `Delete`.

### Running the benchmark

For running all the benchmarks, use `benchmark/avg-bench.py` script. There's a `requirements.txt` file with all the python dependencies needed to run the script.

If you're using `pip`, just run `pip install -r requirements.txt`.
