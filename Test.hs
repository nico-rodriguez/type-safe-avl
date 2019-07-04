{-# LANGUAGE DataKinds #-}

module Test where

import BST
import Nat

sbn0 :: Natty 'Z
sbn0 = Zy

sbn1 :: Natty ('S 'Z)
sbn1 = Sy Zy

sbn2 :: Natty ('S ('S 'Z))
sbn2 = Sy (Sy Zy)

sbn3 :: Natty ('S ('S ('S 'Z)))
sbn3 = Sy (Sy (Sy Zy))

sbn4 :: Natty ('S ('S ('S ('S 'Z))))
sbn4 = Sy (Sy (Sy (Sy Zy)))

sbn5 :: Natty ('S ('S ('S ('S ('S 'Z)))))
sbn5 = Sy (Sy (Sy (Sy (Sy Zy))))

sbn6 :: Natty ('S ('S ('S ('S ('S ('S 'Z))))))
sbn6 = Sy (Sy (Sy (Sy (Sy (Sy Zy)))))

e :: BST 'EmptyTree
e = EmptyBST

t2 :: BST ('ForkTree 'EmptyTree ('S ('S 'Z)) 'EmptyTree)
t2 = ForkBST EmptyBST sbn2 EmptyBST

t3 :: BST
        ('ForkTree
           'EmptyTree
           ('S ('S 'Z))
           ('ForkTree 'EmptyTree ('S ('S ('S ('S ('S ('S 'Z)))))) 'EmptyTree))
t3 = insert sbn6 t2

t4 :: BST
        ('ForkTree
           ('ForkTree 'EmptyTree 'Z 'EmptyTree)
           ('S ('S 'Z))
           ('ForkTree 'EmptyTree ('S ('S ('S ('S ('S ('S 'Z)))))) 'EmptyTree))
t4 = insert sbn0 t3

t5 :: BST
        ('ForkTree
           ('ForkTree 'EmptyTree 'Z ('ForkTree 'EmptyTree ('S 'Z) 'EmptyTree))
           ('S ('S 'Z))
           ('ForkTree 'EmptyTree ('S ('S ('S ('S ('S ('S 'Z)))))) 'EmptyTree))
t5 = insert sbn1 t4

t6 :: BST
        ('ForkTree
           ('ForkTree 'EmptyTree 'Z 'EmptyTree)
           ('S ('S 'Z))
           ('ForkTree 'EmptyTree ('S ('S ('S ('S ('S ('S 'Z)))))) 'EmptyTree))
t6 = delete sbn1 t4

t7 :: BST
        ('ForkTree
           ('ForkTree 'EmptyTree 'Z 'EmptyTree)
           ('S ('S 'Z))
           ('ForkTree 'EmptyTree ('S ('S ('S ('S ('S ('S 'Z)))))) 'EmptyTree))
t7 = delete sbn1 t5

t8 :: BST
        ('ForkTree
           ('ForkTree 'EmptyTree 'Z 'EmptyTree)
           ('S 'Z)
           ('ForkTree 'EmptyTree ('S ('S ('S ('S ('S ('S 'Z)))))) 'EmptyTree))
t8 = delete sbn2 t5
