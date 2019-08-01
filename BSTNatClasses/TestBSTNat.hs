{-# LANGUAGE DataKinds #-}

module BSTNatClasses.Test where

import BSTNatClasses.BSTNat
import Data.Nat

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

sbn7 :: Natty ('S ('S ('S ('S ('S ('S ('S 'Z)))))))
sbn7 = Sy (Sy (Sy (Sy (Sy (Sy (Sy Zy))))))

e :: BST 'EmptyTree
e = EmptyBST

-- [4]
t1 :: BST ('ForkTree 'EmptyTree ('S ('S ('S ('S 'Z)))) 'EmptyTree)
t1 = insert sbn4 e

-- [2,4]
t2 :: BST
        ('ForkTree
           ('ForkTree 'EmptyTree ('S ('S 'Z)) 'EmptyTree)
           ('S ('S ('S ('S 'Z))))
           'EmptyTree)
t2 = insert sbn2 t1

-- [2,4,6]
t3 :: BST
        ('ForkTree
           ('ForkTree 'EmptyTree ('S ('S 'Z)) 'EmptyTree)
           ('S ('S ('S ('S 'Z))))
           ('ForkTree 'EmptyTree ('S ('S ('S ('S ('S ('S 'Z)))))) 'EmptyTree))
t3 = insert sbn6 t2

-- [2,3,4,6]
t4 :: BST
        ('ForkTree
           ('ForkTree
              'EmptyTree
              ('S ('S 'Z))
              ('ForkTree 'EmptyTree ('S ('S ('S 'Z))) 'EmptyTree))
           ('S ('S ('S ('S 'Z))))
           ('ForkTree 'EmptyTree ('S ('S ('S ('S ('S ('S 'Z)))))) 'EmptyTree))
t4 = insert sbn3 t3

-- [2,3,4,5,6]
t5 :: BST
        ('ForkTree
           ('ForkTree
              'EmptyTree
              ('S ('S 'Z))
              ('ForkTree 'EmptyTree ('S ('S ('S 'Z))) 'EmptyTree))
           ('S ('S ('S ('S 'Z))))
           ('ForkTree
              ('ForkTree 'EmptyTree ('S ('S ('S ('S ('S 'Z))))) 'EmptyTree)
              ('S ('S ('S ('S ('S ('S 'Z))))))
              'EmptyTree))
t5 = insert sbn5 t4

-- [0,2,3,4,5,6]
t6 :: BST
        ('ForkTree
           ('ForkTree
              ('ForkTree 'EmptyTree 'Z 'EmptyTree)
              ('S ('S 'Z))
              ('ForkTree 'EmptyTree ('S ('S ('S 'Z))) 'EmptyTree))
           ('S ('S ('S ('S 'Z))))
           ('ForkTree
              ('ForkTree 'EmptyTree ('S ('S ('S ('S ('S 'Z))))) 'EmptyTree)
              ('S ('S ('S ('S ('S ('S 'Z))))))
              'EmptyTree))
t6 = insert sbn0 t5

-- [0,2,3,4,5,6,7]
t7 :: BST
        ('ForkTree
           ('ForkTree
              ('ForkTree 'EmptyTree 'Z 'EmptyTree)
              ('S ('S 'Z))
              ('ForkTree 'EmptyTree ('S ('S ('S 'Z))) 'EmptyTree))
           ('S ('S ('S ('S 'Z))))
           ('ForkTree
              ('ForkTree 'EmptyTree ('S ('S ('S ('S ('S 'Z))))) 'EmptyTree)
              ('S ('S ('S ('S ('S ('S 'Z))))))
              ('ForkTree
                 'EmptyTree ('S ('S ('S ('S ('S ('S ('S 'Z))))))) 'EmptyTree)))
t7 = insert sbn7 t6

t8 :: BST
        ('ForkTree
           ('ForkTree
              ('ForkTree 'EmptyTree 'Z 'EmptyTree)
              ('S ('S 'Z))
              ('ForkTree 'EmptyTree ('S ('S ('S 'Z))) 'EmptyTree))
           ('S ('S ('S ('S 'Z))))
           ('ForkTree
              ('ForkTree 'EmptyTree ('S ('S ('S ('S ('S 'Z))))) 'EmptyTree)
              ('S ('S ('S ('S ('S ('S 'Z))))))
              ('ForkTree
                 'EmptyTree ('S ('S ('S ('S ('S ('S ('S 'Z))))))) 'EmptyTree)))
t8 = insert sbn7 t7

t9 :: BST
        ('ForkTree
           ('ForkTree
              ('ForkTree 'EmptyTree 'Z 'EmptyTree)
              ('S ('S 'Z))
              ('ForkTree 'EmptyTree ('S ('S ('S 'Z))) 'EmptyTree))
           ('S ('S ('S ('S 'Z))))
           ('ForkTree
              ('ForkTree 'EmptyTree ('S ('S ('S ('S ('S 'Z))))) 'EmptyTree)
              ('S ('S ('S ('S ('S ('S 'Z))))))
              'EmptyTree))
t9 = delete sbn7 t7

t10 :: BST
         ('ForkTree
            ('ForkTree
               ('ForkTree 'EmptyTree 'Z 'EmptyTree) ('S ('S 'Z)) 'EmptyTree)
            ('S ('S ('S 'Z)))
            ('ForkTree
               ('ForkTree 'EmptyTree ('S ('S ('S ('S ('S 'Z))))) 'EmptyTree)
               ('S ('S ('S ('S ('S ('S 'Z))))))
               ('ForkTree
                  'EmptyTree ('S ('S ('S ('S ('S ('S ('S 'Z))))))) 'EmptyTree)))
t10 = delete sbn4 t7

t11 :: BST
         ('ForkTree
            ('ForkTree
               ('ForkTree 'EmptyTree 'Z 'EmptyTree)
               ('S ('S 'Z))
               ('ForkTree 'EmptyTree ('S ('S ('S 'Z))) 'EmptyTree))
            ('S ('S ('S ('S 'Z))))
            ('ForkTree
               ('ForkTree 'EmptyTree ('S ('S ('S ('S ('S 'Z))))) 'EmptyTree)
               ('S ('S ('S ('S ('S ('S 'Z))))))
               ('ForkTree
                  'EmptyTree ('S ('S ('S ('S ('S ('S ('S 'Z))))))) 'EmptyTree)))
t11 = delete sbn1 t7
