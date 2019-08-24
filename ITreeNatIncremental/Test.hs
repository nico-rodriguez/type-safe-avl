{-# LANGUAGE DataKinds #-}

module ITreeNatIncremental.Test where

import ITreeNatIncremental.ITree
import ITreeNatIncremental.BST
import ITreeNatIncremental.BBST
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

-- | Test Binary Search Trees

e :: BST 'EmptyTree
e = BST EmptyITree

-- [4]
t1 :: BST ('ForkTree 'EmptyTree ('S ('S ('S ('S 'Z)))) 'EmptyTree)
t1 = insertBST sbn4 e

-- [2,4]
t2 :: BST
        ('ForkTree
           ('ForkTree 'EmptyTree ('S ('S 'Z)) 'EmptyTree)
           ('S ('S ('S ('S 'Z))))
           'EmptyTree)
t2 = insertBST sbn2 t1

-- [2,4,6]
t3 :: BST
        ('ForkTree
           ('ForkTree 'EmptyTree ('S ('S 'Z)) 'EmptyTree)
           ('S ('S ('S ('S 'Z))))
           ('ForkTree 'EmptyTree ('S ('S ('S ('S ('S ('S 'Z)))))) 'EmptyTree))
t3 = insertBST sbn6 t2

-- [2,3,4,6]
t4 :: BST
        ('ForkTree
           ('ForkTree
              'EmptyTree
              ('S ('S 'Z))
              ('ForkTree 'EmptyTree ('S ('S ('S 'Z))) 'EmptyTree))
           ('S ('S ('S ('S 'Z))))
           ('ForkTree 'EmptyTree ('S ('S ('S ('S ('S ('S 'Z)))))) 'EmptyTree))
t4 = insertBST sbn3 t3

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
t5 = insertBST sbn5 t4

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
t6 = insertBST sbn0 t5

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
t7 = insertBST sbn7 t6

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
t8 = insertBST sbn7 t7

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
t9 = deleteBST sbn7 t7

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
t10 = deleteBST sbn4 t7

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
t11 = deleteBST sbn1 t7

-- | Test Balanced Binary Tree

be :: BBST 'EmptyAATree
be = BBST EmptyIAATree

bt :: BBST
        ('ForkAATree
           ('ForkAATree
              ('ForkAATree 'EmptyAATree 'Z 'Z 'EmptyAATree)
              ('S 'Z)
              ('S 'Z)
              ('ForkAATree 'EmptyAATree ('S ('S 'Z)) 'Z 'EmptyAATree))
           ('S ('S ('S 'Z)))
           ('S ('S 'Z))
           ('ForkAATree
              ('ForkAATree 'EmptyAATree ('S ('S ('S ('S 'Z)))) 'Z 'EmptyAATree)
              ('S ('S ('S ('S ('S 'Z)))))
              ('S 'Z)
              ('ForkAATree
                 'EmptyAATree
                 ('S ('S ('S ('S ('S ('S 'Z))))))
                 'Z
                 ('ForkAATree
                    'EmptyAATree
                    ('S ('S ('S ('S ('S ('S ('S 'Z)))))))
                    'Z
                    'EmptyAATree))))
bt = insertBBST sbn7 $ insertBBST sbn6 $ insertBBST sbn5 $ insertBBST sbn4 $ insertBBST sbn3 $ insertBBST sbn2 $ insertBBST sbn1 $ insertBBST sbn0 be

-- [4]
bt1 :: BBST ('ForkAATree 'EmptyAATree ('S ('S ('S ('S 'Z)))) 'Z 'EmptyAATree)
bt1 = insertBBST sbn4 be

-- [2,4]
bt2 :: BBST
         ('ForkAATree
            'EmptyAATree
            ('S ('S 'Z))
            'Z
            ('ForkAATree 'EmptyAATree ('S ('S ('S ('S 'Z)))) 'Z 'EmptyAATree))
bt2 = insertBBST sbn2 bt1

-- [2,4,6]
bt3 :: BBST
        ('ForkAATree
           ('ForkAATree 'EmptyAATree ('S ('S 'Z)) 'Z 'EmptyAATree)
           ('S ('S ('S ('S 'Z)))) ('S 'Z)
           ('ForkAATree 'EmptyAATree ('S ('S ('S ('S ('S ('S 'Z)))))) 'Z 'EmptyAATree))
bt3 = insertBBST sbn6 bt2

-- [2,3,4,6]
bt4 :: BBST
         ('ForkAATree
            ('ForkAATree
               'EmptyAATree
               ('S ('S 'Z))
               'Z
               ('ForkAATree 'EmptyAATree ('S ('S ('S 'Z))) 'Z 'EmptyAATree))
            ('S ('S ('S ('S 'Z))))
            ('S 'Z)
            ('ForkAATree
               'EmptyAATree ('S ('S ('S ('S ('S ('S 'Z)))))) 'Z 'EmptyAATree))
bt4 = insertBBST sbn3 bt3

-- [2,3,4,5,6]
bt5 :: BBST
         ('ForkAATree
            ('ForkAATree
               'EmptyAATree
               ('S ('S 'Z))
               'Z
               ('ForkAATree 'EmptyAATree ('S ('S ('S 'Z))) 'Z 'EmptyAATree))
            ('S ('S ('S ('S 'Z))))
            ('S 'Z)
            ('ForkAATree
               'EmptyAATree
               ('S ('S ('S ('S ('S 'Z)))))
               'Z
               ('ForkAATree
                  'EmptyAATree ('S ('S ('S ('S ('S ('S 'Z)))))) 'Z 'EmptyAATree)))
bt5 = insertBBST sbn5 bt4

-- [0,2,3,4,5,6]
bt6 :: BBST
         ('ForkAATree
            ('ForkAATree 'EmptyAATree 'Z 'Z 'EmptyAATree)
            ('S ('S 'Z))
            ('S 'Z)
            ('ForkAATree
               ('ForkAATree 'EmptyAATree ('S ('S ('S 'Z))) 'Z 'EmptyAATree)
               ('S ('S ('S ('S 'Z))))
               ('S 'Z)
               ('ForkAATree
                  'EmptyAATree
                  ('S ('S ('S ('S ('S 'Z)))))
                  'Z
                  ('ForkAATree
                     'EmptyAATree ('S ('S ('S ('S ('S ('S 'Z)))))) 'Z 'EmptyAATree))))
bt6 = insertBBST sbn0 bt5

-- [0,2,3,4,5,6,7]
bt7 :: BBST
         ('ForkAATree
            ('ForkAATree
               ('ForkAATree 'EmptyAATree 'Z 'Z 'EmptyAATree)
               ('S ('S 'Z))
               ('S 'Z)
               ('ForkAATree 'EmptyAATree ('S ('S ('S 'Z))) 'Z 'EmptyAATree))
            ('S ('S ('S ('S 'Z))))
            ('S ('S 'Z))
            ('ForkAATree
               ('ForkAATree
                  'EmptyAATree ('S ('S ('S ('S ('S 'Z))))) 'Z 'EmptyAATree)
               ('S ('S ('S ('S ('S ('S 'Z))))))
               ('S 'Z)
               ('ForkAATree
                  'EmptyAATree
                  ('S ('S ('S ('S ('S ('S ('S 'Z)))))))
                  'Z
                  'EmptyAATree)))
bt7 = insertBBST sbn7 bt6

bt8 :: BBST
         ('ForkAATree
            ('ForkAATree
               ('ForkAATree 'EmptyAATree 'Z 'Z 'EmptyAATree)
               ('S ('S 'Z))
               ('S 'Z)
               ('ForkAATree 'EmptyAATree ('S ('S ('S 'Z))) 'Z 'EmptyAATree))
            ('S ('S ('S ('S 'Z))))
            ('S ('S 'Z))
            ('ForkAATree
               ('ForkAATree
                  'EmptyAATree ('S ('S ('S ('S ('S 'Z))))) 'Z 'EmptyAATree)
               ('S ('S ('S ('S ('S ('S 'Z))))))
               ('S 'Z)
               ('ForkAATree
                  'EmptyAATree
                  ('S ('S ('S ('S ('S ('S ('S 'Z)))))))
                  'Z
                  'EmptyAATree)))
bt8 = insertBBST sbn7 bt7


-- bt9 = deleteBBST sbn7 bt7

-- bt10 :: BBST
--          ('ForkTree
--             ('ForkTree
--                ('ForkTree 'EmptyTree 'Z 'EmptyTree) ('S ('S 'Z)) 'EmptyTree)
--             ('S ('S ('S 'Z)))
--             ('ForkTree
--                ('ForkTree 'EmptyTree ('S ('S ('S ('S ('S 'Z))))) 'EmptyTree)
--                ('S ('S ('S ('S ('S ('S 'Z))))))
--                ('ForkTree
--                   'EmptyTree ('S ('S ('S ('S ('S ('S ('S 'Z))))))) 'EmptyTree)))
-- bt10 = deleteBBST sbn4 bt7
--
-- bt11 :: BBST
--          ('ForkTree
--             ('ForkTree
--                ('ForkTree 'EmptyTree 'Z 'EmptyTree)
--                ('S ('S 'Z))
--                ('ForkTree 'EmptyTree ('S ('S ('S 'Z))) 'EmptyTree))
--             ('S ('S ('S ('S 'Z))))
--             ('ForkTree
--                ('ForkTree 'EmptyTree ('S ('S ('S ('S ('S 'Z))))) 'EmptyTree)
--                ('S ('S ('S ('S ('S ('S 'Z))))))
--                ('ForkTree
--                   'EmptyTree ('S ('S ('S ('S ('S ('S ('S 'Z))))))) 'EmptyTree)))
-- bt11 = deleteBBST sbn1 bt7
