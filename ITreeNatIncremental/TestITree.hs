{-# LANGUAGE DataKinds #-}

module ITreeNatIncremental.Test where

import ITreeNatIncremental.ITree
import ITreeNatIncremental.BST
import ITreeNatIncremental.BBT2
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

-- be :: BBT 'EmptyTree
-- be = BBT EmptyITree
--
-- -- [4]
-- bt1 :: BBT ('ForkTree 'EmptyTree ('S ('S ('S ('S 'Z)))) 'EmptyTree)
-- bt1 = insertBBT sbn4 be
--
-- -- [2,4]
-- bt2 :: BBT
--         ('ForkTree
--            ('ForkTree 'EmptyTree ('S ('S 'Z)) 'EmptyTree)
--            ('S ('S ('S ('S 'Z))))
--            'EmptyTree)
-- bt2 = insertBBT sbn2 bt1
--
-- -- [2,4,6]
-- bt3 :: BBT
--         ('ForkTree
--            ('ForkTree 'EmptyTree ('S ('S 'Z)) 'EmptyTree)
--            ('S ('S ('S ('S 'Z))))
--            ('ForkTree 'EmptyTree ('S ('S ('S ('S ('S ('S 'Z)))))) 'EmptyTree))
-- bt3 = insertBBT sbn6 bt2
--
-- -- [2,3,4,6]
-- bt4 :: BBT
--         ('ForkTree
--            ('ForkTree
--               'EmptyTree
--               ('S ('S 'Z))
--               ('ForkTree 'EmptyTree ('S ('S ('S 'Z))) 'EmptyTree))
--            ('S ('S ('S ('S 'Z))))
--            ('ForkTree 'EmptyTree ('S ('S ('S ('S ('S ('S 'Z)))))) 'EmptyTree))
-- bt4 = insertBBT sbn3 bt3
--
-- -- [2,3,4,5,6]
-- bt5 :: BBT
--         ('ForkTree
--            ('ForkTree
--               'EmptyTree
--               ('S ('S 'Z))
--               ('ForkTree 'EmptyTree ('S ('S ('S 'Z))) 'EmptyTree))
--            ('S ('S ('S ('S 'Z))))
--            ('ForkTree
--               ('ForkTree 'EmptyTree ('S ('S ('S ('S ('S 'Z))))) 'EmptyTree)
--               ('S ('S ('S ('S ('S ('S 'Z))))))
--               'EmptyTree))
-- bt5 = insertBBT sbn5 bt4
--
-- -- [0,2,3,4,5,6]
-- bt6 :: BBT
--         ('ForkTree
--            ('ForkTree
--               ('ForkTree 'EmptyTree 'Z 'EmptyTree)
--               ('S ('S 'Z))
--               ('ForkTree 'EmptyTree ('S ('S ('S 'Z))) 'EmptyTree))
--            ('S ('S ('S ('S 'Z))))
--            ('ForkTree
--               ('ForkTree 'EmptyTree ('S ('S ('S ('S ('S 'Z))))) 'EmptyTree)
--               ('S ('S ('S ('S ('S ('S 'Z))))))
--               'EmptyTree))
-- bt6 = insertBBT sbn0 bt5
--
-- -- [0,2,3,4,5,6,7]
-- bt7 :: BBT
--         ('ForkTree
--            ('ForkTree
--               ('ForkTree 'EmptyTree 'Z 'EmptyTree)
--               ('S ('S 'Z))
--               ('ForkTree 'EmptyTree ('S ('S ('S 'Z))) 'EmptyTree))
--            ('S ('S ('S ('S 'Z))))
--            ('ForkTree
--               ('ForkTree 'EmptyTree ('S ('S ('S ('S ('S 'Z))))) 'EmptyTree)
--               ('S ('S ('S ('S ('S ('S 'Z))))))
--               ('ForkTree
--                  'EmptyTree ('S ('S ('S ('S ('S ('S ('S 'Z))))))) 'EmptyTree)))
-- bt7 = insertBBT sbn7 bt6
--
-- bt8 :: BBT
--         ('ForkTree
--            ('ForkTree
--               ('ForkTree 'EmptyTree 'Z 'EmptyTree)
--               ('S ('S 'Z))
--               ('ForkTree 'EmptyTree ('S ('S ('S 'Z))) 'EmptyTree))
--            ('S ('S ('S ('S 'Z))))
--            ('ForkTree
--               ('ForkTree 'EmptyTree ('S ('S ('S ('S ('S 'Z))))) 'EmptyTree)
--               ('S ('S ('S ('S ('S ('S 'Z))))))
--               ('ForkTree
--                  'EmptyTree ('S ('S ('S ('S ('S ('S ('S 'Z))))))) 'EmptyTree)))
-- bt8 = insertBBT sbn7 bt7
--
-- bt9 :: BBT
--         ('ForkTree
--            ('ForkTree
--               ('ForkTree 'EmptyTree 'Z 'EmptyTree)
--               ('S ('S 'Z))
--               ('ForkTree 'EmptyTree ('S ('S ('S 'Z))) 'EmptyTree))
--            ('S ('S ('S ('S 'Z))))
--            ('ForkTree
--               ('ForkTree 'EmptyTree ('S ('S ('S ('S ('S 'Z))))) 'EmptyTree)
--               ('S ('S ('S ('S ('S ('S 'Z))))))
--               'EmptyTree))
-- bt9 = deleteBBT sbn7 bt7
--
-- bt10 :: BBT
--          ('ForkTree
--             ('ForkTree
--                ('ForkTree 'EmptyTree 'Z 'EmptyTree) ('S ('S 'Z)) 'EmptyTree)
--             ('S ('S ('S 'Z)))
--             ('ForkTree
--                ('ForkTree 'EmptyTree ('S ('S ('S ('S ('S 'Z))))) 'EmptyTree)
--                ('S ('S ('S ('S ('S ('S 'Z))))))
--                ('ForkTree
--                   'EmptyTree ('S ('S ('S ('S ('S ('S ('S 'Z))))))) 'EmptyTree)))
-- bt10 = deleteBBT sbn4 bt7
--
-- bt11 :: BBT
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
-- bt11 = deleteBBT sbn1 bt7

be :: BBT 'EmptyAATree
be = BBT EmptyIAATree

-- [4]
bt1 :: BBT ('ForkAATree 'EmptyAATree ('S ('S ('S ('S 'Z)))) 'Z 'EmptyAATree)
bt1 = insertBBT sbn4 be

-- [2,4]
bt2 :: BBT
         ('ForkAATree
            'EmptyAATree
            ('S ('S 'Z))
            'Z
            ('ForkAATree 'EmptyAATree ('S ('S ('S ('S 'Z)))) 'Z 'EmptyAATree))
bt2 = insertBBT sbn2 bt1

-- [2,4,6]
bt3 :: BBT
        ('ForkAATree
           ('ForkAATree 'EmptyAATree ('S ('S 'Z)) 'Z 'EmptyAATree)
           ('S ('S ('S ('S 'Z)))) ('S 'Z)
           ('ForkAATree 'EmptyAATree ('S ('S ('S ('S ('S ('S 'Z)))))) 'Z 'EmptyAATree))
bt3 = insertBBT sbn6 bt2

-- [2,3,4,6]
bt4 :: BBT
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
bt4 = insertBBT sbn3 bt3

-- [2,3,4,5,6]
bt5 :: BBT
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
bt5 = insertBBT sbn5 bt4

-- [0,2,3,4,5,6]
bt6 :: BBT
         ('ForkAATree
            ('ForkAATree
               'EmptyAATree
               'Z
               'Z
               ('ForkAATree 'EmptyAATree ('S ('S 'Z)) 'Z 'EmptyAATree))
            ('S ('S ('S ('S 'Z))))
            ('S 'Z)
            ('ForkAATree
               'EmptyAATree
               ('S ('S ('S ('S ('S 'Z)))))
               'Z
               ('ForkAATree
                  'EmptyAATree ('S ('S ('S ('S ('S ('S 'Z)))))) 'Z 'EmptyAATree)))
bt6 = insertBBT sbn0 bt5

-- [0,2,3,4,5,6,7]
bt7 :: BBT
         ('ForkAATree
            ('ForkAATree
               'EmptyAATree
               'Z
               'Z
               ('ForkAATree 'EmptyAATree ('S ('S 'Z)) 'Z 'EmptyAATree))
            ('S ('S ('S ('S 'Z))))
            ('S 'Z)
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
bt7 = insertBBT sbn7 bt6

bt8 :: BBT
         ('ForkAATree
            ('ForkAATree
               'EmptyAATree
               'Z
               'Z
               ('ForkAATree 'EmptyAATree ('S ('S 'Z)) 'Z 'EmptyAATree))
            ('S ('S ('S ('S 'Z))))
            ('S 'Z)
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
bt8 = insertBBT sbn7 bt7


-- bt9 = deleteBBT sbn7 bt7

-- bt10 :: BBT
--          ('ForkTree
--             ('ForkTree
--                ('ForkTree 'EmptyTree 'Z 'EmptyTree) ('S ('S 'Z)) 'EmptyTree)
--             ('S ('S ('S 'Z)))
--             ('ForkTree
--                ('ForkTree 'EmptyTree ('S ('S ('S ('S ('S 'Z))))) 'EmptyTree)
--                ('S ('S ('S ('S ('S ('S 'Z))))))
--                ('ForkTree
--                   'EmptyTree ('S ('S ('S ('S ('S ('S ('S 'Z))))))) 'EmptyTree)))
-- bt10 = deleteBBT sbn4 bt7
--
-- bt11 :: BBT
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
-- bt11 = deleteBBT sbn1 bt7
