{-# LANGUAGE DataKinds #-}

module Test where

import BST

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

e :: BST 'Bot 'Top
e = EmptyBST

t3 :: BST 'Bot 'Top
-- t3 = RootBST (EmptyBST MinusInfy sbn3) sbn3 (EmptyBST sbn3 PlusInfy)
t3 = insert sbn3 e

t2 :: BST 'Bot 'Top
t2 = RootBST EmptyBST sbn2 EmptyBST
