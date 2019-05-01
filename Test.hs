{-# LANGUAGE DataKinds #-}

module Test where

import BST

sbn0 :: SingletonBNat ('Val 'Z)
sbn0 = Natty Zy

sbn1 :: SingletonBNat ('Val ('S 'Z))
sbn1 = Natty (Sy Zy)

sbn2 :: SingletonBNat ('Val ('S ('S 'Z)))
sbn2 = Natty (Sy (Sy Zy))

sbn3 :: SingletonBNat ('Val ('S ('S ('S 'Z))))
sbn3 = Natty (Sy (Sy (Sy Zy)))

sbn4 :: SingletonBNat ('Val ('S ('S ('S ('S 'Z)))))
sbn4 = Natty (Sy (Sy (Sy (Sy Zy))))

sbn5 :: SingletonBNat ('Val ('S ('S ('S ('S ('S 'Z))))))
sbn5 = Natty (Sy (Sy (Sy (Sy (Sy Zy)))))

sbn6 :: SingletonBNat ('Val ('S ('S ('S ('S ('S ('S 'Z)))))))
sbn6 = Natty (Sy (Sy (Sy (Sy (Sy (Sy Zy))))))

e :: BST 'MinusInf 'PlusInf
e = EmptyBST MinusInfy PlusInfy

t3 :: BST 'MinusInf 'PlusInf
-- t3 = RootBST (EmptyBST MinusInfy sbn3) sbn3 (EmptyBST sbn3 PlusInfy)
t3 = insert sbn3 e

t2 :: BST 'MinusInf 'PlusInf
t2 = RootBST (EmptyBST MinusInfy sbn2) sbn2 (EmptyBST sbn2 PlusInfy)
