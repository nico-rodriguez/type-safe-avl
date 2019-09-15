{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Chr where

import           Compare
import           Data.Type.Bool
import           Data.Type.Equality

import qualified Data.Char          as Char

data SBool :: Bool -> * where
  STrue   :: SBool 'True
  SFalse  :: SBool 'False

toBool :: SBool b -> Bool
toBool STrue  = True
toBool SFalse = False

data Chr = Char Bool Bool Bool Bool Bool Bool Bool Bool

type family CompareBool (a :: Bool) (b :: Bool) :: Ordering where
  CompareBool 'False 'False = 'EQ
  CompareBool 'False 'True  = 'LT
  CompareBool 'True  'False = 'GT
  CompareBool 'True  'True  = 'EQ
type instance Compare (a :: Bool) (b :: Bool) = CompareBool a b

type family CompareChr (a :: Chr) (b :: Chr) :: Ordering where
  CompareChr ('Char a0 a1 a2 a3 a4 a5 a6 a7) ('Char b0 b1 b2 b3 b4 b5 b6 b7) =
    (If (CompareBool a0 b0 == 'EQ)
      (If (CompareBool a1 b1 == 'EQ)
        (If (CompareBool a2 b2 == 'EQ)
          (If (CompareBool a3 b3 == 'EQ)
            (If (CompareBool a4 b4 == 'EQ)
              (If (CompareBool a5 b5 == 'EQ)
                (If (CompareBool a6 b6 == 'EQ)
                  (If (CompareBool a7 b7 == 'EQ)
                    'EQ
                    (If (CompareBool a7 b7 == 'LT)
                      'LT
                      'GT
                    )
                  )
                  (If (CompareBool a6 b6 == 'LT)
                    'LT
                    'GT
                  )
                )
                (If (CompareBool a5 b5 == 'LT)
                  'LT
                  'GT
                )
              )
              (If (CompareBool a4 b4 == 'LT)
                'LT
                'GT
              )
            )
            (If (CompareBool a3 b3 == 'LT)
              'LT
              'GT
            )
          )
          (If (CompareBool a2 b2 == 'LT)
            'LT
            'GT
          )
        )
        (If (CompareBool a1 b1 == 'LT)
          'LT
          'GT
        )
      )
      (If (CompareBool a0 b0 == 'LT)
        'LT
        'GT
      )
    )
type instance Compare (a :: Chr) (b :: Chr) = CompareChr a b

data SChar :: Chr -> * where
  SChar :: SBool a0 -> SBool a1 -> SBool a2 -> SBool a3 -> SBool a4 ->
    SBool a5 -> SBool a6 -> SBool a7 -> SChar ('Char a0 a1 a2 a3 a4 a5 a6 a7)

instance Show (SChar c) where
  showsPrec d c = showParen (d > 10) $ showString "SChar " . shows (toChar c)

owotoChr :: SChar c1 -> SChar c2 -> OWOTO c1 c2
owotoChr = undefined

fromBinDigit :: Bool -> Int
fromBinDigit b = if b then 1 else 0

fromBin :: [Bool] -> Int
fromBin = foldr (\d acc -> 2 * acc + fromBinDigit d) 0

toChar :: SChar c -> Char
toChar (SChar a0 a1 a2 a3 a4 a5 a6 a7) = Char.chr $ fromBin [toBool a0,toBool a1,toBool a2,toBool a3,toBool a4,toBool a5,toBool a6,toBool a7]
