{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}


module AVL.FullExtern.Lookup.Lookup70 (v70, main) where

import           Data.Time.Clock                (diffUTCTime, getCurrentTime)
import           Data.Proxy                     (Proxy (Proxy))
import           Data.Tree.AVL.FullExtern       (AVL (AVL))
import           Data.Tree.BST.FullExtern       (lookup)
import           Prelude                        (IO, putStrLn, return, seq, show, (++))
import           AVL.FullExtern.Example.Example70 (t70)


v70 = case t70 of
  AVL t70' _ _ -> lookup (Proxy::Proxy 69) t70'

main :: IO ()
main = do seq t70 (return ())
          t0 <- getCurrentTime
          seq v70 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
