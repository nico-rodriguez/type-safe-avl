{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}


module AVL.FullExtern.Lookup.Lookup10 (v10, main) where

import           Data.Time.Clock                (diffUTCTime, getCurrentTime)
import           Data.Proxy                     (Proxy (Proxy))
import           Data.Tree.AVL.FullExtern       (AVL (AVL))
import           Data.Tree.BST.FullExtern       (lookup)
import           Prelude                        (IO, putStrLn, return, seq, show, (++))
import           AVL.FullExtern.Example.Example10 (t10)


v10 = case t10 of
  AVL t10' _ _ -> lookup (Proxy::Proxy 9) t10'

main :: IO ()
main = do seq t10 (return ())
          t0 <- getCurrentTime
          seq v10 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
