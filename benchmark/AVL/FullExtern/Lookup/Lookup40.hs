{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}


module AVL.FullExtern.Lookup.Lookup40 (v40, main) where

import           Data.Time.Clock                (diffUTCTime, getCurrentTime)
import           Data.Proxy                     (Proxy (Proxy))
import           Data.Tree.AVL.FullExtern       (AVL (AVL))
import           Data.Tree.BST.FullExtern       (lookup)
import           Prelude                        (IO, putStrLn, return, seq, show, (++))
import           AVL.FullExtern.Example.Example40 (t40)


v40 = case t40 of
  AVL t40' _ _ -> lookup (Proxy::Proxy 39) t40'

main :: IO ()
main = do seq t40 (return ())
          t0 <- getCurrentTime
          seq v40 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
