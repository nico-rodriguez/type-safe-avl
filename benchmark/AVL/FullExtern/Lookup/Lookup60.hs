{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}


module AVL.FullExtern.Lookup.Lookup60 (v60, main) where

import           Data.Time.Clock                (diffUTCTime, getCurrentTime)
import           Data.Proxy                     (Proxy (Proxy))
import           Data.Tree.AVL.FullExtern       (AVL (AVL))
import           Data.Tree.BST.FullExtern       (lookup)
import           Prelude                        (IO, putStrLn, return, seq, show, (++))
import           AVL.FullExtern.Example.Example60 (t60)


v60 = case t60 of
  AVL t60' _ _ -> lookup (Proxy::Proxy 59) t60'

main :: IO ()
main = do seq t60 (return ())
          t0 <- getCurrentTime
          seq v60 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
