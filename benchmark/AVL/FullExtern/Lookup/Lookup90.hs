{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}


module AVL.FullExtern.Lookup.Lookup90 (v90, main) where

import           Data.Time.Clock                (diffUTCTime, getCurrentTime)
import           Data.Proxy                     (Proxy (Proxy))
import           Data.Tree.AVL.FullExtern       (AVL (AVL))
import           Data.Tree.BST.FullExtern       (lookup)
import           Prelude                        (IO, putStrLn, return, seq, show, (++))
import           AVL.FullExtern.Example.Example90 (t90)


v90 = case t90 of
  AVL t90' _ _ -> lookup (Proxy::Proxy 89) t90'

main :: IO ()
main = do seq t90 (return ())
          t0 <- getCurrentTime
          seq v90 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
