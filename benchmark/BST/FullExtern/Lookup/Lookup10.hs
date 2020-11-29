{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}


module BST.FullExtern.Lookup.Lookup10 (v10, main) where

import           Data.Time.Clock                (diffUTCTime, getCurrentTime)
import           Data.Proxy                     (Proxy (Proxy))
import           Data.Tree.BST.FullExtern       (BST (BST), lookup)
import           Prelude                        (IO, putStrLn, return, seq, show, (++))
import           BST.FullExtern.Insert.Insert10 (t10)


v10 = case t10 of
  BST t10' _ -> lookup (Proxy::Proxy 9) t10'

main :: IO ()
main = do seq t10 (return ())
          t0 <- getCurrentTime
          seq v10 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
