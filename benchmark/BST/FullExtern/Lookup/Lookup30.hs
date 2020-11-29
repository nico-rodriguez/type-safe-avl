{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}


module BST.FullExtern.Lookup.Lookup30 (v30, main) where

import           Data.Time.Clock                (diffUTCTime, getCurrentTime)
import           Data.Proxy                     (Proxy (Proxy))
import           Data.Tree.BST.FullExtern       (BST (BST), lookup)
import           Prelude                        (IO, putStrLn, return, seq, show, (++))
import           BST.FullExtern.Insert.Insert30 (t30)


v30 = case t30 of
  BST t30' _ -> lookup (Proxy::Proxy 29) t30'

main :: IO ()
main = do seq t30 (return ())
          t0 <- getCurrentTime
          seq v30 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
