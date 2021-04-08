{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}


module BST.Extern.Lookup.Lookup60 (v60, main) where

import           Data.Time.Clock            (diffUTCTime, getCurrentTime)
import           Data.Proxy                 (Proxy (Proxy))
import           Data.Tree.BST.Extern       (lookupBST)
import           Prelude                    (IO, putStrLn, return, seq, show, (++))
import           BST.Extern.Example.Example60 (t60)


v60 = lookupBST (Proxy::Proxy 59) t60

main :: IO ()
main = do seq t60 (return ())
          t0 <- getCurrentTime
          seq v60 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t1 t0) ++ " seconds")
