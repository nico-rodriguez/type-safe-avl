{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}


module BST.Intern.Lookup.Lookup20 (v20, main) where

import           Data.Time.Clock            (diffUTCTime, getCurrentTime)
import           Data.Proxy                 (Proxy (Proxy))
import           Data.Tree.BST.Intern       (lookupBST)
import           Prelude                    (IO, putStrLn, return, seq, show, (++))
import           BST.Intern.Insert.Insert20 (t20)


v20 = lookupBST (Proxy::Proxy 19) t20

main :: IO ()
main = do seq t20 (return ())
          t0 <- getCurrentTime
          seq v20 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
