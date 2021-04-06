{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module BST.Extern.Insert.Insert10 (t10', main) where

import           Data.Proxy             (Proxy (Proxy))
import           Data.Time.Clock        (diffUTCTime, getCurrentTime)
import           Data.Tree.BST.Extern   (insertBST)
import           Prelude                (IO, putStrLn, return, seq, show, ($), (++))
import           BST.Extern.Example.Example10 (t10)


t10' = insertBST (Proxy::Proxy 10) 'a' t10

main :: IO ()
main = do t0 <- getCurrentTime
          seq t10' (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t1 t0) ++ " seconds")
