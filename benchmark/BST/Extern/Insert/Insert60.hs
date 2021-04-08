{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module BST.Extern.Insert.Insert60 (t60', main) where

import           Data.Proxy             (Proxy (Proxy))
import           Data.Time.Clock        (diffUTCTime, getCurrentTime)
import           Data.Tree.BST.Extern   (insertBST)
import           Prelude                (IO, putStrLn, return, seq, show, ($), (++))
import           BST.Extern.Example.Example60 (t60)


t60' = insertBST (Proxy::Proxy 60) 'a' t60

main :: IO ()
main = do t0 <- getCurrentTime
          seq t60' (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t1 t0) ++ " seconds")
