{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module BST.Intern.Insert.Insert50 (t50', main) where

import           Data.Proxy             (Proxy (Proxy))
import           Data.Time.Clock        (diffUTCTime, getCurrentTime)
import           Data.Tree.BST.Intern   (insertBST)
import           Prelude                (IO, putStrLn, return, seq, show, ($), (++))
import           BST.Intern.Example.Example50 (t50)


t50' = insertBST (Proxy::Proxy 50) 'a' t50

main :: IO ()
main = do t0 <- getCurrentTime
          seq t50' (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t1 t0) ++ " seconds")
