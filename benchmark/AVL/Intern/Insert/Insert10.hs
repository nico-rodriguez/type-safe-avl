{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module AVL.Intern.Insert.Insert10 (t10', main) where

import           Data.Proxy             (Proxy (Proxy))
import           Data.Time.Clock        (diffUTCTime, getCurrentTime)
import           Data.Tree.AVL.Intern   (insertAVL)
import           Prelude                (IO, putStrLn, return, seq, show, ($), (++))
import           AVL.Intern.Example.Example10 (t10)


t10' = insertAVL (Proxy::Proxy 10) 'a' t10

main :: IO ()
main = do t0 <- getCurrentTime
          seq t10' (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t1 t0) ++ " seconds")
