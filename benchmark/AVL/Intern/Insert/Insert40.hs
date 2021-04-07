{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module AVL.Intern.Insert.Insert40 (t40', main) where

import           Data.Proxy             (Proxy (Proxy))
import           Data.Time.Clock        (diffUTCTime, getCurrentTime)
import           Data.Tree.AVL.Intern   (insertAVL)
import           Prelude                (IO, putStrLn, return, seq, show, ($), (++))
import           AVL.Intern.Example.Example40 (t40)


t40' = insertAVL (Proxy::Proxy 40) 'a' t40

main :: IO ()
main = do t0 <- getCurrentTime
          seq t40' (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t1 t0) ++ " seconds")
