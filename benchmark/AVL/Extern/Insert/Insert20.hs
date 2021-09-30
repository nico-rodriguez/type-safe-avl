{-# OPTIONS_HADDOCK hide, prune #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module AVL.Extern.Insert.Insert20 (t20', main) where

import           Data.Proxy             (Proxy (Proxy))
import           Data.Time.Clock        (diffUTCTime, getCurrentTime)
import           Data.Tree.AVL.Extern   (insertAVL)
import           Prelude                (IO, putStrLn, return, seq, show, ($), (++))
import           AVL.Extern.Example.Example20 (t20)


t20' = insertAVL (Proxy::Proxy 20) 'a' t20

main :: IO ()
main = do t0 <- getCurrentTime
          seq t20' (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t1 t0) ++ " seconds")
