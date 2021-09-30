{-# OPTIONS_HADDOCK hide, prune #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module AVL.Intern.Insert.Insert30 (t30', main) where

import           Data.Proxy             (Proxy (Proxy))
import           Data.Time.Clock        (diffUTCTime, getCurrentTime)
import           Data.Tree.AVL.Intern   (insertAVL)
import           Prelude                (IO, putStrLn, return, seq, show, ($), (++))
import           AVL.Intern.Example.Example30 (t30)


t30' = insertAVL (Proxy::Proxy 30) 'a' t30

main :: IO ()
main = do t0 <- getCurrentTime
          seq t30' (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t1 t0) ++ " seconds")
