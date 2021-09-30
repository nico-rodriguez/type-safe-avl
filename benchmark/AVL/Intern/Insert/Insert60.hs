{-# OPTIONS_HADDOCK hide, prune #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module AVL.Intern.Insert.Insert60 (t60', main) where

import           Data.Proxy             (Proxy (Proxy))
import           Data.Time.Clock        (diffUTCTime, getCurrentTime)
import           Data.Tree.AVL.Intern   (insertAVL)
import           Prelude                (IO, putStrLn, return, seq, show, ($), (++))
import           AVL.Intern.Example.Example60 (t60)


t60' = insertAVL (Proxy::Proxy 60) 'a' t60

main :: IO ()
main = do t0 <- getCurrentTime
          seq t60' (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t1 t0) ++ " seconds")
