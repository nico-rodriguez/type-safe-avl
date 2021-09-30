{-# OPTIONS_HADDOCK hide, prune #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module AVL.Intern.Example.Example10 (t10, main) where

import           Data.Proxy             (Proxy (Proxy))
import           Data.Time.Clock        (diffUTCTime, getCurrentTime)
import           Data.Tree.AVL.Intern   (emptyAVL, insertAVL)
import           Prelude                (IO, putStrLn, return, seq, show, ($), (++))


t10 = insertAVL (Proxy::Proxy 9) 'a' $ insertAVL (Proxy::Proxy 8) 'a' $ insertAVL (Proxy::Proxy 7) 'a' $ insertAVL (Proxy::Proxy 6) 'a' $ insertAVL (Proxy::Proxy 5) 'a' $ insertAVL (Proxy::Proxy 4) 'a' $ insertAVL (Proxy::Proxy 3) 'a' $ insertAVL (Proxy::Proxy 2) 'a' $ insertAVL (Proxy::Proxy 1) 'a' $ insertAVL (Proxy::Proxy 0) 'a' emptyAVL

main :: IO ()
main = do t0 <- getCurrentTime
          seq t10 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t1 t0) ++ " seconds")
