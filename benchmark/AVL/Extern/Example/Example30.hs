{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module AVL.Extern.Example.Example30 (t30, main) where

import           Data.Proxy             (Proxy (Proxy))
import           Data.Time.Clock        (diffUTCTime, getCurrentTime)
import           Data.Tree.AVL.Extern   (emptyAVL, insertAVL)
import           Prelude                (IO, putStrLn, return, seq, show, ($), (++))


t30 = insertAVL (Proxy::Proxy 29) 'a' $ insertAVL (Proxy::Proxy 28) 'a' $ insertAVL (Proxy::Proxy 27) 'a' $ insertAVL (Proxy::Proxy 26) 'a' $ insertAVL (Proxy::Proxy 25) 'a' $ insertAVL (Proxy::Proxy 24) 'a' $ insertAVL (Proxy::Proxy 23) 'a' $ insertAVL (Proxy::Proxy 22) 'a' $ insertAVL (Proxy::Proxy 21) 'a' $ insertAVL (Proxy::Proxy 20) 'a' $
      insertAVL (Proxy::Proxy 19) 'a' $ insertAVL (Proxy::Proxy 18) 'a' $ insertAVL (Proxy::Proxy 17) 'a' $ insertAVL (Proxy::Proxy 16) 'a' $ insertAVL (Proxy::Proxy 15) 'a' $ insertAVL (Proxy::Proxy 14) 'a' $ insertAVL (Proxy::Proxy 13) 'a' $ insertAVL (Proxy::Proxy 12) 'a' $ insertAVL (Proxy::Proxy 11) 'a' $ insertAVL (Proxy::Proxy 10) 'a' $
      insertAVL (Proxy::Proxy 9) 'a' $ insertAVL (Proxy::Proxy 8) 'a' $ insertAVL (Proxy::Proxy 7) 'a' $ insertAVL (Proxy::Proxy 6) 'a' $ insertAVL (Proxy::Proxy 5) 'a' $ insertAVL (Proxy::Proxy 4) 'a' $ insertAVL (Proxy::Proxy 3) 'a' $ insertAVL (Proxy::Proxy 2) 'a' $ insertAVL (Proxy::Proxy 1) 'a' $ insertAVL (Proxy::Proxy 0) 'a' emptyAVL

main :: IO ()
main = do t0 <- getCurrentTime
          seq t30 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t1 t0) ++ " seconds")
