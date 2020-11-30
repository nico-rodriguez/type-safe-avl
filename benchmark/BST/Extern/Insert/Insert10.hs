{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module BST.Extern.Insert.Insert10 (t10, main) where

import           Data.Proxy             (Proxy (Proxy))
import           Data.Time.Clock        (diffUTCTime, getCurrentTime)
import           Data.Tree.BST.Extern   (emptyBST, insertBST)
import           Prelude                (IO, putStrLn, return, seq, show, ($), (++))


t10 = insertBST (Proxy::Proxy 9) 'a' $ insertBST (Proxy::Proxy 8) 'a' $ insertBST (Proxy::Proxy 7) 'a' $ insertBST (Proxy::Proxy 6) 'a' $ insertBST (Proxy::Proxy 5) 'a' $ insertBST (Proxy::Proxy 4) 'a' $ insertBST (Proxy::Proxy 3) 'a' $ insertBST (Proxy::Proxy 2) 'a' $ insertBST (Proxy::Proxy 1) 'a' $ insertBST (Proxy::Proxy 0) 'a' emptyBST

main :: IO ()
main = do t0 <- getCurrentTime
          seq t10 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t1 t0) ++ " seconds")
