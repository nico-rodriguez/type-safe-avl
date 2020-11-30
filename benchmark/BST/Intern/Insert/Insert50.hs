{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module BST.Intern.Insert.Insert50 (t50, main) where

import           Data.Proxy             (Proxy (Proxy))
import           Data.Time.Clock        (diffUTCTime, getCurrentTime)
import           Data.Tree.BST.Intern   (emptyBST, insertBST)
import           Prelude                (IO, putStrLn, return, seq, show, ($), (++))


t50 = insertBST (Proxy::Proxy 49) 'a' $ insertBST (Proxy::Proxy 48) 'a' $ insertBST (Proxy::Proxy 47) 'a' $ insertBST (Proxy::Proxy 46) 'a' $ insertBST (Proxy::Proxy 45) 'a' $ insertBST (Proxy::Proxy 44) 'a' $ insertBST (Proxy::Proxy 43) 'a' $ insertBST (Proxy::Proxy 42) 'a' $ insertBST (Proxy::Proxy 41) 'a' $ insertBST (Proxy::Proxy 40) 'a' $
      insertBST (Proxy::Proxy 39) 'a' $ insertBST (Proxy::Proxy 38) 'a' $ insertBST (Proxy::Proxy 37) 'a' $ insertBST (Proxy::Proxy 36) 'a' $ insertBST (Proxy::Proxy 35) 'a' $ insertBST (Proxy::Proxy 34) 'a' $ insertBST (Proxy::Proxy 33) 'a' $ insertBST (Proxy::Proxy 32) 'a' $ insertBST (Proxy::Proxy 31) 'a' $ insertBST (Proxy::Proxy 30) 'a' $
      insertBST (Proxy::Proxy 29) 'a' $ insertBST (Proxy::Proxy 28) 'a' $ insertBST (Proxy::Proxy 27) 'a' $ insertBST (Proxy::Proxy 26) 'a' $ insertBST (Proxy::Proxy 25) 'a' $ insertBST (Proxy::Proxy 24) 'a' $ insertBST (Proxy::Proxy 23) 'a' $ insertBST (Proxy::Proxy 22) 'a' $ insertBST (Proxy::Proxy 21) 'a' $ insertBST (Proxy::Proxy 20) 'a' $
      insertBST (Proxy::Proxy 19) 'a' $ insertBST (Proxy::Proxy 18) 'a' $ insertBST (Proxy::Proxy 17) 'a' $ insertBST (Proxy::Proxy 16) 'a' $ insertBST (Proxy::Proxy 15) 'a' $ insertBST (Proxy::Proxy 14) 'a' $ insertBST (Proxy::Proxy 13) 'a' $ insertBST (Proxy::Proxy 12) 'a' $ insertBST (Proxy::Proxy 11) 'a' $ insertBST (Proxy::Proxy 10) 'a' $
      insertBST (Proxy::Proxy 9) 'a' $ insertBST (Proxy::Proxy 8) 'a' $ insertBST (Proxy::Proxy 7) 'a' $ insertBST (Proxy::Proxy 6) 'a' $ insertBST (Proxy::Proxy 5) 'a' $ insertBST (Proxy::Proxy 4) 'a' $ insertBST (Proxy::Proxy 3) 'a' $ insertBST (Proxy::Proxy 2) 'a' $ insertBST (Proxy::Proxy 1) 'a' $ insertBST (Proxy::Proxy 0) 'a' emptyBST

main :: IO ()
main = do t0 <- getCurrentTime
          seq t50 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t1 t0) ++ " seconds")
