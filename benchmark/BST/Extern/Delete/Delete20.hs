{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module BST.Extern.Delete.Delete20 (e20, main) where


import           Data.Time.Clock            (diffUTCTime, getCurrentTime)
import           Data.Proxy                 (Proxy (Proxy))
import           Data.Tree.BST.Extern       (deleteBST)
import           Prelude                    (IO, putStrLn, return, seq,
                                            show, (++), ($))
import           BST.Extern.Insert.Insert20 (t20)


e20 = deleteBST (Proxy::Proxy 19) $ deleteBST (Proxy::Proxy 18) $ deleteBST (Proxy::Proxy 17) $ deleteBST (Proxy::Proxy 16) $ deleteBST (Proxy::Proxy 15) $ deleteBST (Proxy::Proxy 14) $ deleteBST (Proxy::Proxy 13) $ deleteBST (Proxy::Proxy 12) $ deleteBST (Proxy::Proxy 11) $ deleteBST (Proxy::Proxy 10) $
      deleteBST (Proxy::Proxy 9) $ deleteBST (Proxy::Proxy 8) $ deleteBST (Proxy::Proxy 7) $ deleteBST (Proxy::Proxy 6) $ deleteBST (Proxy::Proxy 5) $ deleteBST (Proxy::Proxy 4) $ deleteBST (Proxy::Proxy 3) $ deleteBST (Proxy::Proxy 2) $ deleteBST (Proxy::Proxy 1) $ deleteBST (Proxy::Proxy 0) t20

main :: IO ()
main = do seq t20 (return ())
          t0 <- getCurrentTime
          seq e20 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t1 t0) ++ " seconds")
