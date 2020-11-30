{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module BST.Extern.Delete.Delete60 (e60, main) where


import           Data.Time.Clock            (diffUTCTime, getCurrentTime)
import           Data.Proxy                 (Proxy (Proxy))
import           Data.Tree.BST.Extern       (deleteBST)
import           Prelude                    (IO, putStrLn, return, seq,
                                            show, (++), ($))
import           BST.Extern.Insert.Insert60 (t60)


e60 = deleteBST (Proxy::Proxy 59) $ deleteBST (Proxy::Proxy 58) $ deleteBST (Proxy::Proxy 57) $ deleteBST (Proxy::Proxy 56) $ deleteBST (Proxy::Proxy 55) $ deleteBST (Proxy::Proxy 54) $ deleteBST (Proxy::Proxy 53) $ deleteBST (Proxy::Proxy 52) $ deleteBST (Proxy::Proxy 51) $ deleteBST (Proxy::Proxy 50) $
      deleteBST (Proxy::Proxy 49) $ deleteBST (Proxy::Proxy 48) $ deleteBST (Proxy::Proxy 47) $ deleteBST (Proxy::Proxy 46) $ deleteBST (Proxy::Proxy 45) $ deleteBST (Proxy::Proxy 44) $ deleteBST (Proxy::Proxy 43) $ deleteBST (Proxy::Proxy 42) $ deleteBST (Proxy::Proxy 41) $ deleteBST (Proxy::Proxy 40) $
      deleteBST (Proxy::Proxy 39) $ deleteBST (Proxy::Proxy 38) $ deleteBST (Proxy::Proxy 37) $ deleteBST (Proxy::Proxy 36) $ deleteBST (Proxy::Proxy 35) $ deleteBST (Proxy::Proxy 34) $ deleteBST (Proxy::Proxy 33) $ deleteBST (Proxy::Proxy 32) $ deleteBST (Proxy::Proxy 31) $ deleteBST (Proxy::Proxy 30) $
      deleteBST (Proxy::Proxy 29) $ deleteBST (Proxy::Proxy 28) $ deleteBST (Proxy::Proxy 27) $ deleteBST (Proxy::Proxy 26) $ deleteBST (Proxy::Proxy 25) $ deleteBST (Proxy::Proxy 24) $ deleteBST (Proxy::Proxy 23) $ deleteBST (Proxy::Proxy 22) $ deleteBST (Proxy::Proxy 21) $ deleteBST (Proxy::Proxy 20) $
      deleteBST (Proxy::Proxy 19) $ deleteBST (Proxy::Proxy 18) $ deleteBST (Proxy::Proxy 17) $ deleteBST (Proxy::Proxy 16) $ deleteBST (Proxy::Proxy 15) $ deleteBST (Proxy::Proxy 14) $ deleteBST (Proxy::Proxy 13) $ deleteBST (Proxy::Proxy 12) $ deleteBST (Proxy::Proxy 11) $ deleteBST (Proxy::Proxy 10) $
      deleteBST (Proxy::Proxy 9) $ deleteBST (Proxy::Proxy 8) $ deleteBST (Proxy::Proxy 7) $ deleteBST (Proxy::Proxy 6) $ deleteBST (Proxy::Proxy 5) $ deleteBST (Proxy::Proxy 4) $ deleteBST (Proxy::Proxy 3) $ deleteBST (Proxy::Proxy 2) $ deleteBST (Proxy::Proxy 1) $ deleteBST (Proxy::Proxy 0) t60

main :: IO ()
main = do seq t60 (return ())
          t0 <- getCurrentTime
          seq e60 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
