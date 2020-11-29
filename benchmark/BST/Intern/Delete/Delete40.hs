{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module BST.Intern.Delete.Delete40 (e40, main) where


import           Data.Time.Clock            (diffUTCTime, getCurrentTime)
import           Data.Proxy                 (Proxy (Proxy))
import           Data.Tree.BST.Intern       (deleteBST)
import           Prelude                    (IO, putStrLn, return, seq,
                                            show, (++), ($))
import           BST.Intern.Insert.Insert40 (t40)


e40 = deleteBST (Proxy::Proxy 39) $ deleteBST (Proxy::Proxy 38) $ deleteBST (Proxy::Proxy 37) $ deleteBST (Proxy::Proxy 36) $ deleteBST (Proxy::Proxy 35) $ deleteBST (Proxy::Proxy 34) $ deleteBST (Proxy::Proxy 33) $ deleteBST (Proxy::Proxy 32) $ deleteBST (Proxy::Proxy 31) $ deleteBST (Proxy::Proxy 30) $
      deleteBST (Proxy::Proxy 29) $ deleteBST (Proxy::Proxy 28) $ deleteBST (Proxy::Proxy 27) $ deleteBST (Proxy::Proxy 26) $ deleteBST (Proxy::Proxy 25) $ deleteBST (Proxy::Proxy 24) $ deleteBST (Proxy::Proxy 23) $ deleteBST (Proxy::Proxy 22) $ deleteBST (Proxy::Proxy 21) $ deleteBST (Proxy::Proxy 20) $
      deleteBST (Proxy::Proxy 19) $ deleteBST (Proxy::Proxy 18) $ deleteBST (Proxy::Proxy 17) $ deleteBST (Proxy::Proxy 16) $ deleteBST (Proxy::Proxy 15) $ deleteBST (Proxy::Proxy 14) $ deleteBST (Proxy::Proxy 13) $ deleteBST (Proxy::Proxy 12) $ deleteBST (Proxy::Proxy 11) $ deleteBST (Proxy::Proxy 10) $
      deleteBST (Proxy::Proxy 9) $ deleteBST (Proxy::Proxy 8) $ deleteBST (Proxy::Proxy 7) $ deleteBST (Proxy::Proxy 6) $ deleteBST (Proxy::Proxy 5) $ deleteBST (Proxy::Proxy 4) $ deleteBST (Proxy::Proxy 3) $ deleteBST (Proxy::Proxy 2) $ deleteBST (Proxy::Proxy 1) $ deleteBST (Proxy::Proxy 0) t40

main :: IO ()
main = do seq t40 (return ())
          t0 <- getCurrentTime
          seq e40 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
