{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module BST.FullExtern.Delete.Delete10 (e10, main) where


import           Data.Time.Clock                (diffUTCTime, getCurrentTime)
import           Data.Proxy                     (Proxy (Proxy))
import           Data.Tree.BST.FullExtern       (delete, BST(BST), mkBST)
import           Prelude                        (IO, putStrLn, return, seq,
                                                show, (++), ($))
import           BST.FullExtern.Insert.Insert10 (t10)


e10 = case t10 of
  BST t10' _ -> mkBST t
      where
        t = delete (Proxy::Proxy 9) $ delete (Proxy::Proxy 8) $ delete (Proxy::Proxy 7) $ delete (Proxy::Proxy 6) $ delete (Proxy::Proxy 5) $ delete (Proxy::Proxy 4) $ delete (Proxy::Proxy 3) $ delete (Proxy::Proxy 2) $ delete (Proxy::Proxy 1) $ delete (Proxy::Proxy 0) t10'

main :: IO ()
main = do seq t10 (return ())
          t0 <- getCurrentTime
          seq e10 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
