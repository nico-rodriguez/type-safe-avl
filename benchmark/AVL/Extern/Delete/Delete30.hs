{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module AVL.Extern.Delete.Delete30 (e30, main) where


import           Data.Time.Clock            (diffUTCTime, getCurrentTime)
import           Data.Proxy                 (Proxy (Proxy))
import           Data.Tree.AVL.Extern       (deleteAVL)
import           Prelude                    (IO, putStrLn, return, seq,
                                            show, (++), ($))
import           AVL.Extern.Example.Example30 (t30)


e30 = deleteAVL (Proxy::Proxy 29) $ deleteAVL (Proxy::Proxy 28) $ deleteAVL (Proxy::Proxy 27) $ deleteAVL (Proxy::Proxy 26) $ deleteAVL (Proxy::Proxy 25) $ deleteAVL (Proxy::Proxy 24) $ deleteAVL (Proxy::Proxy 23) $ deleteAVL (Proxy::Proxy 22) $ deleteAVL (Proxy::Proxy 21) $ deleteAVL (Proxy::Proxy 20) $
      deleteAVL (Proxy::Proxy 19) $ deleteAVL (Proxy::Proxy 18) $ deleteAVL (Proxy::Proxy 17) $ deleteAVL (Proxy::Proxy 16) $ deleteAVL (Proxy::Proxy 15) $ deleteAVL (Proxy::Proxy 14) $ deleteAVL (Proxy::Proxy 13) $ deleteAVL (Proxy::Proxy 12) $ deleteAVL (Proxy::Proxy 11) $ deleteAVL (Proxy::Proxy 10) $
      deleteAVL (Proxy::Proxy 9) $ deleteAVL (Proxy::Proxy 8) $ deleteAVL (Proxy::Proxy 7) $ deleteAVL (Proxy::Proxy 6) $ deleteAVL (Proxy::Proxy 5) $ deleteAVL (Proxy::Proxy 4) $ deleteAVL (Proxy::Proxy 3) $ deleteAVL (Proxy::Proxy 2) $ deleteAVL (Proxy::Proxy 1) $ deleteAVL (Proxy::Proxy 0) t30

main :: IO ()
main = do seq t30 (return ())
          t0 <- getCurrentTime
          seq e30 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t1 t0) ++ " seconds")
