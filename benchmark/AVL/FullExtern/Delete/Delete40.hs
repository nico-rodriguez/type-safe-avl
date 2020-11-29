{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module AVL.FullExtern.Delete.Delete40 (e40, main) where


import           Data.Time.Clock                (diffUTCTime, getCurrentTime)
import           Data.Proxy                     (Proxy (Proxy))
import           Data.Tree.AVL.FullExtern       (delete, AVL(AVL), mkAVL)
import           Prelude                        (IO, putStrLn, return, seq,
                                                show, (++), ($))
import           AVL.FullExtern.Insert.Insert40 (t40)


e40 = case t40 of
  AVL t40' _ _ -> mkAVL t
      where
        t = delete (Proxy::Proxy 39) $ delete (Proxy::Proxy 38) $ delete (Proxy::Proxy 37) $ delete (Proxy::Proxy 36) $ delete (Proxy::Proxy 35) $ delete (Proxy::Proxy 34) $ delete (Proxy::Proxy 33) $ delete (Proxy::Proxy 32) $ delete (Proxy::Proxy 31) $ delete (Proxy::Proxy 30) $
            delete (Proxy::Proxy 29) $ delete (Proxy::Proxy 28) $ delete (Proxy::Proxy 27) $ delete (Proxy::Proxy 26) $ delete (Proxy::Proxy 25) $ delete (Proxy::Proxy 24) $ delete (Proxy::Proxy 23) $ delete (Proxy::Proxy 22) $ delete (Proxy::Proxy 21) $ delete (Proxy::Proxy 20) $
            delete (Proxy::Proxy 19) $ delete (Proxy::Proxy 18) $ delete (Proxy::Proxy 17) $ delete (Proxy::Proxy 16) $ delete (Proxy::Proxy 15) $ delete (Proxy::Proxy 14) $ delete (Proxy::Proxy 13) $ delete (Proxy::Proxy 12) $ delete (Proxy::Proxy 11) $ delete (Proxy::Proxy 10) $
            delete (Proxy::Proxy 9) $ delete (Proxy::Proxy 8) $ delete (Proxy::Proxy 7) $ delete (Proxy::Proxy 6) $ delete (Proxy::Proxy 5) $ delete (Proxy::Proxy 4) $ delete (Proxy::Proxy 3) $ delete (Proxy::Proxy 2) $ delete (Proxy::Proxy 1) $ delete (Proxy::Proxy 0) t40'

main :: IO ()
main = do seq t40 (return ())
          t0 <- getCurrentTime
          seq e40 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
