{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module Benchmarking.Extern.Delete.Delete50 (e50, main) where


import           Data.Time.Clock (diffUTCTime, getCurrentTime)
import           Data.Proxy                     (Proxy (Proxy))
import           Extern.AVL                     (deleteAVL)
import           ITree                          (Tree (EmptyTree), ITree (EmptyITree))
import           Prelude                        (Bool (False), IO, putStrLn,
                                                 return, seq, show, (++), ($))
import           Benchmarking.Extern.Insert.Insert50 (t50)


e50 = deleteAVL (mkNode (Proxy::Proxy 49) 'a') $ deleteAVL (mkNode (Proxy::Proxy 48) 'a') $ deleteAVL (mkNode (Proxy::Proxy 47) 'a') $ deleteAVL (mkNode (Proxy::Proxy 46) 'a') $ deleteAVL (mkNode (Proxy::Proxy 45) 'a') $ deleteAVL (mkNode (Proxy::Proxy 44) 'a') $ deleteAVL (mkNode (Proxy::Proxy 43) 'a') $ deleteAVL (mkNode (Proxy::Proxy 42) 'a') $ deleteAVL (mkNode (Proxy::Proxy 41) 'a') $ deleteAVL (mkNode (Proxy::Proxy 40) 'a') $
  deleteAVL (mkNode (Proxy::Proxy 39) 'a') $ deleteAVL (mkNode (Proxy::Proxy 38) 'a') $ deleteAVL (mkNode (Proxy::Proxy 37) 'a') $ deleteAVL (mkNode (Proxy::Proxy 36) 'a') $ deleteAVL (mkNode (Proxy::Proxy 35) 'a') $ deleteAVL (mkNode (Proxy::Proxy 34) 'a') $ deleteAVL (mkNode (Proxy::Proxy 33) 'a') $ deleteAVL (mkNode (Proxy::Proxy 32) 'a') $ deleteAVL (mkNode (Proxy::Proxy 31) 'a') $ deleteAVL (mkNode (Proxy::Proxy 30) 'a') $
    deleteAVL (mkNode (Proxy::Proxy 29) 'a') $ deleteAVL (mkNode (Proxy::Proxy 28) 'a') $ deleteAVL (mkNode (Proxy::Proxy 27) 'a') $ deleteAVL (mkNode (Proxy::Proxy 26) 'a') $ deleteAVL (mkNode (Proxy::Proxy 25) 'a') $ deleteAVL (mkNode (Proxy::Proxy 24) 'a') $ deleteAVL (mkNode (Proxy::Proxy 23) 'a') $ deleteAVL (mkNode (Proxy::Proxy 22) 'a') $ deleteAVL (mkNode (Proxy::Proxy 21) 'a') $ deleteAVL (mkNode (Proxy::Proxy 20) 'a') $
      deleteAVL (mkNode (Proxy::Proxy 19) 'a') $ deleteAVL (mkNode (Proxy::Proxy 18) 'a') $ deleteAVL (mkNode (Proxy::Proxy 17) 'a') $ deleteAVL (mkNode (Proxy::Proxy 16) 'a') $ deleteAVL (mkNode (Proxy::Proxy 15) 'a') $ deleteAVL (mkNode (Proxy::Proxy 14) 'a') $ deleteAVL (mkNode (Proxy::Proxy 13) 'a') $ deleteAVL (mkNode (Proxy::Proxy 12) 'a') $ deleteAVL (mkNode (Proxy::Proxy 11) 'a') $ deleteAVL (mkNode (Proxy::Proxy 10) 'a') $
        deleteAVL (mkNode (Proxy::Proxy 9) 'a') $ deleteAVL (mkNode (Proxy::Proxy 8) 'a') $ deleteAVL (mkNode (Proxy::Proxy 7) 'a') $ deleteAVL (mkNode (Proxy::Proxy 6) 'a') $ deleteAVL (mkNode (Proxy::Proxy 5) 'a') $ deleteAVL (mkNode (Proxy::Proxy 4) 'a') $ deleteAVL (mkNode (Proxy::Proxy 3) 'a') $ deleteAVL (mkNode (Proxy::Proxy 2) 'a') $ deleteAVL (mkNode (Proxy::Proxy 1) 'a') $ deleteAVL (mkNode (Proxy::Proxy 0) 'a') $ t50

main :: IO ()
main = do seq t50 (return ())
          t0 <- getCurrentTime
          seq e50 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
