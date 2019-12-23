{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module Benchmarking.Intern.Delete.Delete80 (e80, main) where


import           Data.Time.Clock (diffUTCTime, getCurrentTime)
import           Data.Proxy                     (Proxy (Proxy))
import           Intern.AVL                     (deleteAVL)
import           ITree                          (Tree (EmptyTree), ITree (EmptyITree))
import           Prelude                        (Bool (False), IO, putStrLn,
                                                 return, seq, show, (++), ($))
import           Benchmarking.Intern.Insert.Insert80 (t80)


e80 = deleteAVL (Proxy::Proxy 79) $ deleteAVL (Proxy::Proxy 78) $ deleteAVL (Proxy::Proxy 77) $ deleteAVL (Proxy::Proxy 76) $ deleteAVL (Proxy::Proxy 75) $ deleteAVL (Proxy::Proxy 74) $ deleteAVL (Proxy::Proxy 73) $ deleteAVL (Proxy::Proxy 72) $ deleteAVL (Proxy::Proxy 71) $ deleteAVL (Proxy::Proxy 70) $
  deleteAVL (Proxy::Proxy 69) $ deleteAVL (Proxy::Proxy 68) $ deleteAVL (Proxy::Proxy 67) $ deleteAVL (Proxy::Proxy 66) $ deleteAVL (Proxy::Proxy 65) $ deleteAVL (Proxy::Proxy 64) $ deleteAVL (Proxy::Proxy 63) $ deleteAVL (Proxy::Proxy 62) $ deleteAVL (Proxy::Proxy 61) $ deleteAVL (Proxy::Proxy 60) $
    deleteAVL (Proxy::Proxy 59) $ deleteAVL (Proxy::Proxy 58) $ deleteAVL (Proxy::Proxy 57) $ deleteAVL (Proxy::Proxy 56) $ deleteAVL (Proxy::Proxy 55) $ deleteAVL (Proxy::Proxy 54) $ deleteAVL (Proxy::Proxy 53) $ deleteAVL (Proxy::Proxy 52) $ deleteAVL (Proxy::Proxy 51) $ deleteAVL (Proxy::Proxy 50) $
      deleteAVL (Proxy::Proxy 49) $ deleteAVL (Proxy::Proxy 48) $ deleteAVL (Proxy::Proxy 47) $ deleteAVL (Proxy::Proxy 46) $ deleteAVL (Proxy::Proxy 45) $ deleteAVL (Proxy::Proxy 44) $ deleteAVL (Proxy::Proxy 43) $ deleteAVL (Proxy::Proxy 42) $ deleteAVL (Proxy::Proxy 41) $ deleteAVL (Proxy::Proxy 40) $
        deleteAVL (Proxy::Proxy 39) $ deleteAVL (Proxy::Proxy 38) $ deleteAVL (Proxy::Proxy 37) $ deleteAVL (Proxy::Proxy 36) $ deleteAVL (Proxy::Proxy 35) $ deleteAVL (Proxy::Proxy 34) $ deleteAVL (Proxy::Proxy 33) $ deleteAVL (Proxy::Proxy 32) $ deleteAVL (Proxy::Proxy 31) $ deleteAVL (Proxy::Proxy 30) $
          deleteAVL (Proxy::Proxy 29) $ deleteAVL (Proxy::Proxy 28) $ deleteAVL (Proxy::Proxy 27) $ deleteAVL (Proxy::Proxy 26) $ deleteAVL (Proxy::Proxy 25) $ deleteAVL (Proxy::Proxy 24) $ deleteAVL (Proxy::Proxy 23) $ deleteAVL (Proxy::Proxy 22) $ deleteAVL (Proxy::Proxy 21) $ deleteAVL (Proxy::Proxy 20) $
            deleteAVL (Proxy::Proxy 19) $ deleteAVL (Proxy::Proxy 18) $ deleteAVL (Proxy::Proxy 17) $ deleteAVL (Proxy::Proxy 16) $ deleteAVL (Proxy::Proxy 15) $ deleteAVL (Proxy::Proxy 14) $ deleteAVL (Proxy::Proxy 13) $ deleteAVL (Proxy::Proxy 12) $ deleteAVL (Proxy::Proxy 11) $ deleteAVL (Proxy::Proxy 10) $
              deleteAVL (Proxy::Proxy 9) $ deleteAVL (Proxy::Proxy 8) $ deleteAVL (Proxy::Proxy 7) $ deleteAVL (Proxy::Proxy 6) $ deleteAVL (Proxy::Proxy 5) $ deleteAVL (Proxy::Proxy 4) $ deleteAVL (Proxy::Proxy 3) $ deleteAVL (Proxy::Proxy 2) $ deleteAVL (Proxy::Proxy 1) $ deleteAVL (Proxy::Proxy 0) $ t80

main :: IO ()
main = do seq t80 (return ())
          t0 <- getCurrentTime
          seq e80 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
