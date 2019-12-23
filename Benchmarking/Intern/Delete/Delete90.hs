{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module Benchmarking.Intern.Delete.Delete90 (e90, main) where


import           Data.Time.Clock (diffUTCTime, getCurrentTime)
import           Data.Proxy                     (Proxy (Proxy))
import           Intern.AVL                     (deleteAVL)
import           ITree                          (Tree (EmptyTree), ITree (EmptyITree))
import           Prelude                        (Bool (False), IO, putStrLn,
                                                 return, seq, show, (++), ($))
import           Benchmarking.Intern.Insert.Insert90 (t90)


e90 = deleteAVL (mkNode (Proxy::Proxy 89) 'a') $ deleteAVL (mkNode (Proxy::Proxy 88) 'a') $ deleteAVL (mkNode (Proxy::Proxy 87) 'a') $ deleteAVL (mkNode (Proxy::Proxy 86) 'a') $ deleteAVL (mkNode (Proxy::Proxy 85) 'a') $ deleteAVL (mkNode (Proxy::Proxy 84) 'a') $ deleteAVL (mkNode (Proxy::Proxy 83) 'a') $ deleteAVL (mkNode (Proxy::Proxy 82) 'a') $ deleteAVL (mkNode (Proxy::Proxy 81) 'a') $ deleteAVL (mkNode (Proxy::Proxy 80) 'a') $
deleteAVL (mkNode (Proxy::Proxy 79) 'a') $ deleteAVL (mkNode (Proxy::Proxy 78) 'a') $ deleteAVL (mkNode (Proxy::Proxy 77) 'a') $ deleteAVL (mkNode (Proxy::Proxy 76) 'a') $ deleteAVL (mkNode (Proxy::Proxy 75) 'a') $ deleteAVL (mkNode (Proxy::Proxy 74) 'a') $ deleteAVL (mkNode (Proxy::Proxy 73) 'a') $ deleteAVL (mkNode (Proxy::Proxy 72) 'a') $ deleteAVL (mkNode (Proxy::Proxy 71) 'a') $ deleteAVL (mkNode (Proxy::Proxy 70) 'a') $
  deleteAVL (mkNode (Proxy::Proxy 69) 'a') $ deleteAVL (mkNode (Proxy::Proxy 68) 'a') $ deleteAVL (mkNode (Proxy::Proxy 67) 'a') $ deleteAVL (mkNode (Proxy::Proxy 66) 'a') $ deleteAVL (mkNode (Proxy::Proxy 65) 'a') $ deleteAVL (mkNode (Proxy::Proxy 64) 'a') $ deleteAVL (mkNode (Proxy::Proxy 63) 'a') $ deleteAVL (mkNode (Proxy::Proxy 62) 'a') $ deleteAVL (mkNode (Proxy::Proxy 61) 'a') $ deleteAVL (mkNode (Proxy::Proxy 60) 'a') $
    deleteAVL (mkNode (Proxy::Proxy 59) 'a') $ deleteAVL (mkNode (Proxy::Proxy 58) 'a') $ deleteAVL (mkNode (Proxy::Proxy 57) 'a') $ deleteAVL (mkNode (Proxy::Proxy 56) 'a') $ deleteAVL (mkNode (Proxy::Proxy 55) 'a') $ deleteAVL (mkNode (Proxy::Proxy 54) 'a') $ deleteAVL (mkNode (Proxy::Proxy 53) 'a') $ deleteAVL (mkNode (Proxy::Proxy 52) 'a') $ deleteAVL (mkNode (Proxy::Proxy 51) 'a') $ deleteAVL (mkNode (Proxy::Proxy 50) 'a') $
      deleteAVL (mkNode (Proxy::Proxy 49) 'a') $ deleteAVL (mkNode (Proxy::Proxy 48) 'a') $ deleteAVL (mkNode (Proxy::Proxy 47) 'a') $ deleteAVL (mkNode (Proxy::Proxy 46) 'a') $ deleteAVL (mkNode (Proxy::Proxy 45) 'a') $ deleteAVL (mkNode (Proxy::Proxy 44) 'a') $ deleteAVL (mkNode (Proxy::Proxy 43) 'a') $ deleteAVL (mkNode (Proxy::Proxy 42) 'a') $ deleteAVL (mkNode (Proxy::Proxy 41) 'a') $ deleteAVL (mkNode (Proxy::Proxy 40) 'a') $
        deleteAVL (mkNode (Proxy::Proxy 39) 'a') $ deleteAVL (mkNode (Proxy::Proxy 38) 'a') $ deleteAVL (mkNode (Proxy::Proxy 37) 'a') $ deleteAVL (mkNode (Proxy::Proxy 36) 'a') $ deleteAVL (mkNode (Proxy::Proxy 35) 'a') $ deleteAVL (mkNode (Proxy::Proxy 34) 'a') $ deleteAVL (mkNode (Proxy::Proxy 33) 'a') $ deleteAVL (mkNode (Proxy::Proxy 32) 'a') $ deleteAVL (mkNode (Proxy::Proxy 31) 'a') $ deleteAVL (mkNode (Proxy::Proxy 30) 'a') $
          deleteAVL (mkNode (Proxy::Proxy 29) 'a') $ deleteAVL (mkNode (Proxy::Proxy 28) 'a') $ deleteAVL (mkNode (Proxy::Proxy 27) 'a') $ deleteAVL (mkNode (Proxy::Proxy 26) 'a') $ deleteAVL (mkNode (Proxy::Proxy 25) 'a') $ deleteAVL (mkNode (Proxy::Proxy 24) 'a') $ deleteAVL (mkNode (Proxy::Proxy 23) 'a') $ deleteAVL (mkNode (Proxy::Proxy 22) 'a') $ deleteAVL (mkNode (Proxy::Proxy 21) 'a') $ deleteAVL (mkNode (Proxy::Proxy 20) 'a') $
            deleteAVL (mkNode (Proxy::Proxy 19) 'a') $ deleteAVL (mkNode (Proxy::Proxy 18) 'a') $ deleteAVL (mkNode (Proxy::Proxy 17) 'a') $ deleteAVL (mkNode (Proxy::Proxy 16) 'a') $ deleteAVL (mkNode (Proxy::Proxy 15) 'a') $ deleteAVL (mkNode (Proxy::Proxy 14) 'a') $ deleteAVL (mkNode (Proxy::Proxy 13) 'a') $ deleteAVL (mkNode (Proxy::Proxy 12) 'a') $ deleteAVL (mkNode (Proxy::Proxy 11) 'a') $ deleteAVL (mkNode (Proxy::Proxy 10) 'a') $
              deleteAVL (mkNode (Proxy::Proxy 9) 'a') $ deleteAVL (mkNode (Proxy::Proxy 8) 'a') $ deleteAVL (mkNode (Proxy::Proxy 7) 'a') $ deleteAVL (mkNode (Proxy::Proxy 6) 'a') $ deleteAVL (mkNode (Proxy::Proxy 5) 'a') $ deleteAVL (mkNode (Proxy::Proxy 4) 'a') $ deleteAVL (mkNode (Proxy::Proxy 3) 'a') $ deleteAVL (mkNode (Proxy::Proxy 2) 'a') $ deleteAVL (mkNode (Proxy::Proxy 1) 'a') $ deleteAVL (mkNode (Proxy::Proxy 0) 'a') $ t90

main :: IO ()
main = do seq t90 (return ())
          t0 <- getCurrentTime
          seq e90 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
