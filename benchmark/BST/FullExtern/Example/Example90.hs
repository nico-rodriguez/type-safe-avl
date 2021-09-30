{-# OPTIONS_HADDOCK hide, prune #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module BST.FullExtern.Example.Example90 (t90, main) where

import           Data.Proxy               (Proxy (Proxy))
import           Data.Time.Clock          (diffUTCTime,
                                           getCurrentTime)
import           Data.Tree.Node           (mkNode)
import           Data.Tree.BST.FullExtern (ITree (EmptyITree), insert, mkBST)
import           Prelude                  (IO, putStrLn, return, seq,
                                           show, ($), (++))


t90 = mkBST t
  where
    t = insert (mkNode (Proxy::Proxy 89) 'a') $ insert (mkNode (Proxy::Proxy 88) 'a') $ insert (mkNode (Proxy::Proxy 87) 'a') $ insert (mkNode (Proxy::Proxy 86) 'a') $ insert (mkNode (Proxy::Proxy 85) 'a') $ insert (mkNode (Proxy::Proxy 84) 'a') $ insert (mkNode (Proxy::Proxy 83) 'a') $ insert (mkNode (Proxy::Proxy 82) 'a') $ insert (mkNode (Proxy::Proxy 81) 'a') $ insert (mkNode (Proxy::Proxy 80) 'a') $ 
        insert (mkNode (Proxy::Proxy 79) 'a') $ insert (mkNode (Proxy::Proxy 78) 'a') $ insert (mkNode (Proxy::Proxy 77) 'a') $ insert (mkNode (Proxy::Proxy 76) 'a') $ insert (mkNode (Proxy::Proxy 75) 'a') $ insert (mkNode (Proxy::Proxy 74) 'a') $ insert (mkNode (Proxy::Proxy 73) 'a') $ insert (mkNode (Proxy::Proxy 72) 'a') $ insert (mkNode (Proxy::Proxy 71) 'a') $ insert (mkNode (Proxy::Proxy 70) 'a') $
        insert (mkNode (Proxy::Proxy 69) 'a') $ insert (mkNode (Proxy::Proxy 68) 'a') $ insert (mkNode (Proxy::Proxy 67) 'a') $ insert (mkNode (Proxy::Proxy 66) 'a') $ insert (mkNode (Proxy::Proxy 65) 'a') $ insert (mkNode (Proxy::Proxy 64) 'a') $ insert (mkNode (Proxy::Proxy 63) 'a') $ insert (mkNode (Proxy::Proxy 62) 'a') $ insert (mkNode (Proxy::Proxy 61) 'a') $ insert (mkNode (Proxy::Proxy 60) 'a') $
        insert (mkNode (Proxy::Proxy 59) 'a') $ insert (mkNode (Proxy::Proxy 58) 'a') $ insert (mkNode (Proxy::Proxy 57) 'a') $ insert (mkNode (Proxy::Proxy 56) 'a') $ insert (mkNode (Proxy::Proxy 55) 'a') $ insert (mkNode (Proxy::Proxy 54) 'a') $ insert (mkNode (Proxy::Proxy 53) 'a') $ insert (mkNode (Proxy::Proxy 52) 'a') $ insert (mkNode (Proxy::Proxy 51) 'a') $ insert (mkNode (Proxy::Proxy 50) 'a') $
        insert (mkNode (Proxy::Proxy 49) 'a') $ insert (mkNode (Proxy::Proxy 48) 'a') $ insert (mkNode (Proxy::Proxy 47) 'a') $ insert (mkNode (Proxy::Proxy 46) 'a') $ insert (mkNode (Proxy::Proxy 45) 'a') $ insert (mkNode (Proxy::Proxy 44) 'a') $ insert (mkNode (Proxy::Proxy 43) 'a') $ insert (mkNode (Proxy::Proxy 42) 'a') $ insert (mkNode (Proxy::Proxy 41) 'a') $ insert (mkNode (Proxy::Proxy 40) 'a') $
        insert (mkNode (Proxy::Proxy 39) 'a') $ insert (mkNode (Proxy::Proxy 38) 'a') $ insert (mkNode (Proxy::Proxy 37) 'a') $ insert (mkNode (Proxy::Proxy 36) 'a') $ insert (mkNode (Proxy::Proxy 35) 'a') $ insert (mkNode (Proxy::Proxy 34) 'a') $ insert (mkNode (Proxy::Proxy 33) 'a') $ insert (mkNode (Proxy::Proxy 32) 'a') $ insert (mkNode (Proxy::Proxy 31) 'a') $ insert (mkNode (Proxy::Proxy 30) 'a') $
        insert (mkNode (Proxy::Proxy 29) 'a') $ insert (mkNode (Proxy::Proxy 28) 'a') $ insert (mkNode (Proxy::Proxy 27) 'a') $ insert (mkNode (Proxy::Proxy 26) 'a') $ insert (mkNode (Proxy::Proxy 25) 'a') $ insert (mkNode (Proxy::Proxy 24) 'a') $ insert (mkNode (Proxy::Proxy 23) 'a') $ insert (mkNode (Proxy::Proxy 22) 'a') $ insert (mkNode (Proxy::Proxy 21) 'a') $ insert (mkNode (Proxy::Proxy 20) 'a') $
        insert (mkNode (Proxy::Proxy 19) 'a') $ insert (mkNode (Proxy::Proxy 18) 'a') $ insert (mkNode (Proxy::Proxy 17) 'a') $ insert (mkNode (Proxy::Proxy 16) 'a') $ insert (mkNode (Proxy::Proxy 15) 'a') $ insert (mkNode (Proxy::Proxy 14) 'a') $ insert (mkNode (Proxy::Proxy 13) 'a') $ insert (mkNode (Proxy::Proxy 12) 'a') $ insert (mkNode (Proxy::Proxy 11) 'a') $ insert (mkNode (Proxy::Proxy 10) 'a') $
        insert (mkNode (Proxy::Proxy 9) 'a') $ insert (mkNode (Proxy::Proxy 8) 'a') $ insert (mkNode (Proxy::Proxy 7) 'a') $ insert (mkNode (Proxy::Proxy 6) 'a') $ insert (mkNode (Proxy::Proxy 5) 'a') $ insert (mkNode (Proxy::Proxy 4) 'a') $ insert (mkNode (Proxy::Proxy 3) 'a') $ insert (mkNode (Proxy::Proxy 2) 'a') $ insert (mkNode (Proxy::Proxy 1) 'a') $ insert (mkNode (Proxy::Proxy 0) 'a') EmptyITree

main :: IO ()
main = do t0 <- getCurrentTime
          seq t90 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
