{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module BST.FullExtern.Delete.Delete60 (e60, main) where


import           Data.Time.Clock                (diffUTCTime, getCurrentTime)
import           Data.Proxy                     (Proxy (Proxy))
import           Data.Tree.BST.FullExtern       (delete, BST(BST), mkBST)
import           Prelude                        (IO, putStrLn, return, seq,
                                                show, (++), ($))
import           BST.FullExtern.Example.Example60 (t60)


e60 = case t60 of
  BST t60' _ -> mkBST t
      where
        t = delete (Proxy::Proxy 59) t60'

main :: IO ()
main = do seq t60 (return ())
          t0 <- getCurrentTime
          seq e60 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
