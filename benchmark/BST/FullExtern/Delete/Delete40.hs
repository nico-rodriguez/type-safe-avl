{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module BST.FullExtern.Delete.Delete40 (e40, main) where


import           Data.Time.Clock                (diffUTCTime, getCurrentTime)
import           Data.Proxy                     (Proxy (Proxy))
import           Data.Tree.BST.FullExtern       (delete, BST(BST), mkBST)
import           Prelude                        (IO, putStrLn, return, seq,
                                                show, (++), ($))
import           BST.FullExtern.Example.Example40 (t40)


e40 = case t40 of
  BST t40' _ -> mkBST t
      where
        t = delete (Proxy::Proxy 39) t40'

main :: IO ()
main = do seq t40 (return ())
          t0 <- getCurrentTime
          seq e40 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
