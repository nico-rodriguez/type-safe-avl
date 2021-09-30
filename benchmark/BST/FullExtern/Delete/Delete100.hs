{-# OPTIONS_HADDOCK hide, prune #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module BST.FullExtern.Delete.Delete100 (e100, main) where


import           Data.Time.Clock                (diffUTCTime, getCurrentTime)
import           Data.Proxy                     (Proxy (Proxy))
import           Data.Tree.BST.FullExtern       (delete, BST(BST), mkBST)
import           Prelude                        (IO, putStrLn, return, seq,
                                                show, (++), ($))
import           BST.FullExtern.Example.Example100 (t100)


e100 = case t100 of
  BST t100' _ -> mkBST t
      where
        t = delete (Proxy::Proxy 99) t100'

main :: IO ()
main = do seq t100 (return ())
          t0 <- getCurrentTime
          seq e100 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
