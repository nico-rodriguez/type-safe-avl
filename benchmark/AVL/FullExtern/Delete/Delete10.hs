{-# OPTIONS_HADDOCK hide, prune #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module AVL.FullExtern.Delete.Delete10 (e10, main) where


import           Data.Time.Clock                (diffUTCTime, getCurrentTime)
import           Data.Proxy                     (Proxy (Proxy))
import           Data.Tree.AVL.FullExtern       (delete, AVL(AVL), mkAVL)
import           Prelude                        (IO, putStrLn, return, seq,
                                                show, (++), ($))
import           AVL.FullExtern.Example.Example10 (t10)


e10 = case t10 of
  AVL t10' _ _ -> mkAVL t
      where
        t = delete (Proxy::Proxy 9) t10'

main :: IO ()
main = do seq t10 (return ())
          t0 <- getCurrentTime
          seq e10 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
