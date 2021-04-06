{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module BST.Extern.Delete.Delete50 (e50, main) where


import           Data.Time.Clock            (diffUTCTime, getCurrentTime)
import           Data.Proxy                 (Proxy (Proxy))
import           Data.Tree.BST.Extern       (deleteBST)
import           Prelude                    (IO, putStrLn, return, seq,
                                            show, (++), ($))
import           BST.Extern.Example.Example50 (t50)


e50 = deleteBST (Proxy::Proxy 49) t50

main :: IO ()
main = do seq t50 (return ())
          t0 <- getCurrentTime
          seq e50 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t1 t0) ++ " seconds")
