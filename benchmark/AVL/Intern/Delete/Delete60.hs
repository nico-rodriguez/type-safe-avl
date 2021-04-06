{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module AVL.Intern.Delete.Delete60 (e60, main) where


import           Data.Time.Clock            (diffUTCTime, getCurrentTime)
import           Data.Proxy                 (Proxy (Proxy))
import           Data.Tree.AVL.Intern       (deleteAVL)
import           Prelude                    (IO, putStrLn, return, seq,
                                            show, (++), ($))
import           AVL.Intern.Example.Example60 (t60)


e60 = deleteAVL (Proxy::Proxy 59) t60

main :: IO ()
main = do seq t60 (return ())
          t0 <- getCurrentTime
          seq e60 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t1 t0) ++ " seconds")
