{-# OPTIONS_HADDOCK hide, prune #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module AVL.FullExtern.Insert.Insert100 (t100', main) where

import           Data.Proxy               (Proxy (Proxy))
import           Data.Time.Clock          (diffUTCTime,
                                           getCurrentTime)
import           Data.Tree.Node           (mkNode)
import           Data.Tree.AVL.FullExtern (AVL (AVL), ITree (EmptyITree),
                                           insert, mkAVL)
import           Prelude                  (IO, putStrLn, return, seq,
                                           show, ($), (++))
import           AVL.FullExtern.Example.Example100 (t100)


t100' = case t100 of
  AVL t' _ _ -> mkAVL $ insert (mkNode (Proxy::Proxy 100) 'a') t'

main :: IO ()
main = do t0 <- getCurrentTime
          seq t100' (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
