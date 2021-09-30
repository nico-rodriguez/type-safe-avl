{-# OPTIONS_HADDOCK hide, prune #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module AVL.FullExtern.Insert.Insert80 (t80', main) where

import           Data.Proxy               (Proxy (Proxy))
import           Data.Time.Clock          (diffUTCTime,
                                           getCurrentTime)
import           Data.Tree.Node           (mkNode)
import           Data.Tree.AVL.FullExtern (AVL (AVL), ITree (EmptyITree),
                                           insert, mkAVL)
import           Prelude                  (IO, putStrLn, return, seq,
                                           show, ($), (++))
import           AVL.FullExtern.Example.Example80 (t80)


t80' = case t80 of
  AVL t' _ _ -> mkAVL $ insert (mkNode (Proxy::Proxy 80) 'a') t'

main :: IO ()
main = do t0 <- getCurrentTime
          seq t80' (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
