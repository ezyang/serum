{-# LANGUAGE BangPatterns #-}
module Main where

import Serum
import Control.Exception
import Control.DeepSeq
import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as LB
import Criterion.Main
import System.Environment

isTree :: BinTree -> BinTree
isTree x = x -- type hint

-- | Generate balanced tree of given depth. Count the number of right
-- turns. This should keep the compiler from optimizing.
generateBalancedTree3 :: Int -> BinTree
generateBalancedTree3 lvls = loop 0 lvls
 where
   loop !k 0 = Leaf k
   loop !k n = Tree (loop k (n-1)) (loop (k+1) (n-1))

main = do
    -- args <- getArgs
    let treedepth = 14 :: Int
    constTree <- evaluate (force (generateBalancedTree3 treedepth))
    defaultMain $
        [ env (return constTree) $ \tree -> bench "ezyang-binary-bintree-mem-serdeser" $
            nf (isTree . B.decode . B.encode) tree
        , env (return constTree) $ \tree -> bench "ezyang-cheney-bintree-mem-copy" $
            nf (isTree . B.decode . B.encode . cheneycopy) tree
        , env (return treedepth) $ \d -> bench "ezyang-binary-genbintree-mem-serdeser" $
            nf (isTree . B.decode . B.encode . force . generateBalancedTree3) d
        , env (return treedepth) $ \d -> bench "ezyang-cheney-genbintree-mem-copy" $
            nf (isTree . B.decode . B.encode . cheneycopy . force . generateBalancedTree3) d

        ]
