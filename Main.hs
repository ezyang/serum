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
    args <- getArgs
    let (treedepth, rest) =
            case args of
                n:rest -> (read n :: Int, rest)
                _ -> error "requires one argument of tree depth"
    constTree <- evaluate (force (generateBalancedTree3 treedepth))
    withArgs rest $
      defaultMain $
        [ env (return constTree) $ \tree -> bench "Ser/deser with binary" $
            nf (isTree . B.decode . B.encode) tree
        , env (return treedepth) $ \d -> bench "Compute tree, Ser/deser with binary" $
            nf (isTree . B.decode . B.encode . generateBalancedTree3) d
        , env (return constTree) $ \tree -> bench "Copy tree" $
            nf (isTree . cheneycopy) tree
        , env (return treedepth) $ \d -> bench "Compute tree, Copy tree" $
            nf (isTree . cheneycopy . generateBalancedTree3) d
        , env (return constTree) $ \tree -> bench "Copy tree, Ser/deser with binary" $
            nf (isTree . B.decode . B.encode . cheneycopy) tree
        , env (return treedepth) $ \d -> bench "Compute tree, Copy tree, Ser/deser with binary" $
            nf (isTree . B.decode . B.encode . cheneycopy . generateBalancedTree3) d
        ]
