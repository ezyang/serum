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

-- See README for some information:
main = do
    args <- getArgs
    let (d, rest) =
            case args of
                n:rest -> (read n :: Int, rest)
                _ -> error "requires one argument of tree depth"
        mkTree = evaluate (force (generateBalancedTree3 d))
    withArgs rest $
      defaultMain $
        tail [ undefined -- just to get leading commas
        , bench "genE" $
            nf (isTree                                    . generateBalancedTree3) d
        , bench "gen-copyE" $
            nf (isTree                       . cheneycopy . generateBalancedTree3) d
        , env mkTree $ \t -> bench "gen-copyF" $
            nf (isTree                       . cheneycopy) t
        , bench "gen-binaryE" $
            nf (isTree . B.decode . B.encode              . generateBalancedTree3) d
        , bench "gen-copy-binaryE" $
            nf (isTree . B.decode . B.encode . cheneycopy . generateBalancedTree3) d
        , env mkTree $ \t -> bench "gen-copy-binaryF" $
            nf (isTree . B.decode . B.encode . cheneycopy) t
        ]
