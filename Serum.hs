{-# LANGUAGE GHCForeignImportPrim, MagicHash, UnliftedFFITypes, UnboxedTuples, DeriveDataTypeable #-}
module Serum where

import GHC.Types
import GHC.Stable
import GHC.Exts
import Control.DeepSeq
import qualified Data.Binary as B
import Data.Word
import Data.Typeable
import Control.Exception
import Foreign.StablePtr
import System.IO.Unsafe

-- ----------------------------------------------------------------------------
--
--                              Binary tree
--
-- ----------------------------------------------------------------------------

data BinTree = Tree BinTree BinTree
             | Leaf {-# UNPACK #-}! Int
  deriving (Show, Eq, Typeable)

instance NFData BinTree where
    rnf (Leaf a) = rnf a `seq` ()
    rnf (Tree left right) = rnf left `seq` rnf right `seq` ()

-- ----------------------------------------------------------------------------
--
--                              Binary instance
--
-- ----------------------------------------------------------------------------

instance B.Binary BinTree where
  put (Leaf a) = B.put (0 :: Word8) >> B.put a
  put (Tree left right) = B.put (1 :: Word8) >> B.put left >> B.put right

  get = do
    t <- B.get :: B.Get Word8
    case t of
      0 -> Leaf <$> B.get
      1 -> Tree <$> B.get <*> B.get
      _ -> error $ "invalid tag for BinTree: " ++ show t

-- ----------------------------------------------------------------------------
--
--                              Cheney copy implementation
--
-- ----------------------------------------------------------------------------

cheneycopy :: NFData a => a -> a
cheneycopy x = unsafePerformIO $ do
    evaluate (rnf x)
    -- This here is a silly hack because we can't return Any
    sp <- IO (\s -> case cheneycopy# (unsafeCoerce# x :: Any) s of
                        (# s', r #) -> (# s', StablePtr r #))
    r <- deRefStablePtr sp
    freeStablePtr sp
    return r

foreign import prim "cheneycopyzh" cheneycopy# :: Any -> State# RealWorld -> (# State# RealWorld, StablePtr# a #)
