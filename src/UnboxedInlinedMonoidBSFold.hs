{-# language BangPatterns #-}
{-# language MagicHash #-}

module UnboxedInlinedMonoidBSFold where

import UnboxedTypes

import Data.Traversable
import Foreign (peek, plusPtr, withForeignPtr)
import GHC.Prim
import GHC.Types

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Internal as LBSI
import qualified Data.ByteString.Lazy.Char8 as LBS8

-- inlinedMonoidBSFold :: [FilePath] -> IO [(FilePath, _)]
-- inlinedMonoidBSFold paths = for paths $ \fp -> do
--   -- count <- countFile <$> BS.readFile fp
--   -- return (fp, count)
--   undefined
-- {-# INLINE inlinedMonoidBSFold #-}

-- countFile :: BS.ByteString -> Counts
-- countFile = BS.foldl' (\counts (C# char) -> counts `appendCounts` countChar char) EmptyCounts
-- {-# INLINE countFile #-}

-------------------------------------------------------------------------
-- Unboxed ByteString functions
-------------------------------------------------------------------------
-- | 'foldl'' is like 'foldl', but strict in the accumulator.
-- 
-- Lifted from Data.ByteString.Lazy and modified to work with unboxed types
unboxedLazyFoldl' :: (a -> Char# -> a) -> a -> LBS.ByteString -> a
unboxedLazyFoldl' f z = go z
  where go !a LBSI.Empty        = a
        go !a (LBSI.Chunk c cs) = go (unboxedStrictFoldl' f a c) cs
{-# INLINE unboxedLazyFoldl' #-}

-- | 'foldl'' is like 'foldl', but strict in the accumulator.
--
-- Lifted from Data.ByteString and modified to work with unboxed types
unboxedStrictFoldl' :: (a# -> Char# -> a#) -> a# -> BS.ByteString -> a#
unboxedStrictFoldl' f v (BSI.PS fp off len) =
  BSI.accursedUnutterablePerformIO $ withForeignPtr fp $ \p ->
    go v (p `plusPtr` off) (p `plusPtr` (off+len))
  where
    -- tail recursive; traverses array left to right
    go !z !p !q | p == q    = return z
                | otherwise = do x <- peek p
                                 go (f z x) (p `plusPtr` 1) q
{-# INLINE unboxedStrictFoldl' #-}
