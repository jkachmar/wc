{-# language BangPatterns #-}
{-# language MagicHash #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language UnboxedTuples #-}

module UnboxedInlinedMonoidBSFold where

import UnboxedTypes

import qualified Types as BoxedTypes

import Data.Traversable
import Foreign
import GHC.Ptr
import GHC.Prim
import GHC.Types

import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Internal as LBSI
import qualified Data.ByteString.Lazy.Char8 as LBS8

unboxedInlinedMonoidBSFold :: [FilePath] -> IO [(FilePath, BoxedTypes.Counts)]
unboxedInlinedMonoidBSFold paths = for paths $ \fp -> do
  bytes <- BS.readFile fp
  let count = toBoxedCounts (countFile bytes)
  return (fp, count)
{-# INLINE unboxedInlinedMonoidBSFold #-}

countFile :: BS.ByteString -> Counts
countFile = unboxedStrictFoldlChar' (\counts char -> counts `appendCounts` countChar char) EmptyCounts
{-# INLINE countFile #-}

-------------------------------------------------------------------------
-- Unboxed ByteString functions
-------------------------------------------------------------------------
-- | 'foldl'' is like 'foldl', but strict in the accumulator.
-- 
-- Lifted from Data.ByteString.Lazy and modified to work with unboxed types
unboxedLazyFoldlChar' :: (Counts -> Char# -> Counts) -> Counts -> LBS.ByteString -> Counts
unboxedLazyFoldlChar' f z = go z
  where go !a LBSI.Empty        = a
        go !a (LBSI.Chunk c cs) = go (unboxedStrictFoldlChar' f a c) cs
{-# INLINE unboxedLazyFoldlChar' #-}

-- | 'foldl'' is like 'foldl', but strict in the accumulator.
--
-- Lifted from Data.ByteString and modified to work with unboxed types
unboxedStrictFoldlChar' :: (Counts -> Char# -> Counts) -> Counts -> BS.ByteString -> Counts
unboxedStrictFoldlChar' f counts (BSI.PS fp offset len) =
  let 
    action = \initialState -> 
      withForeignPtrCounts# initialState fp $ \state ptr ->
        go state counts (ptr `plusPtr` offset) (ptr `plusPtr` (offset + len))
  in 
    -- This is basically accursedUnutterablePerformIO
    case action realWorld# of
    (# _, result #) -> result
  where
    -- tail recursive; traverses array left to right
    go :: State# RealWorld -> Counts -> Ptr Char -> Ptr Char -> (# State# RealWorld, Counts #)
    go !state !z !ptr !q 
      | ptr == q  = (# state, z #)
      | otherwise =
          let (IO !action) = peek ptr
          in case action state of
            (# newState, (C# char) #) -> go newState (f z char) (ptr `plusPtr` 1) q
{-# INLINE unboxedStrictFoldlChar' #-}

withForeignPtrCounts# 
  :: State# RealWorld 
  -> ForeignPtr a
  -> (State# RealWorld -> Ptr a -> (# State# RealWorld, Counts #))
  -> (# State# RealWorld, Counts #)
withForeignPtrCounts# state fo f = 
  let
    (# newState, counts #) = f state (unsafeForeignPtrToPtr fo)
    (IO !action) = touchForeignPtr fo
  in case action newState of
    (# newerState, _ #) -> (# newerState, counts #)
