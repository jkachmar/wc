{-# LANGUAGE MultiWayIf #-}
module LazyUTFAgnostic where

import qualified Data.ByteString.Lazy.Char8 as BL
import Data.ByteString.Internal (c2w)

import Types
import Control.Monad
import Control.Arrow
import Data.Traversable
import Data.Bits

lazyUTF8 :: [FilePath] -> IO [(FilePath, Counts)]
lazyUTF8 paths = for paths $ \fp -> do
    count <- lazyBytestreamCountFile <$> BL.readFile fp
    return (fp, count)
{-# INLINE lazyUTF8 #-}

lazyBytestreamCountFile :: BL.ByteString -> Counts
lazyBytestreamCountFile = BL.foldl' (flip (mappend . countByte)) mempty
{-# INLINE lazyBytestreamCountFile #-}

countByte :: Char -> Counts
countByte c =
    let bitAt = testBit (c2w c)
     in Counts {
                -- Only count bytes at the START of a codepoint, not continuations
                charCount = if (bitAt 7 && not (bitAt 6)) then 0 else 1
               , wordCount = flux c
               , lineCount = if (c == '\n') then 1 else 0
               }
{-# INLINE countByte #-}
