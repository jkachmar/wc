{-# language BlockArguments #-}
{-# language CPP #-}
{-# language GHCForeignImportPrim #-}
{-# language MagicHash #-}
{-# language NamedFieldPuns #-}
{-# language PatternSynonyms #-}
{-# language UnboxedSums #-}
{-# language UnboxedTuples #-}
{-# language UnliftedFFITypes #-}
{-# language ViewPatterns #-}
{-# options_ghc -O2 #-}

module UnboxedTypes where

import qualified Types as BoxedTypes

import Data.Monoid
import Data.Char

import GHC.Prim
import GHC.Types

------------------------------------------------------------------------
-- Necessary for the 'iswspace#' impl

#include "HsBaseConfig.h" 
------------------------------------------------------------------------

------------------------------------------------------------------------
-- CharType
------------------------------------------------------------------------

type CharType = (# (# #) | (# #) #)

pattern IsSpace :: CharType
pattern IsSpace = (# (# #) | #)

pattern NotSpace :: CharType
pattern NotSpace = (# | (# #) #)

{-# complete IsSpace, NotSpace #-}

showCharType :: CharType -> String
showCharType IsSpace = "IsSpace"
showCharType NotSpace = "NotSpace"

toBoxedCharType :: CharType -> BoxedTypes.CharType
toBoxedCharType IsSpace = BoxedTypes.IsSpace
toBoxedCharType NotSpace = BoxedTypes.NotSpace

------------------------------------------------------------------------
-- Flux
------------------------------------------------------------------------

type Flux = (# (# CharType, Int#, CharType #) | (# #) #)

pattern SomeFlux :: CharType -> Int# -> CharType -> Flux
pattern SomeFlux left wordCount right = 
  (# (# left, wordCount, right #) | #)

pattern UnknownFlux :: Flux
pattern UnknownFlux = (# | (# #) #)

{-# complete SomeFlux, UnknownFlux #-}

showFlux :: Flux -> String
showFlux UnknownFlux = "UnknownFlux"
showFlux (SomeFlux left wordCount right) =
  "SomeFlux "
  ++ showCharType left ++ " "
  ++ (show $ I# wordCount) ++ "# "
  ++ showCharType right

appendFlux :: Flux -> Flux -> Flux
appendFlux UnknownFlux x = x
appendFlux x UnknownFlux = x
appendFlux (SomeFlux l n NotSpace) (SomeFlux NotSpace n' r) = 
  SomeFlux l (n +# n' -# 1#) r
appendFlux (SomeFlux l n _) (SomeFlux _ n' r) = 
  SomeFlux l (n +# n') r

-- | Top-level bindings of unlifed types aren't allowed, so we cannot
-- just declare `emptyFlux = UnknownFlux`
--
-- In light of this, we'll just "hack around it" using a pattern 
-- synonym
pattern EmptyFlux :: Flux
pattern EmptyFlux = UnknownFlux

flux :: Char# -> Flux
flux c = case (isSpace# c) of
  1# -> SomeFlux IsSpace 0# IsSpace
  0# -> SomeFlux NotSpace 1# NotSpace
{-# INLINE flux #-}

getFlux :: Flux -> Int#
getFlux (SomeFlux _ n _) = n

toBoxedFlux :: Flux -> BoxedTypes.Flux
toBoxedFlux UnknownFlux = BoxedTypes.Unknown
toBoxedFlux (SomeFlux left n right) =
  BoxedTypes.Flux (toBoxedCharType left) (I# n) (toBoxedCharType right)

------------------------------------------------------------------------
-- Counts'
------------------------------------------------------------------------

type Counts = (# Int#, Flux, Int# #)

pattern MkCounts :: Int# -> Flux -> Int# -> Counts
pattern MkCounts charCount wordCount lineCount =
  (# charCount, wordCount, lineCount #)

{-# complete MkCounts #-}

showCounts :: Counts -> String
showCounts (MkCounts charCount wordCount lineCount) =
  "MkCounts "
  ++ (show $ I# charCount) ++ "# "
  ++ "(" ++ showFlux wordCount ++ ") "
  ++ (show $ I# lineCount) ++ "#"

appendCounts :: Counts -> Counts -> Counts
appendCounts (MkCounts lcc lwc llc) (MkCounts rcc rwc rlc) = 
  MkCounts (lcc +# rcc) (lwc `appendFlux` rwc) (llc +# rlc)

-- | Top-level bindings of unlifed types aren't allowed, so we cannot
-- just declare `emptyCounts' = MkCounts 0# U'`
--
-- In light of this, we'll just "hack around it" using a pattern 
-- synonym
pattern EmptyCounts :: Counts
pattern EmptyCounts = MkCounts 0# UnknownFlux 0#

countChar :: Char# -> Counts
countChar c =
  MkCounts
    1# 
    (flux c) 
    (c `eqChar#` '\n'#)
{-# INLINE countChar #-}

fromTuple :: (# Int#, Int#, Int# #) -> Counts
fromTuple (# cs, ws, ls #) =
  MkCounts
    cs
    (SomeFlux IsSpace ws IsSpace)
    ls

toTuple :: Counts -> (# Int#, Int#, Int# #)
toTuple (MkCounts charCount wordCount lineCount) =
  (# lineCount, getFlux wordCount, charCount #)

toBoxedCounts :: Counts -> BoxedTypes.Counts
toBoxedCounts (MkCounts charCount wordCount lineCount) = 
  BoxedTypes.Counts
    { BoxedTypes.charCount = I# charCount
    , BoxedTypes.wordCount = toBoxedFlux wordCount
    , BoxedTypes.lineCount = I# lineCount
    }

------------------------------------------------------------------------
-- Unboxed Unicode Whitespace Checking
------------------------------------------------------------------------

-- | Check if any given 'Char#' is a space
--
-- Lifted from GHC.Unicode#isSpace
isSpace# :: Char# -> Int#
isSpace# c  =
  case (uc `leWord#` 0x377##) of
    1# -> 
      (uc `eqWord#` 32##) 
      `orI#` 
      ((uc `minusWord#` 0x9##) `leWord#` 4##)
      `orI#`
      (uc `eqWord#` 0xa0##)
    0# -> iswspace# (ord# c) /=# 0#
  where
    uc :: Word#
    uc = int2Word# (ord# c)

foreign import prim "u_iswspace"
  iswspace# :: Int# -> Int#
