{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}
module Parser.SrcInfo where

-- text
import Data.Text (Text)
import qualified Data.Text as Text

-- lens
import Control.Lens

-- base
import Data.Function
import Control.Category
import Data.Maybe

data SrcPos = SrcPos
  { lineNo :: !Int
  , colNo  :: !Int
  }
  deriving (Eq, Ord, Show)

_lineNo :: Lens' SrcPos Int
_lineNo = lens lineNo (\sp ln -> sp {lineNo = ln})

_colNo :: Lens' SrcPos Int
_colNo = lens colNo (\sp cn -> sp {colNo = cn})

data SrcSpan = SrcSpan
  { srcSpanStart :: !SrcPos
  , srcSpanEnd   :: !SrcPos
  } deriving (Eq, Ord, Show)


class HasSrcSpan a where
  _srcSpan :: Lens' a SrcSpan
  

instance Semigroup SrcSpan where
  (SrcSpan s1 e1) <> (SrcSpan s2 e2) = SrcSpan (min s1 s2) (max e1 e2)

mkSrcSpanLen :: SrcPos -> Int -> SrcSpan
mkSrcSpanLen p@(SrcPos l c) n = SrcSpan p (p & _colNo +~ n)

extendLeft :: SrcPos -> SrcSpan -> SrcSpan
extendLeft pos (SrcSpan s e) =
  SrcSpan (min pos s) e

extendRight :: SrcSpan -> SrcPos -> SrcSpan
extendRight (SrcSpan s e) pos =
  SrcSpan s (max pos e)


getSrcSpanStart :: SrcSpan -> SrcPos
getSrcSpanStart = srcSpanStart

getSrcSpanEnd :: SrcSpan -> SrcPos
getSrcSpanEnd = srcSpanEnd


getSrcText :: SrcPos -> Text -> Text
getSrcText (SrcPos {lineNo = line, colNo = _}) t = textLine
  where
    textLine :: Text
    textLine =
      t 
      &   (Text.lines 
      >>> preview (ix (line - 1))
      >>> fromMaybe mempty)
