module Parser.Monad where


-- sebastian
import qualified Parser.SrcInfo as SrcInfo

-- mtl
import Control.Monad.Except
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Control.Monad.State (MonadState)
-- bytestring
import Data.ByteString as ByteString
import Data.ByteString.Internal as ByteString

-- lens
import qualified Control.Lens as Lens
import Control.Lens (Lens', (%=), (<<+=), lens)
import Control.Lens ((.=))

-- unordered-containers
import qualified Data.HashMap.Strict as Map

data LexPos = LexPos
  { lex_line :: !Int
  , lex_col  :: !Int
  , lex_addr :: !Int
  }

lexPosToSrcPos :: LexPos -> SrcInfo.SrcPos
lexPosToSrcPos LexPos{..} = SrcInfo.SrcPos {lineNo = lex_line, colNo = lex_col}

initialLexPos :: LexPos
initialLexPos =
  LexPos
  { lex_line = 0
  , lex_col = 0
  , lex_addr = 0
  }


data LexInput = LexInput
  { lex_pos        :: !LexPos
  , prev_char      :: !Char
  , input_string   :: !ByteString
  }

-- lenses for LexInput
_lex_pos :: Lens' LexInput LexPos
_lex_pos = lens lex_pos (\ps us -> ps {lex_pos = us})

_lex_prev_char :: Lens' LexInput Char
_lex_prev_char = lens prev_char (\ps chr -> ps {prev_char = chr})

_lex_input_string :: Lens' LexInput ByteString
_lex_input_string = lens input_string (\ps bs -> ps {input_string = bs})

--------------------------------------------------
-- Parse errors
data WithSrcSpan a = WithSrcSpan !SrcInfo.SrcSpan !a

data ErrorType =
    LexError !Int !Int
  | ParseError
  | DefNameMisMatch
  deriving Show

lexError :: Int -> Int -> ErrorType
lexError l c = LexError l c

--------------------------------------------------
-- Parser State

type Info     = SrcInfo.SrcSpan
type Unique   = Int
type InfoMap  = Map.HashMap Unique Info

data ParserState = ParserState
  { uniqueSupply    :: !Int
  , infoMap         :: !InfoMap
  , bytes_consumed  :: !Int           -- bytes consumed so far
  , lex_input       :: !LexInput      -- the current input
  }


-- lenses for parser state
_uniqueSupply :: Lens' ParserState Int
_uniqueSupply = lens uniqueSupply (\ps us -> ps {uniqueSupply = us})

_infoMap :: Lens' ParserState InfoMap
_infoMap = lens infoMap (\ps us -> ps {infoMap = us})

_lex_input :: Lens' ParserState LexInput
_lex_input = lens lex_input (\ps li -> ps {lex_input = li})

--------------------------------------------------
-- Parser Monad

-- experiment with unboxing the return type
--instance Functor Parser where
--  fmap f a = \st -> fmap \(\(#_, a#) -> a
newtype Parser a = Parser {
  unParser :: ParserState -> Either ErrorType (a, ParserState)
  }
  deriving (Functor, Applicative, MonadError ErrorType, Monad, MonadState ParserState) via StateT ParserState (Except ErrorType)


newUniqueSupply :: Parser Int
newUniqueSupply = do
  _uniqueSupply <<+= 1  


-- monadic interface happy expects
{-# inline returnP #-}
returnP :: a -> Parser a
returnP = return

{-# inline thenP #-}
thenP :: Parser a -> (a -> Parser b) -> Parser b
thenP = (>>=)

--------------------------------------------------
-- initial states for orchestrating parser

initialLexInput :: ByteString -> LexInput
initialLexInput bs =
  LexInput
  { lex_pos = initialLexPos
  , prev_char = '\n'
  , input_string = bs
  }

initialParserState :: ByteString -> ParserState
initialParserState bs =
  ParserState
    { uniqueSupply = 0
    , infoMap = mempty
    , bytes_consumed = 0
    , lex_input = initialLexInput bs
    }  

runParser :: Parser a -> ByteString -> Either ErrorType a
runParser p bs = fmap fst (unParser p (initialParserState bs))


alexGetSrcLoc :: Parser SrcInfo.SrcPos
alexGetSrcLoc =
  Lens.uses (_lex_input . _lex_pos) lexPosToSrcPos
