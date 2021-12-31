{-# language BangPatterns #-}
{-# language RecordWildCards #-}
module Parser.Alex
  ( thenP
  , returnP
  , Parser
  , lexer
  , runParser
  , lexTokens
  )
  where

----------------------------------------------------------------------------
-- Core
import Parser.Lex
import Parser.Monad
import qualified Parser.SrcInfo as SrcInfo

-- base
import Data.Coerce
import Data.Int (Int64)
import Data.Word (Word8)
import Control.Applicative (liftA2)

-- bytestring
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Internal as ByteString

-- text
import qualified Data.Text as Text

-- lens
import qualified Control.Lens as Lens
import Control.Lens((.=))

-- prelude
import Control.Applicative (many)

-- mtl
import Control.Monad.Except
----------------------------------------------------------------------------


--------------------------------------------------
-- Running the lexer

lexTokens :: Parser [Token]
lexTokens = go
  where
    go = do
      inp__ <- getAlexInput
      --sc <- Lens.use _alex_scd
      -- for now we don't use start codes in the lexer
      let sc = 0
      case alexScan inp__ sc of
        AlexEOF -> pure [alexEOF]
        AlexError LexInput{lex_pos = LexPos{..}} ->
          throwError (lexError lex_line lex_col)
        AlexSkip  inp__' _len -> do
            alexSetInput inp__'
            go
        AlexToken inp__' len action -> do
            alexSetInput inp__'
            liftA2 (:) (action inp__ len) go
            


  
-- run the lexer in continuation style for use with happy
lexer :: (Token -> Parser a) -> Parser a
lexer cont = do
  token <- scanToken
  cont token

scanToken :: Parser Token
scanToken = do
  inp__ <- getAlexInput
  --sc <- Lens.use _alex_scd
  -- for now we don't use start codes in the lexer
  let sc = 0
  case alexScan inp__ sc of
    AlexEOF -> pure alexEOF
    AlexError LexInput{lex_pos = LexPos{..}} ->
      throwError (lexError lex_line lex_col)
    AlexSkip  inp__' _len -> do
        alexSetInput inp__'
        scanToken
    AlexToken inp__' len action -> do
        alexSetInput inp__'
        action inp__ len

--------------------------------------------------
-- helpers for setting lex input

{-# inline getAlexInput #-}
getAlexInput :: Parser LexInput
getAlexInput = do
  Lens.use _lex_input

{-# inline alexSetInput #-}
alexSetInput :: LexInput -> Parser ()
alexSetInput lex_input = do
  _lex_input .= lex_input
