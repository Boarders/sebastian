{
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Parser.Lex where

-- bytestring
import qualified Data.ByteString as Strict
import Data.ByteString.Internal as ByteString (w2c)
import qualified Data.ByteString.Char8 as Char8
import Data.ByteString.Short as Short

-- base
import Data.Word (Word8)
import Data.String

-- lens
import qualified Control.Lens as Lens
import Control.Lens ((.=))

-- sebastian
import Parser.Monad
import qualified Parser.SrcInfo as Info

}

$digit       = [0-9]                    -- digit
$digit_break = [0-9\_]                  -- digit or break
$alpha = [a-zA-Z]                       -- alphabetic characters
$lower = [a-z]                          -- lower alphabetic characters
$upper = [A-Z]                          -- upper alphabetic characters
$sign  = [\-\+]                         -- sign
$dot   = [\.]                           -- dot

@digits = $digit ($digit_break)*        -- digits
@int   = ($sign)? @digits               -- signed int
@float = $sign? @digits $dot @digits    -- signed flat
@var   = $lower [$alpha $digit \_ \']*  -- var
@con   = $upper [$alpha $digit \_ \']*  -- constructor

-- to do: add non-alpha and error for unicode usage

tokens :-
  $white+    ;
  "--".*     ;
  \(     {tok LeftRoundBracket    }
  \)     {tok RightRoundBracket   }
  \[     {tok LeftSquareBracket   }
  \]     {tok RightSquareBracket  }
  \;     { tok SemiColon  }
  @int   { tok_app (\s -> Int    (read . filter (== '_') $ s))}
  let    { tok Let    }
  \=     { tok Equals }
  @var { tok_app (Var . fromString) }


{
-- Each action has type :: Bytestring -> Token

tok'
  :: (Strict.ByteString -> TokenType)
  -> LexInput
  -> Int
  -> Parser Token
tok' f (LexInput {lex_pos = pos, prev_char = chr, input_string = input}) len = do
  let tokenLen = fromIntegral len
  let srcSpan = Info.mkSrcSpanLen (lexPosToSrcPos pos) tokenLen 
  return $
    Token srcSpan (f (Char8.take (fromIntegral len) input))

tok 
  :: TokenType
  -> LexInput
  -> Int
  -> Parser Token
tok x = tok' (const x)

tok_app
  :: (String -> TokenType) 
  -> LexInput
  -> Int
  -> Parser Token
tok_app f = tok' (\s -> f (Char8.unpack s))


data Token = Token
  { getSrcSpan :: !Info.SrcSpan
  , getToken  :: !TokenType
  }
  | EOF
  deriving (Eq, Show)

-- The token type:
data TokenType =
  LeftRoundBracket    |
  RightRoundBracket   |
  LeftSquareBracket   |
  RightSquareBracket  |
  SemiColon           |
  Int Int             |
  Let                 |
  Equals              |
  Var ShortByteString

  deriving (Eq,Show)

getVar :: Token -> ShortByteString
getVar = unVar . getToken

getCon :: Token -> ShortByteString
getCon = unVar . getToken

unVar :: TokenType -> ShortByteString
unVar (Var c) = c
unVar _ = error "Token.x.unVar: used on argument without Var"

  
lexPosMove :: LexPos -> Char -> LexPos
lexPosMove (LexPos l c a) '\t' =
  LexPos l  (c+8-((c-1) `mod` 8)) (a + 1)
lexPosMove (LexPos a l _) '\n' =
  LexPos (l+1) 1 (a + 1)
lexPosMove (LexPos a l c) _ =
  LexPos l (c+1) (a+1)

-- Functions that must be provided to Alex
type AlexInput = LexInput

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte LexInput{..} =
  case Strict.uncons input_string of
    Nothing -> Nothing
    Just (w, rest) ->
      let
        c = ByteString.w2c w
        
        new_pos = lexPosMove lex_pos c
        !newLexInput = LexInput
          { lex_pos = new_pos
          , prev_char = c
          , input_string = rest
          }
      in
        Just (w, newLexInput)

alexEOF :: Token
alexEOF = EOF

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar lex_inp = prev_char lex_inp
}
