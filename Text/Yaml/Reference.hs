{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances, FunctionalDependencies, PostfixOperators #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Text.Yaml.Reference
-- Copyright   :  (c) Oren Ben-Kiki 2007
-- License     :  LGPL
--
-- Maintainer  :  yaml-oren@ben-kiki.org
-- Stability   :  alpha
-- Portability :  portable
--
-- Implementation of the YAML syntax as defined in <http://www.yaml.org>.
-- Actually this file contains the parsing framework and includes (using CPP)
-- the actual productions from @Reference.bnf@.
--
-- The parsing framework is fully streaming (generates output tokens
-- \"immediately\").
-------------------------------------------------------------------------------

module Text.Yaml.Reference
  (
    -- Basic parsing:
    Code(..),
    Token,
    Tokenizer,
    yaml,
    -- For testing:
    Context,
    Chomp,
    tokenizer,
    tokenizerWithN,
    tokenizerWithC,
    tokenizerWithT,
    tokenizerWithNC,
    tokenizerWithNT,
    tokenizerNames,
    showTokens
  )
where

import           Control.Applicative (Applicative, pure, (<*>))
import           Control.Monad
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Char
import qualified Data.DList as D
import qualified Data.Map as Map
import           Text.Regex
import           Debug.Trace
import qualified Prelude
import           Prelude hiding ((/), (*), (+), (-), (^))

-- * Generic operators
--
-- ** Numeric operators
--
-- We rename the four numerical operators @+@ @-@ @*@ @\/@ to start with @.@
-- (@.+@, @.-@, @.*@, @.\/@). This allows us to use the originals for BNF
-- notation (we also hijack the @^@ operator). This is not a generally
-- recommended practice. It is justified in this case since we have very little
-- arithmetic operations, and a lot of BNF rules which this makes extremely
-- readable.

infixl 6 .+
-- | \".+\" is the numeric addition (we use \"+\" for postfix \"one or more\").
(.+) = (Prelude.+)

infixl 6 .-
-- | \".-\" is the numeric subtraction (we use \"-\" for infix \"and not\").
(.-) = (Prelude.-)

infixl 7 .*
-- | \".*\" is the numeric multiplication (we use \"*\" for postfix \"zero or
-- more\").
(.*) = (Prelude.*)

infixl 7 ./
-- | \"./\" is the numeric division (we use \"/\" for infix \"or\").
(./) = (Prelude./)

-- ** Record field access
--
-- We also define @|>@ for record access for increased readability.

infixl 9 |>
-- | @record |> field@ is the same as @field record@,  but is more readable.
(|>) :: record -> (record -> value) -> value
record |> field = field record

-- * UTF decoding
--
-- This really should be factored out to the standard libraries. Since it isn't
-- there, we get to tailor it exactly to our needs. We use lazy byte strings as
-- input, which should give reasonable I\/O performance when reading large
-- files. The output is a normal 'Char' list which is easy to work with and
-- should be efficient enough as long as the 'Parser' does its job right.

-- | Recognized Unicode encodings. As of YAML 1.2 UTF-32 is also required.
data Encoding = UTF8    -- ^ UTF-8 encoding (or ASCII)
              | UTF16LE -- ^ UTF-16 little endian
              | UTF16BE -- ^ UTF-16 big endian
              | UTF32LE -- ^ UTF-32 little endian
              | UTF32BE -- ^ UTF-32 big endian

-- | @show encoding@ converts an 'Encoding' to the encoding name (with a "-")
-- as used by most programs.
instance Show Encoding where
    show UTF8    = "UTF-8"
    show UTF16LE = "UTF-16LE"
    show UTF16BE = "UTF-16BE"
    show UTF32LE = "UTF-32LE"
    show UTF32BE = "UTF-32BE"

-- | @decode bytes@ automatically detects the 'Encoding' used and converts the
-- /bytes/ to Unicode characters, with byte offsets. Note the offset is for
-- past end of the character, not its beginning.
decode :: C.ByteString -> (Encoding, [(Int, Char)])
decode text = (encoding, undoEncoding encoding text)
  where encoding = detectEncoding $ C.unpack $ C.take 4 text

-- | @detectEncoding text@ examines the first few chars (bytes) of the /text/
-- to deduce the Unicode encoding used according to the YAML spec.
detectEncoding :: [Char] -> Encoding
detectEncoding text =
  case text of
    '\x00' : '\x00' : '\xFE' : '\xFF' : _ -> UTF32BE
    '\x00' : '\x00' : '\x00' : _      : _ -> UTF32BE
    '\xFF' : '\xFE' : '\x00' : '\x00' : _ -> UTF32LE
    _      : '\x00' : '\x00' : '\x00' : _ -> UTF32LE
    '\xFE' : '\xFF' : _                   -> UTF16BE
    '\x00' : _      : _                   -> UTF16BE
    '\xFF' : '\xFE' : _                   -> UTF16LE
    _      : '\x00' : _                   -> UTF16LE
    '\xEF' : '\xBB' : '\xBF' : _          -> UTF8
    _                                     -> UTF8

-- | @undoEncoding encoding bytes@ converts a /bytes/ stream to Unicode
-- characters according to the /encoding/.
undoEncoding :: Encoding -> C.ByteString -> [(Int, Char)]
undoEncoding encoding bytes =
  case encoding of
    UTF8    -> undoUTF8 bytes 0
    UTF16LE -> combinePairs $ undoUTF16LE bytes 0
    UTF16BE -> combinePairs $ undoUTF16BE bytes 0
    UTF32LE -> undoUTF32LE bytes 0
    UTF32BE -> undoUTF32BE bytes 0

-- ** UTF-32 decoding

-- | @hasFewerThan bytes n@ checks whether there are fewer than /n/ /bytes/
-- left to read.
hasFewerThan :: Int -> C.ByteString -> Bool
hasFewerThan n bytes
  | n == 1 = C.null bytes
  | n > 1  = C.null bytes || hasFewerThan (n .- 1) (C.tail bytes)

-- | @undoUTF32LE bytes offset@ decoded a UTF-32LE /bytes/ stream to Unicode
-- chars.
undoUTF32LE :: C.ByteString -> Int -> [(Int, Char)]
undoUTF32LE bytes offset
  | C.null bytes = []
  | hasFewerThan 4 bytes = error "UTF-32LE input contains invalid number of bytes"
  | otherwise = let first    = C.head bytes
                    bytes'   = C.tail bytes
                    second   = C.head bytes'
                    bytes''  = C.tail bytes'
                    third    = C.head bytes''
                    bytes''' = C.tail bytes''
                    fourth   = C.head bytes'''
                    rest     = C.tail bytes'''
                in (offset .+ 4,
                    chr $ (ord first)
                        .+ 256 .* ((ord second)
                        .+ 256 .* ((ord third)
                        .+ 256 .* ((ord fourth))))):(undoUTF32LE rest $ offset .+ 4)

-- | @undoUTF32BE bytes offset@ decoded a UTF-32BE /bytes/ stream to Unicode
-- chars.
undoUTF32BE :: C.ByteString -> Int -> [(Int, Char)]
undoUTF32BE bytes offset
  | C.null bytes = []
  | hasFewerThan 4 bytes = error "UTF-32BE input contains invalid number of bytes"
  | otherwise = let first    = C.head bytes
                    bytes'   = C.tail bytes
                    second   = C.head bytes'
                    bytes''  = C.tail bytes'
                    third    = C.head bytes''
                    bytes''' = C.tail bytes''
                    fourth   = C.head bytes'''
                    rest     = C.tail bytes'''
                in (offset .+ 4,
                    chr $ (ord fourth)
                        .+ 256 .* ((ord third)
                        .+ 256 .* ((ord second)
                        .+ 256 .* ((ord first))))):(undoUTF32BE rest $ offset .+ 4)

-- ** UTF-16 decoding

-- | @combinePairs chars@ converts each pair of UTF-16 surrogate characters to a
-- single Unicode character.
combinePairs :: [(Int, Char)] -> [(Int, Char)]
combinePairs []                          = []
combinePairs (head@(_, head_char):tail)
  | '\xD800' <= head_char && head_char <= '\xDBFF' = combineLead head tail
  | '\xDC00' <= head_char && head_char <= '\xDFFF' = error "UTF-16 contains trail surrogate without lead surrogate"
  | otherwise                                      = head:(combinePairs tail)

-- | @combineLead lead rest@ combines the /lead/ surrogate with the head of the
-- /rest/ of the input chars, assumed to be a /trail/ surrogate, and continues
-- combining surrogate pairs.
combineLead :: (Int, Char) -> [(Int, Char)] -> [(Int, Char)]
combineLead lead []                                  = error "UTF-16 contains lead surrogate as final character"
combineLead lead@(_, lead_char) ((trail_offset, trail_char):rest)
  | '\xDC00' <= trail_char && trail_char <= '\xDFFF' = (trail_offset, combineSurrogates lead_char trail_char):combinePairs rest
  | otherwise                                        = error "UTF-16 contains lead surrogate without trail surrogate"

-- | @surrogateOffset@ is copied from the Unicode FAQs.
surrogateOffset :: Int
surrogateOffset = 0x10000 .- (0xD800 .* 1024) .- 0xDC00

-- | @combineSurrogates lead trail@ combines two UTF-16 surrogates into a single
-- Unicode character.
combineSurrogates :: Char -> Char -> Char
combineSurrogates lead trail = chr $ (ord lead) .* 1024 .+ (ord trail) .+ surrogateOffset

-- | @undoUTF18LE bytes offset@ decoded a UTF-16LE /bytes/ stream to Unicode
-- chars.
undoUTF16LE :: C.ByteString -> Int -> [(Int, Char)]
undoUTF16LE bytes offset
  | C.null bytes = []
  | hasFewerThan 2 bytes = error "UTF-16LE input contains odd number of bytes"
  | otherwise = let low    = C.head bytes
                    bytes' = C.tail bytes
                    high   = C.head bytes'
                    rest   = C.tail bytes'
                in (offset .+ 2, chr $ (ord low) .+ (ord high) .* 256):(undoUTF16LE rest $ offset .+ 2)

-- | @undoUTF18BE bytes offset@ decoded a UTF-16BE /bytes/ stream to Unicode
-- chars.
undoUTF16BE :: C.ByteString -> Int -> [(Int, Char)]
undoUTF16BE bytes offset
  | C.null bytes = []
  | hasFewerThan 2 bytes = error "UTF-16BE input contains odd number of bytes"
  | otherwise = let high   = C.head bytes
                    bytes' = C.tail bytes
                    low    = C.head bytes'
                    rest   = C.tail bytes'
                in (offset .+ 2, chr $ (ord low) .+ (ord high) .* 256):(undoUTF16BE rest $ offset .+ 2)

-- ** UTF-8 decoding

-- | @undoUTF8 bytes offset@ decoded a UTF-8 /bytes/ stream to Unicode chars.
undoUTF8 :: C.ByteString -> Int -> [(Int, Char)]
undoUTF8 bytes offset
  | C.null bytes = []
  | otherwise = let first = C.head bytes
                    rest  = C.tail bytes
                in case () of
                      _ | first < '\x80' -> (offset .+ 1, first):(undoUTF8 rest $ offset .+ 1)
                        | first < '\xC0' -> error $ "UTF-8 input contains invalid first byte"
                        | first < '\xE0' -> decodeTwoUTF8 first offset rest
                        | first < '\xF0' -> decodeThreeUTF8 first offset rest
                        | first < '\xF8' -> decodeFourUTF8 first offset rest
                        | otherwise      -> error $ "UTF-8 input contains invalid first byte"

-- | @decodeTwoUTF8 first offset bytes@ decodes a two-byte UTF-8 character,
-- where the /first/ byte is already available and the second is the head of
-- the /bytes/, and then continues to undo the UTF-8 encoding.
decodeTwoUTF8 :: Char -> Int -> C.ByteString -> [(Int, Char)]
decodeTwoUTF8 first offset bytes
  | C.null bytes = error "UTF-8 double byte char is missing second byte at eof"
  | otherwise = let second = C.head bytes
                    rest   = C.tail bytes
                in case () of
                      _ | second < '\x80' || '\xBF' < second -> error $ "UTF-8 double byte char has invalid second byte"
                        | otherwise                          -> (offset .+ 2, combineTwoUTF8 first second):(undoUTF8 rest $ offset .+ 2)

-- | @combineTwoUTF8 first second@ combines the /first/ and /second/ bytes of a
-- two-byte UTF-8 char into a single Unicode char.
combineTwoUTF8 :: Char -> Char -> Char
combineTwoUTF8 first second = chr(((ord first) .- 0xC0) .* 64
                               .+ ((ord second) .- 0x80))

-- | @decodeThreeUTF8 first offset bytes@ decodes a three-byte UTF-8 character,
-- where the /first/ byte is already available and the second and third are the
-- head of the /bytes/, and then continues to undo the UTF-8 encoding.
decodeThreeUTF8 :: Char -> Int -> C.ByteString -> [(Int, Char)]
decodeThreeUTF8 first offset bytes
  | hasFewerThan 2 bytes = error "UTF-8 triple byte char is missing bytes at eof"
  | otherwise = let second = C.head bytes
                    bytes' = C.tail bytes
                    third  = C.head bytes'
                    rest   = C.tail bytes'
                in case () of
                      _ | second < '\x80' || '\xBF' < second -> error "UTF-8 triple byte char has invalid second byte"
                        | third < '\x80' || '\xBF' < third   -> error "UTF-8 triple byte char has invalid third byte"
                        | otherwise                          -> (offset .+ 3, combineThreeUTF8 first second third):(undoUTF8 rest $ offset .+ 3)

-- | @combineThreeUTF8 first second@ combines the /first/, /second/ and /third/
-- bytes of a three-byte UTF-8 char into a single Unicode char.
combineThreeUTF8 :: Char -> Char -> Char -> Char
combineThreeUTF8 first second third = chr(((ord first) .- 0xE0) .* 4096
                                       .+ ((ord second) .- 0x80) .* 64
                                       .+ ((ord third)  .- 0x80))

-- | @decodeFourUTF8 first offset bytes@ decodes a four-byte UTF-8 character,
-- where the /first/ byte is already available and the second, third and fourth
-- are the head of the /bytes/, and then continues to undo the UTF-8 encoding.
decodeFourUTF8 :: Char -> Int -> C.ByteString -> [(Int, Char)]
decodeFourUTF8 first offset bytes
  | hasFewerThan 3 bytes = error "UTF-8 quad byte char is missing bytes at eof"
  | otherwise = let second  = C.head bytes
                    bytes'  = C.tail bytes
                    third   = C.head bytes'
                    bytes'' = C.tail bytes'
                    fourth  = C.head bytes''
                    rest    = C.tail bytes''
                in case () of
                      _ | second < '\x80' || '\xBF' < second -> error "UTF-8 quad byte char has invalid second byte"
                        | third < '\x80' || '\xBF' < third   -> error "UTF-8 quad byte char has invalid third byte"
                        | third < '\x80' || '\xBF' < third   -> error "UTF-8 quad byte char has invalid fourth byte"
                        | otherwise                          -> (offset .+ 4, combineFourUTF8 first second third fourth):(undoUTF8 rest $ offset .+ 4)

-- | @combineFourUTF8 first second@ combines the /first/, /second/ and /third/
-- bytes of a three-byte UTF-8 char into a single Unicode char.
combineFourUTF8 :: Char -> Char -> Char -> Char -> Char
combineFourUTF8 first second third fourth = chr(((ord first)  .- 0xF0) .* 262144
                                             .+ ((ord second) .- 0x80) .* 4096
                                             .+ ((ord third)  .- 0x80) .* 64
                                             .+ ((ord fourth) .- 0x80))

-- * Result tokens
--
-- The parsing result is a stream of tokens rather than a parse tree. The idea
-- is to convert the YAML input into \"byte codes\". These byte codes are
-- intended to be written into a byte codes file (or more likely a UNIX pipe)
-- for further processing.

-- | 'Token' codes.
data Code = Bom             -- ^ BOM, contains \"@TF8@\", \"@TF16LE@\", \"@TF32BE@\", etc.
          | Text            -- ^ Content text characters.
          | Meta            -- ^ Non-content (meta) text characters.
          | Break           -- ^ Separation line break.
          | LineFeed        -- ^ Line break normalized to content line feed.
          | LineFold        -- ^ Line break folded to content space.
          | Indicator       -- ^ Character indicating structure.
          | White           -- ^ Separation white space.
          | Indent          -- ^ Indentation spaces.
          | DirectivesEnd   -- ^ Document start marker.
          | DocumentEnd     -- ^ Document end marker.
          | BeginEscape     -- ^ Begins escape sequence.
          | EndEscape       -- ^ Ends escape sequence.
          | BeginComment    -- ^ Begins comment.
          | EndComment      -- ^ Ends comment.
          | BeginDirective  -- ^ Begins directive.
          | EndDirective    -- ^ Ends directive.
          | BeginTag        -- ^ Begins tag.
          | EndTag          -- ^ Ends tag.
          | BeginHandle     -- ^ Begins tag handle.
          | EndHandle       -- ^ Ends tag handle.
          | BeginAnchor     -- ^ Begins anchor.
          | EndAnchor       -- ^ Ends anchor.
          | BeginProperties -- ^ Begins node properties.
          | EndProperties   -- ^ Ends node properties.
          | BeginAlias      -- ^ Begins alias.
          | EndAlias        -- ^ Ends alias.
          | BeginScalar     -- ^ Begins scalar content.
          | EndScalar       -- ^ Ends scalar content.
          | BeginSequence   -- ^ Begins sequence content.
          | EndSequence     -- ^ Ends sequence content.
          | BeginMapping    -- ^ Begins mapping content.
          | EndMapping      -- ^ Ends mapping content.
          | BeginPair       -- ^ Begins mapping key:value pair.
          | EndPair         -- ^ Ends mapping key:value pair.
          | BeginNode       -- ^ Begins complete node.
          | EndNode         -- ^ Ends complete node.
          | BeginDocument   -- ^ Begins document.
          | EndDocument     -- ^ Ends document.
          | BeginStream     -- ^ Begins YAML stream.
          | EndStream       -- ^ Ends YAML stream.
          | Error           -- ^ Parsing error at this point.
          | Unparsed        -- ^ Unparsed due to errors (or at end of test).
          | Detected        -- ^ Detected parameter (for testing).
  deriving Eq

-- | @show code@ converts a 'Code' to the one-character YEAST token code char.
-- The list of byte codes is also documented in the @yaml2yeast@ program.
instance Show Code where
  show code = case code of
                   Bom             -> "U"
                   Text            -> "T"
                   Meta            -> "t"
                   Break           -> "b"
                   LineFeed        -> "L"
                   LineFold        -> "l"
                   Indicator       -> "I"
                   White           -> "w"
                   Indent          -> "i"
                   DirectivesEnd   -> "K"
                   DocumentEnd     -> "k"
                   BeginEscape     -> "E"
                   EndEscape       -> "e"
                   BeginComment    -> "C"
                   EndComment      -> "c"
                   BeginDirective  -> "D"
                   EndDirective    -> "d"
                   BeginTag        -> "G"
                   EndTag          -> "g"
                   BeginHandle     -> "H"
                   EndHandle       -> "h"
                   BeginAnchor     -> "A"
                   EndAnchor       -> "a"
                   BeginProperties -> "P"
                   EndProperties   -> "p"
                   BeginAlias      -> "R"
                   EndAlias        -> "r"
                   BeginScalar     -> "S"
                   EndScalar       -> "s"
                   BeginSequence   -> "Q"
                   EndSequence     -> "q"
                   BeginMapping    -> "M"
                   EndMapping      -> "m"
                   BeginNode       -> "N"
                   EndNode         -> "n"
                   BeginPair       -> "X"
                   EndPair         -> "x"
                   BeginDocument   -> "O"
                   EndDocument     -> "o"
                   Error           -> "!"
                   Unparsed        -> "-"
                   Detected        -> "$"

-- | Parsed token.
data Token = Token {
    tByteOffset :: Int,   -- ^ 0-base byte offset in stream.
    tCharOffset :: Int,   -- ^ 0-base character offset in stream.
    tLine       :: Int,   -- ^ 1-based line number.
    tLineChar   :: Int,   -- ^ 0-based character in line.
    tCode       :: Code,  -- ^ Specific token 'Code'.
    tText       :: String -- ^ Contained input chars, if any.
  }

-- | @show token@ converts a 'Token' to two YEAST lines: a comment with the
-- position numbers and the actual token line.
instance Show Token where
  show token = "# B: " ++ (show $ token|>tByteOffset)
            ++ ", C: " ++ (show $ token|>tCharOffset)
            ++ ", L: " ++ (show $ token|>tLine)
            ++ ", c: " ++ (show $ token|>tLineChar) ++ "\n"
            ++ (show $ token|>tCode) ++ (escapeString $ token|>tText) ++ "\n"

-- | @escapeString string@ escapes all the non-ASCII characters in the
-- /string/, as well as escaping the \"@\\@\" character, using the \"@\\xXX@\",
-- \"@\\uXXXX@\" and \"@\\UXXXXXXXX@\" escape sequences.
escapeString :: String -> String
escapeString []                                   = []
escapeString (first:rest)
  | ' ' <= first && first /= '\\' && first <= '~' = first:(escapeString rest)
  | first <= '\xFF'                               = "\\x" ++ (toHex 2 $ ord first) ++ (escapeString rest)
  | '\xFF' < first && first <= '\xFFFF'           = "\\u" ++ (toHex 4 $ ord first) ++ (escapeString rest)
  | otherwise                                     = "\\U" ++ (toHex 8 $ ord first) ++ (escapeString rest)

-- | @toHex digits int@ converts the /int/ to the specified number of
-- hexadecimal /digits/.
toHex :: Int -> Int -> String
toHex digits int
  | digits > 1  = (toHex (digits .- 1) (int `div` 16)) ++ [intToDigit $ int `mod` 16]
  | digits == 1 = [intToDigit int]

-- | @showTokens tokens@ converts a list of /tokens/ to a multi-line YEAST
-- text.
showTokens :: [Token] -> String
showTokens tokens = foldr (\ token text -> (show token) ++ text) "" tokens

-- * Parsing framework
--
-- Haskell has no shortage of parsing frameworks. We use our own because:
--
--  * Most available frameworks are inappropriate because of their focus on
--    building a parse tree, and completing all of it before any of it is
--    accessible to the caller. We return a stream of tokens, and would like
--    its head to be accessible as soon as possible to allow for streaming. To
--    do this with bounded memory usage we use a combination of continuation
--    passing style and difference lists for the collected tokens.
--
--  * Haskell makes it so easy to roll your own parsing framework. We need some
--    specialized machinery (limited lookahead, forbidden patterns). It is
--    possible to build these on top of existing frameworks but the end result
--    isn't much shorter than rolling our own.
--
-- Since we roll our own framework we don't bother with making it generalized,
-- so we maintain a single 'State' type rather than having a generic one that
-- contains a polymorphic \"UserState\" field etc.

-- | A 'Parser' is basically a function computing a 'Reply'.
data Parser result = Parser (State -> Reply result)

-- | The 'Result' of each invocation is either an error, the actual result, or
-- a continuation for computing the actual result.
data Result result = Failed String        -- ^ Parsing aborted with a failure.
                   | Result result        -- ^ Parsing completed with a result.
                   | More (Parser result) -- ^ Parsing is ongoing with a continuation.

-- Showing a 'Result' is only used in debugging.
instance (Show result) => Show (Result result) where
  show result = case result of
                     Failed message -> "Failed " ++ message
                     Result result  -> "Result " ++ (show result)
                     More _         -> "More"

-- | Each invocation of a 'Parser' yields a 'Reply'. The 'Result' is only one
-- part of the 'Reply'.
data Reply result = Reply {
    rResult :: !(Result result), -- ^ Parsing result.
    rTokens :: !(D.DList Token), -- ^ Tokens generated by the parser.
    rCommit :: !(Maybe String),  -- ^ Commitment to a decision point.
    rState  :: !State            -- ^ The updated parser state.
  }

-- Showing a 'State' is only used in debugging.
instance (Show result) => Show (Reply result) where
  show reply = "Result: "    ++ (show $ reply|>rResult)
            ++ ", Tokens: "  ++ (show $ D.toList $ reply|>rTokens)
            ++ ", Commit: "  ++ (show $ reply|>rCommit)
            ++ ", State: { " ++ (show $ reply|>rState) ++ "}"

-- A 'Pattern' is a parser that doesn't have an (interesting) result.
type Pattern = Parser ()

-- ** Parsing state

-- | The internal parser state. We don't bother with parameterising it with a
-- \"UserState\", we just bundle the generic and specific fields together (not
-- that it is that easy to draw the line - is @sLine@ generic or specific?).
data State = State {
    sName            :: !String,          -- ^ The input name for error messages.
    sEncoding        :: !Encoding,        -- ^ The input UTF encoding.
    sDecision        :: !String,          -- ^ Current decision name.
    sLimit           :: !Int,             -- ^ Lookahead characters limit.
    sForbidden       :: !(Maybe Pattern), -- ^ Pattern we must not enter into.
    sIsPeek          :: !Bool,            -- ^ Disables token generation.
    sIsSol           :: !Bool,            -- ^ Is at start of line?
    sChars           :: ![Char],          -- ^ (Reversed) characters collected for a token.
    sCharsByteOffset :: !Int,             -- ^ Byte offset of first collected character.
    sCharsCharOffset :: !Int,             -- ^ Char offset of first collected character.
    sCharsLine       :: !Int,             -- ^ Line of first collected character.
    sCharsLineChar   :: !Int,             -- ^ Character in line of first collected character.
    sByteOffset      :: !Int,             -- ^ Offset in bytes in the input.
    sCharOffset      :: !Int,             -- ^ Offset in characters in the input.
    sLine            :: !Int,             -- ^ Builds on YAML's line break definition.
    sLineChar        :: !Int,             -- ^ Character number in line.
    sCode            :: !Code,            -- ^ Of token we are collecting chars for.
    sLast            :: !Char,            -- ^ Last matched character.
    sInput           :: ![(Int, Char)]    -- ^ The decoded input characters.
  }

-- Showing a 'State' is only used in debugging. Note that forcing dump of
-- @sInput@ will disable streaming it.
instance Show State where
  show state = "Name: "              ++ (show $ state|>sName)
            ++ ", Encoding: "        ++ (show $ state|>sEncoding)
            ++ ", Decision: "        ++ (show $ state|>sDecision)
            ++ ", Limit: "           ++ (show $ state|>sLimit)
            ++ ", IsPeek: "          ++ (show $ state|>sIsPeek)
            ++ ", IsSol: "           ++ (show $ state|>sIsSol)
            ++ ", Chars: >>>"        ++ (reverse $ state|>sChars) ++ "<<<"
            ++ ", CharsByteOffset: " ++ (show $ state|>sCharsByteOffset)
            ++ ", CharsCharOffset: " ++ (show $ state|>sCharsCharOffset)
            ++ ", CharsLine: "       ++ (show $ state|>sCharsLine)
            ++ ", CharsLineChar: "   ++ (show $ state|>sCharsLineChar)
            ++ ", ByteOffset: "      ++ (show $ state|>sByteOffset)
            ++ ", CharOffset: "      ++ (show $ state|>sCharOffset)
            ++ ", Line: "            ++ (show $ state|>sLine)
            ++ ", LineChar: "        ++ (show $ state|>sLineChar)
            ++ ", Code: "            ++ (show $ state|>sCode)
            ++ ", Last: "            ++ (show $ state|>sLast)
--          ++ ", Input: >>>"        ++ (show $ state|>sInput) ++ "<<<"

-- | @initialState name input@ returns an initial 'State' for parsing the
-- /input/ (with /name/ for error messages).
initialState :: String -> C.ByteString -> State
initialState name input = let (encoding, decoded) = decode input
                          in State { sName            = name,
                                     sEncoding        = encoding,
                                     sDecision        = "",
                                     sLimit           = -1,
                                     sForbidden       = Nothing,
                                     sIsPeek          = False,
                                     sIsSol           = True,
                                     sChars           = [],
                                     sCharsByteOffset = -1,
                                     sCharsCharOffset = -1,
                                     sCharsLine       = -1,
                                     sCharsLineChar   = -1,
                                     sByteOffset      = 0,
                                     sCharOffset      = 0,
                                     sLine            = 1,
                                     sLineChar        = 0,
                                     sCode            = Unparsed,
                                     sLast            = ' ',
                                     sInput           = decoded }

-- *** Setters
--
-- We need four setter functions to pass them around as arguments. For some
-- reason, Haskell only generates getter functions.

-- | @setLimit limit state@ sets the @sLimit@ field to /limit/.
setLimit :: Int -> State -> State
setLimit limit state = state { sLimit = limit }

-- | @setForbidden forbidden state@ sets the @sForbidden@ field to /forbidden/.
setForbidden :: Maybe Pattern -> State -> State
setForbidden forbidden state = state { sForbidden = forbidden }

-- | @setCode code state@ sets the @sCode@ field to /code/.
setCode :: Code -> State -> State
setCode code state = state { sCode = code }

-- ** Implicit parsers
--
-- It is tedious to have to wrap each expected character (or character range)
-- in an explicit 'Parse' constructor. We let Haskell do that for us using a
-- 'Match' class.

-- | @Match parameter result@ specifies that we can convert the /parameter/ to
-- a 'Parser' returning the /result/.
class Match parameter result | parameter -> result where
    match :: parameter -> Parser result

-- | We don't need to convert a 'Parser', it already is one.
instance Match (Parser result) result where
    match = id

-- | We convert 'Char' to a parser for a character (that returns nothing).
instance Match Char () where
    match code = nextIf (== code)

-- | We convert a 'Char' tuple to a parser for a character range (that returns
-- nothing).
instance Match (Char, Char) () where
    match (low, high) = nextIf $ \ code -> low <= code && code <= high

-- | We convert 'String' to a parser for a sequence of characters (that returns
-- nothing).
instance Match String () where
    match = foldr (&) empty

-- ** Reply constructors

-- | @returnReply state result@ prepares a 'Reply' with the specified /state/
-- and /result/.
returnReply :: State -> result -> Reply result
returnReply state result = Reply { rResult = Result result,
                                   rTokens = D.empty,
                                   rCommit = Nothing,
                                   rState  = state }

-- | @tokenReply state token@ returns a 'Reply' containing the /state/ and
-- /token/. Any collected characters are cleared (either there are none, or we
-- put them in this token, or we don't want them).
tokenReply state token = Reply { rResult = Result (),
                                 rTokens = D.singleton token,
                                 rCommit = Nothing,
                                 rState  = state { sCharsByteOffset = -1,
                                                   sCharsCharOffset = -1,
                                                   sCharsLine       = -1,
                                                   sCharsLineChar   = -1,
                                                   sChars           = [] } }

-- | @failReply state message@ prepares a 'Reply' with the specified /state/
-- and error /message/.
failReply :: State -> String -> Reply result
failReply state message = Reply { rResult = Failed message,
                                  rTokens = D.empty,
                                  rCommit = Nothing,
                                  rState  = state }

-- | @unexpectedReply state@ returns a @failReply@ for an unexpected character.
unexpectedReply :: State -> Reply result
unexpectedReply state = case state|>sInput of
                             ((_, char):_) -> failReply state $ "Unexpected '" ++ [char] ++ "'"
                             []            -> failReply state "Unexpected end of input"

-- | Needed due to the Applicative Monad Proposal.
instance Functor Parser where
  fmap = liftM

-- | Needed due to the Applicative Monad Proposal.
instance Applicative Parser where
  pure = return
  (<*>) = ap

-- | Allow using the @do@ notation for our parsers, which makes for short and
-- sweet @do@ syntax when we want to examine the results (we typically don't).
instance Monad Parser where

  -- @return result@ does just that - return a /result/.
  return result = Parser $ \ state -> returnReply state result

  -- @left >>= right@ applies the /left/ parser, and if it didn't fail
  -- applies the /right/ one (well, the one /right/ returns).
  (Parser left) >>= right = Parser $ \ state ->
                            let reply = left state
                            in case reply|>rResult of
                                    Failed message -> reply { rResult = Failed message }
                                    Result value   -> reply { rResult = More $ right value }
                                    More parser    -> reply { rResult = More $ parser >>= right }

  -- @fail message@ does just that - fails with a /message/.
  fail message = Parser $ \ state -> failReply state message

-- ** Parsing operators
--
-- Here we reap the benefits of renaming the numerical operators. The Operator
-- precedence, in decreasing strength:
--
-- @repeated % n@, @repeated <% n@, @match - rejected@, @match ! decision@,
-- @match ?! decision@, @choice ^ (first \/ second)@.
--
-- @match - first - second@ is @(match - first) - second@.
--
-- @first & second & third@ is @first & (second & third)@. Note that @first -
-- rejected & second@ is @(first - rejected) & second@, etc.
--
-- @match \/ alternative \/ otherwise@ is @match \/ (alternative \/
-- otherwise)@. Note that @first & second \/ third@ is @(first & second) \/
-- third@.
--
-- @( match *)@, @(match +)@, @(match ?)@, @(match <?)@, @(match >?)@, @(match
-- >!)@, @(match <!)@ are the weakest and require the surrounding @()@.

infix  3 ^
infix  3 %
infix  3 <%
infix  3 !
infix  3 ?!
infixl 3 -
infixr 2 &
infixr 1 /
infix  0 ?
infix  0 *
infix  0 +
infix  0 <?
infix  0 >?
infix  0 <!
infix  0 >!

-- | @parser % n@ repeats /parser/ exactly /n/ times.
(%) :: (Match match result) => match -> Int -> Pattern
parser % n
  | n <= 0 = empty
  | n > 0  = parser & parser % n .- 1

-- | @parser <% n@ matches fewer than /n/ occurrences of /parser/.
(<%) :: (Match match result) => match -> Int -> Pattern
parser <% n
  | n < 1 = fail "Fewer than 0 repetitions"
  | n == 1 = reject parser Nothing
  | n > 1  = "<%" ^ ( parser ! "<%" & parser <% n .- 1 / empty )

-- | @decision ^ (option \/ option \/ ...)@ provides a /decision/ name to the
-- choice about to be made, to allow to @commit@ to it.
(^) :: (Match match result) => String -> match -> Parser result
decision ^ parser = choice decision $ match parser

-- | @parser ! decision@ commits to /decision/ (in an option) after
-- successfully matching the /parser/.
(!) :: (Match match result) => match -> String -> Pattern
parser ! decision = parser & commit decision

-- | @parser ?! decision@ commits to /decision/ (in an option) if the current
-- position matches /parser/, without consuming any characters.
(?!) :: (Match match result) => match -> String -> Pattern
parser ?! decision = peek parser & commit decision

-- | @lookbehind <?@ matches the current point without consuming any
-- characters, if the previous character matches the lookbehind parser (single
-- character positive lookbehind)
(<?) :: (Match match result) => match -> Parser result
(<?) lookbehind = prev lookbehind

-- | @lookahead >?@ matches the current point without consuming any characters
-- if it matches the lookahead parser (positive lookahead)
(>?) :: (Match match result) => match -> Parser result
(>?) lookahead = peek lookahead

-- | @lookbehind <!@ matches the current point without consuming any
-- characters, if the previous character does not match the lookbehind parser
-- (single character negative lookbehind)
(<!) :: (Match match result) => match -> Pattern
(<!) lookbehind = prev $ reject lookbehind Nothing

-- | @lookahead >!@ matches the current point without consuming any characters
-- if it does not match the lookahead parser (negative lookahead)
(>!) :: (Match match result) => match -> Pattern
(>!) lookahead = reject lookahead Nothing

-- | @parser - rejected@ matches /parser/, except if /rejected/ matches at this
-- point.
(-) :: (Match match1 result1, Match match2 result2) => match1 -> match2 -> Parser result1
parser - rejected = reject rejected Nothing & parser

-- | @before & after@ parses /before/ and, if it succeeds, parses /after/. This
-- basically invokes the monad's @>>=@ (bind) method.
(&) :: (Match match1 result1, Match match2 result2) => match1 -> match2 -> Parser result2
before & after = (match before) >> (match after)

-- | @first \/ second@ tries to parse /first/, and failing that parses
-- /second/, unless /first/ has committed in which case it fails immediately.
(/) :: (Match match1 result, Match match2 result) => match1 -> match2 -> Parser result
first / second = Parser $ \ state ->
  let Parser parser = decide (match first) (match second)
  in parser state

-- | @(optional ?)@ tries to match /parser/, otherwise does nothing.
(?) :: (Match match result) => match -> Pattern
(?) optional = (optional & empty) / empty

-- | @(parser *)@ matches zero or more occurrences of /repeat/, as long as each
-- one actually consumes input characters.
(*) :: (Match match result) => match -> Pattern
(*) parser = "*" ^ zomParser
  where zomParser = (parser ! "*" & zomParser) / empty

-- | @(parser +)@ matches one or more occurrences of /parser/, as long as each
-- one actually consumed input characters.
(+) :: (Match match result) => match -> Pattern
(+) parser = parser & (parser *)

-- ** Basic parsers

-- | @decide first second@ tries to parse /first/, and failing that parses
-- /second/, unless /first/ has committed in which case it fails immediately.
decide :: Parser result -> Parser result -> Parser result
decide left right = Parser $ \ state ->
  let Parser parser = decideParser state D.empty left right
  in parser state
  where decideParser point tokens (Parser left) right = Parser $ \state ->
          let reply = left state
              tokens' reply = D.append tokens $ reply|>rTokens
          in case (reply|>rResult, reply|>rCommit) of
                  (Failed _,    _)      -> Reply { rState  = point,
                                                   rTokens = D.empty,
                                                   rResult = More right,
                                                   rCommit = Nothing }
                  (More left', Nothing) -> let Parser parser = decideParser point (tokens' reply) left' right
                                           in parser $ reply|>rState
                  _                     -> reply { rTokens = tokens' reply }

-- | @choice decision parser@ provides a /decision/ name to the choice about to
-- be made in /parser/, to allow to @commit@ to it.
choice :: String -> Parser result -> Parser result
choice decision parser = Parser $ \ state ->
  let Parser parser' = choiceParser (state|>sDecision) decision parser
  in parser' state { sDecision = decision }
  where choiceParser parentDecision makingDecision (Parser parser) = Parser $ \ state ->
          let reply   = parser state
              commit' = case reply|>rCommit of
                             Nothing                                    -> Nothing
                             Just decision | decision == makingDecision -> Nothing
                                           | otherwise                  -> reply|>rCommit
              reply'  = case reply|>rResult of
                             More parser' -> reply { rCommit = commit',
                                                     rResult = More $ choiceParser parentDecision makingDecision parser' }
                             _            -> reply { rCommit = commit',
                                                     rState = (reply|>rState) { sDecision = parentDecision } }
          in reply'

-- | @parser ``recovery`` pattern@ parses the specified /parser/; if it fails,
-- it continues to the /recovery/ parser to recover.
recovery :: (Match match1 result, Match match2 result) => match1 -> match2 -> Parser result
recovery pattern recover =
  Parser $ \ state ->
    let (Parser parser) = match pattern
        reply = parser state
    in if state|>sIsPeek
          then reply
          else case reply|>rResult of
                    Result _       -> reply
                    More more      -> reply { rResult = More $ more `recovery` recover }
                    Failed message -> reply { rResult = More $ fake Error message & unparsed & recover }
    where unparsed = let (Parser parser) = match finishToken
                     in Parser $ \ state -> parser $ state { sCode = Unparsed }

-- | @prev parser@ succeeds if /parser/ matches at the previous character. It
-- does not consume any input.
prev :: (Match match result) => match -> Parser result
prev parser = Parser $ \ state ->
  prevParser state (match parser) state { sIsPeek = True, sInput = (-1, state|>sLast) : state|>sInput }
  where prevParser point (Parser parser) state =
          let reply = parser state
          in case reply|>rResult of
                  Failed message -> failReply point message
                  Result value   -> returnReply point value
                  More parser'   -> prevParser point parser' $ reply|>rState

-- | @peek parser@ succeeds if /parser/ matches at this point, but does not
-- consume any input.
peek :: (Match match result) => match -> Parser result
peek parser = Parser $ \ state ->
  peekParser state (match parser) state { sIsPeek = True }
  where peekParser point (Parser parser) state =
          let reply = parser state
          in case reply|>rResult of
                  Failed message -> failReply point message
                  Result value   -> returnReply point value
                  More parser'   -> peekParser point parser' $ reply|>rState

-- | @reject parser name@ fails if /parser/ matches at this point, and does
-- nothing otherwise. If /name/ is provided, it is used in the error message,
-- otherwise the messages uses the current character.
reject :: (Match match result) => match -> Maybe String -> Pattern
reject parser name = Parser $ \ state ->
  rejectParser state name (match parser) state { sIsPeek = True }
  where rejectParser point name (Parser parser) state =
          let reply = parser state
          in case reply|>rResult of
                  Failed message -> returnReply point ()
                  Result value   -> case name of
                                         Nothing   -> unexpectedReply point
                                         Just text -> failReply point $ "Unexpected " ++ text
                  More parser'   -> rejectParser point name parser' $ reply|>rState

-- | @upto parser@ consumes all the character up to and not including the next
-- point where the specified parser is a match.
upto :: Pattern -> Pattern
upto parser = ( (parser >!) & nextIf (const True) *)

-- | @nonEmpty parser@ succeeds if /parser/ matches some non-empty input
-- characters at this point.
nonEmpty :: (Match match result) => match -> Parser result
nonEmpty parser = Parser $ \ state ->
  let Parser parser' = nonEmptyParser (state|>sCharOffset) (match parser)
  in parser' state
  where nonEmptyParser offset (Parser parser) = Parser $ \ state ->
          let reply = parser state
              state' = reply|>rState
          in case reply|>rResult of
                  Failed message -> reply
                  Result value   -> if state'|>sCharOffset > offset
                                       then reply
                                       else failReply state' "Matched empty pattern"
                  More parser'   -> reply { rResult = More $ nonEmptyParser offset parser' }

-- | @empty@ always matches without consuming any input.
empty :: Pattern
empty = return ()

-- | @eof@ matches the end of the input.
eof :: Pattern
eof = Parser $ \ state ->
  if state|>sInput == []
     then returnReply state ()
     else unexpectedReply state

-- | @sol@ matches the start of a line.
sol :: Pattern
sol = Parser $ \ state ->
  if state|>sIsSol
     then returnReply state ()
     else failReply state "Expected start of line"

-- ** State manipulation pseudo-parsers

-- | @commit decision@ commits the parser to all the decisions up to the most
-- recent parent /decision/. This makes all tokens generated in this parsing
-- path immediately available to the caller.
commit :: String -> Pattern
commit decision = Parser $ \ state ->
  Reply { rState  = state,
          rTokens = D.empty,
          rResult = Result (),
          rCommit = Just decision }

-- | @nextLine@ increments @sLine@ counter and resets @sLineChar@.
nextLine :: Pattern
nextLine = Parser $ \ state ->
  returnReply state { sIsSol    = True,
                      sLine     = state|>sLine .+ 1,
                      sLineChar = 0 }
              ()

-- | @with setField getField value parser@ invokes the specified /parser/ with
-- the value of the specified field set to /value/ for the duration of the
-- invocation, using the /setField/ and /getField/ functions to manipulate it.
with :: (value -> State -> State) -> (State -> value) -> value -> Parser result -> Parser result
with setField getField value parser = Parser $ \ state ->
  let value' = getField state
      Parser parser' = value' `seq` withParser value' parser
  in parser' $ setField value state
  where withParser parentValue (Parser parser) = Parser $ \ state ->
          let reply = parser state
          in case reply|>rResult of
                  Failed _     -> reply { rState = setField parentValue $ reply|>rState }
                  Result _     -> reply { rState = setField parentValue $ reply|>rState }
                  More parser' -> reply { rResult = More $ withParser parentValue parser' }

-- | @parser ``forbidding`` pattern@ parses the specified /parser/ ensuring
-- that it does not contain anything matching the /forbidden/ parser.
forbidding :: (Match match1 result1, Match match2 result2) => match1 -> match2 -> Parser result1
forbidding parser forbidden = with setForbidden sForbidden (Just $ forbidden & empty) (match parser)

-- | @parser ``limitedTo`` limit@ parses the specified /parser/
-- ensuring that it does not consume more than the /limit/ input chars.
limitedTo :: (Match match result) => match -> Int -> Parser result
limitedTo parser limit = with setLimit sLimit limit (match parser)

-- ** Consuming input characters

-- | @nextIf test@ fails if the current position matches the 'State' forbidden
-- pattern or if the 'State' lookahead limit is reached. Otherwise it consumes
-- (and buffers) the next input char if it satisfies /test/.
nextIf :: (Char -> Bool) -> Pattern
nextIf test = Parser $ \ state ->
  case state|>sForbidden of
       Nothing     -> limitedNextIf state
       Just parser -> let Parser parser' = reject parser $ Just "forbidden pattern"
                          reply = parser' state { sForbidden = Nothing }
                      in case reply|>rResult of
                              Failed _ -> reply
                              Result _ -> limitedNextIf state
  where limitedNextIf state =
          case state|>sLimit of
               -1    -> consumeNextIf state
               0     -> failReply state "Lookahead limit reached"
               limit -> consumeNextIf state { sLimit = state|>sLimit .- 1 }
        consumeNextIf state =
          case state|>sInput of
               ((offset, char):rest) | test char -> let chars = if state|>sIsPeek
                                                                   then []
                                                                   else char:(state|>sChars)
                                                        byte_offset = charsOf sByteOffset sCharsByteOffset
                                                        char_offset = charsOf sCharOffset sCharsCharOffset
                                                        line        = charsOf sLine       sCharsLine
                                                        line_char   = charsOf sLineChar   sCharsLineChar
                                                        is_sol = if char == '\xFEFF'
                                                                    then state|>sIsSol
                                                                    else False
                                                        state' = state { sInput           = rest,
                                                                         sLast            = char,
                                                                         sChars           = chars,
                                                                         sCharsByteOffset = byte_offset,
                                                                         sCharsCharOffset = char_offset,
                                                                         sCharsLine       = line,
                                                                         sCharsLineChar   = line_char,
                                                                         sIsSol           = is_sol,
                                                                         sByteOffset      = offset,
                                                                         sCharOffset      = state|>sCharOffset .+ 1,
                                                                         sLineChar        = state|>sLineChar .+ 1 }
                                                    in returnReply state' ()
                           | otherwise -> unexpectedReply state
               []                      -> unexpectedReply state
          where charsOf field charsField = if state|>sIsPeek
                                              then -1
                                              else if state|>sChars == []
                                                      then state|>field
                                                      else state|>charsField

-- ** Producing tokens

-- | @finishToken@ places all collected text into a new token and begins a new
-- one, or does nothing if there are no collected characters.
finishToken :: Pattern
finishToken = Parser $ \ state ->
  let state' = state { sChars           = [],
                       sCharsByteOffset = -1,
                       sCharsCharOffset = -1,
                       sCharsLine       = -1,
                       sCharsLineChar   = -1 }
  in if state|>sIsPeek
        then returnReply state' ()
        else case state|>sChars of
                  []          -> returnReply state' ()
                  chars@(_:_) -> tokenReply state' Token { tByteOffset = state|>sCharsByteOffset,
                                                           tCharOffset = state|>sCharsCharOffset,
                                                           tLine       = state|>sCharsLine,
                                                           tLineChar   = state|>sCharsLineChar,
                                                           tCode       = state|>sCode,
                                                           tText       = reverse chars }

-- | @wrap parser@ invokes the /parser/, ensures any unclaimed input characters
-- are wrapped into a token (only happens when testing productions), ensures no
-- input is left unparsed, and returns the parser's result.
wrap :: (Match match result) => match -> Parser result
wrap parser = do result <- match parser
                 finishToken
                 eof
                 return result

-- | @consume parser@ invokes the /parser/ and then consumes all remaining
-- unparsed input characters.
consume :: (Match match result) => match -> Parser result
consume parser = do result <- match parser
                    finishToken
                    clearInput
                    return result
                 where clearInput = Parser $ \ state -> returnReply state { sInput = [] } ()

-- | @token code parser@ places all text matched by /parser/ into a 'Token' with
-- the specified /code/ (unless it is empty). Note it collects the text even if
-- there is an error.
token :: (Match match result) => Code -> match -> Pattern
token code parser = finishToken & with setCode sCode code (parser & finishToken)

-- | @fake code text@ creates a token with the specified /code/ and \"fake\"
-- /text/ characters, instead of whatever characters are collected so far.
fake :: Code -> String -> Pattern
fake code text = Parser $ \ state ->
  if state|>sIsPeek
     then returnReply state ()
     else tokenReply state Token { tByteOffset = value state sByteOffset sCharsByteOffset,
                                   tCharOffset = value state sCharOffset sCharsCharOffset,
                                   tLine       = value state sLine sCharsLine,
                                   tLineChar   = value state sLineChar sCharsLineChar,
                                   tCode       = code,
                                   tText       = text }
    where value state field1 field2 =
            if field2 state == -1
               then field1 state
               else field2 state

-- | @meta parser@ collects the text matched by the specified /parser/ into a
-- | @Meta@ token.
meta :: (Match match result) => match -> Pattern
meta parser = token Meta parser

-- | @indicator code@ collects the text matched by the specified /parser/ into an
-- @Indicator@ token.
indicator :: (Match match result) => match -> Pattern
indicator parser = token Indicator $ parser

-- | @text parser@  collects the text matched by the specified /parser/ into a
-- @Text@ token.
text :: (Match match result) => match -> Pattern
text parser = token Text parser

-- | @emptyToken code@ returns an empty token.
emptyToken :: Code -> Pattern
emptyToken code = finishToken & parser code
  where parser code = Parser $ \ state ->
          if state|>sIsPeek
             then returnReply state ()
             else tokenReply state Token { tByteOffset = state|>sByteOffset,
                                           tCharOffset = state|>sCharOffset,
                                           tLine       = state|>sLine,
                                           tLineChar   = state|>sLineChar,
                                           tCode       = code,
                                           tText       = "" }

-- | @wrapTokens beginCode endCode parser@ wraps the specified /parser/ with
-- matching /beginCode/ and /endCode/ tokens.
wrapTokens :: Code -> Code -> Pattern -> Pattern
wrapTokens beginCode endCode pattern = emptyToken beginCode
                                      & prefixErrorWith pattern (emptyToken endCode)
                                      & emptyToken endCode

-- | @prefixErrorWith pattern prefix@ will invoke the @prefix@ parser if an
-- error is detected during the @pattern@ parser, and then return the error.
prefixErrorWith :: (Match match result) => match -> Pattern -> Parser result
prefixErrorWith pattern prefix =
  Parser $ \ state ->
    let (Parser parser) = match pattern
        reply = parser state
    in case reply|>rResult of
            Result _       -> reply
            More more      -> reply { rResult = More $ prefixErrorWith more prefix }
            Failed message -> reply { rResult = More $ prefix & (fail message :: Parser result) }

-- * Production parameters

-- | Production context.
data Context = BlockOut     -- ^ Outside block sequence.
             | BlockIn      -- ^ Inside block sequence.
             | FlowOut      -- ^ Outside flow collection.
             | FlowIn       -- ^ Inside flow collection.
             | BlockKey     -- ^ Implicit block key.
             | FlowKey      -- ^ Implicit flow key.

-- | @show context@ converts a 'Context' to a 'String'.
instance Show Context where
  show context = case context of
                      BlockOut -> "block-out"
                      BlockIn  -> "block-in"
                      FlowOut  -> "flow-out"
                      FlowIn   -> "flow-in"
                      BlockKey -> "block-key"
                      FlowKey  -> "flow-key"

-- | @read context@ converts a 'String' to a 'Context'. We trust our callers to
-- convert any @-@ characters into @_@ to allow the built-in @lex@ function to
-- handle the names as single identifiers.
instance Read Context where
  readsPrec _ text = [ ((r word), tail) | (word, tail) <- lex text ]
    where r word = case word of
                        "block_out" -> BlockOut
                        "block_in"  -> BlockIn
                        "flow_out"  -> FlowOut
                        "flow_in"   -> FlowIn
                        "block_key" -> BlockKey
                        "flow_key"  -> FlowKey
                        _           -> error $ "unknown context: " ++ word

-- | Chomp method.
data Chomp = Strip -- ^ Remove all trailing line breaks.
           | Clip  -- ^ Keep first trailing line break.
           | Keep  -- ^ Keep all trailing line breaks.

-- | @show chomp@ converts a 'Chomp' to a 'String'.
instance Show Chomp where
  show chomp = case chomp of
                    Strip -> "strip"
                    Clip  -> "clip"
                    Keep  -> "keep"

-- | @read chomp@ converts a 'String' to a 'Chomp'.
instance Read Chomp where
  readsPrec _ text = [ ((r word), tail) | (word, tail) <- lex text ]
    where r word = case word of
                        "strip" -> Strip
                        "clip"  -> Clip
                        "keep"  -> Keep
                        _       -> error $ "unknown chomp: " ++ word

-- * Tokenizers
--
-- We encapsulate the 'Parser' inside a 'Tokenizer'. This allows us to hide the
-- implementation details from our callers.

-- | 'Tokenizer' converts a (named) input text into a list of 'Token'. Errors
-- are reported as tokens with the @Error@ 'Code', and the unparsed text
-- following an error may be attached as a final token (if the @Bool@ is
-- @True@). Note that tokens are available \"immediately\", allowing for
-- streaming of large YAML files with memory requirements depending only on the
-- YAML nesting level.
type Tokenizer = String -> C.ByteString -> Bool -> [Token]

-- | @patternTokenizer pattern@ converts the /pattern/ to a simple 'Tokenizer'.
patternTokenizer :: Pattern -> Tokenizer
patternTokenizer pattern name input withFollowing =
  D.toList $ patternParser (wrap pattern) (initialState name input)
  where patternParser (Parser parser) state =
          let reply = parser state
              tokens = commitBugs reply
              state' = reply|>rState
          in case reply|>rResult of
                  Failed message -> errorTokens tokens state' message withFollowing
                  Result _       -> tokens
                  More parser'   -> D.append tokens $ patternParser parser' state'

-- | @parserTokenizer what parser@ converts the /parser/ returning /what/ to a
-- simple 'Tokenizer' (only used for tests). The result is reported as a token
-- with the @Detected@ 'Code' The result is reported as a token with the
-- @Detected@ 'Code'.
parserTokenizer :: (Show result, Match match result) => String -> match -> Tokenizer
parserTokenizer what parser name input withFollowing =
  D.toList $ parserParser (wrap parser) (initialState name input)
  where parserParser (Parser parser) state =
          let reply = parser state
              tokens = commitBugs reply
              state' = reply|>rState
          in case reply|>rResult of
                  Failed message -> errorTokens tokens state' message withFollowing
                  Result value   -> D.append tokens $ D.singleton Token { tByteOffset = state'|>sByteOffset,
                                                                          tCharOffset = state'|>sCharOffset,
                                                                          tLine       = state'|>sLine,
                                                                          tLineChar   = state'|>sLineChar,
                                                                          tCode       = Detected,
                                                                          tText       = what ++ "=" ++ (show value) }
                  More parser'   -> D.append tokens $ parserParser parser' $ state'

-- | @errorTokens tokens state message withFollowing@ appends an @Error@ token
-- with the specified /message/ at the end of /tokens/, and if /withFollowing/
-- also appends the unparsed text following the error as a final @Unparsed@
-- token.
errorTokens tokens state message withFollowing =
    let tokens' = D.append tokens $ D.singleton Token { tByteOffset = state|>sByteOffset,
                                                        tCharOffset = state|>sCharOffset,
                                                        tLine       = state|>sLine,
                                                        tLineChar   = state|>sLineChar,
                                                        tCode       = Error,
                                                        tText       = message }
    in if withFollowing && state|>sInput /= []
       then D.append tokens' $ D.singleton Token { tByteOffset = state|>sByteOffset,
                                                   tCharOffset = state|>sCharOffset,
                                                   tLine       = state|>sLine,
                                                   tLineChar   = state|>sLineChar,
                                                   tCode       = Unparsed,
                                                   tText       = map snd $ state|>sInput }
       else tokens'

-- | @commitBugs reply@ inserts an error token if a commit was made outside a
-- named choice. This should never happen outside tests.
commitBugs :: Reply result -> D.DList Token
commitBugs reply =
  let tokens = reply|>rTokens
      state = reply|>rState
  in case reply|>rCommit of
          Nothing     -> tokens
          Just commit -> D.append tokens $ D.singleton Token { tByteOffset = state|>sByteOffset,
                                                               tCharOffset = state|>sCharOffset,
                                                               tLine       = state|>sLine,
                                                               tLineChar   = state|>sLineChar,
                                                               tCode       = Error,
                                                               tText       = "Commit to '" ++ commit ++ "' was made outside it" }

-- | @yaml name input@ converts the Unicode /input/ (called /name/ in error
-- messages) to a list of 'Token' according to the YAML spec. This is it!
yaml :: Tokenizer
yaml = patternTokenizer l_yaml_stream

-- | @pName name@ converts a parser name to the \"proper\" spec name.
pName :: String -> String
pName name = regexSub questionRegex "?"
           $ regexSub minusRegex "-"
           $ regexSub plusRegex "+" name
           where regexSub regex value text = subRegex regex text value
                 questionRegex = mkRegex "'"
                 minusRegex = mkRegex "_"
                 plusRegex = mkRegex "__"

-- | @tokenizers@ returns a mapping from a production name to a production
-- tokenizer.
tokenizers :: Map.Map String Tokenizer
tokenizers = par "c_chomping_indicator" c_chomping_indicator "t"
           $ pac "detect_inline_indentation" detect_inline_indentation "m"
           $ pat "b_as_line_feed" b_as_line_feed
           $ pat "b_as_space" b_as_space
           $ pat "b_carriage_return" b_carriage_return
           $ pat "b_break" b_break
           $ pat "b_char" b_char
           $ pat "b_line_feed" b_line_feed
           $ pat "b_non_content" b_non_content
           $ pat "b_comment" b_comment
           $ pat "c_alias" c_alias
           $ pat "c_anchor" c_anchor
           $ pat "c_byte_order_mark" c_byte_order_mark
           $ pat "c_collect_entry" c_collect_entry
           $ pat "c_comment" c_comment
           $ pat "c_directive" c_directive
           $ pat "c_directives_end" c_directives_end
           $ pat "c_document_end" c_document_end
           $ pat "c_double_quote" c_double_quote
           $ pat "c_escape" c_escape
           $ pat "c_flow_indicator" c_flow_indicator
           $ pat "c_folded" c_folded
           $ pat "c_forbidden" c_forbidden
           $ pat "c_indicator" c_indicator
           $ pat "c_literal" c_literal
           $ pat "c_mapping_end" c_mapping_end
           $ pat "c_mapping_key" c_mapping_key
           $ pat "c_mapping_start" c_mapping_start
           $ pat "c_mapping_value" c_mapping_value
           $ pat "c_named_tag_handle" c_named_tag_handle
           $ pat "c_nb_comment_text" c_nb_comment_text
           $ pat "c_non_specific_tag" c_non_specific_tag
           $ pat "c_ns_alias_node" c_ns_alias_node
           $ pat "c_ns_anchor_property" c_ns_anchor_property
           $ pat "c_ns_esc_char" c_ns_esc_char
           $ pat "c_ns_local_tag_prefix" c_ns_local_tag_prefix
           $ pat "c_ns_shorthand_tag" c_ns_shorthand_tag
           $ pat "c_ns_tag_property" c_ns_tag_property
           $ pat "c_primary_tag_handle" c_primary_tag_handle
           $ pat "c_printable" c_printable
           $ pat "c_quoted_quote" c_quoted_quote
           $ pat "c_reserved" c_reserved
           $ pat "c_secondary_tag_handle" c_secondary_tag_handle
           $ pat "c_sequence_end" c_sequence_end
           $ pat "c_sequence_entry" c_sequence_entry
           $ pat "c_sequence_start" c_sequence_start
           $ pat "c_single_quote" c_single_quote
           $ pat "c_tag" c_tag
           $ pat "c_tag_handle" c_tag_handle
           $ pat "c_verbatim_tag" c_verbatim_tag
           $ pat "e_node" e_node
           $ pat "e_scalar" e_scalar
           $ pat "l_any_document" l_any_document
           $ pat "l_bare_document" l_bare_document
           $ pat "l_comment" l_comment
           $ pat "l_directive" l_directive
           $ pat "l_directives_document" l_directives_document
           $ pat "l_document_prefix" l_document_prefix
           $ pat "l_document_suffix" l_document_suffix
           $ pat "l_explicit_document" l_explicit_document
           $ pat "l_yaml_stream" l_yaml_stream
           $ pat "nb_char" nb_char
           $ pat "nb_double_char" nb_double_char
           $ pat "nb_double_one_line" nb_double_one_line
           $ pat "nb_json" nb_json
           $ pat "nb_ns_double_in_line" nb_ns_double_in_line
           $ pat "nb_ns_single_in_line" nb_ns_single_in_line
           $ pat "nb_single_char" nb_single_char
           $ pat "nb_single_one_line" nb_single_one_line
           $ pat "ns_anchor_char" ns_anchor_char
           $ pat "ns_anchor_name" ns_anchor_name
           $ pat "ns_ascii_letter" ns_ascii_letter
           $ pat "ns_char" ns_char
           $ pat "ns_dec_digit" ns_dec_digit
           $ pat "ns_directive_name" ns_directive_name
           $ pat "ns_directive_parameter" ns_directive_parameter
           $ pat "ns_double_char" ns_double_char
           $ pat "ns_esc_16_bit" ns_esc_16_bit
           $ pat "ns_esc_32_bit" ns_esc_32_bit
           $ pat "ns_esc_8_bit" ns_esc_8_bit
           $ pat "ns_esc_backslash" ns_esc_backslash
           $ pat "ns_esc_backspace" ns_esc_backspace
           $ pat "ns_esc_bell" ns_esc_bell
           $ pat "ns_esc_carriage_return" ns_esc_carriage_return
           $ pat "ns_esc_double_quote" ns_esc_double_quote
           $ pat "ns_esc_escape" ns_esc_escape
           $ pat "ns_esc_form_feed" ns_esc_form_feed
           $ pat "ns_esc_horizontal_tab" ns_esc_horizontal_tab
           $ pat "ns_esc_line_feed" ns_esc_line_feed
           $ pat "ns_esc_line_separator" ns_esc_line_separator
           $ pat "ns_esc_next_line" ns_esc_next_line
           $ pat "ns_esc_non_breaking_space" ns_esc_non_breaking_space
           $ pat "ns_esc_null" ns_esc_null
           $ pat "ns_esc_paragraph_separator" ns_esc_paragraph_separator
           $ pat "ns_esc_slash" ns_esc_slash
           $ pat "ns_esc_space" ns_esc_space
           $ pat "ns_esc_vertical_tab" ns_esc_vertical_tab
           $ pat "ns_global_tag_prefix" ns_global_tag_prefix
           $ pat "ns_hex_digit" ns_hex_digit
           $ pat "ns_plain_safe_in" ns_plain_safe_in
           $ pat "ns_plain_safe_out" ns_plain_safe_out
           $ pat "ns_reserved_directive" ns_reserved_directive
           $ pat "ns_s_block_map_implicit_key" ns_s_block_map_implicit_key
           $ pat "ns_single_char" ns_single_char
           $ pat "ns_tag_char" ns_tag_char
           $ pat "ns_tag_directive" ns_tag_directive
           $ pat "ns_tag_prefix" ns_tag_prefix
           $ pat "ns_uri_char" ns_uri_char
           $ pat "ns_word_char" ns_word_char
           $ pat "ns_yaml_directive" ns_yaml_directive
           $ pat "ns_yaml_version" ns_yaml_version
           $ pat "s_b_comment" s_b_comment
           $ pat "s_l_comments" s_l_comments
           $ pat "s_separate_in_line" s_separate_in_line
           $ pat "s_space" s_space
           $ pat "s_tab" s_tab
           $ pat "s_white" s_white
           $ Map.empty
  where pat name pattern     = Map.insert (pName name) $ patternTokenizer     (match pattern)
        par name parser what = Map.insert (pName name) $ parserTokenizer what (match parser)
        pac name parser what = Map.insert (pName name) $ parserTokenizer what (consume parser)

-- | @tokenizer name@ converts the production with the specified /name/ to a
-- simple 'Tokenizer', or @Nothing@ if it isn't known.
tokenizer :: String -> (Maybe Tokenizer)
tokenizer name = Map.lookup name tokenizers

-- | @tokenizersWithN@ returns a mapping from a production name to a production
-- tokenizer (that takes an /n/ argument).
tokenizersWithN :: Map.Map String (Int -> Tokenizer)
tokenizersWithN = par "c_b_block_header" c_b_block_header "(m,t)"
                $ pac "detect_collection_indentation" detect_collection_indentation "m"
                $ pac "detect_scalar_indentation" detect_scalar_indentation "m"
                $ par "c_indentation_indicator" c_indentation_indicator "m"
                $ par "count_spaces" count_spaces "m"
                $ pat "b_l_spaced" b_l_spaced
                $ pat "b_nb_literal_next" b_nb_literal_next
                $ pat "c_l_block_map_explicit_entry" c_l_block_map_explicit_entry
                $ pat "c_l_block_map_explicit_key" c_l_block_map_explicit_key
                $ pat "c_l_block_map_implicit_value" c_l_block_map_implicit_value
                $ pat "c_l_block_seq_entry" c_l_block_seq_entry
                $ pat "c_l__folded" c_l__folded
                $ pat "c_l__literal" c_l__literal
                $ pat "l_block_map_explicit_value" l_block_map_explicit_value
                $ pat "l__block_mapping" l__block_mapping
                $ pat "l__block_sequence" l__block_sequence
                $ pat "l_keep_empty" l_keep_empty
                $ pat "l_nb_diff_lines" l_nb_diff_lines
                $ pat "l_nb_folded_lines" l_nb_folded_lines
                $ pat "l_nb_literal_text" l_nb_literal_text
                $ pat "l_nb_same_lines" l_nb_same_lines
                $ pat "l_nb_spaced_lines" l_nb_spaced_lines
                $ pat "l_strip_empty" l_strip_empty
                $ pat "l_trail_comments" l_trail_comments
                $ pat "nb_double_multi_line" nb_double_multi_line
                $ pat "nb_single_multi_line" nb_single_multi_line
                $ pat "ns_l_block_map_entry" ns_l_block_map_entry
                $ pat "ns_l_block_map_implicit_entry" ns_l_block_map_implicit_entry
                $ pat "ns_l_in_line_mapping" ns_l_in_line_mapping
                $ pat "ns_l_in_line_sequence" ns_l_in_line_sequence
                $ pat "s_block_line_prefix" s_block_line_prefix
                $ pat "s_double_break" s_double_break
                $ pat "s_double_escaped" s_double_escaped
                $ pat "s_double_next_line" s_double_next_line
                $ pat "s_flow_folded" s_flow_folded
                $ pat "s_flow_line_prefix" s_flow_line_prefix
                $ pat "s_indent" s_indent
                $ pat "s_indent_le" s_indent_le
                $ pat "s_indent_lt" s_indent_lt
                $ pat "s_l__flow_in_block" s_l__flow_in_block
                $ pat "s_nb_folded_text" s_nb_folded_text
                $ pat "s_nb_spaced_text" s_nb_spaced_text
                $ pat "s_separate_lines" s_separate_lines
                $ pat "s_single_next_line" s_single_next_line
                $ Map.empty
  where pat name pattern     = Map.insert (pName name) (\ n -> patternTokenizer     (match   $ pattern n))
        par name parser what = Map.insert (pName name) (\ n -> parserTokenizer what (match   $ parser  n))
        pac name parser what = Map.insert (pName name) (\ n -> parserTokenizer what (consume $ parser  n))

-- | @tokenizerWithN name n@ converts the production (that requires an /n/
-- argument) with the specified /name/ to a simple 'Tokenizer', or @Nothing@ if
-- it isn't known.
tokenizerWithN :: String -> Int -> Maybe Tokenizer
tokenizerWithN name n =
  case Map.lookup name tokenizersWithN of
    Just tokenizer -> Just $ tokenizer n
    Nothing        -> Nothing

-- | @tokenizersWithC@ returns a mapping from a production name to a production
-- tokenizer (that takes a /c/ argument).
tokenizersWithC :: Map.Map String (Context -> Tokenizer)
tokenizersWithC = pat "c_s_implicit_json_key" c_s_implicit_json_key
                $ pat "nb_ns_plain_in_line" nb_ns_plain_in_line
                $ pat "ns_plain_char" ns_plain_char
                $ pat "ns_plain_first" ns_plain_first
                $ pat "ns_plain_one_line" ns_plain_one_line
                $ pat "ns_plain_safe" ns_plain_safe
                $ pat "ns_s_implicit_yaml_key" ns_s_implicit_yaml_key
                $ Map.empty
  where pat name pattern = Map.insert (pName name) (\ c -> patternTokenizer (match $ pattern c))

-- | @tokenizerWithC name c@ converts the production (that requires a /c/
-- argument) with the specified /name/ to a simple 'Tokenizer', or @Nothing@ if
-- it isn't known.
tokenizerWithC :: String -> Context -> Maybe Tokenizer
tokenizerWithC name c =
  case Map.lookup name tokenizersWithC of
    Just tokenizer -> Just $ tokenizer c
    Nothing        -> Nothing

-- | @tokenizersWithT@ returns a mapping from a production name to a production
-- tokenizer (that takes a /t/ argument).
tokenizersWithT :: Map.Map String (Chomp -> Tokenizer)
tokenizersWithT = pat "b_chomped_last" b_chomped_last
                $ Map.empty
  where pat name pattern = Map.insert (pName name) (\ t -> patternTokenizer (match $ pattern t))

-- | @tokenizerWithT name t@ converts the production (that requires an /t/
-- argument) with the specified /name/ to a simple 'Tokenizer', or @Nothing@ if
-- it isn't known.
tokenizerWithT :: String -> Chomp -> Maybe Tokenizer
tokenizerWithT name t =
  case Map.lookup name tokenizersWithT of
    Just tokenizer -> Just $ tokenizer t
    Nothing        -> Nothing

-- | @tokenizersWithNC@ returns a mapping from a production name to a
-- production tokenizer (that requires /n/ and /c/ arguments).
tokenizersWithNC :: Map.Map String (Int -> Context -> Tokenizer)
tokenizersWithNC = pat "b_l_folded" b_l_folded
                 $ pat "b_l_trimmed" b_l_trimmed
                 $ pat "c_double_quoted" c_double_quoted
                 $ pat "c_flow_json_content" c_flow_json_content
                 $ pat "c_flow_json_node" c_flow_json_node
                 $ pat "c_flow_mapping" c_flow_mapping
                 $ pat "c_flow_sequence" c_flow_sequence
                 $ pat "c_ns_flow_map_adjacent_value" c_ns_flow_map_adjacent_value
                 $ pat "c_ns_flow_map_empty_key_entry" c_ns_flow_map_empty_key_entry
                 $ pat "c_ns_flow_map_json_key_entry" c_ns_flow_map_json_key_entry
                 $ pat "c_ns_flow_map_separate_value" c_ns_flow_map_separate_value
                 $ pat "c_ns_flow_pair_json_key_entry" c_ns_flow_pair_json_key_entry
                 $ pat "c_ns_properties" c_ns_properties
                 $ pat "c_single_quoted" c_single_quoted
                 $ pat "l_empty" l_empty
                 $ pat "nb_double_text" nb_double_text
                 $ pat "nb_single_text" nb_single_text
                 $ pat "ns_flow_content" ns_flow_content
                 $ pat "ns_flow_map_entry" ns_flow_map_entry
                 $ pat "ns_flow_map_explicit_entry" ns_flow_map_explicit_entry
                 $ pat "ns_flow_map_implicit_entry" ns_flow_map_implicit_entry
                 $ pat "ns_flow_map_yaml_key_entry" ns_flow_map_yaml_key_entry
                 $ pat "ns_flow_node" ns_flow_node
                 $ pat "ns_flow_pair" ns_flow_pair
                 $ pat "ns_flow_pair_entry" ns_flow_pair_entry
                 $ pat "ns_flow_pair_yaml_key_entry" ns_flow_pair_yaml_key_entry
                 $ pat "ns_flow_seq_entry" ns_flow_seq_entry
                 $ pat "ns_flow_yaml_content" ns_flow_yaml_content
                 $ pat "ns_flow_yaml_node" ns_flow_yaml_node
                 $ pat "ns_plain" ns_plain
                 $ pat "ns_plain_multi_line" ns_plain_multi_line
                 $ pat "ns_s_flow_map_entries" ns_s_flow_map_entries
                 $ pat "ns_s_flow_seq_entries" ns_s_flow_seq_entries
                 $ pat "s_l__block_collection" s_l__block_collection
                 $ pat "s_l__block_in_block" s_l__block_in_block
                 $ pat "s_l__block_indented" s_l__block_indented
                 $ pat "s_l__block_node" s_l__block_node
                 $ pat "s_l__block_scalar" s_l__block_scalar
                 $ pat "s_line_prefix" s_line_prefix
                 $ pat "s_ns_plain_next_line" s_ns_plain_next_line
                 $ pat "s_separate" s_separate
                 $ Map.empty
  where pat name pattern = Map.insert (pName name) (\ n c -> patternTokenizer (match $ pattern n c))

-- | @tokenizerWithNC name n c@ converts the production (that requires /n/ and
-- /c/ arguments) with the specified /name/ to a simple 'Tokenizer', or
-- @Nothing@ if it isn't known.
tokenizerWithNC :: String -> Int -> Context -> Maybe Tokenizer
tokenizerWithNC name n c =
  case Map.lookup name tokenizersWithNC of
    Just tokenizer -> Just $ tokenizer n c
    Nothing        -> Nothing

-- | @tokenizersWithNT@ returns a mapping from a production name to a
-- production tokenizer (that requires /n/ and /t/ arguments).
tokenizersWithNT :: Map.Map String (Int -> Chomp -> Tokenizer)
tokenizersWithNT = pat "l_chomped_empty" l_chomped_empty
                 $ pat "l_folded_content" l_folded_content
                 $ pat "l_literal_content" l_literal_content
                 $ Map.empty
  where pat name pattern = Map.insert (pName name) (\ n t -> patternTokenizer (match $ pattern n t))

-- | @tokenizerWithNT name n t@ converts the production (that requires /n/ and
-- /t/ arguments) with the specified /name/ to a simple 'Tokenizer', or
-- @Nothing@ if it isn't known.
tokenizerWithNT :: String -> Int -> Chomp -> Maybe Tokenizer
tokenizerWithNT name n t =
  case Map.lookup name tokenizersWithNT of
    Just tokenizer -> Just $ tokenizer n t
    Nothing        -> Nothing

-- | @tokenizerNames@ returns the list of all productions (tokenizers).
tokenizerNames :: [String]
tokenizerNames = (Map.keys tokenizers)
              ++ (Map.keys tokenizersWithN)
              ++ (Map.keys tokenizersWithC)
              ++ (Map.keys tokenizersWithT)
              ++ (Map.keys tokenizersWithNC)
              ++ (Map.keys tokenizersWithNT)

-- * Productions

-- ** BNF compatibility helpers

-- | @detect_utf_encoding@ doesn't actually detect the encoding, we just call it
-- this way to make the productions compatible with the spec. Instead it simply
-- reports the encoding (which was already detected when we started parsing).
bom code = code
         & (Parser $ \ state -> let text = case state|>sEncoding of
                                                UTF8    -> "TF-8"
                                                UTF16LE -> "TF-16LE"
                                                UTF16BE -> "TF-16BE"
                                                UTF32LE -> "TF-32LE"
                                                UTF32BE -> "TF-32BE"
                                    Parser parser = fake Bom text
                                in parser state)

-- | @na@ is the \"non-applicable\" indentation value. We use Haskell's laziness
-- to verify it really is never used.
na :: Int
na = error "Accessing non-applicable indentation"

-- | @asInteger@ returns the last consumed character, which is assumed to be a
-- decimal digit, as an integer.
asInteger :: Parser Int
asInteger = Parser $ \ state -> returnReply state $ ord (state|>sLast) .- 48

-- | @result value@ is the same as /return value/ except that we give the
-- Haskell type deduction the additional boost it needs to figure out this is
-- wrapped in a 'Parser'.
result :: result -> Parser result
result = return

#include "Reference.bnf"
