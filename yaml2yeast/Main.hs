-------------------------------------------------------------------------------
-- |
-- Module      :  yaml2yeast
-- Copyright   :  (c) Oren Ben-Kiki 2007
-- License     :  LGPL
--
-- Maintainer  :  yaml-oren@ben-kiki.org
-- Stability   :  alpha
-- Portability :  portable
--
-- Convert an input YAML file to a YEAST (YAML Elaborate Atomic Syntax Tokens).
-- Command line options are:
--
--  [@-h@ @--help@] Prints short usage message
--
--  [@-o@ @--output@ @file@] Specify output file (default is \"@-@\" for
--  stdout)
--
--  [@-u@ @--unbuffered@] Disable @stdout@ buffering.
--
--  [@-p@ @--production@ @production@] Specify the top production (default is
--  full YAML stream).
--
--  [@-n@ @--n-value@ @n@] Specify the /n/ parameter of the production.
--
--  [@-c@ @--c-value@ @c@] Specify the /c/ parameter of the production.
--
--  [@-t@ @--t-value@ @t@] Specify the /t/ parameter of the production.
--
--  [@input@] Specify the input file (default is \"@-@\" for stdin).
--
-- The YEAST format is designed to allow trivial post-processing. Since YEAST
-- contains all the syntactical information contained in the original YAML
-- file, such processing can perform tasks which are impossible or difficult
-- using higher-level YAML parsers, such as pretty-printing. For example, the
-- @yeast2html@ program generates an HTML visualization of the syntactical
-- structure of the original YAML file.
--
-- YEAST contains one line per token, where the first character is the token
-- code and the following characters are the input characters contained in the
-- token, if any. Non printable\/non ASCII characters (including line breaks)
-- are escaped using either \"@\\x@/XX/\", \"@\\u@/XXXX/\" or
-- \"@\\U@/XXXXXXXX/\" notation. Therefore YEAST files are restricted to 7-bit
-- printable ASCII, making them sufficiently human-readable for debugging
-- purposes.
--
-- The YEAST token codes are:
--
--  [@U@] BOM, contains \"@TF8@\", \"@TF16LE@\" or \"@TF16BE@\"
--
--  [@T@] Contains preserved content text characters
--
--  [@t@] Contains non-content (meta) text characters
--
--  [@b@] Contains separation line break
--
--  [@L@] Contains line break normalized to content line feed
--
--  [@l@] Contains line break folded to content space
--
--  [@I@] Contains character indicating structure
--
--  [@w@] Contains separation white space
--
--  [@i@] Contains indentation spaces
--
--  [@K@] Directives end marker
--
--  [@k@] Document end marker
--
--  [@E@] Begins escape sequence
--
--  [@e@] Ends escape sequence
--
--  [@C@] Begins comment
--
--  [@c@] Ends comment
--
--  [@D@] Begins directive
--
--  [@d@] Ends directive
--
--  [@G@] Begins tag
--
--  [@g@] Ends tag
--
--  [@H@] Begins tag handle
--
--  [@h@] Ends tag handle
--
--  [@A@] Begins anchor
--
--  [@a@] Ends anchor
--
--  [@P@] Begins node properties
--
--  [@p@] Ends node properties
--
--  [@R@] Begins alias (reference)
--
--  [@r@] Ends alias (reference)
--
--  [@S@] Begins scalar content
--
--  [@s@] Ends scalar content
--
--  [@Q@] Begins sequence content
--
--  [@q@] Ends sequence content
--
--  [@M@] Begins mapping content
--
--  [@m@] Ends mapping content
--
--  [@N@] Begins complete node
--
--  [@n@] Ends complete node
--
--  [@X@] Begins mapping key:value pair
--
--  [@x@] Ends mapping key:value pair
--
--  [@O@] Begins document
--
--  [@o@] Ends document
--
--  [@!@] Parsing error at this point.
--
--  [@-@] Unparsed text following error point.
--
-- In addition, the following code is used for testing partial productions
-- and do not appear when parsing a complete YAML stream:
--
--  [@$@] Value of detected parameters
--
-------------------------------------------------------------------------------
module Main (main) where

import           Control.Monad
import qualified Data.ByteString.Lazy.Char8 as C
import           System.Console.GetOpt
import           System.Environment
import           System.IO
import           Text.Regex
import           Text.Yaml.Reference


-- | Command line flag.
data Flag = Help                   -- ^ Request printing usage.
          | Output String          -- ^ Specify output file name.
          | Production String      -- ^ Specify start production name.
          | Unbuffered             -- ^ Disable @stdout@ buffering.
          | Following              -- ^ Emit unparsed text following an error.
          | ParamN (Maybe Int)     -- ^ Specify $n$ parameter.
          | ParamT (Maybe Chomp)   -- ^ Specify $t$ parameter.
          | ParamC (Maybe Context) -- ^ Specify $c$ parameter.
          | Input String           -- ^ Specify input file name.
    deriving Show

-- | Command line options.
optionDescriptions :: [OptDescr Flag]
optionDescriptions = [
      Option ['h', '?'] ["help"]
             (NoArg Help)
             "print usage and exit",
      Option ['o'] ["output"]
             (ReqArg Output "file")
             "output file",
      Option ['u'] ["unbuffered"]
             (NoArg Unbuffered)
             "disable stdout buffering",
      Option ['f'] ["following"]
             (NoArg Following)
             "emit unparsed input following an error",
      Option ['p'] ["production"]
             (ReqArg Production "production")
             "top production",
      Option ['n'] ["n-value"]
             (ReqArg (ParamN . readParameter) "indentation")
             "n parameter value (indentation)",
      Option ['c'] ["context"]
             (ReqArg (ParamC . readParameter) "context")
             "c parameter value (context)",
      Option ['t'] ["chomp"]
             (ReqArg (ParamT . readParameter) "chomp")
             "t parameter value (chomp)"
  ]

-- | @testParameter parameter@ converts the /parameter/ to a typed value. Patch
-- the @-@ characters in the /c/ parameter into @_@ to make it possible for the
-- built-in lexer to handle them.
readParameter :: (Read t) => String -> Maybe t
readParameter text = Just $ read $ subRegex (mkRegex "([a-z])-") text "\\1_"

-- | @usage@ returns the usage string for the program.
usage :: String
usage = usageInfo "yaml2yeast: [options] [file]" optionDescriptions

-- | @collectFlags args@ converts the command line /args/ to list of 'Flag'.
collectFlags :: [String] -> IO [Flag]
collectFlags args =
  case getOpt Permute optionDescriptions args of
       (_,     _,     [error:errors]) -> ioError $ userError $ (concat [error:errors]) ++ usage
       (flags, [],    [])             -> do return flags
       (flags, [arg], [])             -> do return $ [Input arg] ++ flags
       (flags, _,     [])             -> ioError $ userError "more than one input file"

-- | Options controlling program behavior
data Options = Options {
      oToHelp        :: Bool,          -- ^ Whether to just print the usage.
      oToUnbuffer    :: Bool,          -- ^ Whether to disable @stdout@ buffering.
      oWithFollowing :: Bool,          -- ^ Whether to emit unparsed text following an error.
      oInput         :: String,        -- ^ Name of input file ("-": @stdin@).
      oOutput        :: String,        -- ^ Name of output file ("-": @stdout@).
      oProduction    :: String,        -- ^ Name of start production.
      oN             :: Maybe Int,     -- ^ N parameter, if any.
      oC             :: Maybe Context, -- ^ C parameter, if any.
      oT             :: Maybe Chomp    -- ^ T parameter, if any.
  }

-- | Default options if no flags are specified.
defaultOptions = Options {
      oToHelp        = False,
      oToUnbuffer    = False,
      oWithFollowing = False,
      oInput         = "-",
      oOutput        = "-",
      oProduction    = "l-yaml-stream",
      oN             = Nothing,
      oC             = Nothing,
      oT             = Nothing
  }

-- | @applyFlags flags options@ applies the specified /flags/ to the /options/.
applyFlags :: [Flag] -> Options -> IO Options
applyFlags []           options = return options
applyFlags (flag:flags) options =
  case flag of
       Help            -> applyFlags flags options { oToHelp = True }
       Unbuffered      -> applyFlags flags options { oToUnbuffer = True }
       Following       -> applyFlags flags options { oWithFollowing = True }
       Output name     -> applyFlags flags options { oOutput = name }
       Input name      -> applyFlags flags options { oInput = name }
       ParamN value    -> applyFlags flags options { oN = value }
       ParamC value    -> applyFlags flags options { oC = value }
       ParamT value    -> applyFlags flags options { oT = value }
       Production name -> applyFlags flags options { oProduction = name }

-- | @fromFile name@ returns the contents of the specified input file called
-- /name/ (may be "-" for @stdin@).
fromFile :: String -> IO C.ByteString
fromFile name =
  case name of
       "-"  -> C.getContents
       path -> C.readFile path

-- | @intoFile name text@ writes the /text/ into the specified output file
-- called /name/ (may be "-" for @stdout@).
intoFile :: String -> String -> IO ()
intoFile name text =
  case name of
       "-"  -> putStr text
       path -> writeFile path $ text

-- | @runWith options@ runs the program with the specified /options/.
runWith :: Options -> IO ()
runWith options =
  do let resolved = case ((oN options), (oC options), (oT options)) of
                         (Nothing,    Nothing,    Nothing) -> tokenizer       (oProduction options)
                         (Just n,     Nothing,    Nothing) -> tokenizerWithN  (oProduction options) n
                         (Nothing,    Just c,     Nothing) -> tokenizerWithC  (oProduction options) c
                         (Nothing,    Nothing,    Just t)  -> tokenizerWithT  (oProduction options) t
                         (Just n,     Just c,     Nothing) -> tokenizerWithNC (oProduction options) n c
                         (Just n,     Nothing,    Just t)  -> tokenizerWithNT (oProduction options) n t
                         _                                 -> error "No production with this combination of parameters"
     case resolved of
          Nothing     -> error $ "No production " ++ (oProduction options) ++ " or it doesn't take this combination of parameters"
          Just parser -> do text <- fromFile (oInput options)
                            let tokens = parser (oInput options) text (oWithFollowing options)
                            intoFile (oOutput options) (showTokens tokens)

-- | @main@ converts an input YAML file to YEAST tokens.
main :: IO ()
main = do args <- getArgs
          flags <- collectFlags args
          options <- applyFlags flags defaultOptions
          when (oToUnbuffer options) $ hSetBuffering stdout NoBuffering
          if oToHelp options
             then putStrLn usage
             else runWith options
