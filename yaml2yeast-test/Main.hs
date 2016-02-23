-------------------------------------------------------------------------------
-- |
-- Module      :  yaml2yeast-test
-- Copyright   :  (c) Oren Ben-Kiki 2007
-- License     :  LGPL
-- 
-- Maintainer  :  oren@ben-kiki.org
-- Stability   :  alpha
-- Portability :  portable
--
-- Execute all test cases located in a specified list of directories. Usage
-- is:
-- @
--  yaml2yeast-test [directories...]
-- @
-- If no directories are given, @yaml2yeast-test@ looks for tests in the
-- current working directory (\"@.@\"). Note that @yaml2yeast-test@ does not
-- recurse into sub-directories.
--
-- Each tests consists of two files, with the names
-- \"/production/@.@/testcase/@.input@\" and
-- \"/production/@.@/testcase/@.output@\", where /production/ is the syntax
-- production to be tested and /testcase/ is an arbitrary name. The @.input@
-- file contains the YAML text fed to the parser and the @.output@ file
-- contains the expected output, which is either a set of YEAST tokens or the
-- expected parsing error message.
--
-- If the @.output@ file is missing, the test will automatically fail. If a
-- test fails, a @.error@ file is created. This makes it easy to set up new
-- tests, simply create the input files, run @yaml2yeast-test@, and rename the
-- @.error@ files to @.output@ files (after reviewing them for correctness, of
-- course).
--
-- Exit status is the number of failed tests (0 - success - if all tests pass).
-------------------------------------------------------------------------------
module Main (main) where

import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Trans
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.HashMap as Hash
import           System.Directory
import           System.Environment
import           System.Exit
import           System.IO
import           Test.HUnit
import           Text.Regex
import           Text.Yaml.Reference

-- | Map each tokenizer name to whether a test for it was seen.
type Seen = Hash.Map String Bool

-- | The type of the test runner program.
type TestsRunner = StateT Seen IO

-- | @reportMissing seen@ reports the productions (tokenizers) which were not
-- /seen/ and returns their number.
reportMissing :: Seen -> IO Int
reportMissing seen = do let list = Hash.toList seen
                        missing <- foldM reportTest 0 list
                        if missing > 0
                           then hPutStrLn stderr $ "Missing: " ++ (show missing)
                           else return ()
                        return missing
                        where reportTest count (name, wasSeen)
                                | wasSeen   = return count
                                | otherwise = do hPutStrLn stderr $ "No tests for " ++ name
                                                 return $ count + 1

-- | Different types of test files.
data TestType = Plain  -- ^ Production without arguments.
              | WithN  -- ^ Production requiring $n$ argument.
              | WithC  -- ^ Production requiring $c$ argument.
              | WithT  -- ^ Production requiring $t$ argument.
              | WithNC -- ^ Production requiring $n$ and $c$ arguments.
              | WithNT -- ^ Production requiring $n$ and $t$ arguments.
  deriving Eq

-- | @show testType@ converts a /testType/ to a human-friendly name for error
-- messages.
instance Show TestType where
  show testType =
      case testType of
           Plain  -> ""
           WithN  -> " n"
           WithC  -> " c"
           WithT  -> " t"
           WithNC -> " n c"
           WithNT -> " n t"

-- | @isTestInputFile file@ returns whether the specified /file/ is a test
-- input file (ends with \"@.input@\").
isTestInputFile :: FilePath -> TestsRunner Bool
isTestInputFile file = do isFile <- (lift . doesFileExist) file
                          if not isFile
                             then return False
                             else case matchRegex (mkRegex "\\.input$") file of
                                       Just _  -> return True
                                       Nothing -> return False

-- | @isWith parameter file@ returns whether the specified /file/ is for a production
-- that requires the specified /parameter/ (file name contains @.@/parameter/@=@).
isWith :: String -> FilePath -> IO Bool
isWith parameter file =
  case matchRegex (mkRegex $ "\\." ++ parameter ++ "=") file of
       Just _  -> return True
       Nothing -> return False

-- | @testType file@ deduces the type of test stored in the /file/.
testType :: FilePath -> IO TestType
testType file = do withN <- isWith "n" file
                   withC <- isWith "c" file
                   withT <- isWith "t" file
                   case (withN, withC, withT) of
                        (False, False, False) -> return Plain
                        (True,  False, False) -> return WithN
                        (False, True,  False) -> return WithC
                        (False, False, True)  -> return WithT
                        (True,  True,  False) -> return WithNC
                        (True,  False, True)  -> return WithNT
                        (_,     _,     _)     -> error $ file ++ ": unknown parameters combination"

-- | @testProduction file@ extracts the production name from a test input
-- /file/ name (file name starts with \"/pattern/@.@\").
testProduction :: FilePath -> String
testProduction file = subRegex (mkRegex "^.*/([0-9a-z+-]+)\\.[^/]*$") file "\\1"

-- | @testParameter parameter file@ extracts the /parameter/ value from a test
-- input /file/ name (file name contains \"@.@/parameter/@=@/value/@.@\"). Also
-- patch the @-@ characters in the @c@ parameter into @_@ to make it possible
-- for the built-in lexer to handle them.
testParameter :: (Read t) => String -> FilePath -> t
testParameter parameter file =
  read $ subRegex patchRegex (subRegex extractRegex file "\\1") "\\1_"
  where extractRegex = mkRegex $ "^.*\\." ++ parameter ++ "=([^.]+)\\.[^/]*$"
        patchRegex = mkRegex "([a-z])-"

-- | @testOutputFile file@ converts a test input /file/ name to test output
-- file name.
testOutputFile :: FilePath -> FilePath
testOutputFile file = subRegex (mkRegex "\\.input$") file ".output"

-- | @testErrorFile file@ converts a test input /file/ name to test error
-- file name.
testErrorFile :: FilePath -> FilePath
testErrorFile file = subRegex (mkRegex "\\.input$") file ".error"

-- | @embedVariables text inputFile@ embeds variables in the /text/ instead of
-- their expanded values; currently only /InputFile/ is embedded instead of the
-- input file name (we cheat by replacing whatever looks like one).
embedVariables :: String -> String
embedVariables text = subRegex (mkRegex "!.*: line ") text "!$InputFile$: line "

-- | @assertTest inputFile@ runs the parser on the input contained in the
-- /inputFile/ using the production extracted from the file name, asserting the
-- result is identical to the content of the matching output file.
assertTest :: FilePath -> Assertion
assertTest inputFile =
  do input <- C.readFile inputFile
     let outputFile = testOutputFile inputFile
     existsOutputFile <- doesFileExist outputFile
     expected <- if existsOutputFile
                    then readFile outputFile
                    else return "(missing file)"
     runType <- testType inputFile
     let result = case runType of
                       Plain  -> tokenizer (testProduction inputFile)
                       WithN  -> tokenizerWithN (testProduction inputFile)  (testParameter "n" inputFile)
                       WithC  -> tokenizerWithC (testProduction inputFile)  (testParameter "c" inputFile)
                       WithT  -> tokenizerWithT (testProduction inputFile)  (testParameter "t" inputFile)
                       WithNC -> tokenizerWithNC (testProduction inputFile) (testParameter "n" inputFile) (testParameter "c" inputFile)
                       WithNT -> tokenizerWithNT (testProduction inputFile) (testParameter "n" inputFile) (testParameter "t" inputFile)
     case result of
          Nothing -> assertFailure $ inputFile ++ ": unknown production" ++ (show runType) ++ ": " ++ (testProduction inputFile)
          Just resolved -> do let actual = embedVariables $ showTokens $ resolved inputFile input False
                              when (actual /= expected) $ writeFile (testErrorFile inputFile) actual
                              assertEqual inputFile expected actual

-- | @fileTest seen file@ wraps @assertTest@ in a test case named after the
-- /file/ and marks it as /seen/.
fileTest :: FilePath -> TestsRunner Test
fileTest file = do seen <- get
                   put $ Hash.adjust (const True) (testProduction file) seen
                   return $ TestLabel file $ TestCase $ assertTest file

-- | @directoryTestInputFiles directory@ returns the list of test input files
-- contained in the /directory/.
directoryTestInputFiles :: String -> TestsRunner [FilePath]
directoryTestInputFiles directory = do entries <- (lift . getDirectoryContents) directory
                                       filterM isTestInputFile $ map ((directory ++) . ("/" ++)) entries

-- | @directoryTests seen directory@ returns the list of test cases contained
-- in the /directory/, wrapped in a test case named after it, and updates the
-- /seen/ hash.
directoryTests :: String -> TestsRunner Test
directoryTests directory = do files <- directoryTestInputFiles directory
                              tests <- mapM fileTest files
                              return $ TestLabel directory $ TestList tests

-- | @allTests seen directories@ returns the list of test cases contained in
-- the /directories/ (or \"@.@\" if none is specified), wrapped in a test case
-- named @all@ if there is more than one directory, updating the /seen/ hash.
allTests :: [String] -> TestsRunner Test
allTests directories = do case directories of
                               [] -> directoryTests "."
                               [directory] -> directoryTests directory
                               _ -> do tests <- mapM directoryTests directories
                                       return $ TestLabel "all" $ TestList tests

-- | @main@ executes all the tests contained in the directories specified in
-- the command line (or \"@.@\" if none is specified).
main :: IO ()
main = do argDirectories <- getArgs
          let directories = if argDirectories == []
                            then [ "tests" ]
                            else argDirectories
          let notSeen = Hash.fromList $ zip tokenizerNames $ repeat False
          (tests, seen) <- runStateT (allTests directories) notSeen
          missing <- reportMissing seen
          results <- runTestTT tests
          case missing + (errors results) + (failures results) of
               0 -> exitWith ExitSuccess
               n -> exitWith $ ExitFailure n
