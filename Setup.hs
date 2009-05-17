import Distribution.Simple
import System.Cmd
main = defaultMainWithHooks $ simpleUserHooks { runTests = run, preClean = clean }
    where run _ _ _ _ = do
            system "dist/build/yaml2yeast-test/yaml2yeast-test tests"
            return ()
          clean _ _ = do
            system "rm -f tests/*.error"
            return (Nothing, [])
