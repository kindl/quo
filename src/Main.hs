module Main where

import qualified Data.Text.IO as Text
import Cgen
import System.Environment
import qualified Statements
import Expressions(parse)
import Resolver(runResolve)
import Specializer(specializeModule)
import Qbegen(moduleToQbe, prettyMod)
import Helpers(toText)
import System.FilePath(takeBaseName)


main = do
    args <- getArgs
    case args of
        [path] -> compile path "out"
        _ -> putStrLn "Run with a input path like this `cabal run exes -- examples/example.h`"

compile inputPath outputPath = do
    content <- Text.readFile inputPath
    parsed <- either fail return (parse Statements.moduleDefinition content)
    -- Turn generic definitions into concrete definitions
    specialized <- specializeModule parsed
    -- Annotate variables with types
    resolved <- runResolve specialized
    let outputBase = outputPath <> "/" <> takeBaseName inputPath
    Text.writeFile (outputBase <> ".c") (toText (toC resolved))
    putStrLn ("Wrote " ++ outputBase ++ ".c")
    qbe <- moduleToQbe resolved
    Text.writeFile (outputBase <> ".qbe") (toText (prettyMod qbe))
    putStrLn ("Wrote " ++ outputBase ++ ".qbe")
