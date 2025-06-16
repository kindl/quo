module Main where

import qualified Data.Text.IO as Text
import Cgen
import System.Environment
import qualified Statements
import Expressions
import Resolver(runResolve)
import Specializer(specializeModule)
import Qbegen(moduleToQbe, prettyMod)
import Helpers(toText)


main = do
    args <- getArgs
    case args of
        [path] -> compile (parse Statements.moduleDefinition) path "out/Test"
        _ -> putStrLn "Run with a input path like this `cabal run exes -- examples/example.h`"

compile parser inputPath outputPath = do
    content <- Text.readFile inputPath
    parsed <- either fail return (parser content)
    specialized <- specializeModule parsed
    resolved <- runResolve specialized
    Text.writeFile (outputPath <> ".c") (toText (toC resolved))
    qbe <- moduleToQbe resolved
    Text.writeFile (outputPath <> ".qbe") (toText (prettyMod qbe))
    putStrLn ("Written to " ++ outputPath)
