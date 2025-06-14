module Main where

import qualified Data.Text.IO as Text
import Compiler
import System.Environment
import qualified Statements
import Expressions
import Drucker(toText)
import Resolver(runResolve)
import Specializer(specializeModule)


main = do
    args <- getArgs
    case args of
        [path] -> compile (parse Statements.moduleDefinition) path "out/Test.c"
        _ -> putStrLn "Run with a input path like this `cabal run exes -- examples/example.h`"

compile parser inputPath outputPath = do
    content <- Text.readFile inputPath
    parsed <- either fail return (parser content)
    specialized <- specializeModule parsed
    resolved <- runResolve specialized
    Text.writeFile outputPath (toText (toC resolved))
    putStrLn ("Written to " ++ outputPath)
