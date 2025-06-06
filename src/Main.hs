module Main where

import qualified Data.Text.IO as Text
import Compiler
import System.Environment
import qualified Statements
import Expressions
import Drucker(toText)
import Resolver(runResolve)


main = do
    args <- getArgs
    case args of
        [path] -> compile (parse Statements.moduleDefinition) path "out/clike.ssa"
        _ -> putStrLn "Run with a input path like this `cabal run exes -- examples/example.h`"

compile parser inputPath outputPath = do
    content <- Text.readFile inputPath
    parsed <- either fail return (parser content)
    resolved <- runResolve parsed
    --Text.writeFile outputPath (toText (toQbe transformed))
    Text.writeFile "out/Test.cs" (toText (toCs parsed))
    Text.writeFile "out/TestResolved.cs" (toText (toCs resolved))
    putStrLn ("Written to " ++ outputPath)
