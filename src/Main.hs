module Main where

import qualified Data.Text.IO as Text
import Compiler
import System.Environment
import qualified Clike
import Parser
import Drucker(toText)


main = do
    args <- getArgs
    case args of
        [path] -> compile (parse Clike.moduleDefinition) path "out/clike.ssa"
        _ -> putStrLn "Run with a input path like this `cabal run exes -- examples/example.h`"

compile parser inputPath outputPath = do
    content <- Text.readFile inputPath
    parsed <- either fail return (parser content)
    transformed <- runTransformations parsed
    --Text.writeFile outputPath (toText (toQbe transformed))
    Text.writeFile "out/Test.cs" (toText (toCs parsed))
    putStrLn ("Written to " ++ outputPath)
