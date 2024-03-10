module Main where

import qualified Data.Text.IO as Text
import Compiler
import System.Environment
import Data.List(isSuffixOf)
import qualified Golike
import qualified Clike
import Parser


main = do
    args <- getArgs
    case args of
        [path] -> runDependingOnEnding path
        _ -> putStrLn "Run with a input path like this `cabal run exes -- examples/example.h`"

run parser inputPath outputPath = do
    content <- Text.readFile inputPath
    parsed <- either fail return (parser content)
    transformed <- runTransformations parsed
    Text.writeFile outputPath (toQbe transformed)
    putStrLn ("Written to " ++ outputPath)

runDependingOnEnding path =
    if isSuffixOf ".rs" path
        then run (parse Rslike.statements) path "out/rslike.ssa"
        else run (parse Clike.statements) path "out/clike.ssa"
