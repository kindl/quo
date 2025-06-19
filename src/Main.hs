module Main where

import qualified Data.Text.IO as Text
import Cgen(prettyC)
import System.Environment(getArgs)
import qualified Statements
import Expressions(parse)
import Resolver(runResolve)
import Specializer(specializeModule)
import Qbegen(moduleToQbe, prettyMod)
import Helpers(toText)
import System.FilePath(takeBaseName)
import System.Process(readProcess)


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
    let cOut = outputBase <> ".c"
    Text.writeFile cOut (toText (prettyC resolved))
    putStrLn ("Wrote " ++ cOut)
    qbe <- moduleToQbe resolved
    let qbeOut = outputBase <> ".qbe"
    Text.writeFile qbeOut (toText (prettyMod qbe))
    putStrLn ("Wrote " ++ qbeOut)
    let asmOut = outputBase <> ".s"
    qbeProcess <- readProcess "qbe" [qbeOut, "-o", asmOut] ""
    putStrLn qbeProcess
    putStrLn ("Wrote " ++ asmOut)
    let objOut = outputBase <> ".out"
    ccProcess <- readProcess "cc" [asmOut, "-o", objOut] ""
    putStrLn ccProcess
    putStrLn ("Wrote " ++ objOut)
    putStrLn "Done"
