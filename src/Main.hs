module Main where

import qualified Data.Text.IO as Text
import Cgen(prettyC)
import qualified Statements
import Expressions(parse)
import Resolver(runResolve)
import Specializer(specializeModule)
import Qbegen(moduleToQbe, prettyMod)
import Helpers(toText)
import System.FilePath(takeBaseName)
import System.Process(readProcess)
import System.Directory(createDirectoryIfMissing)
import Options.Applicative


data Options = Options {
    getAssemblerOptions :: String,
    getInputPath :: FilePath,
    getOutputFolder :: FilePath
}

optionsParser :: ParserInfo Options
optionsParser = info (helper <*> commands) mempty

commands :: Parser Options
commands =
    hsubparser (command "compile" (info compileCommand (progDesc "Compile a quo file")))

assemblerOptions :: Parser String
assemblerOptions =
    strOption (
        long "assembler-options"
        <> help "Forward command line options to the assembler"
        <> value ""
        <> metavar "OPTIONS")

compileCommand :: Parser Options
compileCommand =
    Options
        <$> assemblerOptions
        <*> argument str (metavar "FILEPATH")
        <*> pure "out"

main :: IO ()
main = do
    options <- execParser optionsParser
    compile
        (getAssemblerOptions options)
        (getInputPath options)
        (getOutputFolder options)

compile :: String -> FilePath -> FilePath -> IO ()
compile asmOpts inputPath outputFolder = do
    content <- Text.readFile inputPath
    parsed <- either fail return (parse inputPath Statements.moduleDefinition content)
    -- Turn generic definitions into concrete definitions
    specialized <- specializeModule parsed
    -- Annotate variables with types
    resolved <- runResolve specialized
    createDirectoryIfMissing True outputFolder
    let outputBase = outputFolder <> "/" <> takeBaseName inputPath
    let cOut = outputBase <> ".c"
    Text.writeFile cOut (toText (prettyC resolved))
    putStrLn ("Wrote " ++ cOut)
    qbe <- moduleToQbe resolved
    let qbeOut = outputBase <> ".qbe"
    Text.writeFile qbeOut (toText (prettyMod qbe))
    putStrLn ("Wrote " ++ qbeOut)
    let asmOut = outputBase <> ".s"
    qbeProcess <- readProcess "qbe" ["-o", asmOut, qbeOut] ""
    putStrLn qbeProcess
    putStrLn ("Wrote " ++ asmOut)
    let objOut = outputBase <> ".out"
    let ccOptions = (if asmOpts == "" then [] else [asmOpts]) <> ["-o", objOut, asmOut]
    ccProcess <- readProcess "cc" ccOptions ""
    putStrLn ccProcess
    putStrLn ("Wrote " ++ objOut)
    putStrLn "Done"
