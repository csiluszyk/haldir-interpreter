import System.IO (stdin, stderr, hPutStrLn,
                  hGetContents, withFile, IOMode(ReadMode))
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)

import LexHaldir
import ParHaldir
import AbsHaldir
import TErrorHaldir
import IntHaldir

import ErrM

runInterpreter progH = do
    progStr <- hGetContents progH
    case pProgram (tokens progStr) of
        Bad s -> do
            hPutStrLn stderr "Parse failed..."
            hPutStrLn stderr s
            exitFailure
        Ok prog -> do
            case checkTypeErrors prog of
                Just error -> do
                    hPutStrLn stderr error
                    exitFailure
                Nothing -> do
                    case interpret prog of
                        Left error -> do
                            hPutStrLn stderr error
                            exitFailure
                        Right result -> do
                            -- result :: [Value]
                            mapM_ print result
                            exitSuccess

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> runInterpreter stdin
        [filename] -> withFile filename ReadMode runInterpreter
        _ -> usage

usage = getProgName >>= (\n -> putStrLn $ "Usage: " ++ n ++ " [filename]")
