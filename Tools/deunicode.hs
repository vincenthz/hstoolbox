module Main where

import SafeShred.Traverse
import System.Directory
import System.FilePath
import System.Environment
import Data.List

main = do
    args <- getArgs
    case args of
        []                -> error "usage: deunicode [--dummy] <directory> ..."
        ("--dummy":files) -> mapM_ (\p -> dirTraverse_ p (fileCallback True) dirCallback) args
        _                 -> mapM_ (\p -> dirTraverse_ p (fileCallback False) dirCallback) args
  where dirCallback _ = return True
        fileCallback dummy file = do
            if takeExtension file == ".hs"
                then deUnicode dummy file
                else return ()

        deUnicode dummy file = do
            content <- readFile file
            let totalChars   = length content
                unicodeChars = howManyUnicodes content
                asciiContent = toAscii content
            if dummy
                then putStrLn ("  " ++ file ++ " : "  ++ show unicodeChars ++ " (total: " ++ show totalChars ++ ") after: " ++ show (howManyUnicodes asciiContent) ++ " " ++ getUnicodes asciiContent)
                else do
                    putStrLn ("  " ++ file ++ " : "  ++ show unicodeChars ++ " (total: " ++ show totalChars ++ ")")
                    writeFile (file ++ ".tmp") asciiContent
                    renameFile (file ++ ".tmp") file

        howManyUnicodes d = length $ filter (\c -> fromEnum c > 127) d
        getUnicodes d = sort $ filter (\c -> fromEnum c > 127) d

        toAscii [] = []
        toAscii (x:xs) = 
            let z = case x of
                        '→' -> "->"
                        '⇒' -> "=>"
                        '←' -> "<-"
                        '∷' -> "::"
                        'α' -> "a"
                        'β' -> "b"
                        '∘' -> "."
                        '≡' -> "=="
                        '≠'  -> "/="
                        '⊕' -> "`mappend`" -- not the same precedence, so expect to have to fix by hands in some place
                        '≤' -> "<="
                        '∀' -> "forall"
                        '©' -> "(C)"
                        'μ' -> "u"
                        'ν' -> "v"
                        'π' -> "pi"
                        _   -> [x]
             in z ++ toAscii xs
    
