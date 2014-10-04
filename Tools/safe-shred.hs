{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import           Data.List
import           Data.FileFormat
import           SafeShred.Traverse
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO
import           System.Process
import           "cryptonite" Crypto.Hash
--import "cryptohash" Crypto.Hash
--

data Configuration = Configuration
    { shredDestination    :: FilePath
    , shredDestinationLog :: FilePath
    , badExtensions       :: [String]
    } deriving (Show,Eq)

digestFile :: FilePath -> IO (Digest SHA256)
digestFile f =
    hashlazy <$> L.readFile f :: IO (Digest SHA256)

inDirectory :: FilePath -> String -> FilePath -> [String] -> IO ()
inDirectory dir cmdName cmd cmdArgs = do
    saved <- getCurrentDirectory
    setCurrentDirectory dir
    putStrLn ("===> " ++ cmdName)
    exitCode <- rawSystem cmd cmdArgs
    if exitCode /= ExitSuccess
        then error ("cmd: " ++ cmd ++ " " ++ show cmdArgs ++ " failed with code: " ++ show exitCode)
        else setCurrentDirectory saved >> return ()

logNew cfg dir = do
    let logDir = shredDestinationLog cfg
        baseFile = dirToLogName dir
    exist <- doesFileExist (logDir </> baseFile)
    logFile <- if exist
                    then findFree logDir baseFile 1
                    else return (logDir </> baseFile)
    log <- openFile logFile WriteMode
    return log
  where dirToLogName [] = []
        dirToLogName (x:xs)
            | x == '/'  = '#' : dirToLogName xs
            | otherwise = x   : dirToLogName xs
        findFree dir f i = do
            let file = dir </> (f ++ "." ++ show i)
            e <- doesFileExist file
            if e
                then findFree dir f (i+1)
                else return file


logAppend log file digest = do
    hPutStrLn log (file ++ " == " ++ show digest)
    return ()

logFlush cfg log dir = do
    hClose log
    return ()

shred cfg log file = do
    digest <- digestFile file
    -- add the file somewhere
    let dest = shredDestination cfg
        (digestDir,digestFile) = splitAt 2 $ show digest
    let destDir  = dest </> digestDir
        destFile = destDir </> digestFile
    createDirectoryIfMissing True destDir
    alreadyExist <- doesFileExist destFile
    unless alreadyExist $ do
        putStrLn ("adding: " ++ file ++ " at : " ++ destFile)
        copyFile file destFile
        logAppend log file digest

safeShred cfg dir = do
    log <- logNew cfg dir
    dirTraverse_ dir (fileCallback log) (dirCallback log)
    logFlush cfg log dir
  where fileCallback log f
            | isRubbishByName cfg f = return ()
            | otherwise = do
                isRubbish <- isContentRubbish cfg f
                unless isRubbish $ shred cfg log f
                return ()
        dirCallback log dirPath
            | isSuffixOf ".git" dirPath = do
                let outName = takeFileName dirPath ++ ".tar.gz"
                inDirectory (takeDirectory dirPath) ("taring git repo " ++ (takeFileName dirPath))
                        "tar" ["cvzf", outName, takeFileName dirPath]
                shred cfg log (dirPath ++ ".tar.gz")
                return False
            | otherwise = do
                return True

        isRubbishByName cfg f
            | hasExt    = (drop 1 ext) `elem` badExtensions cfg
            | otherwise = False
          where ext    = takeExtension f
                hasExt = not (null ext) && head ext == '.'
        isContentRubbish cfg f = do
            sz <- withFile f ReadMode hFileSize
            if sz > 500000
                then do format <- getFileformat f
                        if format `elem` [FT_ELF,FT_AR,FT_MACH_O]
                            then return True
                            else return False
                else return False

main = do
    home <- getHomeDirectory
    let shredDestination = home </> ".shred"
        shredDestinationLog = shredDestination </> "log"
    
    createDirectoryIfMissing True shredDestination
    createDirectoryIfMissing True shredDestinationLog

    args <- getArgs
    -- FIXME do arguments parsing here
    let dirs = args
    let cfg = Configuration shredDestination shredDestinationLog ["o","dyn_o","hi","so","dylib","a","dyn_hi"]
    mapM_ (safeShred cfg) dirs
