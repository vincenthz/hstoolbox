module Main where

import Control.Applicative
import Control.Monad
import System.Environment
import System.Process
import Data.Function
import Data.Word
import Data.List (sortBy)
import Numeric

import qualified Data.Map as M

data R = R
    { ty   :: String
    , addr :: Word64
    , sym  :: String
    , size :: Word64
    }

parse _    table []     = table -- ignore last symbol. don't know the size
parse prev table (l:ls) =
    case words l of
        [addrS,ty,sym] ->
            let addr = fst $ head $ readHex addrS
             in case prev of
                Nothing            ->
                    parse (Just (ty, addr, sym)) table ls
                Just (prevTy,startAddr,prevSym) ->
                    let nTable = M.insert prevSym (R ty startAddr prevSym (addr - startAddr)) table
                     in parse (Just (ty, addr, sym)) nTable ls
        [ty,sym]      -> parse prev table ls
        _             -> parse prev table ls

run :: FilePath -> Int -> IO ()
run file nb = do
    nmContent <- parse Nothing M.empty . lines <$> readProcess "nm" ["-n", file] "" 
    let biggest = take nb $ reverse $ sortBy (compare `on` size) $ map snd $ M.toList nmContent
    forM_ biggest $ \r -> do
        putStrLn (sym r ++ ": " ++ show (size r))

main = do
    args      <- getArgs
    case args of
        file:nb:_ -> run file (read nb)
        [file]    -> run file 20
        []        -> error "cmd <exe-file> [nb-items]"
