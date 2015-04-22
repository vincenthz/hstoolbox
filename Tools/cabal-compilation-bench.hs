module Main where

import Control.Applicative
import Control.Monad (void)
import Data.Hourglass
import System.Hourglass
import System.Process
import System.Environment
import Data.Ratio
import Text.Read

type X = Double

toX (Seconds s, NanoSeconds ns) =
    let allNS = fromIntegral s * nsToS + fromIntegral ns :: Integer
     in fromRational (allNS % nsToS)
  where
    nsToS :: Integer
    nsToS = 1000000000

bench :: IO () -> IO X
bench f = do
    e1 <- timeCurrentP
    f
    e2 <- timeCurrentP
    return $ toX (e2 `timeDiffP` e1)

run :: Int -> IO () -> IO () -> IO () -> IO () -> IO [X]
run nTimes pre post between act = do
    pre
    r <- loop $ replicate nTimes act
    post
    return r
  where
        loop :: [IO ()] -> IO [X]
        loop []     = return []
        loop (x:[]) = (:[]) <$> bench x
        loop (x:xs) = do
            r <- bench x
            between
            rs <- loop xs
            return (r:rs)

cabalConfigure args = void $ rawSystem "cabal" (["configure"] ++ args)
cabalBuild          = void $ rawSystem "cabal" ["build"]
cabalClean          = void $ rawSystem "cabal" ["clean"]

testCompile n = do
    r <- run n (cabalClean >> cabalConfigure [])
               (cabalClean >> cabalConfigure [])
               (cabalClean)
               (cabalBuild)
    mapM_ (\(i, v) -> putStrLn ("run " ++ show i ++ " : " ++ show v)) (zip [1..] r)
    putStrLn ("average: " ++ show (sum r / fromIntegral n))

main = do
    a <- getArgs
    case a of
        [] -> testCompile 5
        [n] -> case readMaybe n of
                    Just nVal -> testCompile nVal
                    Nothing   -> error "argument is not an integer"
