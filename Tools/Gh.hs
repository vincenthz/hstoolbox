module Main where

import Data.List
import System.Environment
import Github.Issues
import Github.Repos
import Text.Printf

handleError :: String -> [String] -> Either Error a -> IO a
handleError action args (Left err) =
    error (action ++ ": " ++ intercalate " : " args ++ " : " ++ show err)
handleError action args (Right a) = return a

listIssues :: String -> String -> IO ()
listIssues owner repo = do
    issues <- handleError "listing issues" [owner,repo] =<< issuesForRepo owner repo [Open]
    mapM_ display issues
  where display i =
            printf "#%d: %s [%s]\n" (issueNumber i) (issueTitle i)
                (intercalate ", " $ map labelName $ issueLabels i)

listRepos :: String -> IO ()
listRepos owner = do
    repos <- handleError "listing repos" [owner] =<< userRepos owner All
    mapM_ display repos
  where display r =
            printf "%.20s [%d ★ ,%d ⚠ ]\n"
                   (repoName r)  (maybe 0 id $ repoWatchers r) (maybe 0 id $ repoOpenIssues r)

main = do
    args <- getArgs
    case args of
        owner:[]      -> listRepos owner
        owner:repo:[] -> listIssues owner repo
        _             -> error "usage: gh <owner> [repo]"
