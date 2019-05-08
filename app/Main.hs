{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Control.Concurrent as CC
import qualified Control.Monad as M
import qualified Data.DateTime as DT
import qualified Data.Map as Map
import qualified Data.Vector as Vector

import qualified GitHub.Endpoints.PullRequests as GitHub
import GitHub.Auth

import Lib


{--

type PullRequestID = Int

type PullRequestRate = Float

type Seconds = Integer  -- Maximum age of that PR in seconds

data PullRequest = PullRequest
    { id             :: PullRequestID  -- redundant?
    , createdAt      :: DT.DateTime    -- redundant?
    , secondsToClose :: Seconds
    }

--}


type State = Map.Map GitHub.IssueNumber Integer


delayMilliseconds ms = CC.threadDelay (ms * 1000)


getPullRequests :: GitHub.Name GitHub.Owner -> GitHub.Name GitHub.Repo -> IO (Vector.Vector GitHub.SimplePullRequest)
getPullRequests author repo = do
    prs <- GitHub.pullRequestsFor author repo
    case prs of
        Left a   -> return Vector.empty  -- yikes - fix this
        Right xs -> return xs


insertNewPRs :: Vector.Vector GitHub.SimplePullRequest -> State -> State
insertNewPRs prs state =
    foldl (\s pr -> Map.insertWith (flip const) pr 5 s) state issueNumbers
    where
        issueNumbers = fmap GitHub.simplePullRequestNumber prs


updatePullRequests :: DT.DateTime -> Vector.Vector GitHub.SimplePullRequest -> State -> (State, State)
updatePullRequests now openPRs state =
    Map.partition (const True) $
    insertNewPRs openPRs $
    Map.filterWithKey isOpen state
    where
        openIssueNumbers = fmap GitHub.simplePullRequestNumber openPRs
        isOpen issueNo _ = issueNo `elem` openIssueNumbers


doChaos :: State -> GitHub.Name GitHub.Owner -> GitHub.Name GitHub.Repo -> IO ()
doChaos state author repo = do
    delayMilliseconds 5000

    now <- DT.getCurrentTime
    prs <- getPullRequests author repo

    let (newState, toMerge) = updatePullRequests now prs state

    -- do any merges
    print $ show now
    print $ show state
    print $ show $ fmap GitHub.simplePullRequestNumber prs

    doChaos newState author repo



main :: IO ()
main = do
    -- config
    let author = "torvaney"
    let repo   = "example-pr-repo"
    -- auth <- ...
    -- minTime <- ..
    -- rate <- ...

    -- x <- GitHub.pullRequestsFor author repo

    -- doChaos Map.empty auth repo minTime rate
    doChaos Map.empty author repo
