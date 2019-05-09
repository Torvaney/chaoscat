{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import qualified Control.Concurrent as CC
import qualified Control.Monad as M
import qualified Data.DateTime as DateTime
import qualified Data.Map as Map
import qualified Data.Vector as Vector
import qualified GitHub.Endpoints.PullRequests as GitHub
import qualified System.Random.MWC as Random
import qualified System.Random.MWC.Distributions as RandDist

import GitHub.Auth

import Lib


{--

type PullRequestID = Int

type PullRequestRate = Float

data PullRequest = PullRequest
    { id             :: PullRequestID  -- redundant?
    , createdAt      :: DateTime.DateTime    -- redundant?
    , secondsToClose :: Seconds
    }

--}

-- A PR can have a time set or "pending". We use this to delay RNG (and therefore limit the need for IO)
data TimeToMerge
    = Seconds Integer
    | Pending
    deriving (Show)


type State = Map.Map GitHub.IssueNumber TimeToMerge


delayMilliseconds ms = CC.threadDelay (ms * 1000)


getPullRequests :: GitHub.Name GitHub.Owner
                -> GitHub.Name GitHub.Repo
                -> IO (Vector.Vector GitHub.SimplePullRequest)
getPullRequests author repo = do
    prs <- GitHub.pullRequestsFor author repo
    case prs of
        Left a   -> return Vector.empty  -- yikes! fix this
        Right xs -> return xs


insertNewPRs :: Vector.Vector GitHub.SimplePullRequest -> State -> State
insertNewPRs prs state =
    foldl (\s pr -> Map.insertWith (flip const) pr Pending s) state issueNumbers
    where
        issueNumbers = fmap GitHub.simplePullRequestNumber prs


isExpired :: GitHub.IssueNumber -> TimeToMerge -> Bool
isExpired issueNumber (Seconds secs) = True
isExpired _ Pending = True


updatePullRequests :: DateTime.DateTime
                   -> Vector.Vector GitHub.SimplePullRequest
                   -> State
                   -> (State, State)
updatePullRequests now openPRs state =
    Map.partitionWithKey isExpired $  -- TODO actually work out if age > state
    insertNewPRs openPRs $
    Map.filterWithKey isOpen state
    where
        openIssueNumbers = fmap GitHub.simplePullRequestNumber openPRs
        isOpen issueNo _ = issueNo `elem` openIssueNumbers


initialisePullRequests :: State -> IO State
initialisePullRequests state =
    -- TODO replace this with actual RNG
    pure $
    Map.map (\case Pending   -> Seconds 3
                   Seconds s -> Seconds s) state


doChaos :: State -> GitHub.Name GitHub.Owner -> GitHub.Name GitHub.Repo -> IO ()
doChaos state author repo = do
    now <- DateTime.getCurrentTime
    prs <- getPullRequests author repo

    let (newState, toMerge) = updatePullRequests now prs state

    -- debugging/development jank
    print $ show now
    print $ show newState

    delayMilliseconds 5000

    -- TODO: do any merges at this point
    rng <- Random.create
    -- ...

    -- Initialise any unset PRs using RNG
    newState <- initialisePullRequests newState

    doChaos newState author repo



main :: IO ()
main = do
    -- config (TODO: get from environment)
    let author = "torvaney"
    let repo   = "example-pr-repo"
    -- auth <- ...
    -- minTime <- ..
    -- rate <- ...

    -- x <- GitHub.pullRequestsFor author repo

    -- doChaos Map.empty auth repo minTime rate
    doChaos Map.empty author repo
