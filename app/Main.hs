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


-- A PR can have a time set or "pending". We use this to delay RNG (and therefore limit the need for IO)
data MergeDate
    = MergeAt DateTime.DateTime
    | Pending DateTime.DateTime
    deriving (Show)


type State = Map.Map GitHub.IssueNumber MergeDate


delayMilliseconds ms = CC.threadDelay (ms * 1000)


getPullRequests :: GitHub.Name GitHub.Owner
                -> GitHub.Name GitHub.Repo
                -> IO (Vector.Vector GitHub.SimplePullRequest)
getPullRequests author repo = do
    prs <- GitHub.pullRequestsFor author repo
    case prs of
        Left a   -> return Vector.empty  -- yikes! fix this
        Right xs -> return xs


-- TODO: swap args and use foldr? Is that more idiomatic?
insertPR :: State -> GitHub.SimplePullRequest -> State
insertPR state pr =
    Map.insertWith (flip const) issueNumber (Pending createdAt) state
    where
        createdAt   = GitHub.simplePullRequestCreatedAt pr
        issueNumber = GitHub.simplePullRequestNumber pr


insertNewPRs :: Vector.Vector GitHub.SimplePullRequest -> State -> State
insertNewPRs prs state =
    foldl insertPR state prs


isExpired :: DateTime.DateTime -> MergeDate -> Bool
isExpired now (MergeAt d) = now > d
isExpired _   (Pending _) = False


updatePullRequests :: DateTime.DateTime
                   -> Vector.Vector GitHub.SimplePullRequest
                   -> State
                   -> (State, State)
updatePullRequests now openPRs state =
    Map.partition (isExpired now) $  -- TODO actually work out if age > state
    insertNewPRs openPRs $
    Map.filterWithKey isOpen state
    where
        openIssueNumbers = fmap GitHub.simplePullRequestNumber openPRs
        isOpen issueNo _ = issueNo `elem` openIssueNumbers


initialisePullRequests :: State -> IO State
initialisePullRequests state =
    -- TODO replace this with actual RNG for the number of seconds
    pure $
    Map.map
        (\case Pending d -> MergeAt (DateTime.addSeconds 95 d)
               MergeAt d -> MergeAt d)
        state


doChaos :: State -> GitHub.Name GitHub.Owner -> GitHub.Name GitHub.Repo -> IO ()
doChaos state author repo = do
    now <- DateTime.getCurrentTime
    prs <- getPullRequests author repo

    let (toMerge, newState) = updatePullRequests now prs state

    -- janky debugging/development cruft
    print $ show now
    print $ show toMerge
    print $ show newState

    delayMilliseconds 5000

    -- TODO: do any merges at this point
    rng <- Random.create
    -- mergePullRequests now toMerge

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
