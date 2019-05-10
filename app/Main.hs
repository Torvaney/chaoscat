{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Control.Concurrent as CC
import qualified Control.Monad as M
-- import qualified Control.Monad.Primitive as Prim
import qualified Data.DateTime as DateTime
import qualified Data.Map as Map
import qualified Data.Vector as Vector
import qualified GitHub.Endpoints.PullRequests as GitHub
import qualified System.Random.MWC as Random
import qualified System.Random.MWC.Distributions as RandDist

import GitHub.Auth

import Lib


data Config = Config
    { owner   :: GitHub.Name GitHub.Owner
    , repo    :: GitHub.Name GitHub.Repo
    , auth    :: Auth
    , minTime :: Double
    , rate    :: Double
    }
    deriving (Show)


-- A PR can have a time set or "pending". We use this to delay RNG (and therefore limit the need for IO)
data MergeDate
    = MergeAt DateTime.DateTime
    | Pending DateTime.DateTime
    deriving (Show)


type State = Map.Map GitHub.IssueNumber MergeDate


delayMilliseconds ms = CC.threadDelay (ms * 1000)


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
isExpired now (MergeAt d) = d < now
isExpired _   (Pending _) = False


updatePullRequests :: DateTime.DateTime
                   -> Vector.Vector GitHub.SimplePullRequest
                   -> State
                   -> (State, State)
updatePullRequests now openPRs state =
    Map.partition (isExpired now) $
    insertNewPRs openPRs $
    Map.filterWithKey isOpen state
    where
        openIssueNumbers = fmap GitHub.simplePullRequestNumber openPRs
        isOpen issueNo _ = issueNo `elem` openIssueNumbers


getPullRequests :: GitHub.Name GitHub.Owner
                -> GitHub.Name GitHub.Repo
                -> IO (Vector.Vector GitHub.SimplePullRequest)
getPullRequests author repo = do
    prs <- GitHub.pullRequestsFor author repo
    case prs of
        Left a   -> return Vector.empty  -- yikes! fix this
        Right xs -> return xs


initialisePullRequest :: Random.GenIO -> MergeDate -> IO MergeDate
initialisePullRequest gen (MergeAt d) = pure $ MergeAt d
initialisePullRequest gen (Pending d) = do
    secs <- RandDist.exponential (1 / 86400) gen
    return (MergeAt (DateTime.addSeconds (round secs) d))


initialisePullRequests :: Random.GenIO -> State -> IO State
initialisePullRequests gen state =
    sequence $ Map.map (initialisePullRequest gen) state


mergePullRequests :: Config -> State -> IO ()
mergePullRequests config state =
    print "pass!"
    -- GitHub.mergePullRequest (auth config) (owner config)


doChaos :: Config -> State -> IO ()
doChaos config state = do
    now <- DateTime.getCurrentTime
    prs <- getPullRequests (owner config) (repo config)

    let (toMerge, newState) = updatePullRequests now prs state

    -- janky debugging/development cruft
    print $ show now
    print $ show toMerge
    print $ show newState

    delayMilliseconds 5000

    gen <- Random.create
    -- mergePullRequests now toMerge

    -- Initialise any unset PRs using RNG
    newState <- initialisePullRequests gen newState

    doChaos config newState


main :: IO ()
main = do
    -- config (TODO: get from environment)
    let owner = "torvaney"
    let repo   = "example-pr-repo"
    -- auth <- ...
    -- minTime <- ..
    -- rate <- ...

    let config = Config { owner   = owner
                        , repo    = repo
                        , auth    = OAuth "abc"
                        , minTime = 0
                        , rate    = 86400  -- 1 day in seconds
                        }

    -- x <- GitHub.pullRequestsFor author repo

    doChaos config Map.empty
