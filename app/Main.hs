{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Control.Concurrent as CC
import qualified Control.Monad as M
import qualified Data.ByteString.Char8 as C
import qualified Data.DateTime as DateTime
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Vector as Vector
import qualified GitHub.Data
import qualified GitHub.Endpoints.PullRequests as GitHub
import qualified System.Environment as Env
import qualified System.Random.MWC as Random
import qualified System.Random.MWC.Distributions as RandDist

import GitHub.Auth


data Config = Config
    { owner   :: GitHub.Name GitHub.Owner
    , repo    :: GitHub.Name GitHub.Repo
    , auth    :: Auth
    , minSecs :: Double
    , rate    :: Double
    }
    deriving (Show)


-- A PR can have a time set or "pending". We use this to delay RNG and therefore
-- separate/isolate IO.
data MergeDate
    = MergeAt DateTime.DateTime
    | Pending DateTime.DateTime
    deriving (Show)


-- The program's state is simply a mapping of open Pull Requests to the time that
-- they should be merged
type State = Map.Map GitHub.IssueNumber MergeDate


delayMilliseconds ms = CC.threadDelay (ms * 1000)


hoursToSeconds hours = 60 * 60 * hours


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


-- Insert any new PRs into the app state with 'pending' status, and split out any
-- PRs which have "expired" (exceeded their merge-at time)
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


-- Fetch the open PRs from GitHub. On fail, do nothing(!)
getPullRequests :: Config -> IO (Vector.Vector GitHub.SimplePullRequest)
getPullRequests config = do
    prs <- GitHub.pullRequestsFor' (Just (auth config)) (owner config) (repo config)
    case prs of
        Left a   -> return Vector.empty  -- yikes! fix this?
        Right xs -> return xs


-- Set a merge time for a PR if it has "Pending" status. If it already has a
-- time set, do nothing
setMergeAt :: Random.GenIO -> Config -> MergeDate -> IO MergeDate
setMergeAt gen config (MergeAt d) = pure $ MergeAt d
setMergeAt gen config (Pending d) = do
    secs <- RandDist.exponential (1 / rate config) gen
    return (MergeAt (DateTime.addSeconds (round (minSecs config + secs)) d))


initialisePullRequests :: Random.GenIO -> Config -> State -> IO State
initialisePullRequests gen config state =
    sequence $ Map.map (setMergeAt gen config) state


mergePullRequest :: Config -> GitHub.IssueNumber -> IO (Either GitHub.Error GitHub.MergeResult)
mergePullRequest config id =
    GitHub.mergePullRequest
        (auth  config)
        (owner config)
        (repo  config)
        id
        (Just "Whoops - time's up!")


mergePullRequests :: Config -> State -> IO ()
mergePullRequests config state =
    mapM_ (mergePullRequest config) (Map.keys state)


doChaos :: Random.GenIO -> Config -> State -> IO ()
doChaos gen config state = do
    now <- DateTime.getCurrentTime
    prs <- getPullRequests config

    let (toMerge, newState) = updatePullRequests now prs state

    delayMilliseconds (60 * 1000)

    mergePullRequests config toMerge

    -- Initialise any new PRs using RNG
    newState <- initialisePullRequests gen config newState

    doChaos gen config newState


main :: IO ()
main = do
    owner    <- Env.getEnv "CHAOSCAT_OWNER"
    repo     <- Env.getEnv "CHAOSCAT_REPO"
    auth     <- Env.getEnv "CHAOSCAT_OATH"
    rate     <- Env.getEnv "CHAOSCAT_RATE"      -- Mean hours before closing (+ minHours)
    minHours <- Env.getEnv "CHAOSCAT_MINHOURS"  -- Min. hours before a PR can be closed

    let config = Config { owner   = GitHub.Data.mkOwnerName $ T.pack owner
                        , repo    = GitHub.Data.mkRepoName  $ T.pack repo
                        , auth    = OAuth (C.pack auth)
                        , minSecs = hoursToSeconds $ read minHours::Double
                        , rate    = hoursToSeconds $ read rate::Double
                        }

    gen <- Random.createSystemRandom

    doChaos gen config Map.empty
