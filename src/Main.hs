-- Allow me to track / be reminded of things I want to do regularly 
-- (daily, weekly)

module Main where

import Control.Exception
import Control.Monad
import Data.List
import Data.Maybe
import Data.Time
import Data.Time.Clock.POSIX
import Database.HDBC
import Database.HDBC.PostgreSQL
import FUtil
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.IO
import Text.Printf
import qualified Data.Map as Map
import qualified Data.Set as Set

data Options = Options {
  optKillLast :: Bool,
  optListRecent :: Int,
  optActuallyDid :: Bool,
  optComment :: String,
  optUsername :: String,
  optGroupView :: Bool
  }

defaultOptions :: Options
defaultOptions = Options {
  optKillLast = False,
  optListRecent = 0,
  optActuallyDid = True,
  optComment = "",
  optUsername = "",
  optGroupView = False
  }

options :: [OptDescr (Options -> Options)]
options = [
  Option "l" ["list-recent"] 
    (OptArg (\ n o -> o {optListRecent = read $ fromMaybe "5" n}) "N")
    "list last N (default 5) records",
  Option "k" ["kill-last"] 
    (NoArg (\ o -> o {optKillLast = True}))
    "kill last record for the task",
  Option "n" ["did-not-do"] 
    (NoArg (\ o -> o {optActuallyDid = False}))
    "mark as not-actually-completed when silencing reminder",
  Option "c" ["comment"] 
    (ReqArg (\ c o -> o {optComment = c}) "COMMENT")
    "record comment along with silencing reminder",
  Option "g" ["group-view"] 
    (NoArg (\ o -> o {optGroupView = True}))
    "show group view of task list"
  ]

cPS :: IO Connection
cPS = handleSqlError $ connectPostgreSQL "dbname=me_log"

getTimeInt :: IO Integer
getTimeInt = fmap floor getPOSIXTime

recordTask :: String -> String -> Bool -> String -> IO ()
recordTask username taskName actuallyDid comment = do
  didTime <- getTimeInt
  conn <- cPS
  ret <- withTransaction conn (\ conn -> do
    run conn 
      "INSERT INTO task_log (task_name, username, actually_did, comment, \
      \did_time) VALUES (?, ?, ?, ?, ?)"
      [toSql taskName, toSql username, toSql actuallyDid, 
      toSql comment, toSql didTime]
    )
  disconnect conn

unrecordTask :: String -> String -> IO ()
unrecordTask username taskName = do
  timeMb <- getTaskRecentTime username taskName
  when (isJust timeMb) $ do
    conn <- cPS
    ret <- withTransaction conn (\ conn -> do
      run conn 
        "DELETE FROM task_log WHERE username = ? AND task_name = ? AND \
        \did_time = ?"
        [toSql username, toSql taskName, toSql $ fromJust timeMb]
      )
    disconnect conn

-- find all tasks that haven't been done in the current time block nor
-- too recently
getTodoTasks :: String -> Integer -> Integer -> IO (Set.Set String)
getTodoTasks username timeBlockSize tooRecentDelta = do
  curTime <- getTimeInt
  let
    timeBlockStart = curTime - curTime `mod` timeBlockSize
    -- things done after this time don't need to be repeated yet
    timeCutoff = min timeBlockStart (curTime - tooRecentDelta)
  conn <- cPS
  ret <- withTransaction conn (\ conn -> do
    quickQuery conn 
      -- grab things that _have_ been done
      "SELECT DISTINCT(task_name) FROM task_log WHERE username = ? AND \
      \did_time >= ?"
      [toSql username, toSql timeCutoff]
    )
  return $ Set.fromList $ map (fromSql . head) ret

getLastDone :: String -> Int -> IO [(String, UTCTime)]
getLastDone username n = do
  conn <- cPS
  ret <- withTransaction conn (\ conn -> do
    quickQuery conn 
      "SELECT task_name, did_time FROM task_log WHERE username = ? ORDER BY did_time DESC LIMIT ?"
      [toSql username, toSql n]
    )
  return $ map (\ x -> (fromSql (x !! 0), posixSecondsToUTCTime $
    fromIntegral (fromSql (x !! 1) :: Int))) ret

getTaskRecentTime :: String -> String -> IO (Maybe Integer)
getTaskRecentTime username taskName = do
  conn <- cPS
  ret <- withTransaction conn (\ conn -> do
    quickQuery conn
      "SELECT MAX(did_time) FROM task_log WHERE username = ? AND task_name = ?"
      [toSql username, toSql taskName]
    )
  return $ fromSql $ head $ head ret

type IntvlInfo = Map.Map String Int

rcName :: [Char]
rcName = ".rrrc"
dayTime :: Integer
dayTime = 24 * 60 * 60
intvlInfos :: Map.Map [Char] Integer
intvlInfos = Map.fromList [
  ("daily", dayTime),
  ("weekly", dayTime * 7),
  ("monthly", dayTime * 30),
  ("yearly", dayTime * 365)]

subLBreak :: Eq a => [a] -> [a] -> Maybe ([a], [a])
l `subLBreak` s =
  let
    (l1, sL2) = break (s `isPrefixOf`) $ tails l
  in if sL2 == [] then Nothing
    else Just (take (length l1) l, drop (length s) (head sL2))

subLBreakOrL :: Eq a => [a] -> [a] -> ([a], Maybe [a])
l `subLBreakOrL` s = case l `subLBreak` s of
  Nothing -> (l, Nothing)
  Just (l1, l2) -> (l1, Just l2)

-- interval-group to (name to (possible description))
type RcType = Map.Map String (Map.Map String (Maybe String))

parseRc :: [String] -> RcType
parseRc ls = snd $ foldr parseLine (Nothing, Map.empty) $ reverse ls where
  parseLine :: String -> (Maybe String, RcType) -> 
    (Maybe String, RcType)
  parseLine ('#':_) r = r
  parseLine "" r = r
  parseLine l@(l_s:l_r) (t, rc) = 
    let (l_m, [l_e]) = splitAt (length l_r - 1) l_r in
    if l_s == '<' && l_e == '>'
      then (Just l_m, rc)
      else (t, Map.insertWith Map.union (fromJust t) 
        (Map.singleton name mbyDesc) rc) where
          (name, mbyDesc) = l `subLBreakOrL` " - "

toDay :: Integer -> Maybe Integer -> [Char]
toDay nowTime Nothing = "never!"
toDay nowTime (Just x) = show $ (nowTime - x) `div` dayTime

divF :: Integer -> Integer -> Float
divF a b = (fromIntegral a) / (fromIntegral b)

mbyCompare :: (a -> a -> Ordering) -> Maybe a -> Maybe a -> Ordering
mbyCompare f Nothing Nothing = EQ
mbyCompare f Nothing y = LT
mbyCompare f x Nothing = GT
mbyCompare f (Just x) (Just y) = f x y

showMN :: (Text.Printf.PrintfArg t) => Maybe t -> [Char]
showMN Nothing = "!"
showMN (Just p) = printf "%.1f" p

showTime :: UTCTime -> DiffTime -> TimeOfDay
showTime (UTCTime utctDay utctDayTime) = timeToTimeOfDay

showRecent :: Options -> IO ()
showRecent opts = do
  dones <- getLastDone (optUsername opts) (optListRecent opts)
  tz <- getCurrentTimeZone
  putStr . unlines $ map (\ (task, time) -> 
    show (utcToLocalTime tz time) ++ "\t" ++ task) dones

showTasks :: Options -> IO ()
showTasks opts = do
  nowTime <- getTimeInt
  homeDir <- getHomeDirectory
  c <- openFile (homeDir ++ "/" ++ rcName) ReadMode >>= hGetContents
  let
    freqHeaderToSecs h = let
      (timesPerStr, intvlStr) = break (== '/') h
      (timesPer, intvlStr') =
        if null intvlStr then (1, h) else (read timesPerStr, tail intvlStr)
      in (fromJust $ Map.lookup intvlStr' intvlInfos) `div` timesPer
    rc = Map.mapWithKey (\h v -> (freqHeaderToSecs h, v)) $
      parseRc $ lines c
  intvlsHeadersItemssTimess <- mapM (\ (adverb, (intvl, allTasksMap)) -> do
      t <- getTodoTasks (optUsername opts) intvl (intvl `div` 2)
      let
        tasks = Map.toList $ foldr Map.delete allTasksMap (Set.toList t)
        (taskNames, _) = unzip tasks
        taskLines = map (\ (name, mbyDesc) -> name ++ (case mbyDesc of
              Nothing -> ""
              Just desc -> " - " ++ desc
            )
          ) tasks
      times <- mapM (getTaskRecentTime (optUsername opts)) taskNames
      return (intvl, adverb ++ " tasks:", taskLines, times)
    ) $ Map.toList rc
  if optGroupView opts
    then do
      let (_, headers, itemss, timess) = unzip4 intvlsHeadersItemssTimess
      putStrLn $ interlines $
        zipWith3 (\ h is ds -> interlines $ [h] ++ 
          (zipWith (\ a b -> a ++ "  " ++ b) is $
            map (toDay nowTime) ds))
        headers (spaceBlocks $ map (map ("- " ++)) itemss) timess
    else do
      let
        intvlsItemsTimes = concat $ map 
          (\ (iv, _, is, ts) -> map (\ (i, t) -> (iv, i, t)) $ zip is ts)
          intvlsHeadersItemssTimess
        pctsItems = map (\ (iv, i, t) -> 
          (liftM2 divF (liftM2 (-) (Just nowTime) t) (Just iv), i)) 
          intvlsItemsTimes
        pctsItemsOrd = sortBy (\ (x, _) (y, _) -> 
          mbyCompare (flip compare) x y) pctsItems
        (pcts, items) = unzip pctsItemsOrd
        pctsS = map showMN pcts
        ls = zipWith (\ x y -> x ++ "  " ++ y) (spaceBlock pctsS) items
      putStrLn $ interlines ls

main :: IO ()
main = do
  args <- getArgs
  let
    (optsPre, tasks, errs) = getOpt Permute options args
    opts = foldl (flip id) defaultOptions optsPre
    usage = "usage: ./rr [options] [task]"
    doErrs errs = error $ concat errs ++ usageInfo usage options
  unless (null errs) $ doErrs errs
  case tasks of
    [] -> if (optListRecent opts > 0) then showRecent opts else showTasks opts
    [task] -> if optKillLast opts
      then unrecordTask (optUsername opts) task
      else recordTask (optUsername opts) task 
        (optActuallyDid opts) (optComment opts)
    _ -> doErrs []
