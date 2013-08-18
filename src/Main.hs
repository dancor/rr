-- Allow me to track / be reminded of things I want to do regularly
-- (daily, weekly)

module Main where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Ratio
import qualified Data.Set as Set
import Data.Time
import Data.Time.Clock.POSIX
import Database.HDBC
import Database.HDBC.PostgreSQL
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import System.Process
import Text.Printf

import Opt (Opts)
import qualified Opt
import Util

cPS :: IO Connection
cPS = handleSqlError $ connectPostgreSQL "dbname=me_log"

getTimeInt :: IO Integer
getTimeInt = fmap floor getPOSIXTime

recordTask :: Opts -> String -> IO ()
recordTask opts taskName = do
  didTime <- (subtract . round $ Opt.hoursAgo opts * 3600) <$> getTimeInt
  conn <- cPS
  ret <- withTransaction conn (\ c -> do
    run c
      "INSERT INTO task_log (task_name, username, actually_did, comment, \
      \did_time) VALUES (?, ?, ?, ?, ?)"
      [toSql taskName, toSql $ Opt.username opts,
      toSql . not $ Opt.didNotDo opts,
      toSql $ Opt.comment opts, toSql didTime]
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

-- find all tasks that have been done recently
getDoneTasks :: String -> Integer -> IO (Set.Set String)
getDoneTasks username recentDelta = do
  curTime <- getTimeInt
  conn <- cPS
  ret <- withTransaction conn (\ conn -> do
    quickQuery conn
      -- grab things that _have_ been done
      "SELECT DISTINCT(task_name) FROM task_log WHERE username = ? AND \
      \did_time >= ?"
      [toSql username, toSql $ curTime - recentDelta]
    )
  return . Set.fromList $ map (fromSql . head) ret

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

progDir :: FilePath
progDir = ".rr"

rcName :: FilePath
rcName = "rc"

dayTime :: Integer
dayTime = 24 * 60 * 60

intvlInfos :: Map.Map [Char] Integer
intvlInfos = Map.fromList [
  ("daily", dayTime),
  ("weekly", dayTime * 7),
  ("monthly", dayTime * 30),
  ("yearly", dayTime * 365)]

-- FIXME: surely have something in FUtil?

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
  parseLine ('0':_) r = r
  parseLine "" r = r
  parseLine l@(l_s:l_r) (t, rc) =
    let (l_m, [l_e]) = splitAt (length l_r - 1) l_r in
    if l_s == '<' && l_e == '>'
      then (Just l_m, rc)
      else (t, Map.insertWith Map.union (fromJust t)
        (Map.singleton name mbyDesc) rc) where
          (_, Just l2) = l `subLBreakOrL` " - "
          (name, mbyDesc) = l2 `subLBreakOrL` " - "

toDayDiffStr :: Integer -> Maybe Integer -> [Char]
toDayDiffStr nowTime Nothing = "never!"
toDayDiffStr nowTime (Just x) = printf "%.1f"
  ((realToFrac (nowTime - x)) / (realToFrac dayTime) :: Float)

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

showRecent :: Opts -> IO ()
showRecent opts = do
  dones <- getLastDone (Opt.username opts) (Opt.listRecent opts)
  tz <- getCurrentTimeZone
  putStr . unlines $ map (\ (task, time) ->
    show (utcToLocalTime tz time) ++ "\t" ++ task) dones

rrrcTaskTree :: IO [([Char], (Integer, Map.Map String (Maybe String)))]
rrrcTaskTree = do
  homeDir <- getHomeDirectory
  c <- readFile (homeDir </> progDir </> rcName)
  let
    freqHeaderToSecs h = let
      (timesPerStr, intvlStr) = break (== '/') h
      (timesPer, intvlStr') =
        if null intvlStr then (1, h) else (read timesPerStr, tail intvlStr)
      in round $
         fromIntegral (fromJust $ Map.lookup intvlStr' intvlInfos) /
         timesPer
  return . Map.toList .
    Map.mapWithKey (\ h v -> (freqHeaderToSecs h, v)) . parseRc $ lines c

rrrcTasks :: IO (Map.Map String (Maybe String))
rrrcTasks = do
  rc <- rrrcTaskTree
  return . Map.unions $ map (\ (_, (_, allTasksMap)) -> allTasksMap) rc

showTasks :: Opts -> IO ()
showTasks opts = if Opt.quiet opts then return () else do
  nowTime <- getTimeInt
  homeDir <- getHomeDirectory
  c <- openFile (homeDir </> progDir </> rcName) ReadMode >>= hGetContents
  rc <- rrrcTaskTree
  intvlsHeadersItemssTimess <- mapM (\ (adverb, (intvl, allTasksMap)) -> do
    t <- getDoneTasks (Opt.username opts) . floor $
      Opt.intvlFracToShow opts * fromIntegral intvl
    let
      tasks = Map.toList . foldr Map.delete allTasksMap $ Set.toList t
      (taskNames, _) = unzip tasks
      taskLines = map (\ (name, mbyDesc) -> name ++ (case mbyDesc of
            Nothing -> ""
            Just desc -> " - " ++ desc
          )
        ) tasks
    times <- mapM (getTaskRecentTime $ Opt.username opts) taskNames
    return (intvl, adverb ++ " tasks:", taskLines, times)
    ) rc
  if Opt.groupView opts
    then do
      let (_, headers, itemss, timess) = unzip4 intvlsHeadersItemssTimess
      putStrLn $ intercalate "\n" $
        zipWith3 (\ h is ds -> intercalate "\n" $ [h] ++
          (zipWith (\ a b -> a ++ "  " ++ b) is $
            map (toDayDiffStr nowTime) ds))
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
      putStrLn $ intercalate "\n" ls

lookupPrefix :: (Ord a1) => [a1] -> Map.Map [a1] a -> [([a1], a)]
lookupPrefix k m = case Map.lookup k m of
  Just v -> [(k, v)]
  Nothing -> filter ((k `isPrefixOf`) . fst) $ Map.assocs m

doTask :: Opts -> String -> IO ()
doTask opts task = if Opt.killLast opts
  then unrecordTask (Opt.username opts) task
  else do
    rc <- rrrcTasks
    case lookupPrefix task rc of
      [(taskFull, desc)] -> do
        when (Opt.run opts && not (Opt.didNotDo opts)) $
          case desc of
            Just ('-':' ':cmd) -> system cmd >> return ()
            _ -> return ()
        recordTask opts taskFull
        case Opt.fwdServ opts of
          Just host -> do
            waitForProcess =<<
              runProcess "ssh" (["-t", host, "rr", "-q", taskFull, "-a",
                show . realToDouble $ Opt.hoursAgo opts] ++
                if Opt.run opts then [] else ["-m"])
              Nothing Nothing Nothing Nothing Nothing
            return ()
          Nothing -> return ()
      [] -> doErrs ["task is not in your ~/.rr/rc: " ++ task ++ "\n"]
      taskDescs -> doErrs ["task prefix is ambiguous: " ++ task ++ ": " ++
        intercalate " " (map fst taskDescs) ++ "\n"]

realToDouble :: (Real a) => a -> Double
realToDouble = realToFrac

usage :: String
usage = "usage: rr [options] [task]\n" ++ Opt.optInfo

doErrs :: [String] -> a
doErrs errs = error $ concat errs ++ usage

main :: IO ()
main = do
  homeDir <- getHomeDirectory
  (opts, tasks) <- Opt.getOpts (homeDir </> ".rr" </> "config") usage
  args <- getArgs
  case tasks of
    [] -> case Opt.fwdServ opts of
      Nothing ->
        if Opt.listRecent opts > 0 then showRecent opts else showTasks opts
      Just host -> do
        let
          killServ [] = []
          killServ ("-s":_:l) = killServ l
          killServ l@("--":_) = l
          killServ (x:l) = x:killServ l
        waitForProcess =<<
          runProcess "ssh" (["-t", host, "rr"] ++ killServ args)
          Nothing Nothing Nothing Nothing Nothing
        return ()
    _ -> mapM_ (doTask opts) tasks >> case Opt.fwdServ opts of
      Nothing -> showTasks opts
      Just host -> do
        waitForProcess =<< runProcess "ssh" ["-t", host, "rr"]
          Nothing Nothing Nothing Nothing Nothing
        return ()
