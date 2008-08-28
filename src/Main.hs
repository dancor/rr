{- Allow me to track / be reminded of things I want to do regularly
 -  (daily, weekly)
 -}

module Main where
import qualified Control.Monad as CM
import qualified Control.Exception as CE
import Data.List
import qualified Data.Map as Map
import qualified Data.Maybe as Mby
import qualified Data.Set as Set
import qualified Database.HDBC as DH
import qualified Database.HDBC.PostgreSQL as DHP
import Database.HDBC
import Database.HDBC.PostgreSQL
import qualified System.Directory as SD
import qualified System.Environment as Env
import qualified System.IO as SIO
import qualified System.Time as Time
import qualified Text.Printf as TP
import Util

cPS = handleSqlError $ connectPostgreSQL "dbname=me_log"

recordTask :: String -> String -> Bool -> String -> IO ()
recordTask taskName username actuallyDid comment = do
  Time.TOD didTime _ <- Time.getClockTime
  conn <- cPS
  ret <- DH.withTransaction conn (\ conn -> do
    DH.run conn 
      "INSERT INTO task_log (task_name, username, actually_did, comment, \
      \did_time) VALUES (?, ?, ?, ?, ?)"
      [DH.toSql taskName, DH.toSql username, DH.toSql actuallyDid, 
      DH.toSql comment, DH.toSql didTime]
    )
  DH.disconnect conn

-- find all tasks that haven't been done in the current time block nor
-- too recently
getDoneTasks :: String -> Integer -> Integer -> IO (Set.Set String)
getDoneTasks username timeBlockSize tooRecentDelta = do
  Time.TOD curTime _ <- Time.getClockTime
  let
    timeBlockStart = curTime - curTime `mod` timeBlockSize
    -- things done after this time don't need to be repeated yet
    timeCutoff = min timeBlockStart (curTime - tooRecentDelta)
  conn <- cPS
  ret <- DH.withTransaction conn (\ conn -> do
    DH.quickQuery conn 
      -- grab things that _have_ been done
      "SELECT DISTINCT(task_name) FROM task_log WHERE username = ? AND \
      \did_time >= ?"
      [DH.toSql username, DH.toSql timeCutoff]
    )
  return $ Set.fromList $ map (DH.fromSql . head) ret

--getTaskRecentTime :: String -> String -> IO (Maybe Int)
getTaskRecentTime :: String -> String -> IO (Maybe Integer)
getTaskRecentTime username taskName = do
  conn <- cPS
  ret <- DH.withTransaction conn (\ conn -> do
    DH.quickQuery conn
      "SELECT MAX(did_time) FROM task_log WHERE username = ? AND task_name = ?"
      [DH.toSql username, DH.toSql taskName]
    )
  return $ DH.fromSql $ head $ head ret

type IntvlInfo = Map.Map String Int

rcName = ".rrrc"
dayTime = 24 * 60 * 60
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
  parseLine :: String -> (Mby.Maybe String, RcType) -> 
    (Mby.Maybe String, RcType)
  parseLine ('#':_) r = r
  parseLine "" r = r
  parseLine l@(l_s:l_r) (t, rc) = 
    let (l_m, [l_e]) = splitAt (length l_r - 1) l_r in
    if l_s == '<' && l_e == '>'
      then (Mby.Just l_m, rc)
      else (t, Map.insertWith Map.union (Mby.fromJust t) 
        (Map.singleton name mbyDesc) rc) where
          (name, mbyDesc) = l `subLBreakOrL` " - "

toDay nowTime Nothing = "never!"
toDay nowTime (Just x) = show $ (nowTime - x) `div` dayTime

divF :: Integer -> Integer -> Float
divF a b = (fromIntegral a) / (fromIntegral b)

mbyCompare :: (a -> a -> Ordering) -> Maybe a -> Maybe a -> Ordering
mbyCompare f Nothing Nothing = EQ
mbyCompare f Nothing y = LT
mbyCompare f x Nothing = GT
mbyCompare f (Just x) (Just y) = f x y

showMN Nothing = "!"
showMN (Just p) = TP.printf "%.1f" p

main :: IO ()
main = do
  args <- Env.getArgs
  let
    username = ""
    argN = length args
  Time.TOD nowTime _ <- Time.getClockTime
  if argN == 3 then recordTask (args!!0) "" (args!!2 /= "f") (args!!1) else if argN == 1 && (args!!0 /= ".")
    then recordTask (args!!0) "" True ""
    else do
      homeDir <- SD.getHomeDirectory
      c <- SIO.openFile (homeDir ++ "/" ++ rcName) SIO.ReadMode >>= 
        SIO.hGetContents
      let
        freqHeaderToSecs h =
          let
            (timesPerStr, intvlStr) = break (== '/') h
            (timesPer, intvlStr') =
              if null intvlStr then (1, h) else (read timesPerStr, tail intvlStr)
          in
            (Mby.fromJust $ Map.lookup intvlStr' intvlInfos) `div` timesPer
        rc = Map.mapWithKey (\h v -> (freqHeaderToSecs h, v)) $
          parseRc $ lines c
      intvlsHeadersItemssTimess <- mapM (\ (adverb, (intvl, allTasksMap)) -> do
          t <- getDoneTasks username intvl (intvl `div` 2)
          let
            tasks = Map.toList $ foldr Map.delete allTasksMap (Set.toList t)
            (taskNames, _) = unzip tasks
            taskLines = map (\ (name, mbyDesc) -> name ++ (case mbyDesc of
                  Nothing -> ""
                  Just desc -> " - " ++ desc
                )
              ) tasks
          times <- mapM (getTaskRecentTime username) taskNames
          return (intvl, adverb ++ " Tasks:", taskLines, times)
        ) $ Map.toList rc
      if argN == 1
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
              (CM.liftM2 divF (CM.liftM2 (-) (Just nowTime) t) (Just iv), i)) 
              intvlsItemsTimes
            pctsItemsOrd = sortBy (\ (x, _) (y, _) -> 
              mbyCompare (flip compare) x y) pctsItems
            (pcts, items) = unzip pctsItemsOrd
            pctsS = map showMN pcts
            ls = zipWith (\ x y -> x ++ "  " ++ y) (spaceBlock pctsS) items
          putStrLn $ interlines ls
