{-# LANGUAGE TemplateHaskell #-}

module Opt where

import Data.PolyOpt
import Data.Ratio

readDecRat :: String -> Rational
readDecRat s = case break (== '.') s of
  (i, _:r) -> let rInt = read r
    in fromIntegral (read i) + rInt % 10 ^ length (show rInt)
  _ -> fromIntegral $ read s

$(polyOpt [
  noArg ["kill-last"] "k"
    "kill last record for the task",
  optArgGen ["list-recent"] "l"
    "N" [t|Int|] [|0|] [|maybe 5 read|]
    "list last N (default 5) records",
  noArg ["did-not-do"] "n"
    "mark as not-actually-completed when\n\
    \silencing reminder",
  reqArgGen ["comment"] "c"
    "COMMENT" [t|String|] [|""|] [|id|]
    "record comment along with silencing\n\
    \reminder",
  reqArgGen ["username"] "u"
    "USERNAME" [t|String|] [|""|] [|id|]
    "set the username to see/mark\n\
    \reminders for",
  noArg ["just-mark"] "m"
    "if task is runnable, still just mark\n\
    \it instead of also running it",
  noArg ["group-view"] "g"
    "show group view of task list",
  reqArgGen ["intvl-frac-to-show"] "f"
    "FRAC" [t|Rational|] [|0.5|] [|readDecRat|]
    "show tasks undone in the past\n\
    \FRAC * do-interval (default 0.5)",
  reqArgGen ["hours-ago"] "a"
    "FRAC" [t|Rational|] [|0|] [|readDecRat|]
    "mark a task as done N hours ago",
  reqArg ["fwd-serv"] "s"
    "HOST"
    "forward the rr register (i.e., after\n\
    \command, do: ssh -t HOST rr TASK)",
  noArg ["quiet"] "q"
    "just perform and mark tasks; do not\n\
    \display task list"])

run = not . justMark
