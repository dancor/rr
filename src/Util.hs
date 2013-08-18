module Util where

max0 :: (Num a, Ord a) => [a] -> a
max0 l = if null l then 0 else maximum l

-- rename to spaceCol?
spaceBlock :: [String] -> [String]
spaceBlock b = let
    lens = map length b
    w = max0 lens in
  zipWith (++) b $ map (\ l -> take (w - l) $ repeat ' ') lens

-- if you want to equally-space several blocks but keep them separate
spaceBlocks :: [[String]] -> [[String]]
spaceBlocks bs = let
    lenss = map (map length) bs
    w = max0 $ map max0 lenss in
  zipWith
   (\ b lens -> zipWith (++) b $ map (\ l -> take (w - l) $ repeat ' ') lens)
    bs lenss
