import Data.Char
import Data.List

main1 :: String -> IO ()
main1 source = do
  f <- readFile source
  let split_f = lines f
  let score = sum $ map calculateLineScore split_f
  print score

calculateLineScore :: String -> Int
calculateLineScore line
  = calculatePriority $ sharedItem line

sharedItem :: String -> Char
sharedItem line
  = head $ intersect segment1 segment2
  where
    segment1 = take (length line `div` 2) line
    segment2 = drop (length line `div` 2) line

calculatePriority :: Char -> Int
calculatePriority c
  = if (n > 96) then (n - 96) else (n - 38)
  where
    n = ord c

main2 :: String -> IO ()
main2 source = do
  f <- readFile source
  let split_f = lines f
  let score = calculateBadgeScore split_f
  print score

calculateBadgeScore :: [String] -> Int
calculateBadgeScore [] = 0
calculateBadgeScore lines
  = (calculateBadgeScore (drop 3 lines)) + (calculatePriority.head $ foldl1 intersect (take 3 lines))