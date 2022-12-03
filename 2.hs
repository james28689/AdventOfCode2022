import Data.Char

main1 :: String -> IO ()
main1 source = do
  f <- readFile source
  let split_f = lines f
  let score = sum $ map calculateScore split_f
  print score

calculateScore :: String -> Int
calculateScore line
  | line `elem` win  = choseItemScore + 6
  | line `elem` draw = choseItemScore + 3
  | otherwise        = choseItemScore
  where
    win = ["B Z", "C X", "A Y"]
    draw = ["A X", "B Y", "C Z"]
    choseItemScore = ord(last line) - 87

main2 :: String -> IO ()
main2 source = do
  f <- readFile source
  let split_f = lines f
  let score = sum $ map calculateScore2 split_f
  print score

calculateScore2 :: String -> Int
calculateScore2 line
  | lastLetter == 'X' = ord (lost !! index) - 87
  | lastLetter == 'Y' = ord (draw !! index) - 84
  | lastLetter == 'Z' = ord (win !! index) - 81
  where
    lost=['Z', 'X', 'Y']                
    win=['Y', 'Z', 'X']
    draw=['X','Y','Z']

    firstLetter = head line
    index = ord(firstLetter) - 65
    lastLetter = last line

-- paper > rock, rock > scissors, scissors > paper