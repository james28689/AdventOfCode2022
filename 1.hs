import System.Environment
import Data.List

main1 :: String -> IO ()
main1 source = do
  f <- readFile source
  let split_file = lines f :: [String]
  let nums = getCaloriesFrom [0] split_file
  print (maximum nums)

getCaloriesFrom :: [Int] -> [String] -> [Int]
getCaloriesFrom xs [] = xs
getCaloriesFrom xs ("":ys) = getCaloriesFrom (0:xs) ys
getCaloriesFrom (x:xs) (y:ys) = getCaloriesFrom (x+read y:xs) ys

main2 :: String -> IO ()
main2 source = do
  f <- readFile source
  let split_file = lines f :: [String]
  let sorted_nums = reverse $ sort $ getCaloriesFrom [0] split_file
  print (sum $ take 3 sorted_nums)