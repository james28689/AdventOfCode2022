import Data.List
import Data.List.Split

main1 :: String -> IO ()
main1 source = do
  f <- readFile source
  let split_f = lines f
  let n = length $ filter checkContained split_f
  print n

checkContained :: String -> Bool
checkContained l
  = (r1L <= r2L && r1U >= r2U) || (r2L <= r1L && r2U >= r1U)
  where
    splitL = splitOn "," l
    r1 = getRangeBounds $ head splitL
    r1L = head r1
    r1U = last r1
    r2 = getRangeBounds $ last splitL
    r2L = head r2
    r2U = last r2

getRangeBounds :: String -> [Int]
getRangeBounds r
  = [n1,n2]
  where
    ns = splitOn "-" r
    n1 = read (head ns) :: Int
    n2 = read (last ns) :: Int

main2 :: String -> IO ()
main2 source = do
  f <- readFile source
  let split_f = lines f
  let n = length $ filter checkOverlap split_f
  print n

checkOverlap :: String -> Bool
checkOverlap l
  = (length $ intersect [r1L..r1U] [r2L..r2U]) /= 0
  where
    splitL = splitOn "," l
    r1 = getRangeBounds $ head splitL
    r1L = head r1
    r1U = last r1
    r2 = getRangeBounds $ last splitL
    r2L = head r2
    r2U = last r2