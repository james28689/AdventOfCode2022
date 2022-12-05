import Data.List
import Data.List.Split

{- 
hard coding the original stacks - might change later

model stack as a list:
  - take x stack ++ drop x stack - take x vals from top
  - xs : stack - add xs to top
-}

stack1 = reverse "WBDNCFJ"
stack2 = reverse "PZVQLST"
stack3 = reverse "PZBGJT"
stack4 = reverse "DTLJZBHC"
stack5 = reverse "GVBJS"
stack6 = reverse "PSQ"
stack7 = reverse "BVDFLMPN"
stack8 = reverse "PSMFBDLR"
stack9 = reverse "VDTR"
stacks = [stack1, stack2, stack3, stack4, stack5, stack6, stack7, stack8, stack9]

main1 :: String -> IO ()
main1 source = do
  f <- readFile source
  let commands = lines $ last (splitOn "\n\n" f)

  let rearranged = rearrange commands stacks
  let final = map head (filter (/="") rearranged)

  print final

rearrange :: [String] -> [String] -> [String]
rearrange [] stacks = stacks
rearrange (c:cs) stacks
  = rearrange cs finalStacks
  where
    splitC = words c
    n = read (splitC !! 1) :: Int
    nFrom = read (splitC !! 3) :: Int
    nTo = read (splitC !! 5) :: Int
    from = drop n (stacks !! (nFrom - 1))
    to = (reverse (take n (stacks !! (nFrom - 1)))) ++ (stacks !! (nTo - 1))
    stacksAfterFrom = take (nFrom - 1) stacks ++ [from] ++ drop (nFrom) stacks
    finalStacks = take (nTo - 1) stacksAfterFrom ++ [to] ++ drop (nTo) stacksAfterFrom

main2 :: String -> IO ()
main2 source = do
  f <- readFile source
  let commands = lines $ last (splitOn "\n\n" f)

  let rearranged = rearrange2 commands stacks
  let final = map head (filter (/="") rearranged)

  print final

rearrange2 :: [String] -> [String] -> [String]
rearrange2 [] stacks = stacks
rearrange2 (c:cs) stacks
  = rearrange2 cs finalStacks
  where
    splitC = words c
    n = read (splitC !! 1) :: Int
    nFrom = read (splitC !! 3) :: Int
    nTo = read (splitC !! 5) :: Int
    from = drop n (stacks !! (nFrom - 1))
    to = (take n (stacks !! (nFrom - 1))) ++ (stacks !! (nTo - 1))
    stacksAfterFrom = take (nFrom - 1) stacks ++ [from] ++ drop (nFrom) stacks
    finalStacks = take (nTo - 1) stacksAfterFrom ++ [to] ++ drop (nTo) stacksAfterFrom