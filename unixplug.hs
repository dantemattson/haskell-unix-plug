-- Run with ./unixplug < unixplug.txt

import Control.Monad
import Data.List

-- Wrapper function as `edges` needs an empty list too
-- fromToken : a token to start from in the graph indicated by graph
-- toToken   : a token to represent the goal
-- graph     : A list of tokens in tuple form denoting paths
-- Assumes acyclical graph
-- Example:  "A"       "B"   [("A","B")]           Path/s from A to B given graph
paths :: String -> String -> [(String, String)] -> [[String]]
paths fromToken toToken graph = edges fromToken toToken graph []

-- Gets edges for x == a using the information above
edges :: String -> String -> [(String, String)] -> [String] -> [[String]]
edges fromToken toToken graph current = traversePaths fromToken toToken graph current nextEdges
  where
    nextEdges = [y | (x,y) <- graph, x == fromToken]

-- Walks the graph, exhaustively, using the information above
traversePaths :: String -> String -> [(String, String)] -> [String] -> [String] -> [[String]]
-- Stops the recursion
traversePaths fromToken toToken _ current [] = [current ++ [toToken] | fromToken == toToken]
-- Main traversePaths
-- Builds current as is goes along the graph
traversePaths fromToken toToken graph current (x:xs)
  | fromToken == toToken = [current ++ [toToken]]
  | fromToken `elem` current = []
  | otherwise = edges x toToken graph (current++[fromToken]) ++ deeper
  where
    deeper = traversePaths fromToken toToken graph current xs

-- Generates a set of tuples given an input string
-- Takes    ["laptop B","phone C","pager B"]
-- Returns  [(laptop, b), (phone, C), (pager, B)]
-- Assumes input is correct
stringToTuple :: [String] -> [(String, String)]
stringToTuple [] = []
stringToTuple xs = map (\ x -> (head (words x), words x !! 1)) xs

-- Returns the sockets that are not immediately avaliable by checking if they are in the list 'g'
-- Takes [("laptop","B"),("phone","C")...]      ["A","B","C","D"]]
checkIfSocketAvalaible :: [(String, String)] -> [String] -> [String]
checkIfSocketAvalaible [x] g = [snd x | snd x `elem` g]
checkIfSocketAvalaible (x:xs) g
  | snd x `elem` g = snd x : checkIfSocketAvalaible xs g
  | otherwise = [] : checkIfSocketAvalaible xs g
checkIfSocketAvalaible [] _ = []

-- This is a main component of this program this is a bit like a driver function
-- Takes the tuple list of the devices and their sockets, [("laptop","B"), ...]
-- The graph of tuples denoting the relationship between all the sockets [("B","X"),("X","A"),...]
-- And the list of availiable plugs 
-- [[1000]] is just a dummy value to check for later when adding up the devices for the final output
canYouConnect :: [(String, String)] -> [(String, String)] -> [String] -> [[[Int]]]
canYouConnect [] [] [] = []
canYouConnect ((_,s):xs) c g
  | s `elem` g = [[1000]] : canYouConnect xs c g
  | otherwise = [map length (paths s d c) | d <- g] : canYouConnect xs c g
canYouConnect _ _ _ = []

-- Adds up and finds the answer using pattern matching
-- [[1000]] from the previous function indicates that there was a path, so it connected just fine
-- [[],[]] indicates there was no connection, so add 1 to the number of devices that can't connect
addUp :: [[[Int]]] -> Int
addUp [] = 0
addUp [x]
  | x == [[1000]] = 0
  | x == [[],[]] = 1
  | otherwise = 0
addUp (x:xs)
  | x == [[1000]] = 0 + addUp xs
  | x == [[],[]] = 1 + addUp xs
  | otherwise = 0

-- Removes adjacent duplicates, from my "myUniq" submission
uniq :: Eq a => [a] -> [a]
uniq (x:ys@(y:_))
  | x == y = uniq ys
  | otherwise = x : uniq ys
uniq x = x

main :: IO ()
main = do
  -- Get the 'n' as defined in the task sheet and read it in as an int
  nStr <- getLine
  let n = read nStr :: Int
  socketTypes <- replicateM n getLine

  -- Read in 'm' as defined in the task
  mStr <- getLine
  let m = read mStr :: Int
  -- Gets 'm' lines
  devicesAndPlugs <- replicateM m getLine
  
  -- Gets 'k' as defined in task
  kStr <- getLine
  let k = read kStr :: Int
  tAdaptersAndVariety <- replicateM k getLine
  -- After all the lines needed have been read, now we can process
  let adaptersAndVariety = stringToTuple tAdaptersAndVariety
      processedDevicesAndPlugs = stringToTuple devicesAndPlugs
  -- Remove duplicates, sort and get the sockets we need to check if there is a path for them.
  let sockets = uniq $ sort $ checkIfSocketAvalaible processedDevicesAndPlugs socketTypes
  -- Prints the result
  print $ addUp $ canYouConnect processedDevicesAndPlugs adaptersAndVariety sockets
