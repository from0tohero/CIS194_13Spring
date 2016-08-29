{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

-- Ex 1
parse :: String -> [LogMessage]
parse blob = map parseMessage (lines blob)
  where 
    parseMessage :: String -> LogMessage
    parseMessage str =
      let readTs s = (read s::TimeStamp) in
      let list = words str in
      case head list of
        "I" -> LogMessage Info (readTs (list!!1)) (unwords (drop 2 list))
        "W" -> LogMessage Warning (readTs (list!!1)) (unwords (drop 2 list))
        "E" -> LogMessage (Error (read (list!!1)::Int)) (readTs (list!!2)) (unwords (drop 3 list))
        _   -> Unknown str

-- Ex 2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) root = root
insert logM@(LogMessage _ ts _) (Node lt dt@(LogMessage _ tsn _) rt)
  | ts < tsn = Node (insert logM lt) dt rt
  | otherwise = Node lt dt (insert logM rt)
insert logM Leaf = Node Leaf logM Leaf
insert (LogMessage _ _ _) (Node _ (Unknown _) _) = error "Unknown should not be in the tree"

-- Ex 3
build :: [LogMessage] -> MessageTree
build [] = Leaf
build [x] = insert x Leaf
build (h:t) = insert h (build t)

-- Ex 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node lt dt rt) = inOrder lt ++ [dt] ++ inOrder rt

-- Ex 5
isSevere :: LogMessage -> Bool
isSevere (LogMessage (Error x) _ _)
  | x > 50 = True
  | otherwise = False
isSevere _ = False

severeLogs :: [LogMessage] -> [LogMessage]
severeLogs logs = filter isSevere logs

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logs = 
  let logsAfterFilter = severeLogs logs in
  let printLogContent (LogMessage _ _ content) = content 
      printLogContent (Unknown content) = content 
  in 
  map printLogContent (inOrder (build logsAfterFilter))