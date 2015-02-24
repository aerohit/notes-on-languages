module LogAnalysis where
import Log

-- Exercise 1
tokensToLogMessage :: [String] -> LogMessage
tokensToLogMessage ("E":(level:(timestamp:msgs))) = LogMessage (Error (read level)) (read timestamp) (unwords msgs)
tokensToLogMessage ("I":(timestamp:msgs))         = LogMessage Info (read timestamp) (unwords msgs)
tokensToLogMessage ("W":(timestamp:msgs))         = LogMessage Warning (read timestamp) (unwords msgs)
tokensToLogMessage msgs                           = Unknown (unwords msgs)

parseMessage :: String -> LogMessage
parseMessage str = tokensToLogMessage (words str)

parseToMessages :: [String] -> [LogMessage]
parseToMessages [] = []
parseToMessages (x:xs) = parseMessage x : parseToMessages xs

parse :: String -> [LogMessage]
parse content = parseToMessages (lines content)

-- Exercise 2

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree      = tree
insert logMessage Leaf       = Node Leaf logMessage Leaf
insert logMessage@(LogMessage _ logTimeStamp _) (Node leftNode nodeMessage@(LogMessage _ nodeTimeStamp _) rightNode)
  | logTimeStamp < nodeTimeStamp = Node (insert logMessage leftNode) nodeMessage rightNode
  | otherwise                    = Node leftNode nodeMessage (insert logMessage rightNode)

-- Exercise 3
build :: [LogMessage] -> MessageTree
build []     = Leaf
build (x:xs) = insert x (build xs)

-- Exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node leftTree nodeMessage rightTree) = (inOrder leftTree) ++ [nodeMessage] ++ (inOrder rightTree)

-- Exercise 5
