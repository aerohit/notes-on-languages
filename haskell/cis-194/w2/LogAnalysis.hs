{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage logline = case (words logline) of
                         ("E":level:timestamp:rest) -> LogMessage (Error (read level :: Int)) (read timestamp :: Int) (unwords rest)
                         ("I":timestamp:rest)       -> LogMessage Info (read timestamp :: Int) (unwords rest)
                         ("W":timestamp:rest)       -> LogMessage Warning (read timestamp :: Int) (unwords rest)
                         msg                        -> Unknown (unwords msg)

parse :: String -> [LogMessage]
parse = map parseMessage . lines

createNode :: LogMessage -> MessageTree
createNode msg = Node Leaf msg Leaf

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf         = createNode msg
insert msg@(LogMessage _ timestamp _) (Node left msg'@(LogMessage _ timestamp' _) right)
  | timestamp < timestamp' = Node (insert msg left) msg' right
  | otherwise              = Node left msg' (insert msg right)
insert _ _ = error "can't insert " -- ++ (show msg) ++ " into " ++ (show tree)

build :: [LogMessage] -> MessageTree
build logmsgs = foldr insert Leaf logmsgs

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf                  = []
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right

isSevereError :: LogMessage -> Bool
isSevereError (LogMessage (Error level) _ _) = level >= 50
isSevereError _                              = False

getMessage :: LogMessage -> String
getMessage (LogMessage _ _ msg) = msg
getMessage (Unknown msg)        = msg

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getMessage . inOrder . build . filter isSevereError
