{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseWords :: [String] -> LogMessage
parseWords ("I":ts:rest) = LogMessage Info (read ts) (unwords rest)
parseWords ("W":ts:rest) = LogMessage Warning (read ts) (unwords rest)
parseWords ("E":sev:ts:rest) = LogMessage (Error (read sev)) (read ts) (unwords rest)
parseWords s = Unknown (unwords s)

parseMessage :: String -> LogMessage
parseMessage = parseWords . words

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert l (Leaf) = Node Leaf l Leaf
insert l@(LogMessage _ ts _) (Node lt l2@(LogMessage _ ts2 _) gt)
  | ts <= ts2 = Node (insert l lt) l2 gt
  | ts > ts2 = Node lt l2 (insert l gt)
insert _ (Node _ (Unknown _) _) = undefined
insert _ (Node _ (LogMessage _ _ _) _) = undefined

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder (Leaf) = []
inOrder (Node lt l gt) = inOrder lt ++ [l] ++ inOrder gt

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map justMessage . inOrder . build . filter cond
  where cond (LogMessage (Error s) _ _) = s >= 50
        cond _ = False
        justMessage (LogMessage _ _ m) = m
