{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module HW05 where

import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map

import Parser
import Data.Bits
import Data.List
import Data.Ord

-- Exercise 1 -----------------------------------------

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret original modified = do
    o <- BS.readFile original
    m <- BS.readFile modified
    return . BS.pack . filter (/=0) $ zipWith xor (BS.unpack o) (BS.unpack m)

-- Exercise 2 -----------------------------------------

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey k file = do
    f <- BS.readFile $ file ++ ".enc"
    let d = BS.pack $ zipWith xor (BS.unpack f) (concat . repeat $ BS.unpack k)
    BS.writeFile file d

-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile file = do
    f <- BS.readFile file
    return $ decode f

-- Exercise 4 -----------------------------------------


selectTId :: Maybe [TId] -> Maybe [Transaction] -> Maybe [Transaction]
selectTId (Just i) (Just t) = Just (filter (flip elem i . tid) t)
selectTId _ _ = Nothing

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs victims transactions = do
    v <- parseFile victims
    t <- parseFile transactions
    return $ selectTId v t

-- Exercise 5 -----------------------------------------

getFlow :: [Transaction] -> Map String Integer
getFlow t = Map.fromListWith (+) $ payees ++ payers where
  payees = zip (map to t) (map amount t)
  payers = zip (map from t) (map (negate . amount) t)

-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal = fst . maximumBy (comparing snd) . Map.toList

-- Exercise 7 -----------------------------------------

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs m i = pay payers payees i  where
  l = sortBy (flip $ comparing snd) $ Map.toList m
  payers = takeWhile ((> 0) . snd) l
  payees = reverse $ dropWhile ((>= 0) . snd) l
  pay ((from,f):fs) ((to,t'):ts) (tid:tids)
    | f == t = Transaction from to f tid : (pay fs ts tids)
    | f > t = Transaction from to t tid : (pay ((from,f-t):fs) ts tids)
    | t > f = Transaction from to f tid : (pay fs ((to,t'+f):ts) tids)
    where t = abs t'
  pay _ _ _ = []

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON file j = do
    BS.writeFile file (encode j)

-- Exercise 9 -----------------------------------------

doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
  key <- getSecret dog1 dog2
  decryptWithKey key vict
  mts <- getBadTs vict trans
  case mts of
    Nothing -> error "No Transactions"
    Just ts -> do
      mids <- parseFile fids
      case mids of
        Nothing  -> error "No ids"
        Just ids -> do
          let flow = getFlow ts       
          writeJSON out (undoTs flow ids)
          return (getCriminal flow)

main :: IO ()
main = do
  args <- getArgs
  crim <- 
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "dog-original.jpg"
                        "dog.jpg"
                        "transactions.json"
                        "victims.json"
                        "new-ids.json"
                        "new-transactions.json"
  putStrLn crim

