{-# LANGUAGE MonadComprehensions, RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
module HW07 where

import Prelude hiding (mapM)
import Cards

import Control.Monad hiding (mapM, liftM)
import Control.Monad.Random
import Data.Functor
import Data.Monoid
import Data.Vector (Vector, cons, (!), (!?), (//))
import System.Random
import Data.Maybe

import qualified Data.Vector as V


-- Exercise 1 -----------------------------------------

liftM :: Monad m => (a -> b) -> m a -> m b

liftM f mx = do
    x <- mx
    return $ f x
--liftM f mx = mx >>= \x -> return $ f x

swapV :: Int -> Int -> Vector a -> Maybe (Vector a)
swapV i j vx = liftM2 swap (vx !? i) (vx !? j)
  where swap x y = vx // [(i, y), (j, x)]

-- Exercise 2 -----------------------------------------

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f = sequence . map f

getElts :: [Int] -> Vector a -> Maybe [a]
getElts xs vx = mapM (vx !?) xs

-- Exercise 3 -----------------------------------------

type Rnd a = Rand StdGen a

randomElt :: Vector a -> Rnd (Maybe a)
randomElt vx = do
    r <- getRandomR (0, V.length vx - 1)
    return $ vx !? r

-- Exercise 4 -----------------------------------------

randomVec :: Random a => Int -> Rnd (Vector a)
randomVec n = sequence $ V.replicate n getRandom

randomVecR :: Random a => Int -> (a, a) -> Rnd (Vector a)
randomVecR n b = sequence . V.replicate n $ getRandomR b

-- Exercise 5 -----------------------------------------

shuffle :: Vector a -> Rnd (Vector a)
shuffle vx = swapRandom (V.length vx - 1) vx
  where
    swapRandom :: Int -> Vector a -> Rnd (Vector a)
    swapRandom 0 vx' = return vx'
    swapRandom i vx' = do
      j <- getRandomR (0, i)
      let vx'' = fromJust $ swapV i j vx'
      swapRandom (i - 1) vx''

-- Exercise 6 -----------------------------------------

pop :: Vector a -> Int -> (a, Vector a)
pop vx i = (V.head rest, before <> (V.tail rest))
    where
      (before, rest) = V.splitAt i vx

partitionAt :: Ord a => Vector a -> Int -> (Vector a, a, Vector a)
partitionAt vx i = (lt, x, ge)
    where 
          x = vx ! i
          (before, after') = V.splitAt i vx
          after = V.tail after'
          (lt, ge) = V.unstablePartition (< x) (before V.++ after)

-- Exercise 7 -----------------------------------------

-- Quicksort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [ y | y <- xs, y < x ]
                   <> (x : quicksort [ y | y <- xs, y >= x ])

qsort :: Ord a => Vector a -> Vector a
qsort vx
  | V.null vx = vx
  | otherwise = qsort [ x | x <- vx', x < x' ]
                <> V.singleton x'
                <> qsort [ x | x <- vx', x >= x' ]
                where vx' = V.tail vx
                      x' = V.head vx

-- Exercise 8 -----------------------------------------

qsortR :: Ord a => Vector a -> Rnd (Vector a)
qsortR vx = do
    r <- getRandomR (0, V.length vx - 1)
    let (lt, p, gt) = partitionAt vx r
    lt' <- qsortR lt
    gt' <- qsortR gt
    return $ lt' <> V.singleton p <> gt'

-- Exercise 9 -----------------------------------------

-- Selection
select :: Ord a => Int -> Vector a -> Rnd (Maybe a)
select = undefined

-- Exercise 10 ----------------------------------------

allCards :: Deck
allCards = undefined

newDeck :: Rnd Deck
newDeck =  undefined

-- Exercise 11 ----------------------------------------

nextCard :: Deck -> Maybe (Card, Deck)
nextCard = undefined

-- Exercise 12 ----------------------------------------

getCards :: Int -> Deck -> Maybe ([Card], Deck)
getCards = undefined

-- Exercise 13 ----------------------------------------

data State = State { money :: Int, deck :: Deck }

repl :: State -> IO ()
repl s@State{..} | money <= 0  = putStrLn "You ran out of money!"
                 | V.null deck = deckEmpty
                 | otherwise   = do
  putStrLn $ "You have \ESC[32m$" ++ show money ++ "\ESC[0m"
  putStrLn "Would you like to play (y/n)?"
  cont <- getLine
  if cont == "n"
  then putStrLn $ "You left the casino with \ESC[32m$"
           ++ show money ++ "\ESC[0m"
  else play
    where deckEmpty = putStrLn $ "The deck is empty. You got \ESC[32m$"
                      ++ show money ++ "\ESC[0m"
          play = do
            putStrLn "How much do you want to bet?"
            amt <- read <$> getLine
            if amt < 1 || amt > money
            then play
            else do
              case getCards 2 deck of
                Just ([c1, c2], d) -> do
                  putStrLn $ "You got:\n" ++ show c1
                  putStrLn $ "I got:\n" ++ show c2
                  case () of
                    _ | c1 >  c2  -> repl $ State (money + amt) d
                      | c1 <  c2  -> repl $ State (money - amt) d
                      | otherwise -> war s{deck = d} amt
                _ -> deckEmpty
          war (State m d) amt = do
            putStrLn "War!"
            case getCards 6 d of
              Just ([c11, c21, c12, c22, c13, c23], d') -> do
                putStrLn $ "You got\n" ++ ([c11, c12, c13] >>= show)
                putStrLn $ "I got\n" ++ ([c21, c22, c23] >>= show)
                case () of
                  _ | c13 > c23 -> repl $ State (m + amt) d'
                    | c13 < c23 -> repl $ State (m - amt) d'
                    | otherwise -> war (State m d') amt
              _ -> deckEmpty 

main :: IO ()
main = evalRandIO newDeck >>= repl . State 100
