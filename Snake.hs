{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall   #-}

import Control.Concurrent       (threadDelay)
import Control.Concurrent.Async (concurrently)
import Control.Monad.Random

import Data.List
import Data.Maybe               (fromMaybe)

import System.IO
import System.Console.ANSI      (clearScreen, setCursorPosition)
import System.Timeout           (timeout)

type Vector = (Int, Int)

data State = State
    { board :: Int
    , snake :: [Vector]
    , fruit :: Vector
    , move  :: Vector
    } deriving Show

main :: IO ()
main =
  clearScreen >>
  evalRandIO initialState >>= \is ->
  getStdGen >>= \g ->
  loop is g

loop :: RandomGen g => State -> g -> IO ()
loop s g
  | gameOver s = return ()
  | otherwise  =
      sample sampleLength getInput >>= \i ->
      let (state, g') = runRand (updateState s (vectorFromChar i)) g
      in  setCursorPosition 0 0 >>
          putStr (render state) >>
          loop state g'

sampleLength :: Int
sampleLength = oneSecond `div` 4
  where
    oneSecond = 10 ^ (6 :: Int)

initialState :: RandomGen g => Rand g State
initialState =
    uniform (concat (buildBoard 15)) >>= \f ->
    return $ State 15 [(4, 0), (3, 0), (2, 0), (1, 0), (0, 0)] f (1,0)

newFruit :: (RandomGen g) => State -> Rand g Vector
newFruit state = uniform validPositions
  where
    allPositions   = concat . buildBoard $ board state
    validPositions = allPositions \\ snake state

vectorFromChar :: Char -> Vector
vectorFromChar = \case
    'w' -> ( 0,  1)
    'a' -> (-1,  0)
    's' -> ( 0, -1)
    'd' -> ( 1,  0)
    _   -> ( 0,  0)

getInput :: IO Char
getInput = hSetEcho stdin False >>
           hSetBuffering stdin NoBuffering >>
           getChar

gameOver :: State -> Bool
gameOver (State { snake = [] }) = True
gameOver (State { board = bs, snake = (hd@(x, y):tl) }) =
    x >= bs || x < 0 || y >= bs || y < 0 || hd `elem` tl

render :: State -> String
render s = unlines $ applyBorder (board s)
                   $ map (renderRow s)
                   $ buildBoard (board s)

applyBorder :: Int -> [String] -> [String]
applyBorder size renderedRows =
    border ++ map (\row -> "X" ++ row ++ "X") renderedRows ++ border
      where
        border = [replicate (size + 2) 'X']

renderRow :: State -> [Vector] -> String
renderRow state = map (characterForPosition state)

characterForPosition :: State -> Vector -> Char
characterForPosition s p
    | p `elem` snake s = '#'
    | fruit s == p     = '@'
    | otherwise        = ' '

snakeHasFruitInMouth :: State -> Bool
snakeHasFruitInMouth state = fruit state == head (snake state)

buildBoard :: Int -> [[(Int, Int)]]
buildBoard size = [ [(x, size - y - 1) | x <- [0 .. size - 1] ]
                                       | y <- [0 .. size - 1] ]

updateState :: RandomGen g => State -> Vector -> Rand g State
updateState s i = updateFruit . updateSnake $ updateMove s i

updateMove :: State -> Vector -> State
updateMove s@(State { move = vector }) i
    | i == negateV vector || i == (0, 0) = s
    | otherwise                          = s { move = i }

updateSnake :: State -> State
updateSnake = updateSnakeTail . updateSnakeHead

updateFruit :: RandomGen g => State -> Rand g State
updateFruit s
    | snakeHasFruitInMouth s = fmap (\nf -> s {fruit = nf}) (newFruit s)
    | otherwise              = return s

updateSnakeHead :: State -> State
updateSnakeHead s@(State { move = vector }) =
    s { snake = head (snake s) ^+^ vector : snake s }

updateSnakeTail :: State -> State
updateSnakeTail s
    | snakeHasFruitInMouth s = s
    | otherwise              = s { snake = init $ snake s }

infixl 6 ^+^

(^+^) :: Vector -> Vector -> Vector
(^+^) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

negateV :: Vector -> Vector
negateV (x, y) = (-x, -y)

sample :: Int -> IO Char -> IO Char
sample n f
    | n <=  0   =  f
    | otherwise = concurrently (timeout n f) (threadDelay n) >>=
                      (return . fromMaybe '-' . fst)
