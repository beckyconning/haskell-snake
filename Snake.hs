{-# OPTIONS_GHC -Wall #-}

import Data.List
import Data.IORef

import System.IO
import System.Random
import System.Console.ANSI

import Control.Concurrent
import Control.Monad
import Control.Monad.Loops
import Control.Applicative
import Control.Exception

startInput :: IO (IO Char, ThreadId)
startInput = hSetEcho stdin False
    >> newIORef ' '
    >>= \ charRef -> (forkIO . forever) (getChar >>= writeIORef charRef)
    >>= \ threadId -> return (readIORef charRef, threadId)

type Vector = (Int, Int)

data State = State {
    board :: Int,
    snake :: [Vector],
    fruit :: Maybe (Vector, StdGen),
    move  :: Maybe Vector
} deriving Show

oneSecond :: Int
oneSecond = (10 :: Int) ^ (6 :: Int)

sampleLength :: Int
sampleLength = oneSecond `div` 4

initialState :: IO State
initialState = getStdGen 
    >>= \stdGen -> return State {
        board = 15,
        snake = [(4, 0), (3, 0), (2, 0), (1, 0), (0, 0)],
        fruit = randomElem (concat (buildBoard 15)) stdGen,
        move  = Just (1, 0)
    }

randomElem :: [a] -> StdGen -> Maybe (a, StdGen)
randomElem [] _  = Nothing
randomElem xs inputStdGen  = Just (element, stdGen)
    where indexStdGenTuple = randomR (0, length xs - 1) inputStdGen
          index            = fst indexStdGenTuple
          stdGen           = snd indexStdGenTuple
          element          = xs !! index

newFruit :: State -> Maybe (Vector, StdGen)
newFruit (State { fruit = Nothing }) = Nothing
newFruit state@(State { fruit = Just (_, stdGen) })
    = randomElem validPositions stdGen
        where allPositions   = concat $ buildBoard $ board state
              validPositions = allPositions \\ snake state

main :: IO State
main = bracket startInput
               stopInput
               (gameLoop . fst)
               
gameLoop :: IO Char -> IO State
gameLoop getInput = initialState 
    >>= (iterateUntilM gameOver (step getInput))
               
step :: IO Char -> State -> IO State
step getInput state = displayState $ 
                      (updateStateFromInput state) $
                      (sampleInput getInput)

sampleInput :: IO Char -> IO Char
sampleInput getInput = threadDelay sampleLength
    >> getInput

updateStateFromInput :: State -> IO Char -> IO State
updateStateFromInput state inputSample = inputSample
    >>= \ char -> return (updateState state $ vectorFromChar char)

stopInput :: (IO Char, ThreadId) -> IO ()
stopInput (_, threadId) = killThread threadId

displayState :: IO State -> IO State
displayState getState = getState
    >>= \ state -> clearScreen 
    >> putStr (render state) 
    >> return state

vectorFromChar :: Char -> Maybe Vector
vectorFromChar 'w' = Just ( 0,  1)
vectorFromChar 'a' = Just (-1,  0)
vectorFromChar 's' = Just ( 0, -1)
vectorFromChar 'd' = Just ( 1,  0)
vectorFromChar _   = Nothing

gameOver :: State -> Bool
gameOver (State { snake = [] }) = True
gameOver (State {
    board = boardSize,
    snake = (snakeHead@(snakeHeadX, snakeHeadY):snakeBody)
})
    | snakeHeadX >= boardSize || snakeHeadX < 0 = True
    | snakeHeadY >= boardSize || snakeHeadY < 0 = True
    | snakeHead `elem` snakeBody                = True
    | otherwise                                 = False

render :: State -> String
render state
    = unlines $ applyBorder (board state)
              $ map (renderRow state)
              $ buildBoard (board state)

applyBorder :: Int -> [String] -> [String]
applyBorder size renderedRows
    = border ++ map (\row -> "X" ++ row ++ "X") renderedRows ++ border
        where border = [replicate (size + 2) 'X']

renderRow :: State -> [Vector] -> String
renderRow state = map (characterForPosition state)

characterForPosition :: State -> Vector -> Char
characterForPosition state position
    | position `elem` snake state                = '#'
    | fruit state `fruitPositionEquals` position = '@'
    | otherwise                                  = ' '

fruitPositionEquals :: Maybe (Vector, StdGen) -> Vector -> Bool
fruitPositionEquals (Just (position, _)) vector = position == vector
fruitPositionEquals _ _                         = False

snakeHasFruitInMouth :: State -> Bool
snakeHasFruitInMouth state
    = fruit state `fruitPositionEquals` head (snake state)

buildBoard :: Int -> [[(Int, Int)]]
buildBoard size
    = [[(x, y) | x <- [0 .. size - 1]] | y <- reverse [0 .. size - 1]]

updateState :: State -> Maybe Vector -> State
updateState state inputMove
    = updateFruit $ updateSnake $ updateMove state inputMove

updateMove :: State -> Maybe Vector -> State
updateMove state@(State { move = Just vector }) inputMove@(Just inputVector)
    | inputVector == vectorOpposite vector
        = state
    | otherwise
        = state { move = inputMove <|> move state }
updateMove state _ = state

updateSnake :: State -> State
updateSnake = updateSnakeTail . updateSnakeHead

updateFruit :: State -> State
updateFruit state
    | snakeHasFruitInMouth state = state { fruit = newFruit state }
    | otherwise                  = state

updateSnakeHead :: State -> State
updateSnakeHead state@(State { move = (Just vector) })
    = state { snake = head (snake state) `vectorAdd` vector : snake state }
updateSnakeHead state = state

updateSnakeTail :: State -> State
updateSnakeTail state
    | snakeHasFruitInMouth state = state
    | otherwise                  = state { snake = init $ snake state }

vectorAdd :: Vector -> Vector -> Vector
vectorAdd (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

vectorOpposite :: Vector -> Vector
vectorOpposite (x, y) = (-x, -y)
