import Data.Maybe
import Data.List
import Data.Tuple

import System.IO
import System.Timeout
import System.Random
import System.Console.ANSI

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.Loops
import Control.Applicative

type Vector = (Int, Int)

instance (Random x, Random y) => Random (x, y) where
    random gen1 = ((x, y), gen3)
        where (x, gen2) = random gen1
              (y, gen3) = random gen2
    randomR ((x1, y1), (x2, y2)) gen1 = ((x, y), gen3)
        where (x, gen2) = randomR (x1, x2) gen1
              (y, gen3) = randomR (y1, y2) gen2

data State = State { 
    board :: Int,
    snake :: [Vector],
    fruit :: Maybe (Vector, StdGen),
    move  :: Maybe Vector
} deriving Show

sampleLength :: Int
sampleLength = ((10 ^ 6) `div` 4)

initialState :: IO State
initialState = getStdGen >>= \stdGen ->
    return (State {
        board = 15,
        snake = [(4, 0), (3, 0), (2, 0), (1, 0), (0, 0)],
        fruit = randomElem (foldl1 (++) (buildBoard 15)) stdGen, 
        move  = Just (1, 0)
    })

randomElem :: [a] -> StdGen -> Maybe (a, StdGen)
randomElem [] stdGen  = Nothing
randomElem xs stdGen  = Just (element, newStdGen)
    where random    = randomR (0, (length xs - 1)) stdGen
          index     = fst random
          newStdGen = snd random
          element   = xs !! index 

newFruit :: State -> Maybe (Vector, StdGen)
newFruit state@(State { fruit = Nothing }) = Nothing
newFruit state@(State { fruit = Just (_, stdGen) })
    = randomElem validPositions stdGen
        where allPositions   = foldl1 (++) $ buildBoard $ board state
              validPositions = allPositions \\ (snake state)

main = initialState >>= \ state ->
    iterateUntilM gameOver 
                  step 
                  state
                     
step :: State -> IO State
step state = sample sampleLength getInput >>= \ move ->
        displayState $ updateState state (vectorFromChar move)

displayState :: State -> IO State
displayState state = clearScreen >> putStr (render state) >> return state

vectorFromChar :: Maybe Char -> Maybe Vector
vectorFromChar (Just 'w') = Just ( 0,  1)
vectorFromChar (Just 'a') = Just (-1,  0)
vectorFromChar (Just 's') = Just ( 0, -1)
vectorFromChar (Just 'd') = Just ( 1,  0)
vectorFromChar _          = Nothing

getInput :: IO (Char)
getInput = (hSetEcho stdin False >> hSetBuffering stdin NoBuffering 
                                 >> hGetChar stdin)

gameOver :: State -> Bool
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

applyBorder :: Int -> [[Char]] -> [[Char]]
applyBorder size renderedRows 
    = border ++ map (\row -> ['X'] ++ row ++ ['X']) renderedRows ++ border
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
    = (fruit state) `fruitPositionEquals` (head $ snake state)

buildBoard :: Int -> [[(Int, Int)]]
buildBoard size 
    = [[(x, y) | x <- [0 .. size - 1]] | y <- reverse [0 .. size - 1]]

updateState :: State -> Maybe Vector -> State 
updateState state move = updateFruit $ updateSnake $ updateMove state move

updateMove :: State -> Maybe Vector -> State
updateMove state@(State { move = Just vector }) inputMove@(Just inputVector)
    | inputVector == vectorOpposite vector 
        = state
    | otherwise                            
        = state { move = inputMove <|> (move state) }
updateMove state _ = state

updateSnake :: State -> State
updateSnake = updateSnakeTail . updateSnakeHead

updateFruit :: State -> State 
updateFruit state
    | snakeHasFruitInMouth state = state { fruit = newFruit state }
    | otherwise                  = state 

updateSnakeHead :: State -> State 
updateSnakeHead state@(State { move = (Just vector) })
    = state { snake = [head (snake state) `vectorAdd` vector] ++ snake state }
updateSnakeHead state = state

updateSnakeTail :: State -> State
updateSnakeTail state
    | snakeHasFruitInMouth state = state 
    | otherwise                  = state { snake = init $ snake state }

vectorAdd :: Vector -> Vector -> Vector
vectorAdd (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

vectorOpposite :: Vector -> Vector
vectorOpposite (x, y) = (-x, -y)

sample :: Int -> IO a -> IO (Maybe a)
sample n f
    | n <  0    = fmap Just f
    | n == 0    = return Nothing
    | otherwise = 
        concurrently (timeout n f) (threadDelay n) >>= \ (result, _) ->
            return result
