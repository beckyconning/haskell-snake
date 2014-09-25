import Control.Applicative
import Data.Maybe
import Control.Monad.Loops
import System.IO
import System.Console.ANSI
import Control.Concurrent
import Control.Concurrent.Async
import System.Timeout
import System.Random

type Vector = (Int, Int)



data State = State { 
    board    :: Int,
    snake    :: [Vector],
    lastMove :: Maybe Vector
} deriving Show

sampleLength :: Int
sampleLength = ((1*10^6) `div` 4)

initialState :: State
initialState = (State {
    board    = 15,
    snake    = [(4, 0), (3, 0), (2, 0), (1, 0), (0, 0)],
    lastMove = Just (1, 0)
})

main = iterateUntilM gameOver 
                     step 
                     initialState
                     
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
              $ map (renderRow (snake state)) 
              $ buildBoard (board state)

applyBorder :: Int -> [[Char]] -> [[Char]]
applyBorder size renderedRows 
    = border ++ map (\row -> ['X'] ++ row ++ ['X']) renderedRows ++ border
    where border = [replicate (size + 2) 'X']

renderRow :: [Vector] -> [Vector] -> String
renderRow snakePositions = map (\cell -> snakeChar $ cell `elem` snakePositions)

snakeChar :: Bool -> Char
snakeChar True  = '#'
snakeChar False = ' '

buildBoard :: Int -> [[(Int, Int)]]
buildBoard size = [[(x, y) | x <- [0 .. size - 1]] | y <- reverse [0 .. size - 1]]

updateState :: State -> Maybe Vector -> State 
updateState state inputMove
    = (State {
        board = board state,
        snake = slither (snake state) validMove,
        lastMove = validMove
    })
    where validMove = inputMove <|> (lastMove state)

slither :: [Vector] -> Maybe Vector -> [Vector]
slither snake (Just vector) = [(head snake) `vectorAdd` vector] ++ init snake
slither snake _             = snake

vectorAdd :: Vector -> Vector -> Vector
vectorAdd (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

sample :: Int -> IO a -> IO (Maybe a)
sample n f
    | n <  0    = fmap Just f
    | n == 0    = return Nothing
    | otherwise = 
        concurrently (timeout n f) (threadDelay n) >>= \ (result, _) ->
            return result
