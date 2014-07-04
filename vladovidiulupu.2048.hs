import Control.Monad            ( foldM_
                                , when
                                , sequence
                                )
import System.Exit              ( exitSuccess )
import System.Console.Haskeline ( runInputT
                                , defaultSettings
                                , getInputChar
                                )
import System.Console.ANSI      ( clearScreen )
import Data.List                ( group
                                , transpose
                                , findIndices
                                , find
                                )
import Data.List.Split          ( chunksOf )
import System.Random            ( randomRIO )
import Data.Functor             ( (<$>) )
import Text.Printf              ( printf )

type GameState = [[Maybe Int]]

data Action = SwipeLeft | SwipeRight | SwipeDown | SwipeUp

rowLength :: Int
rowLength = 4

numCells :: Int
numCells = rowLength * rowLength

targetValue :: Int
targetValue = 2048

showCell :: Maybe Int -> String
showCell Nothing = "   ."
showCell (Just x) = printf "%4d" x

showRow :: [Maybe Int] -> String
showRow row = (foldl (++) "" $ map showCell row) ++ "\n" :: String

showBoard :: GameState -> String
showBoard board = foldl (++) "" $ map showRow board

printBoard :: GameState -> IO ()
printBoard = putStr . showBoard 

swipeLeft :: [Maybe Int] -> [Maybe Int]
swipeLeft inputList = 
  take (length inputList) $   
  (map (fmap sum . sequence) $ concat $ (map (chunksOf 2)) $ 
  group $ filter (/= Nothing) inputList) ++ repeat Nothing

flatten :: GameState -> [Maybe Int]
flatten = foldl (++) []

freeCellIndices :: GameState -> [Int]
freeCellIndices board = findIndices (== Nothing) $ flatten board

addValueToBoard :: GameState -> Int -> Int -> GameState
addValueToBoard board index value = 
  let flatBoard = flatten board 
  in  
    chunksOf rowLength $ (take index flatBoard) ++ [Just value] ++ (drop (index + 1) flatBoard)

impureGetIndexAndValue :: [Int] -> IO (Int, Int)
impureGetIndexAndValue indices = do
  index <- (indices !!) <$> randomRIO (0, length indices - 1) :: IO Int
  value <- (*2) <$> randomRIO(1, 2) :: IO Int
  return (index, value)

impureCreateInitialState :: IO GameState
impureCreateInitialState = do
  let board = chunksOf rowLength $ replicate numCells Nothing

  let indices = freeCellIndices board
  (index, value) <- impureGetIndexAndValue indices
  let board1 = addValueToBoard board index value

  let indices2 = freeCellIndices board1
  (index2, value2) <- impureGetIndexAndValue indices2
  let board2 = addValueToBoard board1 index2 value2

  return board2

outOfMoves :: GameState -> Bool
outOfMoves board = pureStepperFunction SwipeLeft board == board &&
                   pureStepperFunction SwipeRight board == board &&
                   pureStepperFunction SwipeUp board == board &&
                   pureStepperFunction SwipeDown board == board

winCondition :: GameState -> Bool
winCondition board = (find (== Just targetValue) $ flatten board) /= Nothing

pureStepperFunction :: Action -> GameState -> GameState
pureStepperFunction SwipeLeft board = map swipeLeft board
pureStepperFunction SwipeRight board = map reverse $ pureStepperFunction SwipeLeft $ map reverse board
pureStepperFunction SwipeUp board = transpose $ pureStepperFunction SwipeLeft $ transpose board
pureStepperFunction SwipeDown board = transpose $ pureStepperFunction SwipeRight $ transpose board

impureStepperFunction :: GameState -> () -> IO GameState
impureStepperFunction board _ = do
  printBoard board
  maybeKeyboardInput <- runInputT defaultSettings $ getInputChar ""
  clearScreen

  when (winCondition board) exitSuccess
  when (outOfMoves board) exitSuccess
  when (maybeKeyboardInput == Just 'q') exitSuccess

  let (afterMove, valid) = computeAfterMoveValidTuple board maybeKeyboardInput

  let indices = freeCellIndices afterMove
  (index, value) <- impureGetIndexAndValue indices
  let newBoard = if valid 
                 then addValueToBoard afterMove index value
                 else afterMove
  return $ newBoard
    where
      computeAfterMoveValidTuple currentBoard maybeKeyboardInput
        | maybeKeyboardInput == Just 'w' =  let afterMove = pureStepperFunction SwipeUp board 
                                              in (afterMove, afterMove /= currentBoard)
        | maybeKeyboardInput == Just 'a' =  let afterMove = pureStepperFunction SwipeLeft board 
                                              in (afterMove, afterMove /= currentBoard)
        | maybeKeyboardInput == Just 's' =  let afterMove = pureStepperFunction SwipeDown board 
                                              in (afterMove, afterMove /= currentBoard)
        | maybeKeyboardInput == Just 'd' =  let afterMove = pureStepperFunction SwipeRight board 
                                              in (afterMove, afterMove /= currentBoard)
        | otherwise                      =  (currentBoard, False)

main :: IO ()
main = do
  clearScreen
  initialState <- impureCreateInitialState
  foldM_ impureStepperFunction initialState $ repeat ()
  return ()
