import Control.Monad            ( foldM_
                                , when
                                )
import System.Exit              ( exitSuccess )
import System.Console.Haskeline ( runInputT
                                , defaultSettings
                                , getInputChar
                                )
import System.Console.ANSI      ( clearScreen )
import Data.List                ( elemIndices
                                , group
                                , transpose
                                )
import Data.List.Split          ( chunksOf )
import System.Random            ( randomRIO )
import Control.Applicative      ( (<$>) )
import Text.Printf              ( printf )

type GameState = [[Maybe Int]]

showBoard :: GameState -> String
showBoard = concat . (map showRow) 
                  where showRow line = (concat $ "." : map showCell line) ++ "\n"
                        showCell Nothing = "      . " 
                        showCell (Just x) = printf " %4d . " x           

randomIndex :: GameState -> IO Int
randomIndex board = do
    let nothingIndices = elemIndices Nothing (concat board)
    indexNothingIndices <- randomRIO (0, (length nothingIndices) - 1)  :: IO Int
    return (nothingIndices !! indexNothingIndices)

placeCell :: GameState -> Int -> Int -> GameState
placeCell board newIndex choose = chunksOf 4 $ addToBoard (concat board) newIndex choose
                                  where addToBoard vect index value = if (index == 0) 
                                                                  then  (Just value) : tail vect
                                                                  else (head vect) : addToBoard (tail vect) (index - 1) value

initializeGameState :: IO GameState
initializeGameState = do
  firstIndex <- randomRIO (0, 15)  :: IO Int
  let initial = placeCell (chunksOf 4 $ replicate 16 Nothing) firstIndex 2
  secondIndex <- randomIndex initial
  let newInitial = placeCell initial secondIndex 2
  return newInitial
 
move :: ([Int] -> [Int] -> [Int]) -> [Int] -> [Int]
move operator inputList = operator (comprisedList inputList) (replicate finalLen 0)
            where finalLen = len1 - len2
                  len1 = length inputList
                  len2 = length $ comprisedList inputList
                  comprisedList = (map sum) . concat . (map (chunksOf 2)) . group . (filter (/= 0))
                   
swipeLeft :: [[Int]] -> [[Int]]
swipeLeft = map (move (++))

swipeRight :: [[Int]] -> [[Int]]
swipeRight = map (move (flip (++)))

swipeUp :: [[Int]] -> [[Int]]
swipeUp = transpose . map (move (++)) . transpose

swipeDown :: [[Int]] -> [[Int]]
swipeDown = transpose . map (move (flip (++))) . transpose

chooseKeyboard :: Maybe Char -> GameState -> GameState
chooseKeyboard maybeKeyboardInput board
  | maybeKeyboardInput == Just 'w' = fromMatrixToGameState $ swipeUp $ fromGameStateToMatrix board
  | maybeKeyboardInput == Just 'a' = fromMatrixToGameState $ swipeLeft $ fromGameStateToMatrix board
  | maybeKeyboardInput == Just 's' = fromMatrixToGameState $ swipeDown $ fromGameStateToMatrix board
  | maybeKeyboardInput == Just 'd' = fromMatrixToGameState $ swipeRight $ fromGameStateToMatrix board
  | otherwise = board
  where fromMatrixToGameState matrix = chunksOf 4 $ map fromIntToMaybe (concat matrix)
        fromIntToMaybe 0 = Nothing
        fromIntToMaybe x = Just x
        fromGameStateToMatrix gameState = chunksOf 4 $ map fromMaybeToInt (concat gameState)
        fromMaybeToInt Nothing = 0
        fromMaybeToInt (Just x) = x

impureStepperFunction :: GameState -> () -> IO GameState
impureStepperFunction board () = do
  maybeKeyboardInput <- runInputT defaultSettings $ getInputChar ""
  clearScreen
  choose2or4 <- (* 2) <$> (randomRIO (1, 2)  :: IO Int)
  freeIndex <- randomIndex board
  let fictiveNextState = chooseKeyboard maybeKeyboardInput board
  let nextState = if (board == fictiveNextState) then board else placeCell fictiveNextState freeIndex choose2or4
  putStr $ showBoard nextState
  when (maybeKeyboardInput == Just 'q') exitSuccess
  return nextState

main :: IO ()
main = do
  clearScreen
  initial <- initializeGameState
  putStr $ showBoard initial
  foldM_ impureStepperFunction initial $ repeat ()
  return ()
