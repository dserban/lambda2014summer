import Data.List            ( replicate
                            , transpose
                            , elemIndices
                            )
import Data.List.Split      ( chunksOf )
import Text.Printf          ( printf )
import System.Random        ( randomRIO )
import System.Console.ANSI  ( clearScreen )
import Control.Monad.Loops  ( iterateM_ )
import Control.Concurrent   ( threadDelay )
import System.Exit          ( exitSuccess )

type GameState = [[Maybe Char]]

data BoardSquare = A1 | B1 | C1
                 | A2 | B2 | C2
                 | A3 | B3 | C3 deriving (Enum, Show)

fromIntToBoardSquare :: Int -> BoardSquare
fromIntToBoardSquare = toEnum

showBoard :: GameState -> String
showBoard gameState = showColumns ++ (showFullLine 0) ++ (showFullLine 1) ++ (showFullLine 2)
                  where showRow line = (concat $ "·" : map showCell line) ++ showDelimiter
                        showCell Nothing = "   ·" 
                        showCell (Just x) = printf " %c ·" x
                        showColumns = "  · a · b · c ·" ++ showDelimiter
                        showDelimiter = "\n· ·············\n"
                        prepareFullBoard = zip3 (map show ([1,2,3] :: [Int])) [" ", " ", " "] (map showRow gameState) 
                        showFullLine index = first (current index) ++ second (current index) ++ third (current index)
                        current index = prepareFullBoard !! index
                        first (x, _, _) = x
                        second (_, y, _) = y
                        third (_, _, z) = z
                 
initialGameState :: GameState
initialGameState = chunksOf 3 $ replicate 9 Nothing

placeCell :: GameState -> Int -> Char -> GameState
placeCell gameState newIndex player = 
                          if (concat gameState) !! newIndex == Nothing 
                            then placeCellWherever gameState newIndex player
                            else gameState
                          where placeCellWherever board index choose = chunksOf 3 $ addToBoard (concat board) index choose
                                addToBoard vect ind value = let h = head vect
                                                                t = tail vect
                                                            in if ind == 0 then  (Just value) : t
                                                               else h : addToBoard t (ind - 1) value
                                                              
winning :: Char -> GameState -> Bool 
winning character gameState = row || column || diagonal                   
                       where row = straightLine character gameState
                             column = straightLine character $ transpose gameState
                             diagonal = let vect = concat gameState
                                        in (vect !! 0 == vect !! 4 && vect !! 4 == vect !! 8 && vect !! 8 == Just character) 
                                        || (vect !! 2 == vect !! 4 && vect !! 4 == vect !! 6 && vect !! 6 == Just character) 
                             straightLine c board = 1 == length (filter (\l -> length (filter (== Just c) l) == 3) board)
                                                                               
isWon :: GameState -> Bool
isWon = winning 'X'

isLost :: GameState -> Bool
isLost = winning 'O'

isTie :: GameState -> Bool
isTie gameState = not $ isWon gameState || isLost gameState || Nothing `elem` (concat gameState)

squaresOf :: [Maybe Char] -> Maybe Char -> [BoardSquare] 
squaresOf list player = squaresOfHelper list 0
          where squaresOfHelper vect index = let h = head vect
                                                 t = tail vect
                                                 square = fromIntToBoardSquare index
                                             in if null vect then [] 
                                                else if h == player then square : (squaresOfHelper t (index + 1)) 
                                                     else squaresOfHelper t (index + 1)
              
randomIndex :: GameState -> IO Int
randomIndex board = do
    let nothingIndices = Nothing `elemIndices` (concat board)
    indexNothingIndices <- randomRIO (0, (length nothingIndices) - 1)  :: IO Int
    return (nothingIndices !! indexNothingIndices)
             
pureStepperFunction :: Maybe String -> GameState -> GameState
pureStepperFunction maybeKeyboardInput gameState
  | maybeKeyboardInput == Just "a1" = placeCell gameState 0 'X'
  | maybeKeyboardInput == Just "a2" = placeCell gameState 3 'X'
  | maybeKeyboardInput == Just "a3" = placeCell gameState 6 'X'
  | maybeKeyboardInput == Just "b1" = placeCell gameState 1 'X'
  | maybeKeyboardInput == Just "b2" = placeCell gameState 4 'X'
  | maybeKeyboardInput == Just "b3" = placeCell gameState 7 'X'
  | maybeKeyboardInput == Just "c1" = placeCell gameState 2 'X'
  | maybeKeyboardInput == Just "c2" = placeCell gameState 5 'X'
  | maybeKeyboardInput == Just "c3" = placeCell gameState 8 'X'
  | otherwise = gameState

outputBoardToTheConsole :: GameState -> Char -> IO ()
outputBoardToTheConsole gameState player = do
  clearScreen
  putStrLn $ showBoard gameState
  putStrLn ""
  putStrLn ""
  putStrLn $ "X: " ++ (show $ squaresOf (concat gameState) (Just 'X'))
  putStrLn $ "O: " ++ (show $ squaresOf (concat gameState) (Just 'O'))
  putStrLn ""
  putStrLn $ if player == 'X' 
             then "Your turn, human! Select one of " ++ (show (squaresOf (concat gameState) Nothing))
             else "I'm thinking ... "

getCorrectCommand :: GameState -> IO GameState
getCorrectCommand currentGameState = do
    keyboardInput <- getLine
    let fictiveNextState = pureStepperFunction (Just keyboardInput) currentGameState
    if fictiveNextState == currentGameState then getCorrectCommand currentGameState 
    else return fictiveNextState

impureStepperFunction :: GameState -> IO GameState
impureStepperFunction currentGameState = do
    clearScreen
    outputBoardToTheConsole currentGameState 'X'
    nextStateX <- getCorrectCommand currentGameState
    outputBoardToTheConsole nextStateX 'O'
    if isWon nextStateX
      then do
        putStrLn "     GAME OVER - X won"
        putStrLn ""
        exitSuccess
      else do
        if isTie nextStateX
        then do
          putStrLn "     GAME OVER - Tie"
          putStrLn ""
          exitSuccess
        else do
          putStrLn ""
          putStrLn ""
          threadDelay 400000
          newIndex <- randomIndex nextStateX
          let nextStateO = placeCell nextStateX newIndex 'O'
          outputBoardToTheConsole nextStateO 'X'
          if isLost nextStateO
            then do
              putStrLn "     GAME OVER - O won"
              putStrLn ""
              exitSuccess
            else do
              return nextStateO
                                                                  
main :: IO ()
main = do
  clearScreen
  iterateM_ impureStepperFunction initialGameState

