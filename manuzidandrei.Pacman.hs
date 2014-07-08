import           Data.Set            ( Set )
import qualified Data.Set            as S
import           Data.List           ( intercalate )
import           Control.Concurrent  ( threadDelay )
import           System.IO           ( hFlush
                                     , stdout
                                     )
import           System.Console.ANSI ( clearScreen )
import           System.Exit         ( exitSuccess )
import           Control.Monad.Loops

data BoardSquare = A1 | A2 | A3 | A4 | A5
                 | B1                | B5
                 | C1                | C5
                 | D1                | D5
                 | E1 | E2 | E3 | E4 | E5
                 | F1                | F5
                 | G1                | G5
                 | H1                | H5
                 | I1 | I2 | I3 | I4 | I5 
                 | J1                | J5
                 | K1                | K5
                 | L1                | L5
                 | M1 | M2 | M3 | M4 | M5 
                 deriving (Eq,Ord,Show)

data PacmanGhostDanger =
  MkPacmanGhostDanger { pLocationPGD :: BoardSquare
                      , gLocationPGD :: BoardSquare
                      , dangerPGD    :: Bool
                      }

data GameState =
  MkGameState { pLocation       :: BoardSquare
              , gLocation       :: BoardSquare
              , danger          :: Bool
              , pelletLocations :: Set BoardSquare
              , score           :: Int
              , stage           :: Bool
              } deriving Eq

pacmanInfiniteList :: [BoardSquare]
pacmanInfiniteList = cycle fullBackAndForthPacmanPath
  where
    incompletePacmanForwardPath =
      [A5,A4,A3,A2,A1,B1,C1,D1,E1,E2,E3,E4,E5,F5,G5,H5,I5,I4,I3,I2,I1,J1,K1,L1,M1,M2,M3,M4,M5]
    fullBackAndForthPacmanPath =
      incompletePacmanForwardPath ++
      reverse incompletePacmanForwardPath

ghostInfiniteList :: [BoardSquare]
ghostInfiniteList = cycle [M1,M2,M3,M4,M5,L5,J5,I5,H5,G5,F5,E5,D5,C5,B5,A5,A4,A3,A2,A1,B1,C1,D1,E1,F1,G1,H1,I1,J1,K1]

dangerInfiniteList :: Int -> [Bool]
dangerInfiniteList howOften = map ((== 0) . (`mod` howOften)) [1..]

pGDInfiniteList :: Int -> [PacmanGhostDanger]
pGDInfiniteList howOften = zipWith3 MkPacmanGhostDanger
                                    pacmanInfiniteList
                                    ghostInfiniteList
                                    (dangerInfiniteList howOften)

initialGameState :: GameState
initialGameState = MkGameState A5 M1 False S.empty 0 True

advanceToNextFrame :: GameState -> PacmanGhostDanger -> GameState
advanceToNextFrame gameState pgd =
  let
    pelletLocationsAtStage1 = pelletLocations gameState
    scoreAtStage1           = score gameState
    finalPLocation          = pLocationPGD pgd
    finalGLocation          = gLocationPGD pgd
    finalDanger             = dangerPGD pgd
    pelletCountAtStage1     = S.size pelletLocationsAtStage1
    pelletLocationsAtStage2 = S.delete finalPLocation pelletLocationsAtStage1
    pelletCountAtStage2     = S.size pelletLocationsAtStage2
    finalScore              = scoreAtStage1 +
                              pelletCountAtStage1 -
                              pelletCountAtStage2
    finalStage              = if pLocation gameState == A5 then True
                              else if pLocation gameState == M5 then False 
                              else stage gameState                         
    pelletLocationsAtStage3 = if finalDanger
                                then S.insert finalGLocation pelletLocationsAtStage2
                                else pelletLocationsAtStage2
  in
    MkGameState finalPLocation
                finalGLocation
                finalDanger
                pelletLocationsAtStage3
                finalScore
                finalStage

gameStateInfiniteList :: Int -> [GameState]
gameStateInfiniteList howOften = tail $ scanl advanceToNextFrame
                                              initialGameState
                                              (pGDInfiniteList howOften)

visualizeDirection :: BoardSquare -> GameState -> String
visualizeDirection boardSquare gameState = if (not $ stage gameState) then
                                            if boardSquare `elem` [A1, A2, A3, A4, A5, I1, I2, I3, I4] then "▶"  
                                            else if boardSquare `elem` [E2, E3, E4, E5, M2, M3, M4, M5] then "◀"
                                             else "▲"
                                           else
                                            if boardSquare `elem` [A1, A2, A3, A4, A5, I1, I2, I3, I4] then "◀"  
                                            else if boardSquare `elem` [E2, E3, E4, E5, M2, M3, M4, M5] then "▶"
                                             else "▼"
                                             
visualizeOneBoardSquare :: GameState -> BoardSquare -> String
visualizeOneBoardSquare gameState boardSquare =
  let
    phase1 = "·"
    phase2 = if S.member boardSquare (pelletLocations gameState)
               then "○"
               else phase1
    phase3 = if boardSquare == pLocation gameState
               then visualizeDirection boardSquare gameState
               else phase2
    phase4 = if boardSquare == gLocation gameState
               then "G"
               else phase3
    phase5 = if boardSquare == gLocation gameState && danger gameState
               then "⚡"
               else phase4
  in phase5

visualizeRowOfFiveBoardSquares
  :: GameState
     -> BoardSquare
     -> BoardSquare
     -> BoardSquare
     -> BoardSquare
     -> BoardSquare
     -> String
visualizeRowOfFiveBoardSquares gameState bsq1 bsq2 bsq3 bsq4 bsq5 =
  "     " ++ (intercalate "  " $ map (visualizeOneBoardSquare gameState) [bsq1, bsq2, bsq3, bsq4, bsq5])

visualizeRowOfTwoBoardSquares
  :: GameState
     -> BoardSquare
     -> BoardSquare
     -> String
visualizeRowOfTwoBoardSquares gameState bsq1 bsq2 =
  "     " ++ (intercalate "           " $ map (visualizeOneBoardSquare gameState) [bsq1, bsq2])

visualizeTheEntireBoard :: GameState -> [String]
visualizeTheEntireBoard gameState =
  [ visualizeRowOfFiveBoardSquares gameState A1 A2 A3 A4 A5
  , visualizeRowOfTwoBoardSquares  gameState B1 B5
  , visualizeRowOfTwoBoardSquares  gameState C1 C5
  , visualizeRowOfTwoBoardSquares  gameState D1 D5
  , visualizeRowOfFiveBoardSquares gameState E1 E2 E3 E4 E5
  , visualizeRowOfTwoBoardSquares  gameState F1 F5
  , visualizeRowOfTwoBoardSquares  gameState G1 G5
  , visualizeRowOfTwoBoardSquares  gameState H1 H5
  , visualizeRowOfFiveBoardSquares gameState I1 I2 I3 I4 I5
  , visualizeRowOfTwoBoardSquares  gameState J1 J5
  , visualizeRowOfTwoBoardSquares  gameState K1 K5
  , visualizeRowOfTwoBoardSquares  gameState L1 L5
  , visualizeRowOfFiveBoardSquares gameState M1 M2 M3 M4 M5
  ]

outputBoardAndScoreToTheConsole :: GameState -> IO ()
outputBoardAndScoreToTheConsole gameState = do
  clearScreen
  putStrLn ""
  putStrLn ""
  putStrLn ""
  mapM_ putStrLn $ visualizeTheEntireBoard gameState
  putStrLn ""
  putStrLn $ "     Score: " ++ show (score gameState)
  putStrLn ""

pureStepperFunction :: Int -> GameState -> GameState
pureStepperFunction howOften gameState = let allStates = gameStateInfiniteList howOften
                                             howMany = countElementsUpTo gameState allStates
                                         in head $ drop howMany allStates
                                         where countElementsUpTo val list = if val == head list then 1 
                                                                            else 1 + countElementsUpTo val (tail list)
  
impureStepperFunction :: Int -> GameState -> IO GameState
impureStepperFunction howOften currentGameState = do
    outputBoardAndScoreToTheConsole currentGameState
    if pLocation currentGameState == gLocation currentGameState && danger currentGameState
      then do
        putStrLn "     GAME OVER"
        putStrLn ""
        exitSuccess
      else do
        putStrLn ""
        putStrLn ""
        threadDelay 400000
    return $ pureStepperFunction howOften $ currentGameState
        
main :: IO ()
main = do
  clearScreen
  putStrLn ""
  putStr "     Ghost becomes toxic every how many clock ticks: "
  hFlush stdout
  input <- getLine
  let howOften = read input :: Int
  outputBoardAndScoreToTheConsole initialGameState
  putStrLn ""
  putStrLn ""
  threadDelay 2000000
  iterateM_ (impureStepperFunction howOften) initialGameState
