--Creation of a rudimentary Mario type game

--aam71
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
import Prelude
import Random
import Control.Concurrent
import Control.Monad
import Data.Maybe
import Data.List
import System.IO

--import Control.Monad.Random.Class



--q1
data Cell = Empty | X | O
    deriving (Eq, Show)
type Board = [[Cell]]
type Move = (Int, Int)
data Outcome = XWin | OWin | Draw
    deriving (Eq, Show)

--q2
showCell :: Cell -> String
showCell Empty = " "
showCell X = "X"
showCell O = "O"

printBoard :: Board -> IO ()
printBoard board = putStrLn $ unlines
  [ "+---+---+---+"
  , "| " ++ showCell (head (head board)) ++ " | " ++ showCell (head board !! 1) ++ " | " ++ showCell (head board !! 2) ++ " |"
  , "+---+---+---+"
  , "| " ++ showCell (head (board !! 1)) ++ " | " ++ showCell (board !! 1 !! 1) ++ " | " ++ showCell (board !! 1 !! 2) ++ " |"
  , "+---+---+---+"
  , "| " ++ showCell (head (board !! 2)) ++ " | " ++ showCell (board !! 2 !! 1) ++ " | " ++ showCell (board !! 2 !! 2) ++ " |"
  , "+---+---+---+"
  ]

--q3
updateCell :: Board -> Int -> Int -> Cell -> Board
updateCell board x y player =
  take x board ++ [take y (board !! x) ++ [player] ++ drop (y + 1) (board !! x)] ++ drop (x + 1) board

-- playerTurn returns the player whose turn it currently is
playerTurn :: Board -> Cell
playerTurn board = if length xs > length os then O else X
  where xs = filter (\case X -> True; _ -> False) flatBoard
        os = filter (\case O -> True; _ -> False) flatBoard
        flatBoard = concat board

legalMove :: Board -> Move -> Maybe Board
legalMove board (x, y)
  | x < 0 || x > 2 || y < 0 || y > 2 = Nothing
  | cellIsEmpty (board !! x !! y) = Just (updateCell board x y currentPlayer)
  | otherwise = Nothing
  where currentPlayer = playerTurn board
        cellIsEmpty Empty = True
        cellIsEmpty _ = False


--q4

type Row = [Cell]

checkRow :: Row -> Maybe Outcome
checkRow [X,X,X] = Just XWin
checkRow [O,O,O] = Just OWin
checkRow _ = Nothing

checkRows :: Board -> Maybe Outcome
checkRows [r1,r2,r3] = case map checkRow [r1,r2,r3] of
  [Just XWin, _, _] -> Just XWin
  [_, Just OWin, _] -> Just OWin
  _ -> Nothing
checkRows _ = Nothing

checkColumns :: Board -> Maybe Outcome
checkColumns board = checkRows (transpose board)

checkDiagonals :: Board -> Maybe Outcome
checkDiagonals [[a,_,c],[_,b,_],[d,_,e]] = case [a,b,e] of
  [X,X,X] -> Just XWin
  [O,O,O] -> Just OWin
  _ -> case [c,b,d] of
    [X,X,X] -> Just XWin
    [O,O,O] -> Just OWin
    _ -> Nothing
checkDiagonals _ = Nothing

checkDraw :: Board -> Maybe Outcome
checkDraw board = if all (\c -> case c of Empty -> False; _ -> True) (concat board) then Nothing else Just Draw


checkBoard :: Board -> Maybe Outcome
checkBoard board = case catMaybes [checkRows board, checkColumns board, checkDiagonals board, checkDraw board] of
  [result] -> Just result
  _ -> Nothing

--q5
humanPlayer :: Chan Move -> Board -> IO ()
humanPlayer moveChan board = do
  printBoard board
  putStr $ "Player " ++ show (playerTurn board) ++ ", enter your move (row column): "
  hFlush stdout
  line <- getLine
  case words line of
    [xStr, yStr] -> case legalMove board (read xStr, read yStr) of
      Just newBoard -> writeChan moveChan (read xStr, read yStr)
      Nothing -> do
        putStrLn "Invalid move. Please try again."
        humanPlayer moveChan board
    _ -> do
      putStrLn "Invalid input. Please enter two space-separated numbers."
      humanPlayer moveChan board

--q6

botPlayer :: Chan Move -> Board -> IO ()
botPlayer ch board = do
  let moves = [(x, y) | x <- [0..2], y <- [0..2], isNothing (legalMove board (x, y))]
  case moves of
    [] -> putStrLn "No valid moves."
    [move] -> writeChan ch move
    _ -> do
      let goodMoves = filter (isGoodMove board) moves
      case goodMoves of
        [] -> writeChan ch (head moves)
        _ -> do
          i <- pickRandom (0, length goodMoves - 1)
          writeChan ch (goodMoves !! i)



isGoodMove :: Board -> Move -> Bool
isGoodMove board (x, y) = any (\r -> length (filter (== r) row) == 2) [X,O]
                      || any (\c -> length (filter (== c) col) == 2) [X,O]
                      || length (filter (uncurry (==)) diag1) == 2
                      || length (filter (\(r,c) -> r + c == 2) diag2) == 2
  where row = board !! x
        col = map (!! y) board
        diag1 = [(i,i) | i <- [0..2]]
        diag2 = [(i,2-i) | i <- [0..2]]


--q7



-- Game manager process-- Human player
humanplayer :: Chan Move -> IO ()
humanplayer ch = do
  putStrLn "Enter your move in the format 'x y' (0-indexed):"
  input <- getLine
  let [x,y] = words input
  writeChan ch (read x, read y)

-- Bot player
botplayer :: Chan Move -> Board -> IO ()
botplayer ch board = do
  let moves = [(x,y) | x <- [0..2], y <- [0..2], isJust (legalMove board (x,y))]
  move <- if length moves == 1
            then return (head moves)
            else pickRandom moves
  writeChan ch move


gameManager :: Board -> Chan Move -> Chan Move -> IO ()
gameManager board player1 player2 = do
  printBoard board
  let currentPlayer = playerTurn board
  move <- if currentPlayer == X
            then readChan player1
            else readChan player2
  case legalMove board move of
    Just newBoard -> do
      let outcome = checkBoard newBoard
      if isJust outcome
        then do
          printBoard newBoard
          putStrLn $ "Game over. " ++ show (fromJust outcome)
        else do
          gameManager newBoard player2 player1
    Nothing -> do
      putStrLn "Illegal move. Try again."
      threadDelay 1000000
      gameManager board player1 player2


--q8
gameStart :: (Board -> IO Move) -> (Board -> IO Move) -> IO ()
gameStart player1 player2 = do
  boardChan <- newChan
  moveChan <- newChan
  let board0 = replicate 3 (replicate 3 Empty)
  writeChan boardChan board0
  _ <- forkIO (playerLoop player1 boardChan moveChan X)
  _ <- forkIO (playerLoop player2 boardChan moveChan O)
  gameLoop boardChan moveChan

playerLoop :: (Board -> IO Move) -> Chan Board -> Chan Move -> Cell -> IO ()
playerLoop player boardChan moveChan playerCell = do
  board <- readChan boardChan
  move <- player board
  writeChan moveChan move
  playerLoop player boardChan moveChan playerCell

gameLoop boardChan moveChan = do
  board <- readChan boardChan
  case checkBoard board of
    Just outcome -> do
      printBoard board
      putStrLn $ case outcome of
        XWin -> "X wins!"
        OWin -> "O wins!"
        Draw -> "Draw!"
    Nothing -> do
      printBoard board
      move <- readChan moveChan
      case legalMove board move of
        Just newBoard -> do
          writeChan boardChan newBoard
          gameLoop boardChan moveChan
        Nothing -> do
          putStrLn "Illegal move, try again."
          gameLoop boardChan moveChan

