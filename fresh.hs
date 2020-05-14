import Data.List
import Text.Read
import Text.Printf
data Board = Board { misses :: [[Int]], shiploc :: [[Int]], hitlist :: [Bool] }
  deriving (Eq, Show)
initBoard = Board {misses = [[]], shiploc = [[1,1],[1,2],[1,3],[1,4]], hitlist = map (\shiploc -> False) [1,1,1,1]}

printBoard :: Board -> [Int] -> IO()
printBoard board coord = do
  if (coord!!1 == 0 && coord!!0 /= 5)
    then putStr (show (coord!!0))
    else putStr $ ""
  if (coord!!0 > 4)
    then return()
    else do
      if (coord!!1 == 4)
        then do
          putStrLn $ getOutput board coord
          printBoard board [(coord!!0)+1, 0]
        else do
          putStr $ getOutput board coord
          printBoard board [coord!!0, (coord!!1) + 1]
      return()

getOutput :: Board -> [Int] -> String
getOutput board coord =
  if elem coord (misses board)
    then "0"
    else
      if (elem coord (shiploc board))
        then
          case elemIndex coord (shiploc board) of
            Just n ->
              if (hitlist board)!!n == True
                then "X"
                else "*"
            Nothing -> "*"
        else "*"

isSunk :: Board -> Bool
isSunk board = not (elem False (hitlist board))

shoot :: Board -> [Int] -> Board
shoot board coord =
  if elem coord (shiploc board)
    then
      case elemIndex coord (shiploc board) of
        Just n ->
          Board {misses = (misses board), shiploc = (shiploc board), hitlist = (take n (hitlist board)) ++ [True] ++ (drop (n+1) (hitlist board))}
        Nothing ->
          Board {misses = (misses board) ++ [coord], shiploc = (shiploc board), hitlist = (hitlist board)}
    else Board {misses = (misses board) ++ [coord], shiploc = (shiploc board), hitlist = (hitlist board)}

getCoord x = do
  putStrLn x
  input <- getLine
  return input


readInt s = case readMaybe s :: Maybe Int of
  Just s -> s
  Nothing -> 5

updateBoard board = do
  putStrLn $ " 01234"
  printBoard board [0,0]
  putStrLn $ ""
  if isSunk board
    then do
      putStrLn $ "You Win!"
      return()
    else do
      x <- getCoord "Input Column Number: "
      if (x == "q")
        then do
          putStrLn $ "Goodbye!"
          return()
        else do
          putStrLn $ ""
          if (readInt x) <= 4 && (readInt x) >= 0
            then do
              y <- getCoord "Input Row Number: "
              if (y /= "q")
                then do
                  if (readInt y) <= 4 && (readInt y) >= 0
                    then do
                      updateBoard (shoot board [readInt y,readInt x])
                    else do
                      putStrLn "Input Not Valid!"
                      updateBoard board
                else do
                  putStrLn $ "Goodbye!"
                  return()
            else do
              putStrLn "Input Not Valid!"
              updateBoard board

main :: IO()
main = do
  putStrLn $ ""
  putStrLn $ "Sink The Battleship! Type 'q' to quit."
  putStrLn $ ""
  updateBoard initBoard
