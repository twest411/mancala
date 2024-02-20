module Main where
import Mancala
import IO
import Solver
import System.IO
import System.Environment
import Text.Read (readMaybe)
import Data.Char
import System.Console.GetOpt

data Flag = Help | Win | Depth String | Move String | Verbose | File String deriving (Eq,Show)

defaultBoard ="testCases/baseBoard.txt"
defaultDepth = 6

options :: [OptDescr Flag]
options = [Option ['h'] ["help"] (NoArg Help) "Help"
          ,Option ['w'] ["win"] (NoArg Win) "Uses defaults to compute winner"
--          ,Option ['f'] ["file"] (ReqArg File "Path") "Takes a filePath to a board save"
          ,Option ['d'] ["depth"] (ReqArg Depth "#") "Sets depth to #"
          ,Option ['m'] ["move"] (ReqArg Move "#") "Moves Beans at Slot Pos #"
	  ,Option ['v'] ["verbose"] (NoArg Verbose) "Output both move and rating"
	  ]

getDepth :: [Flag] -> Int
getDepth [] = defaultDepth
getDepth ((Depth x):_) =
  case readMaybe x of
    Nothing -> error "invalid depth input"
    Just depth -> depth
getDepth (_:flags) = getDepth flags

getPos :: [Flag] -> Int
getPos [] = 1
getPos ((Move x):_) =
  case readMaybe x of
    Nothing -> error "invalid pos input"
    Just move -> move
getPos (_:flags) = getPos flags


hasVerbose :: [Flag] -> Bool
hasVerbose [] = False
hasVerbose ((Verbose):_) = True
hasVerbose (_:flags) = hasVerbose flags

hasMove :: [Flag] -> Bool
hasMove [] = False
hasMove ((Move _):_) = True
hasMove (_:flags) = hasMove flags

getPath :: [Flag] -> String
getPath [] = defaultBoard
getPath ((File x):_) =
  case readMaybe x of
    Nothing -> error "invalid file input"
    Just path -> path
getPath (_:flags) = getPath flags


main :: IO ()
main = 
  do args <- getArgs
     let (flags, inputs, error) = getOpt Permute options args
     let fName = if null inputs then defaultBoard else head inputs
     brd <- loadGame fName
     if Help `elem` flags || (not $ null error)
     then putStrLn $ usageInfo "Usage: Mancala [options] [file]" options
     else do
       winner <- (chooseAction flags brd)
       return winner


chooseAction :: [Flag] -> Board -> IO ()
chooseAction flags brd
  | Win `elem` flags = putStrLn (show (whoWillWin brd))
  | hasMove flags = 
	case (move brd (getPos flags)) of
	   Nothing -> putStrLn "Invalid Move"
	   Just x -> putStrLn $ showGame x
  | hasVerbose flags = let brd_ = bestMove brd
	in case move brd brd_ of
             Nothing -> putStrLn "Invalid Something"
	     Just x -> do putStrLn ("The best move is " ++ show brd_ ++ " and its outcome is:")
                          putWinner x
  | otherwise = putStrLn (show(bestMoveBounded brd (getDepth flags)))
