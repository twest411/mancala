module IO where
import Mancala
import Solver
import System.Console.GetOpt

-- "0 1 2 3 4 5 6 0 1 2 3 4 5 6 P1"


readGame :: String -> Maybe Board
readGame str = readGameHelp (map (\x -> read x :: Slot)(init split)) (read (last split) :: Player)
        where split = words str

readGameHelp [g1, a, b, c, d, e, f, g2, h, i, j, k, l, m] P1 = Just (Board g1 [a, b, c, d, e, f] g2 [h, i, j, k, l, m] P1)
readGameHelp [g1, a, b, c, d, e, f, g2, h, i, j, k, l, m] P2 = Just (Board g1 [a, b, c, d, e, f] g2 [h, i, j, k, l, m] P2)
readGameHelp _ _ = Nothing


showGame :: Board -> String
showGame (Board g1 lst g2 lst2 P1) = concat [(show g1), " ", showGameHelp lst, (show g2), " ", showGameHelp lst, (show P1)]
showGame (Board g1 lst g2 lst2 P2) = concat [(show g1), " ", showGameHelp lst, (show g2), " ", showGameHelp lst, (show P2)]

showGameHelp [] = []
showGameHelp (x:xs) = concat[(show x), " ", showGameHelp xs]


writeGame :: Board -> FilePath -> IO ()
writeGame brd path = writeFile path (showGame brd)


loadGame :: FilePath -> IO Board
loadGame path = 
  do brd <- readFile path
     case (readGame brd) of
	Nothing -> error "No Board"
	Just board -> return board


-- can complete when the algorithm is complete
putWinner :: Board -> IO ()
putWinner brd = case  (updateOutcome brd) of
		   Just (Winner P2) -> putStrLn "P2 Wins"
		   Just (Winner P1) -> putStrLn "P1 Wins"
		   Just Tie -> putStrLn "Tie"
		   Nothing -> putStrLn ("Rating of " ++ show (getBoardState brd))
