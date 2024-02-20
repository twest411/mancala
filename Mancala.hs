module Mancala where
import Debug.Trace
import Data.Maybe

type Bean = Int
type Slot = Bean

data Player = P1 | P2 deriving (Show, Eq, Read, Ord)

data Board = Board {goalP1 :: Slot, slotsP1 :: [Slot],
                    goalP2 :: Slot, slotsP2 :: [Slot], playerTurn :: Player} deriving (Show, Eq, Ord) --add record notation
data Outcome =  Winner Player | Tie deriving (Show, Eq)

board = Board {goalP1 = 0, slotsP1 = [4,4,1,0,4,4], goalP2 = 0, slotsP2 = [4,4,4,4,4,4], playerTurn = P1}

--Show Function
------------------------------------
-- ha ha funny show function: use putStr (showBoard board) when trying to print in ghci
showBoard :: Board -> String
showBoard Board {goalP1 = g1, slotsP1 = s1, goalP2 = g2, slotsP2 = s2, playerTurn = p} =if p == P1 then  "    (6) (5) (4) (3) (2) (1) \n" ++ body else body ++ "    (1) (2) (3) (4) (5) (6)\n"
  where border = "@><><><><><><><@><><><><><><><@\n"
        body = border ++ "|  "++displaySideOne s1++"|  |\n"++ "|"++show g1++    " |-----------------------| "++show g2++"|\n" ++ "|  "++displaySideTwo s2 ++"|  |\n"++ border

-- displayes slotsP1 backwards with border
displaySideOne :: [Slot] -> String
displaySideOne [] = ""
displaySideOne (b:bs) = displaySideOne bs ++ "| "++show b++" "

-- displays slotsP2 with border
displaySideTwo :: [Slot] -> String
displaySideTwo [] = ""
displaySideTwo (b:bs) = "| "++show b++" "++displaySideTwo bs


--Gameplay Functions
----------------------------------

across :: Int -> Int
across x = if x == 7 then 14 else 14 - x

move :: Board -> Int -> Maybe Board
move brd pos = if pos `elem` filtered then Just (makeMove brd pos) else Nothing
  where moves = getSide brd
        filtered = [x | x <- [1..6], not (moves !! (x-1) == 0)]


makeMove :: Board -> Int -> Board
makeMove brd@(Board g1 s1 g2 s2 p) pos =
  let slots = getSide brd
      (startCount, s, beans) = insideMovement slots pos
      newBoard = setSide brd s
      finalSquare = ((pos + startCount - 1) `mod` 13) + 1
      finalBoard = playInGoal newBoard beans 
      newPl = playerTurn finalBoard
  in (checkCapture (finalBoard {playerTurn = p}) finalSquare) {playerTurn = newPl}


playInGoal :: Board -> Bean -> Board
playInGoal brd 0 = brd {playerTurn = opponent (playerTurn brd)}
playInGoal brd 1 = incrementGoal brd
playInGoal brd x
  | x < 1 = brd {playerTurn = opponent (playerTurn brd)}
  | otherwise = let newBrd = incrementGoal brd
                in playOppSide newBrd (x-1)

playOppSide brd x 
  | x <= 0 = brd {playerTurn = opponent (playerTurn brd)} 
  | otherwise = let slots = getOtherSide brd
                    (s, rem) = sideMovement slots 1 x
                    newBrd = setOtherSide brd s
                in playMySide newBrd rem
  
playMySide brd x  
  | x <= 6 = brd {playerTurn = opponent (playerTurn brd)}
  | otherwise = playInGoal newBrd rem
    where slots = getSide brd
          (s, rem) = sideMovement slots 1 x
          newBrd = setSide brd s


opponent :: Player -> Player
opponent P1 = P2
opponent P2 = P1 

-- gets the player's [Slot]
getSide :: Board -> [Slot]
getSide (Board g1 s1 g2 s2 P1) = s1
getSide (Board g1 s1 g2 s2 P2) = s2

setSide :: Board -> [Slot] -> Board
setSide brd newSlots = 
  if playerTurn brd == P1 
  then brd {slotsP1 = newSlots} 
  else brd {slotsP2 = newSlots}
  
updateGoal x (Board g1 s1 g2 s2 P1)  =  (Board (g1+x) s1 g2 s2 P1)
updateGoal x (Board g1 s1 g2 s2 P2)  =  (Board g1 s1 (g2+x) s2 P2)


incrementGoal brd = updateGoal 1 brd
-- gets the other player's [Slot] 
getOtherSide :: Board -> [Slot]
getOtherSide (Board g1 s1 g2 s2 p) = if p == P1 then s2 else s1

setOtherSide :: Board -> [Slot] -> Board
setOtherSide brd newSlots =
  if playerTurn brd == P2
  then brd {slotsP1 = newSlots}
  else brd {slotsP2 = newSlots}


--Moves the Peices on one side of the board, does not delete the starting peice
--needs to be renamed 
sideMovement :: [Slot] -> Int -> Bean -> ([Slot], Bean)
--sideMovement _ _ 0 = error "Shouldnt Have a zero bean input"
sideMovement slots pos beans = splitAndRebuild slots beans


--adds 1 to every index until it runs out of beans or reaches the end of the list
splitAndRebuild :: [Slot] -> Bean -> ([Slot], Bean)
splitAndRebuild slots 0 = (slots, 0)
splitAndRebuild [] x = ([],x)
splitAndRebuild (b:bs) beans = 
  let (newBs, rem) = (splitAndRebuild bs (beans-1))
  in ((b+1):newBs,rem)



--Does operations for the first part of a turn
--needs to be renamed
insideMovement :: [Slot] -> Int -> (Bean,[Slot],Bean)
insideMovement slots pos = 
  case splitAt (pos-1) slots of
  (front, beans:back) -> 
    let (newBack, rem) = sideMovement back (pos) beans 
    in (beans, newFront front ++ [0] ++ newBack, rem)
  --(front, back) -> (newFront front ++ sideMovement back (pos-1) beans, beans-(6-pos))
  _ -> error "this should never happen"
   


-- updates the front of a [Slot] to show that a slot has been emptied
newFront :: [Slot] -> [Slot]
newFront [] = []
newFront (b:bs) = b:(newFront bs)


-- continues distributing beans until it runs out
--executePlay :: Board -> Bean -> Int -> Board
--executePlay (Board g1 s1 g2 s2 p) beans 1 = if beans > 0 then if p == P1 then executePlay (Board (g1+1) s1 g2 s2 p) (beans-1) 4 else executePlay (Board g1 s1 g2 s2 p) (beans) 4 else Board g1 s1 g2 s2 p
--executePlay (Board g1 s1 g2 s2 p) beans 2 = if beans > 0 then executePlay (Board g1 (sideMovement s1 1 beans) g2 s2 p) (beans-6) 1 else Board g1 s1 g2 s2 p
--executePlay (Board g1 s1 g2 s2 p) beans 3 = if beans > 0 then if p == P2 then executePlay (Board g1 s1 (g2+1) s2 p) (beans-1) 2 else executePlay (Board g1 s1 g2 s2 p) (beans) 2 else Board g1 s1 g2 s2 p
--executePlay (Board g1 s1 g2 s2 p) beans 4 = if beans > 0 then executePlay (Board g1 s1 g2 (sideMovement s2 1 beans) p) (beans-6) 3 else Board g1 s1 g2 s2 p

-- check if a capture is possible and if so then caputre else return the board
checkCapture :: Board -> Int -> Board
checkCapture brd pos
  | pos > 6 = brd
  | pos == 0 = error "pos is 0, it shouldnt be"
  | otherwise =  
    let mySide = getSide brd
        otherSide = getOtherSide brd
        (mySide', myCount) = (makePosZero mySide (pos))
        (otherSide', otherCount) = (makePosZero otherSide (7-pos))
        brd' = updateGoal (myCount + otherCount) $ setSide (setOtherSide brd otherSide') mySide'
    in if otherCount /= 0 && myCount == 1 
       then brd'
       else brd

-- does the operations for checkCapture
-- needs to take a FauxBoard
--capture :: Board -> Int -> Board
--capture brd pos =
--  let currentSide = getSide brd
--      otherSide = getOtherSide brd
--      beanTotal = 1 + (otherSide !! (pos) 

-- returns player
getPlayer :: Board -> Player
getPlayer (Board _ _ _ _ p) = p

--returns slotsP1
getSide1 :: Board -> [Slot]
getSide1 (Board _ s1 _ _ _) = s1

--returns slotsP2
getSide2 :: Board -> [Slot]
getSide2 (Board _ _ _ s2 _) = s2

-- changes the value of a slot at a position to 0
makePosZero :: [Slot] -> Int -> ([Slot], Bean)
makePosZero slots pos = 
  case splitAt (pos-1) slots of
      (front, beans:back) -> (front ++ [0] ++ back, beans)
      _ -> error "AAAAAH"

-- returns value of slot at index. First int is starting pos for current player, second int is number moved
boardIndexer :: Board -> Int -> Int -> Int
boardIndexer brd@(Board g1 s1 g2 s2 P1) start move = if start + move <= 6 then s1 !! (start + move)
  else undefined

-- an actual complete turn
{-
turn:: Board -> Int -> Board
turn brd@(Board _ s1 _ s2 P1) pos = if
  where (Board _ cs1 _ cs2 _) = move brd pos
-}
-- GameState Functions
------------------------------------

--takes a board and returns: Turn, Winner, or Tie
updateOutcome :: Board -> Maybe Outcome
updateOutcome board@(Board {slotsP1 = s1, slotsP2 = s2, playerTurn = p}) = if not (sum s1 == 0 || sum s2 == 0) then Nothing else Just $ getWinner board


-- helper function/ unsafe check for winner DONT USE PLEASE IT MIGHT BREAK THINGS
getWinner :: Board -> Outcome
getWinner (Board g1 s1 g2 s2 p)
  | (g1 + sum s1) > (g2 + sum s2) = Winner P1
  | (g2 + sum s2) > (g1 + sum s1) = Winner P2
  | otherwise = Tie

-- takes a board and returns a board with the other player
updateTurn :: Board -> Board
updateTurn Board {slotsP1 = s1, goalP1 = g1, slotsP2 = s2, goalP2 = g2, playerTurn = p} = if p == P1 then Board g1 s1 g2 s2 P2 else Board g1 s1 g2 s2 P1

--zips up a list of slots with its position, POSITION DOES NOT START AT 0
zipSlots :: [Slot] -> [(Slot,Int)]
zipSlots slots = zip slots [1..length slots]

--Checks if it is possible to move from a slot and then returns a list of all possible movement positions
validMoves :: Board -> [Int]
validMoves (Board _ s1 _ _ P1) = [x|(slot,x) <- zipSlots s1, slot /= 0]
validMoves (Board _ _ _ s2 P2) = [x|(slot,x) <- zipSlots s2, slot /= 0]

