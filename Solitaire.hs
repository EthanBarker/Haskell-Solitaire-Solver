--List of imports
import Data.Char
--List of imports
import Data.Char
import Data.List
import Data.Maybe
import System.Random
import Debug.Trace

--Data Types
data Suit = Club | Diamond | Heart | Spade   deriving (Show,Enum,Eq,Ord) -- The 4 suit types from a deck
data Pip = Two | Three | Four | Five | Six | Seven
          | Eight | Nine | Ten | Jack | Queen
          | King | Ace  deriving (Show, Enum,Eq,Ord,Bounded) -- All the possible pips 
type Card = (Suit, Pip) -- Defined the Card as tuple of a Suit and a Pip
type Deck = [Card] --Defined as a list of cards

--Datatypes for an eight-off board
type Columns = [Deck] -- 8 Columns 
type Reserves = [Card] -- 8 Cells
type Foundations = [Deck] -- 4 Piles
type EOBoard = (Columns, Reserves, Foundations) -- EOBoard containing the three types

--Datatypes for an spider board
type SColumns = [Deck] -- 8 Columns
type Stock = [Card] --8 Cells
type SFoundations = [Deck] -- 4 Piles
type SBoard = (SColumns, Stock, SFoundations) --SBoard contraining the three types

--datatype for Board which involves EOBoard and SBoard
data Board = EOBoard (Columns, Reserves, Foundations) | SBoard (SColumns, Stock, SFoundations) deriving (Eq) 
--This is a helper show instance to explain if a card is in coumns, reserves or foundations.
instance Show Board where
  show (EOBoard (columns,reserves,foundation)) = "Columns:" ++ show columns ++ "Reserves:" ++ show reserves ++ "Foundations:" ++ show foundation
  show (SBoard (scolumns,stock,sfoundation)) = "Columns:" ++ show scolumns ++ "Stock:" ++ show stock ++ "Foundations:" ++ show sfoundation
--This is the definition for myBoard that is used later on for testing
myEOBoard = eoDeal 4333
mySBoard = sDeal 1234

--Builds the pack for EOSolitaire by list comprehention
pack :: Deck 
pack = [(x,y) | x <- [Club .. Spade], y <- [Two .. Ace]]

--Builds the pack for spider solitaire, doubles the size of the pack built also by list comp
sPack :: Deck
sPack = concatMap (replicate 2 ) [(x,y) | x <- [Club .. Spade], y <- [Two .. Ace]]

--Shows the next card from the current one
sCard :: Card -> Card
sCard a =(fst a ,sPip (snd a))
--Finds the pip for the next card
sPip :: Pip -> Pip
sPip Ace = Two --Loops the enums
sPip a = succ a

--Shows the previous card from the current one
pCard :: Card -> Card
pCard a =(fst a ,pPip (snd a))
--Finds the pip for the previous card
pPip :: Pip -> Pip
pPip Two = Ace -- Loops the enums
pPip a = pred a

-- Function for if a card is an ace
isAce :: Card -> Bool
isAce a = snd a == Ace --Checks pip is an ace

-- Function for if a card is an king
isKing:: Card -> Bool
isKing a =snd a == King --Checks pip is a king

--Shuffle the pack of Cards with help from system.random
shuffle:: Int -> Deck
cmp (x1,y1) (x2,y2) = compare y1 y2
shuffle n = [pack | (pack,n) <- sortBy cmp (zip pack ((randoms (mkStdGen n)) :: [Int]))] 

-- Deals the opening board for EOSoltiaire
eoDeal :: Int -> Board
eoDeal n = (EOBoard(columns,reserves,foundation)) where 
     shuffledDeck = shuffle n --Shuffles the deck
     columns = splitDeck (drop 4 shuffledDeck) --Puts all of the rest of the cards into the columns
     reserves = take 4 shuffledDeck -- Takes 48 cards into the reserves
     foundation = [] --Sets the foundations to an empty list

--Splits the Deck in half
splitDeck :: Deck -> [Deck]
splitDeck [] = []-- If a list is empty return a empty list
splitDeck deck = n :splitDeck m  where 
       (n, m) = splitAt mid deck where -- Function to split the deck in half
              mid = 6

-- Deals the opening board for SpiderSolitaire
sDeal :: Int -> Board
sDeal n = (SBoard(scolumn, stock, sfoundation)) where
     shuffledDeck = shuffle n --Shuffles the deck the same way
     stock = drop 100 shuffledDeck  --Takes 100 cards into the stock
     sfoundation = [] --Empty foundation again
     scolumn = splitDeck (drop 4 shuffledDeck) --Puts the rest of the cards into the columns

--Recursively finds the top deck in a list of decks
getHead :: [Deck] -> Deck
getHead [] = []-- Base case returns an empty list
getHead (x:_) = x -- Or returns the first value

--Recursively finds the top card in a list of cards
getHead2 :: [Card] -> Card
getHead2 [] = (Club, Two) -- Base case returns the first card
getHead2 (x:_) = x -- Or returns the first value

--Function to check if a card can move by checking the card below is smaller than the current one
canMove :: [Deck] -> Card -> Bool
canMove foundation card = isAce card || elem (pCard card) (map head foundation) -- is the card an ace or is the card on the foundation the smaller card in that suit

--Helper function that moves ace to foundations
moveAceFoundation :: Card -> Board -> Board
moveAceFoundation card (EOBoard(columns,reserves,foundation))
     |isAce card = (EOBoard(removeColumns, delete card reserves, ([card]:foundation))) -- If the card is an ace it moves to an foundation
     |otherwise = moveCardFoundations card (EOBoard(columns,reserves,foundation)) -- if it isnt an ace it referes to the moveCardFoundations function
       where removeColumns = (map (delete card) columns) -- Function to remove a card from a column

--Second helper function that moves other cards to the foundations
moveCardFoundations :: Card -> Board -> Board
moveCardFoundations card (EOBoard(columns,reserves,foundation))
   |(EOBoard(columns,reserves,foundation)) == (EOBoard(columns,reserves,[])) = (EOBoard(columns,reserves,foundation)) -- If foundations is empty return the EOBoard
   |getHead2 (getHead foundation) == pCard card = (EOBoard(removeColumns, delete card reserves, ([card]: foundation))) -- If the foundations lisy is the previous card you can plase the current card ontop of it.
   |otherwise = (EOBoard( newColumns , newReserves , front: newFoundation)) -- Othewise add the card to a new foundation
   where removeColumns = (map (delete card) columns) -- Function to remove a card from a column
         (front:rest) = foundation -- Splits foundations into the front card and the rest as a list
         (EOBoard(newColumns,newReserves,newFoundation)) = moveCardFoundations card (EOBoard(columns,reserves,rest)) --Recursively calls this function to do more than one card

--Function that returns all of the possible moves to the foundations
toFoundations :: Board -> Board
toFoundations (EOBoard(columns, reserves, foundation))
    |any (canMove foundation) (getHead columns ++ reserves) = toFoundations (EOBoard(newColumns,newReserves,newFoundation)) -- gets the heads of the colums and reserves and sees if this can be moves
    |otherwise = (EOBoard(columns,reserves,foundation)) --otherwise return the EOBoard
    where (EOBoard(newColumns,newReserves,newFoundation)) = foldr (moveAceFoundation) (EOBoard(columns,reserves,foundation)) (getHead columns ++ reserves) -- Recursively calls toFoundations so it repeats more than once

--Helper function to move a the card to reserves, creates a newReserves and newColumns list with the changes
columnToReserve :: Card -> Board -> Board
columnToReserve card (EOBoard(columns, reserves, foundation)) = (EOBoard(newColumns, newReserves, foundation))     
     where newColumns = if (length reserves < 5) then (map (delete card) columns) else columns -- Removes the card from the columns list
           newReserves = if (length reserves < 5) then (card : reserves) else reserves -- Adds the card to the reserves list

--Moves a card from reserves to a column
reserveToColumn :: Card -> Board -> Board
reserveToColumn card (EOBoard(columns, reserves, foundation))
   |(isKing card) && (length columns < 8) = (EOBoard(columns, card : reserves, foundation)) -- If the card is a king can the column isnt full adds card to column from reserves
   |otherwise = (EOBoard(newColumns, newReserves, foundation)) -- Else place in a new column or a new reserve
     where newColumns = if (length reserves < 8) then (map (delete card) columns) else columns --Same idea of remove a card from a columns list and adds it to a new one
           newReserves = if (length reserves < 8) then ( reserves)  else reserves --Same idea as before with removing a card from the reserves list

--Moves a card from a column to a column
columnToColumn :: Card -> Board -> Board
columnToColumn card (EOBoard(columns, reserves, foundation))
   |(isKing card) && (length columns < 8) = (EOBoard([card]:columns,reserves, foundation))-- If the card is a king can the column isnt full adds card to column from a diffent column
   |otherwise = (EOBoard(newColumns, reserves, foundation)) --Else place in a new column
     where newColumns = if (length reserves < 8) then (map (delete card) columns) else columns -- Sets the newColumns if length < 8 then move the card from one column to another

--Finds all the possible moves
findMoves:: Board -> [Board]
findMoves (EOBoard(columns, reserves, foundation)) = colToRes ++ resToCol ++ colToCol -- Creates a list of all possible moves
    where possibleColumnToColumn = [move | move <- (map getHead2 columns), elem (sCard move) (map getHead2 columns)] --Creates possible moves from columns to columns
          possibleReserveToColumn = [move | move <- reserves, elem (sCard move) (map getHead2 columns)] -- Creates possible moves from reserves to columns
          colToRes = filter (/= (EOBoard(columns, reserves, foundation))) (map (\x -> columnToReserve x (EOBoard(columns, reserves, foundation))) (map getHead2 columns)) --filters though the current board and moves a column to a reserve
          resToCol = filter (/= (EOBoard(columns, reserves, foundation))) (map (\x -> reserveToColumn x (EOBoard(columns, reserves, foundation))) possibleColumnToColumn) --filters though the current board and moves a reserve to a column
          colToCol = filter (/= (EOBoard(columns, reserves, foundation))) (map (\x -> columnToColumn x (EOBoard(columns, reserves, foundation))) possibleReserveToColumn) --filters though the current board and moves a column to a column
          
--Chooses the next best move 
chooseMove:: Board -> Maybe Board
chooseMove (EOBoard(columns, reserves, foundation))
  |findMoves (EOBoard(columns, reserves, foundation)) == [] = Nothing -- If there are no moves do nothing
  |otherwise = Just moves --If there is a possible move take it
  where
    moves = head (findMoves (toFoundations (EOBoard(columns, reserves, foundation)))) -- Calls find moves to find all next possible moves

--Plays the game and produces a score 
playSolitaire :: Board -> Int
playSolitaire (EOBoard(columns, reserves, foundation))
  |haveWon (EOBoard(columns, reserves, foundation)) = 52 -- If the columns/reserves are empty this is a win with score 52
  |chooseMove (EOBoard(columns, reserves, foundation)) == Nothing = sum (map length foundation)
  |otherwise = playSolitaire (fromJust (chooseMove (EOBoard(columns, reserves, foundation)))) -- otherwise call the function again to get the score

--Tests if the game has been won
haveWon :: Board -> Bool
haveWon (EOBoard(columns, reserves, foundation))
    |(EOBoard(columns,reserves,foundation))  == (EOBoard([],[],foundation))  = True --The game has been won if columns/reserves are empty
    |otherwise = False --Otherwise the game has not been won

--analyseEO takes 50 games and produces the average score(doesnt work fully yet, says all scores = 0)
analyseEO :: Int -> (Int,Int)
analyseEO 0 = error "You need to insert a value"
analyseEO n = (amountWon,averageScore)
  where boards = map eoDeal (take 100 (randoms (mkStdGen n)) :: [Int]) -- creates 50 initial boards
        totalScore = (map playSolitaire boards) -- finds the total scores of all the games
        amountWon = length (filter (== 52) totalScore)-- Finds the amount of wins by checking if the score is equal to 52
        addedTotalScore = sum totalScore -- Sums up the score of all the games
        averageScore = addedTotalScore `div` 100
  
{- Paste the contents of this file, including this comment, into your source file, below all
     of your code. You can change the indentation to align with your own, but other than this,
     ONLY make changes as instructed in the comments.
   -}
  -- Constants that YOU must set:
studentName = "Ethan Barker"
studentNumber = "190143158"
studentUsername = "ebarker4"

initialBoardDefined = myEOBoard  {- replace XXX with the name of the constant that you defined
                               in step 3 of part 1 -}
secondBoardDefined = mySBoard {- replace YYY with the constant defined in step 5 of part 1,
                              or if you have chosen to demonstrate play in a different game
                              of solitaire for part 2, a suitable contstant that will show
                              your play to good effect for that game -}

  {- Beyond this point, the ONLY change you should make is to change the comments so that the
     work you have completed is tested. DO NOT change anything other than comments (and indentation
     if needed). The comments in the template file are set up so that only the constant eight-off
     board from part 1 and the toFoundations function from part 1 are tested. You will probably
     want more than this tested.

     CHECK with Emma or one of the demonstrators if you are unsure how to change this.

     If you mess this up, your code will not compile, which will lead to being awarded 0 marks
     for functionality and style.
  -}

main :: IO()
main =
   do
     putStrLn $ "Output for " ++ studentName ++ " (" ++ studentNumber ++ ", " ++ studentUsername ++ ")"

     putStrLn "***The eight-off initial board constant from part 1:"
     print initialBoardDefined

     let board = toFoundations initialBoardDefined
     putStrLn "***The result of calling toFoundations on that board:"
     print board

      {- Move the start comment marker below to the appropriate position.
        If you have completed ALL the tasks for the assignment, you can
        remove the comments from the main function entirely.
        DO NOT try to submit/run non-functional code - you will receive 0 marks
        for ALL your code if you do, even if *some* of your code is correct.
      -}

     let boards = findMoves board      -- show that findMoves is working
     putStrLn "***The possible next moves after that:"
     print boards

     let chosen = chooseMove board     -- show that chooseMove is working
     putStrLn "***The chosen move from that set:"
     print chosen

     putStrLn "***Now showing a full game"     -- display a full game
     score <- displayGame initialBoardDefined 0
     putStrLn $ "Score: " ++ score
     putStrLn $ "and if I'd used playSolitaire, I would get score: " ++ show (playSolitaire initialBoardDefined)

        {- start comment marker - move this if appropriate
      putStrLn "\n\n\n************\nNow looking at the alternative game:"

      putStrLn "***The spider initial board constant from part 1 (or equivalent if playing a different game of solitaire):"
      print secondBoardDefined          -- show the suitable constant. For spider solitaire this
                                        -- is not an initial game, but a point from which the game
                                        -- can be won

      putStrLn "***Now showing a full game for alternative solitaire"
      score <- displayGame secondBoardDefined 0 -- see what happens when we play that game (assumes chooseMove
                                                -- works correctly)
      putStrLn $ "Score: " ++ score
      putStrLn $ "and if I'd used playSolitaire, I would get score: " ++ show (playSolitaire secondBoardDefined)

      -}

  {- displayGame takes a Board and move number (should initially be 0) and
     displays the game step-by-step (board-by-board). The result *should* be
     the same as performing playSolitaire on the initial board, if it has been
     implemented correctly.
     DO NOT CHANGE THIS CODE other than aligning indentation with your own.
  -}
displayGame :: Board -> Int ->IO String
displayGame board n =
   if haveWon board
     then return "A WIN"
     else
        do
          putStr ("Move " ++ show n ++ ": " ++ show board)
          let maybeBoard = chooseMove board
          if isJust maybeBoard then
            do
              let (Just newBoard) = maybeBoard
              displayGame newBoard (n+1)
          else
            do
              let score = show (playSolitaire board)
              return score