{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant <$>" #-}
{-# HLINT ignore "Monad law, left identity" #-}

module Chess where

import Debug.Trace (trace)

import Control.Monad      (when)
import Control.Monad.ST   (ST, runST)
import Data.Array.ST      (STArray, newListArray, readArray, writeArray, runSTArray)
import Data.Char          (toLower)
import Data.Foldable      (toList)
import Data.List          (intercalate, intersperse, findIndex)
import Data.Maybe         (isNothing, fromJust, maybe)

import Data.Array.MArray qualified as MA


data Colour = White | Black
  deriving (Eq, Show)

opposite :: Colour -> Colour
opposite colour = case colour of
  Black -> White
  White -> Black

data Piece = P Colour      -- Pawn
           | B Colour      -- Bishop
           | N Colour      -- Knight
           | R Colour      -- Rook
           | Q Colour      -- Queen
           | K Colour      -- King
  deriving (Eq, Show)

col :: Piece -> Colour
col (P c) = c
col (B c) = c
col (N c) = c
col (R c) = c
col (Q c) = c
col (K c) = c

type Square = (Int,Int)

indexToSquare :: Int -> Square
indexToSquare index = (index `mod` 8 + 1, 8 - index `div` 8)

type SquareState = Maybe Piece

showSquareState :: SquareState -> String
showSquareState Nothing          = " "
showSquareState (Just (P White)) = "P"
showSquareState (Just (B White)) = "B"
showSquareState (Just (N White)) = "N"
showSquareState (Just (R White)) = "R"
showSquareState (Just (Q White)) = "Q"
showSquareState (Just (K White)) = "K"
showSquareState (Just (P Black)) = "p"
showSquareState (Just (B Black)) = "b"
showSquareState (Just (N Black)) = "n"
showSquareState (Just (R Black)) = "r"
showSquareState (Just (Q Black)) = "q"
showSquareState (Just (K Black)) = "k"

data Move = Move { from :: Square, to :: Square }
  deriving (Eq, Show)

isStraight, isDiagonal :: Move -> Bool
isStraight (Move (x1,y1) (x2,y2)) = x1 == x2 || y1 == y2
isDiagonal (Move (x1,y1) (x2,y2)) = abs (x2 - x1) == abs (y2 - y1)

type Board s = STArray s Int SquareState

readSquare :: Board s -> Square -> ST s SquareState
readSquare board (x,y) = readArray board (8*(8 - y) + (x-1))

writeSquare :: Board s -> Square -> SquareState -> ST s ()
writeSquare board (x,y) = writeArray board (8*(8 - y) + (x-1))

correctColour :: Piece -> Colour -> ST s Bool
correctColour piece colour = return $ col piece == colour

validPieceMovement :: Piece -> Move -> Bool
validPieceMovement piece move@(Move (x1,y1) (x2,y2)) =
  -- Checks for moves that only move themselves
  case piece of
    P col -> (x1 == x2 &&             {- regular pawn movement -}
                case col of
                  Black -> y2 == y1 - 1 || (y1 == 7 && y2 == 5)
                  White -> y2 == y1 + 1 || (y1 == 2 && y2 == 4)
              ) ||
              (abs (x2 - x1) == 1 &&   {- capture diagonally -}
                case col of
                  Black -> y2 == y1 - 1
                  White -> y2 == y1 + 1
              )
    B _   -> isDiagonal move
    N _   -> abs (x2 - x1) == 1 && abs (y2 - y1) == 2 ||
                      abs (x2 - x1) == 2 && abs (y2 - y1) == 1
    R _   -> isStraight move
    Q _   -> isDiagonal move || isStraight move
    K _   -> abs (x2 - x1) <= 1 && abs (y2 - y1) <= 1


validPieceMovement' :: Piece -> Move -> ST s Bool
validPieceMovement' = fmap return . validPieceMovement

takingOwnPiece :: Board s -> Piece -> Move -> ST s Bool
takingOwnPiece board movingPiece (Move _ sq) = do
  sqState <- readSquare board sq
  return $ maybe False ((== col movingPiece) . col) sqState

hasPiecesBetween :: Board s -> Piece -> Move -> ST s Bool
hasPiecesBetween board piece move =
  not . all isNothing <$> sequence (tail (getBetweenPieces board move))
    where
      getBetweenPieces :: Board s -> Move -> [ST s SquareState]
      getBetweenPieces board m@(Move (x1,y1) (x2,y2))
        | x1 == x2 && y1 == y2 = case piece of
            (P _) -> [readSquare board (x1,y1)] -- only pawns cannot take a piece (forwards)
            _     -> []
        | otherwise
          = readSquare board (x1,y1) : getBetweenPieces board
                                       (Move (incTowards x1 x2, incTowards y1 y2) (x2,y2))

      incTowards i1 i2 | i1 == i2  = i1
                       | otherwise = if i1 < i2 then i1 + 1 else i1 - 1

obstructed :: Board s -> Piece -> Move -> ST s Bool
obstructed _ (N _) _ = return False
obstructed _ (K _) _ = return False
obstructed board p@(P _) m = if isStraight m
  then hasPiecesBetween board p m             -- forward move is blocked by any pieces
  else isNothing <$> readSquare board (to m)  -- diagonal move must be capture
obstructed board piece m@(Move (x1,y1) (x2,y2))
  | abs (x1 - x2) > 1 || abs (y1 - y2) > 1 = hasPiecesBetween board piece m
  | otherwise = return False

getKingSquare :: Board s -> Colour -> ST s (Maybe Square)
getKingSquare board colour = do
  es <- MA.getElems board
  return $ findIndex (isKing colour) es >>= \index ->
           Just (indexToSquare index)
  where
    isKing :: Colour -> SquareState -> Bool
    isKing c (Just (K col)) = c == col
    isKing _ _ = False

getColourPieces :: Board s -> Colour -> ST s [(Square,Piece)]
getColourPieces board colour = do
  es <- MA.getAssocs board
  return $ foldr findColourPieces [] es
    where
      findColourPieces :: (Int,SquareState) -> [(Square,Piece)] -> [(Square,Piece)]
      findColourPieces (_, Nothing) l = l
      findColourPieces (i, Just p) l
        | col p == colour = (indexToSquare i, p) : l
        | otherwise       = l

attacking :: Board s -> Piece -> Move -> ST s Bool
attacking board piece move =
  (&&) <$>
    validPieceMovement' piece move <*>
    (not <$> obstructed board piece move)

inCheck :: Board s -> Colour -> ST s Bool
inCheck board colour = do
  maybeKingSquare <- getKingSquare board colour
  -- since it can't be guaranteed this is called after all checks for invalid moves,
  -- need to have a guard for king being "taken"
  case maybeKingSquare of
    Nothing -> return True
    Just kingSquare -> do
      oppPieces <- getColourPieces board (opposite colour)
      or <$> mapM (\(square,piece) -> attacking board piece (Move square kingSquare)) oppPieces

inCheckAfterMove :: Board s -> Piece -> Move -> ST s Bool
inCheckAfterMove board piece (Move (x1,y1) (x2,y2)) = do
  -- Do move and then determine if there is a check
  orig <- readSquare board (x2,y2)
  writeSquare board (x2,y2) (Just piece)
  writeSquare board (x1,y1) Nothing
  res <- inCheck board (col piece)
  -- Undo the move
  writeSquare board (x2,y2) orig
  writeSquare board (x1,y1) (Just piece)
  return res

legalMove :: Board s -> Piece -> Move -> Colour -> ST s Bool
-- NOTE: order of evaluation for each function is not guaranteed
legalMove board piece move colour =
  correctColour piece colour >>= \r1 -> (r1 &&)
    . not <$> takingOwnPiece board piece move >>= \r2 -> (r2 &&)
    . not <$> obstructed board piece move >>= \r3 -> (r3 &&)
    . not <$> inCheckAfterMove board piece move

{- First do basic piece check, then make move after passing legality check -}
makeMove :: ST s (Board s) -> Move -> Colour -> ST s (Board s)
makeMove board move@(Move s1 s2) colour = do
  st   <- board
  from <- readSquare st s1
  case from of
    Nothing    -> return st
    Just piece -> do
      legal <- legalMove st piece move colour
      when legal $ do
        writeSquare st s2 (Just piece)
        writeSquare st s1 Nothing
      return st


data Result = Checkmate | Stalemate
  deriving (Eq, Show)

-- TODO: Improve slow check
hasLegalMoves :: Board s -> Colour -> ST s Bool
hasLegalMoves board colour = do
  colourPieces <- getColourPieces board colour
  let validMoves = filter (uncurry validPieceMovement) (movesToCheck colourPieces)
  or <$> mapM (\(piece, move) -> legalMove board piece move colour) validMoves
    where
      squares = [(x,y) | x <- [1..8], y <- [1..8]]
      movesToCheck pieceSquares = [(piece, Move from to) | (from,piece) <- pieceSquares, to <- squares]

checkGameResult :: (forall s. ST s (Board s)) -> Colour -> Maybe Result
checkGameResult board colour = runST $ do
  st <- board
  r1 <- hasLegalMoves st colour
  if r1 then
    return Nothing
  else do
    r2 <- inCheck st colour
    if r2 then
      return $ Just Checkmate
    else
      return $ Just Stalemate

boardStateUnchanged :: (forall s. ST s (Board s)) -> (forall s. ST s (Board s)) -> Bool
boardStateUnchanged board1 board2 = runST $ do
  b1 <- board1
  b2 <- board2
  l1 <- MA.getElems b1
  l2 <- MA.getElems b2
  return $ l1 == l2

{- Divide a list in integer steps -}
divideList :: Int -> [a] -> [[a]]
divideList _ [] = []
divideList i l = as : divideList i rest
                 where (as, rest) = splitAt i l

showBoard :: (forall s. ST s (Board s)) -> String
showBoard board = intercalate line $
                  map (intersperse ' ' . intersperse '|' . concatMap showSquareState) boardList
  where
    line = "\n" ++ replicate 30 '-' ++ "\n"
    boardList = runST $ do
      st <- board   -- get board in ST monad
      l <- MA.getElems st
      return $ divideList 8 l


startPos :: ST s (Board s)
startPos = newListArray (0, 63)
  [
  Just (R Black), Just (N Black), Just (B Black), Just (Q Black), Just (K Black), Just (B Black), Just (N Black), Just (R Black),
  Just (P Black), Just (P Black), Just (P Black), Just (P Black), Just (P Black), Just (P Black), Just (P Black), Just (P Black),
  Nothing       , Nothing       , Nothing       , Nothing       , Nothing       , Nothing       , Nothing       , Nothing       ,
  Nothing       , Nothing       , Nothing       , Nothing       , Nothing       , Nothing       , Nothing       , Nothing       ,
  Nothing       , Nothing       , Nothing       , Nothing       , Nothing       , Nothing       , Nothing       , Nothing       ,
  Nothing       , Nothing       , Nothing       , Nothing       , Nothing       , Nothing       , Nothing       , Nothing       ,
  Just (P White), Just (P White), Just (P White), Just (P White), Just (P White), Just (P White), Just (P White), Just (P White),
  Just (R White), Just (N White), Just (B White), Just (Q White), Just (K White), Just (B White), Just (N White), Just (R White)
  ]
