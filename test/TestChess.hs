{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant <$>" #-}
module TestChess where

import Chess

import Control.Monad.ST (ST, runST)
import Data.Array.ST    (newListArray)
import Data.List        (isInfixOf)


obstructedTest :: Bool
obstructedTest = runST $ do
  st <- startPos
  r1 <- obstructed st (Q White) (Move (4,1) (4,8))
  r2 <- obstructed st (P White) (Move (2,2) (1,3))
  r3 <- not <$> obstructed st (P White) (Move (2,2) (1,3))
  return (r1 && r2)

attackingTest :: Bool
attackingTest = runST $ do
  st <- startPos
  r1 <- attacking st (N White) (Move (2,1) (1,3))
  r2 <- not <$> attacking st (Q Black) (Move (4,8) (5,1))
  r3 <- not <$> attacking st (Q White) (Move (4,1) (4,8))
  return (r1 && r2 && r3)

getKingSquareTest :: Bool
getKingSquareTest = squareW == Just (5,1) && squareB == Just (5,8)
  where
    squareW = runST $ do
      st <- startPos
      getKingSquare st White
    squareB = runST $ do
      st <- startPos
      getKingSquare st Black

getColourPiecesTest :: Bool
getColourPiecesTest = runST $ do
  st <- startPos
  pW <- getColourPieces st White
  pB <- getColourPieces st Black
  return $
    pW == [((1,2),P White),((2,2),P White),((3,2),P White),((4,2),P White),((5,2),P White),((6,2),P White),((7,2),P White),((8,2),P White),
           ((1,1),R White),((2,1),N White),((3,1),B White),((4,1),Q White),((5,1),K White True),((6,1),B White),((7,1),N White),((8,1),R White)]
    &&
    pB == [((1,8),R Black),((2,8),N Black),((3,8),B Black),((4,8),Q Black),((5,8),K Black True),((6,8),B Black),((7,8),N Black),((8,8),R Black),
           ((1,7),P Black),((2,7),P Black),((3,7),P Black),((4,7),P Black),((5,7),P Black),((6,7),P Black),((7,7),P Black),((8,7),P Black)]

inCheckAfterMoveTest :: Bool
inCheckAfterMoveTest = runST $ do
  st <- newListArray (0, 63)
    [
    Just (R Black), Just (N Black), Nothing       , Nothing       , Just (K Black True), Just (B Black), Just (N Black), Just (R Black),
    Just (P Black), Just (P Black), Just (P Black), Just (P Black), Just (P Black), Just (P Black), Just (P Black), Just (P Black),
    Nothing       , Nothing       , Nothing       , Nothing       , Nothing       , Nothing       , Nothing       , Nothing       ,
    Nothing       , Nothing       , Nothing       , Nothing       , Just (Q Black), Nothing       , Nothing       , Nothing       ,
    Nothing       , Nothing       , Nothing       , Nothing       , Nothing       , Nothing       , Nothing       , Nothing       ,
    Nothing       , Nothing       , Nothing       , Nothing       , Nothing       , Nothing       , Nothing       , Just (B Black),
    Just (P White), Just (P White), Just (P White), Just (P White), Just (B White), Just (P White), Nothing       , Just (P White),
    Just (R White), Just (N White), Nothing       , Just (Q White), Just (K White True), Nothing       , Just (N White), Just (R White)
    ] :: ST s (Board s)
  r1 <- inCheckAfterMove st (B White) (Move (5,2) (4,3))
  r2 <- not <$> inCheckAfterMove st (P White) (Move (4,2) (4,3))
  r3 <- inCheckAfterMove st (K White True) (Move (5,1) (6,1))
  return (r1 && r2 && r3)

checkGameResultCheckmateTest :: Bool
checkGameResultCheckmateTest =
  checkGameResult
    (newListArray (0, 63)
      [
      Just (R Black), Just (N Black), Just (B Black), Nothing       , Just (K Black False), Just (B Black), Just (N Black), Just (R Black),
      Just (P Black), Just (P Black), Just (P Black), Just (P Black), Nothing       , Just (P Black), Just (P Black), Just (P Black),
      Nothing       , Nothing       , Nothing       , Nothing       , Just (P Black), Nothing       , Nothing       , Nothing       ,
      Nothing       , Nothing       , Nothing       , Nothing       , Nothing       , Nothing       , Nothing       , Nothing       ,
      Nothing       , Nothing       , Nothing       , Nothing       , Nothing       , Nothing       , Just (P White), Just (Q Black),
      Nothing       , Nothing       , Nothing       , Nothing       , Nothing       , Just (P White), Nothing       , Nothing       ,
      Just (P White), Just (P White), Just (P White), Just (P White), Just (P White), Nothing       , Nothing       , Just (P White),
      Just (R White), Just (N White), Just (B White), Just (Q White), Just (K White True), Just (B White), Just (N White), Just (R White)
      ]
    )
    White
  == Just Checkmate

checkGameResultStalemateTest :: Bool
checkGameResultStalemateTest =
  checkGameResult
    (newListArray (0, 63)
      [
      Nothing       , Nothing       , Nothing       , Nothing       , Nothing       , Nothing       , Nothing       , Nothing       ,
      Nothing       , Nothing       , Nothing       , Nothing       , Nothing       , Nothing       , Nothing       , Nothing       ,
      Nothing       , Nothing       , Nothing       , Nothing       , Nothing       , Nothing       , Nothing       , Nothing       ,
      Nothing       , Nothing       , Nothing       , Nothing       , Nothing       , Nothing       , Nothing       , Nothing       ,
      Nothing       , Nothing       , Nothing       , Nothing       , Nothing       , Nothing       , Just (K Black False), Nothing       ,
      Nothing       , Nothing       , Nothing       , Nothing       , Nothing       , Nothing       , Just (Q Black), Nothing       ,
      Nothing       , Nothing       , Nothing       , Nothing       , Nothing       , Nothing       , Nothing       , Nothing       ,
      Nothing       , Nothing       , Nothing       , Nothing       , Nothing       , Nothing       , Nothing       , Just (K White False)
      ]
    )
    White
  == Just Stalemate

main :: IO ()
main = do
  let testResults = test [
        ("obstructed",obstructedTest),
        ("attacking",attackingTest),
        ("getKingSquare",getKingSquareTest),
        ("getColourPieces",getColourPiecesTest),
        ("inCheckAfterMove",inCheckAfterMoveTest),
        ("checkGameResultCheckmate",checkGameResultCheckmateTest),
        ("checkGameResultStalemate",checkGameResultStalemateTest)
        ]
  putStrLn testResults
  if "FAILED" `isInfixOf` testResults then
    putStrLn "Some tests failed!"
  else
    putStrLn "All tests passed!"
  where
    test :: [(String,Bool)] -> String
    test = concatMap (\(name,result) -> (if result then "PASSED" else "FAILED") ++ " --- " ++ name ++ "\n")
