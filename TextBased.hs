{-# LANGUAGE RankNTypes #-}


module TextBased where

import Chess            (Board, Colour(..), Move, Result(..), opposite, makeMove, checkGameResult,
                         boardStateUnchanged, startPos, showBoard)
import Parser           (runParser, moveParser)

import Control.Monad.ST (ST, runST)
import System.IO        (BufferMode (LineBuffering), hFlush, stdout, hSetBuffering, stdin)


getMove :: Colour -> IO Move
getMove colour = do
  putStr $ "Move (" ++ show colour ++ "): "
  hFlush stdout   -- force IO sequencing
  move <- getLine
  case runParser moveParser move of
    Just (mv, _) -> return mv
    Nothing      -> do
      putStrLn "Invalid move (format e.g. e3 e5)"
      getMove colour

turn :: (forall s. ST s (Board s)) -> Colour -> IO ()
turn board colour = do
  move <- getMove colour
  let newBoard = makeMove board move colour
  if boardStateUnchanged board newBoard then do
    putStrLn "Illegal move, please try again."
    turn board colour
  else do
    putStrLn ('\n' : showBoard newBoard)
    case checkGameResult newBoard (opposite colour) of
      Nothing        -> turn newBoard (opposite colour)
      Just Checkmate -> putStrLn $ show colour ++ " wins by checkmate!"
      Just Stalemate -> putStrLn "Draw by stalemate!"

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  putStrLn ('\n' : showBoard startPos)
  turn startPos White
