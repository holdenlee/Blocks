{-# OPTIONS
 
 -XMultiParamTypeClasses
 -XFunctionalDependencies
 -XFlexibleInstances
 -XRank2Types
 -XGADTs
 -XPolyKinds
 -XLambdaCase
#-}

module Main where
import System.Environment
import Control.Monad
import Data.Tree
import Data.List
import qualified Data.Set as S
import qualified Data.Map.Strict as Map
import qualified Data.Hashable
import Data.Either
import Data.Maybe
import Text.Printf

import Utilities

data Color = Cyan | Brown | Red | Orange deriving (Eq, Show)

data Set = All | With Color | Not Set | Diff Set Set | Leftmost Set | Rightmost Set deriving (Eq, Show)

data Act = Add Set Color | Remove Set deriving (Eq, Show)

type World = [[Color]]

colorToChar :: Color -> Char
colorToChar = \case
              Cyan -> 'C'
              Brown -> 'B'
              Red -> 'R'
              Orange -> 'O'

colorToNL :: Color -> String
colorToNL = \case
            Cyan -> "cyan"
            Brown -> "brown"
            Red -> "red"
            Orange -> "orange"

actToNL :: Act -> String
actToNL = \case
          Add s c -> printf "Add %s to %s." (colorToNL c) (setToNL s)
          Remove s -> printf "Remove %s." (setToNL s)

setToNL :: Set -> String
setToNL = \case
          All -> "all blocks"
          With c -> printf "%s blocks" (colorToNL c)
          Not s -> printf "blocks except %s" (setToNL s)
          Diff s t -> printf "%s that are not %s" (setToNL s) (setToNL t)
          Leftmost s -> printf "leftmost %s" (setToNL s)
          Rightmost s -> printf "rightmost %s" (setToNL s)

evalSet :: Set -> (World -> (Int -> Bool))
evalSet s w i = case s of
          All -> True
          With c -> listToMaybe (w!!i) == Just c
          Not s -> not (evalSet s w i)
          Diff s t -> (evalSet s w i) && (not $ evalSet t w i)
          Leftmost s -> Just i == (listToMaybe $ filter (evalSet s w) [0..(length w)-1])
          Rightmost s -> Just i == (listToMaybe $ filter (evalSet s w) $ reverse [0..(length w)-1])

evalAct :: Act -> World -> World
evalAct a w = case a of
                Add s c -> zemap (\(i,li) -> if evalSet s w i then c:li else li) w
                Remove s -> zemap (\(i,li) -> if evalSet s w i then tail li else li) w

display :: World -> String
display w = 
    let
        l = length w
        h = maximum $ map length w
    in
      unlines $ (++[replicate l 'X']) $ map (\y -> map (\x -> case mlookup (y - (h-(length (w!!x)))) (w!!x) of {Just c -> colorToChar c; Nothing -> ' '}) [0..(l-1)]) [0..(h-1)] 

initWorld :: Int -> World
initWorld i = replicate i []

seqActs :: [Act] -> World -> World
seqActs = foldIterate evalAct 

evalActDisp :: Act -> World -> IO World
evalActDisp a w = do
  let w' = evalAct a w
  putStrLn (show a)
  putStrLn (actToNL a)
  putStrLn (display w')
  return w'

seqActsDisp :: [Act] -> World -> IO World
seqActsDisp as w = case as of
                     [] -> return w
                     h:rest -> (evalActDisp h w) >>= (seqActsDisp rest)

main = [[Cyan],[Cyan],[Cyan],[Brown],[Red]]
     |> seqActsDisp
        [Add (With Cyan) Orange,
         Remove (With Brown),
         Add (Leftmost (With Orange)) Red]
