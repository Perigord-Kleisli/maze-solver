{-# LANGUAGE BlockArguments #-}

module Main where

import Control.Lens ((^?))
import Control.Lens.Combinators
import Data.Map qualified as M
import Data.Maybe (catMaybes, fromJust)
import Data.PQueue.Prio.Min qualified as PQ
import Data.Set qualified as S
import Data.Vector qualified as V
import Graphics.Gloss

aStar ::
  (Ord state, Show state) =>
  -- | Function to get graph neighbors and costs
  (state -> [(Int, state)]) ->
  -- | Function to get distance of item from goal
  (state -> Int) ->
  -- | Function to check if at goal
  (state -> Bool) ->
  -- | Initial State, can also be thought of the index
  state ->
  -- | if no path was found, Nothing, otherwise return (cost,path,searched)
  Maybe (Int, [state], [state])
aStar next heuristic isGoalNode initialState =
  aStar'
    (PQ.singleton (heuristic initialState) (0, initialState))
    S.empty
    M.empty
    []
 where
  -- \| Main aStar function is basically the initial stage, this is the actual algo
  aStar' queue visited path accum
    | PQ.null queue = Nothing
    | isGoalNode node = Just (cost, getPath path node, accum)
    | node `S.member` visited = aStar' queueTail visited path accum
    | otherwise =
        let
          visited' = S.insert node visited
          succesors =
            [ (heuristic node', (cost', node'))
            | (cost', node') <- next node
            , node' `S.notMember` visited'
            ]
          newQueue = foldr (uncurry PQ.insert) queueTail succesors
          path' = foldr (\(_, (_, n)) -> M.insert n node) path succesors
         in
          aStar' newQueue visited' path' (node : accum)
   where
    ((_, (cost, node)), queueTail) = PQ.deleteFindMin queue
    getPath path' node'
      | node' `M.member` path' = getPath path' (path' M.! node') ++ [node']
      | otherwise = [node']

type Grid = V.Vector (V.Vector Char)

main :: IO ()
main = do
  (maze, [[startX, startY], [endX, endY]]) <-
    bimap
      (V.fromList . map V.fromList)
      (map (map (read @Int) . words))
      . (splitAt =<< subtract 2 . length)
      . map (filter (/= '\r'))
      . lines
      <$> readFile "input.txt"

  let getNeighbors (x, y) =
        [ (1, (x', y'))
        | (x', y') <- [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
        , Just ' ' == (maze ^? ix y' . ix x')
        ]
  Just (_, path, reverse -> searched) <-
    return $
      aStar
        getNeighbors
        (\(x, y) -> abs (x - endY) + abs (y - endY))
        (== (endY, endX))
        (startX, startY)

  let drawSquare col x y '1' =
        Just $
          color col $
            translate (fromIntegral x) (-fromIntegral y) $
              rectangleSolid 1 1
      drawSquare _ _ _ _ = Nothing
      drawSearched (y, x) = fromJust $ drawSquare (greyN 0.4) x y '1'
      drawPath (y, x) = fromJust $ drawSquare white x y '1'
      percentOf t xs =
        let len = fromIntegral (length xs) * t
         in take (floor len) xs

      renderer t =
        Scale 3 3 $
          Pictures $
            (++ map drawPath ((t / 20) `percentOf` path)) $
              (++ map drawSearched ((t / 20) `percentOf` searched)) $
                (++ catMaybes [drawSquare blue startX startY '1', drawSquare red endX endY '1']) $
                  map (Pictures . V.toList) $
                    V.toList $
                      V.imap (V.imapMaybe . drawSquare green) maze

  animate FullScreen black renderer
