module Hasktris.Shapes ( Shape
                       , cwRotate
                       , ccwRotate
                       , tetriminos
                       , drawTetriminos
       )

where

import           UI.NCurses

data Shape = L | Node Shape Shape Shape Shape deriving  (Show, Eq)

tetriminos :: [Shape]
tetriminos = [ Node L
                    (Node L L
                            (Node L L L L)
                            L)
                    L
                    (Node L L L L)
             , Node L
                    (Node L L L
                              (Node L L L L))
                    (Node L L L L)
                    L
             , Node L L
                      (Node L L
                              (Node L L L L)
                              L)
                      (Node L L L L)
             , Node L
                    (Node L L L L)
                    (Node L
                          (Node L L L L)
                          L L)
                    L
             , Node L
                    (Node L L
                            (Node L L L L)
                            (Node L L L L))
                    L L
             , Node L L
                      (Node L
                            (Node L L L L)
                            L L)
                      (Node L L L L)
             , Node L L
                      (Node L L L L)
                      (Node L
                            (Node L L L L)
                            L L)]

cwRotate :: Shape -> Shape
cwRotate L = L
cwRotate piece = (Node (cwRotate west) (cwRotate east) (cwRotate north) (cwRotate south))
  where (Node north south east west) = piece

ccwRotate :: Shape -> Shape
ccwRotate L = L
ccwRotate piece = (Node (ccwRotate east) (ccwRotate west) (ccwRotate south) (ccwRotate north))
  where (Node north south east west) = piece

drawTetriminos :: Integer -> Integer -> Shape -> Update ()
drawTetriminos _ _ L = return ()
drawTetriminos lin col piece = do moveCursor lin col
                                  drawString "[]"
                                  let (Node north south east west) = piece
                                  drawTetriminos (lin-1) col north
                                  drawTetriminos (lin+1) col south
                                  drawTetriminos lin (col+2) east
                                  drawTetriminos lin (col-2) west
