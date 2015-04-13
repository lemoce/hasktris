module Hasktris.Graphics


where

import           Hasktris.Shapes
import           UI.NCurses

renderTetriminos :: Int -> Int -> Shape -> Update ()
renderTetriminos _ _ L = return ()
renderTetriminos lin col tetriminos = do moveCursor lin col
                                         drawString "[]"
                                         let (Node north, south, east, west) = tetriminos
                                         renderTetriminos (lin+1) col north
                                         renderTetriminos (lin-1) col south
                                         renderTetriminos lin (col+1) east
                                         renderTetriminos lin (col-1) west
