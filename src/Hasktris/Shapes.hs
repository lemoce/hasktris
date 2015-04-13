module Hasktris.Shapes ( cwRotate
                       , ccwRotate
                       , Shape, L, Node)

where

import           Data.List (transpose)

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
                    L ]

shape :: [[[Char]]]
shape = [ [ [' ', '#', ' ']
          , ['#', '#', '#'] ]
        , [ ['#', '#']
          , ['#', '#'] ]
        , [ ['#', ' ', ' ']
          , ['#', '#', '#'] ]
        , [ [' ', ' ', '#']
          , ['#', '#', '#'] ]
        , [ ['#', '#', ' ']
          , [' ', '#', '#'] ]
        , [ [' ', '#', '#']
          , ['#', '#', ' '] ]
        , [ ['#', '#', '#', '#' ] ] ]

cwRotate :: [[Char]] -> [[Char]]
cwRotate = (map reverse) . transpose

ccwRotate :: [[Char]] -> [[Char]]
ccwRotate = transpose . (map reverse)
