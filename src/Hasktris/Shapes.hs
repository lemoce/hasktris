module Hasktris.Shapes ( cwRotate
                       , ccwRotate
                       , shape)

where

import           Data.List (transpose)

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
