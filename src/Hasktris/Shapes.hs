module Hasktris.Shapes ( Shape
                       , cwRotate
                       , ccwRotate
                       , tetriminos
                       , drawTetriminos
       )

where

import           UI.HSCurses.Curses

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


castEnum = toEnum . fromEnum

drawTetriminos L _ _ = return ()
drawTetriminos piece pY pX = do mvAddCh pY pX (castEnum '[')
                                mvAddCh pY (pX + 1) (castEnum ']')
                                let (Node north south east west) = piece
                                drawTetriminos north (pY - 1) pX
                                drawTetriminos south (pY + 1) pX
                                drawTetriminos east pY (pX + 2)
                                drawTetriminos west pY (pX - 2)
