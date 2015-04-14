module Hasktris.Main (main) where

import           Hasktris.Shapes
import           UI.NCurses

main :: IO ()
main = runCurses $ do
    setEcho False
    w <- defaultWindow
    updateWindow w $ do moveCursor 1 10
                        drawString "Hello World!"
                        moveCursor 3 10
                        drawString "(press q to quit)"
                        drawTetriminos 5 10 (tetriminos !! 0)
                        drawTetriminos 7 10 (tetriminos !! 1)
                        drawTetriminos 9 10 (tetriminos !! 2)
                        drawTetriminos 11 10 (tetriminos !! 3)
                        drawTetriminos 13 10 (tetriminos !! 4)
                        drawTetriminos 15 10 (tetriminos !! 5)
                        drawTetriminos 17 10 (tetriminos !! 6)
                        let myPiece = tetriminos !! 5
                        drawTetriminos 5 20 (myPiece)
                        drawTetriminos 9 20 (cwRotate myPiece)
                        drawTetriminos 13 20 (cwRotate (cwRotate myPiece))
                        drawTetriminos 17 20 (cwRotate (cwRotate (cwRotate myPiece)))
                        drawTetriminos 5 30 (myPiece)
                        drawTetriminos 9 30 (ccwRotate myPiece)
                        drawTetriminos 13 30 (ccwRotate (ccwRotate myPiece))
                        drawTetriminos 17 30 (ccwRotate (ccwRotate (ccwRotate myPiece)))
    render
    waitFor w (\ev -> ev == EventCharacter 'q' || ev == EventCharacter 'Q')

waitFor :: Window -> (Event -> Bool) -> Curses ()
waitFor w p = loop where
        loop = do ev <- getEvent w Nothing
                  case ev of
                       Nothing -> loop
                       Just ev' -> if p ev' then return () else loop

