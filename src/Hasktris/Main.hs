module Hasktris.Main (main) where

import           Hasktris.Shapes
import           UI.HSCurses.Curses



dealTetriminos piece pY pX = do erase
                                drawTetriminos piece pY pX
                                c <- getCh
                                case c of
                                  KeyChar '0' -> dealTetriminos (tetriminos !! 0) pY pX
                                  KeyChar '1' -> dealTetriminos (tetriminos !! 1) pY pX
                                  KeyChar '2' -> dealTetriminos (tetriminos !! 2) pY pX
                                  KeyChar '3' -> dealTetriminos (tetriminos !! 3) pY pX
                                  KeyChar '4' -> dealTetriminos (tetriminos !! 4) pY pX
                                  KeyChar '5' -> dealTetriminos (tetriminos !! 5) pY pX
                                  KeyChar '6' -> dealTetriminos (tetriminos !! 6) pY pX
                                  KeyUp -> dealTetriminos piece (pY - 1) pX
                                  KeyDown -> dealTetriminos piece (pY + 1) pX
                                  KeyLeft -> dealTetriminos piece pY (pX - 1)
                                  KeyRight -> dealTetriminos piece pY (pX + 1)
                                  KeyChar ' ' -> dealTetriminos (cwRotate piece) pY pX
                                  _ -> return ()


main :: IO ()
main = do initCurses
          keypad stdScr True
          echo False
          cursSet CursorInvisible
          (sizeY, sizeX) <- scrSize
          let myPiece = tetriminos !! 4
          dealTetriminos myPiece (sizeY `div` 2) (sizeX `div` 2)
          endWin


