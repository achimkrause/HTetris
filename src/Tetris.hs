import UI.NCurses
import Game
import Piece
import System.CPUTime
import System.Random
import qualified Data.Set as S
import Control.Monad.IO.Class
import Control.Monad
import Data.List (genericLength)

box :: Box
box = Box 0 9 0 23

main :: IO ()
main = do let (sx,sy) = spawn box
          g <- getStdGen

          finalScore <- runCurses $ do
              w <- newWindow (height box) (width box+6) 0 0
              (black:white:restcs) <- makeColors
              let stdPiecesColor = zipWith (\c piece -> at sx sy . fmap (\char-> (char,c)) $ piece) (white:restcs) standardPieces
              let pieces = randomElems stdPiecesColor g
              let game = startGame pieces
              updateWindow w $ clean black box
              --liftIO $ putStrLn $ show game
              render
              (mainLoop black white w box game 0)
          putStrLn ("Final score: " ++ show finalScore)


clean :: ColorID -> Box -> Update ()
clean c box = do setColor c
                 forM_ [0..height box - 1] $
                  \i -> do moveCursor i 0
                           drawString (replicate (fromIntegral (width box)) ' ')
                 moveCursor 0 (width box + 1)
                 drawString (replicate 5 ' ')

mainLoop :: ColorID -> ColorID -> Window ->  Box ->  Game (Char,ColorID) -> Integer -> Curses Integer
mainLoop black white w box g t = if lost g then return (gameScore g) else
 do updateWindow w (clean black box >> renderGame white box g >> setColor black)
    render
    t' <- liftIO $ getCPUTime
    if t' >= t + (gameDelay g)
     then mainLoop black white w box (gameStep box g) t'
     else do  ev <- getEvent w (Just 0)
              case ev of
               Just (EventSpecialKey KeyUpArrow)  -> mainLoop black white w box (gameMove rotate box g) t
               Just (EventSpecialKey KeyRightArrow)  -> mainLoop black white w box (gameMove right box g) t
               Just (EventSpecialKey KeyLeftArrow)  -> mainLoop black white w box (gameMove left box g) t
               Just (EventSpecialKey KeyDownArrow)  -> mainLoop black white w box (gameDrop box g) t'
               Just (EventCharacter 'q')            -> return (gameScore g)
               _ -> mainLoop black white w box g t

makeColors :: Curses [ColorID]
makeColors = mapM (\(c,n) -> newColorID c ColorBlack n)
   $ zip [ColorBlack, ColorWhite, ColorRed, ColorGreen, ColorYellow, ColorBlue, ColorMagenta, ColorCyan] [1..8]

renderPoint :: Box -> Point (Char,ColorID) -> Update ()
renderPoint box p@(Point x y (char,c)) = if p `contained` box
                                    then do moveCursor (y- boxTopY box) (x-boxLeftX box)
                                            setColor c
                                            drawString [char]
                                    else return ()

renderScore :: ColorID -> Box -> Integer -> Update ()
renderScore white box n = do  let str = show n
                              setColor white
                              moveCursor 0 (width box + 6 - genericLength str)
                              drawString str

renderGame :: ColorID -> Box -> Game (Char,ColorID) -> Update ()
renderGame white box (Game pts (p@(Piece _ pts2):_) sc) = do mapM_ (renderPoint box) (S.toList pts ++ pts2)
                                                             let p' = dropPiece box pts $ p
                                                             when (pointY (pieceOrigin p') - pointY (pieceOrigin p) >4)
                                                               (mapM_ (renderPoint box) (piecePoints . fmap (\_ -> ('O',white)) $ p'))
                                                             renderScore white box sc
                                                             moveCursor 0 0
