module Game where

import Piece
import qualified Data.Set as S
import System.Random
import Data.List (genericLength)


data Box = Box {
           boxLeftX :: Integer,
           boxRightX :: Integer,
           boxTopY :: Integer,
           boxBottomY :: Integer
} deriving Show

contained :: Point a -> Box -> Bool
contained (Point x y _) box = not $    (x < boxLeftX box)
                                    || (x > boxRightX box)
                                    || (y < boxTopY box)
                                    || (y > boxBottomY box)

spawn :: Box -> (Integer,Integer)
spawn (Box lx rx ty by) = ((rx - lx)`div`2, ty)

width :: Box -> Integer
width b = boxRightX b - boxLeftX b + 1

height :: Box -> Integer
height b = boxBottomY b - boxTopY b + 1


collide :: Box -> S.Set (Point a) -> Piece a -> Bool
collide box points (Piece _ pointsP) = not $
                        S.null (points `S.intersection` (S.fromList pointsP))
                     && null (filter ( (<(boxLeftX box)) . pointX ) pointsP)
                     && null (filter ( (>(boxRightX box)) . pointX ) pointsP)
                     && null (filter ( (>(boxBottomY box)). pointY) pointsP)


data Game a = Game {
  gamePoints :: S.Set (Point a),
  gamePieceQueue :: [Piece a],
  gameScore :: Integer
} deriving Show

gameStep :: Box -> Game a -> Game a
gameStep box (Game pts (piece:queue) sc) =
     if collide box pts (down piece)
        then score box $ (Game (pts`S.union`(S.fromList $ piecePoints piece)) queue sc)
        else (Game pts ((down piece):queue) sc)

gameMove :: (Piece a -> Piece a) -> Box -> Game a -> Game a
gameMove f box (Game pts (piece:queue) sc) =
     if collide box pts (f piece)
      then (Game pts (piece:queue) sc)
      else (Game pts (f piece : queue) sc)

gameDrop :: Box -> Game a -> Game a
gameDrop box (Game pts (piece:queue) sc) = let droppedPiece = dropPiece box pts piece in
                                           let newPts = pts`S.union` (S.fromList $ piecePoints droppedPiece) in
                                          score box $ Game newPts queue sc

score :: Box -> Game a -> Game a
score box (Game pts queue sc) = let (pts', scoreInc) = scoreLines box pts in
                                  Game pts' queue (sc+scoreInc)

gameDelay :: Game a -> Integer
gameDelay g = let sc = gameScore g in
               (500000000000 * 20) `div` (20 + sc)


randomElem :: (RandomGen g) => [a] -> g -> (a,g)
randomElem xs g = let (n, g') = randomR (0,length xs - 1) g
                   in (xs !! n, g')

randomElems :: (RandomGen g) => [a] -> g -> [a]
randomElems xs g = unfold (randomElem xs) g where
                    unfold f s = let (x, s') = f s in
                                   x: unfold f s'

startGame :: [Piece a] -> Game a
startGame pieces = Game (S.fromList []) pieces 0


fullLines :: Box -> S.Set (Point a) -> [Integer]
fullLines box pts = [i|i <- [boxTopY box..boxBottomY box], (== width box) . fromIntegral . S.size . S.filter ((==i).pointY) $ pts]

scoreLines :: Box -> S.Set (Point a) -> (S.Set (Point a), Integer)
scoreLines box pts = let indices = fullLines box pts in
                      (S.fromList . concat . map (dropLines indices) . S.toList $ pts, genericLength indices)

dropLines :: [Integer] -> Point a -> [Point a]
dropLines indices (Point x y val) = let bigger = dropWhile (<y) indices in
                                     if not (null bigger) && head bigger == y then []
                                     else [Point x (y + genericLength bigger) val]

dropPiece :: Box -> S.Set (Point a) -> Piece a -> Piece a
dropPiece box points piece = let drops = iterate down piece in
                             let p x = not $ collide box points (down x) in
                              head $ dropWhile p drops --fails if piece intersects points

lost :: Game a -> Bool
lost (Game pts (piece:_) sc) =  (not . S.null . S.filter ((<0).pointY) $ pts)
                          || (not . S.null $ (pts `S.intersection` (S.fromList $ piecePoints piece)))
