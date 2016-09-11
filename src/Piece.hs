{-# LANGUAGE Rank2Types #-}

module Piece where


import Data.List


data Point a = Point {
           pointX :: Integer,
           pointY :: Integer,
           val :: a
}

instance Ord (Point a) where
 compare (Point x1 y1 _) (Point x2 y2 _) = compare (x1,y1) (x2,y2)

instance Functor Point where
 fmap f (Point x y v) = Point x y (f v)

instance Eq (Point a) where
 (Point x1 y1 _) == (Point x2 y2 _) = (x1,y1) == (x2,y2)

instance Show (Point a) where
 show (Point x y _) = "Point "++ show x ++ " " ++ show y

data Piece a = Piece {
           pieceOrigin :: Point (),
           piecePoints :: [Point a]
}deriving Show

instance Functor Piece where
 fmap f (Piece origin pts) = Piece origin (map (fmap f) pts)

transform :: (forall a . Point a -> Point a) -> Piece b -> Piece b
transform f (Piece origin points) = Piece (f origin) (map f points)

translate :: Integer -> Integer -> Point a -> Point a
translate dx dy (Point x y v) = (Point (x+dx) (y+dy) v)

rotateAround :: Point b -> Point a -> Point a
rotateAround (Point x y _) (Point x1 y1 v) = Point (x - (y1-y)) (y + (x1-x)) v

rotate :: Piece a -> Piece a
rotate p@(Piece origin _) = transform (rotateAround origin) p

right :: Piece a -> Piece a
right = transform (translate 1 0)

left :: Piece a -> Piece a
left = transform (translate (-1) 0)

down :: Piece a -> Piece a
down = transform (translate 0 1)

at :: Integer -> Integer -> Piece a -> Piece a
at x y p@(Piece (Point x1 y1 _) _) = transform (translate (x-x1) (y-y1)) p

--fails on empty string!
readPiece :: Integer -> Integer -> String -> Piece Char
readPiece originX originY str = let ls = lines str in
                                let coords = concat $ zipWith zip [[(x,y) | x <- [0..length (head ls)-1]]|y <- [0..length ls - 1]] ls in
                                let pts = concat . map (\((x,y),c) -> if c == ' ' then [] else [Point (fromIntegral x) (fromIntegral y) c]) $ coords in
                                 Piece (Point originX originY ()) pts

standardPieces :: [Piece Char]
standardPieces = [
  readPiece 0 0
    ( "##\n"
    ++"##\n"),
  readPiece 1 0
    ( " ##\n"
    ++"## \n"),
  readPiece 1 0
    ( "## \n"
    ++" ##\n"),
  readPiece 1 1
    ( " # \n"
    ++"###\n"),
  readPiece 0 1
    ( "#  \n"
    ++"###\n"),
  readPiece 2 1
    ( "  #\n"
    ++"###\n"),
  readPiece 1 0
    ( "####\n")
 ]
