module Halbebenen where
import Tools
import Test.QuickCheck


data Halbebene = H K K K deriving (Eq, Show)

inside :: Point -> Halbebene -> Bool
inside (Point x y) (H a b c)
    = (a * x + b * y) >= c
    where z = 1

instance Num Point where
    (+) (Point x0 y0) (Point x1 y1)
        = Point (x0+x1) (y0+y1)
    (-) (Point x0 y0) (Point x1 y1)
        = Point (x0-x1) (y0-y1)
    abs (Point x y)
        = Point (abs x) y
    fromInteger x
        = Point (fromInteger x) 0
    signum (Point x y)
           = (Point (signum x) y)

makeH :: Point -> Point -> Halbebene
makeH p0@(Point x0 y0) p1@(Point x1 y1)
    = H a b c
    where (Point a b) = ccw90 (p1 - p0)
          c = a * x0 + b * y0

--    a * x0 + b * y0 = c 
--    a * x1 + b * y1 = c
-- (a,b) ~*~ (x,y) = c

ccw90 (Point x y) = Point (-y) x

mkPoint  = uncurry Point

prop_ccw90 p = p == (ccw90 . ccw90 . ccw90 . ccw90 $ p)
prop_ccw1 = (take 5 . iterate ccw90 $ (Point 1 0))
            == (map mkPoint $
                [(1,0)
                ,(0,1)
                ,(-1,0)
                ,(0,-1)
                ,(1,0)])


prop_makeH p0 p1 p = inside p0 h &&
                     inside p1 h &&
                     inside p h == (isCCW p0 p1 p >= 0)
    where h = makeH p0 p1

prop_makeH1 p0 p = inside p (makeH p0 p0) 

a2 ((Point ax ay), (Point bx by), (Point cx cy))
    = ax * (by - cy) +
      bx * (cy - ay) +
      cx * (ay - by)

isCCW p0 p1 p = a2 (p0,p1,p)

prop_isCCW = isCCW (m 0 1) (m 0 0) (m 1 0) > 0

m = Point

-- -> lineare Optimierung! 
-- aber einfach 2d spezialfall!
-- nur interessiert am rechtesten element (max x)
-- empty :: [Halbebene] -> Bool
-- empty hs = do [h1, h2] <- choose 2 hs
              