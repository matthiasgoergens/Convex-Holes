module Halbebenen where
import ATools
import Tools
import Test.QuickCheck
import Control.Monad
import Data.List

-- Ord nur fuer Data.Set und so.  Zumindest Eq kann man auch richtig definieren.
data Halbebene = H !K !K !K deriving (Show)

normalizeH h@(H a b c) | c < 0 = H (-a) (-b) (-c)
                       | otherwise = h 

instance Ord Halbebene where
    compare (H xa ya za) (H xb yb zb)
        = compare (xa * zb, ya * zb, za*zb) (xb * za, yb * za, zb*za)

instance Eq Halbebene where
    (==) p0 p1 = compare p0 p1 == EQ

instance Arbitrary Halbebene where
    arbitrary = do (a,b,c) <- arbitrary
                   return $ H a b c
    shrink (H a b c) = do a' <- shrink a
                          b' <- shrink b
                          c' <- shrink c
                          return (H a' b' c')

inside :: Point -> Halbebene -> Bool
inside (Point x y z) (H a b c)
    = a * x + b * y + c*z >= 0

--- does not work with negative z!
prop_insideNorm a b c = inside a h == inside (normalize a) h
    where h = makeH b c

dotH (Point x y z) (H a b c)
    = a * x + b * y + c*z

makeH :: Point -> Point -> Halbebene
makeH p0 p1
    = H a b c
    where (a,b,c) = cross (x0,y0,z0) (x1,y1,z1)
          (Point x0 y0 z0) = normalize p0
          (Point x1 y1 z1) = normalize p1

prop_makeHNormA a b = makeH a b == makeH (normalize a) b
prop_makeHNormB a b = makeH a b == makeH a (normalize b)

meet2 :: Halbebene -> Halbebene -> Point
meet2 (H a0 b0 c0) (H a1 b1 c1) = normalize (Point x y z)
    where (x,y,z) = cross (a0,b0,c0) (a1,b1,c1)

-- prop_meet2 

--    a * x0 + b * y0 = c 
--    a * x1 + b * y1 = c
-- (a,b) ~*~ (x,y) = c

ccw90 (Point x y z) = Point (-y) x z

mkPoint (x,y,z) = Point x y z
mkPointI (x,y) = Point x y 1

prop_ccw90 p = p == (ccw90 . ccw90 . ccw90 . ccw90 $ p)
prop_ccw1 = (take 5 . iterate ccw90 $ (Point 1 0 1))
            == (map mkPoint $
                [(1,0,1)
                ,(0,1,1)
                ,(-1,0,1)
                ,(0,-1,1)
                ,(1,0,1)])


prop_makeH p0 p1 p = inside p0 h &&
                     inside p1 h
--                     && inside p h == (isCCW p0 p1 p >= 0)
    where h = makeH p0 p1

prop_makeH1 p0 p = inside p (makeH p0 p0) 

prop_ccwA a b c = ccw a b c == ccw (normalize a) b c
prop_ccwB a b c = ccw a b c == ccw a (normalize b) c
prop_ccwC a b c = ccw a b c == ccw a b (normalize c)

prop_ccwRot a b c = ccw a b c == ccw c a b
prop_ccwRot2 a b c = ccw a b c == ccw b c a
prop_ccwNeg a b c = ccw a b c == - ccw a c b

ccw (Point ax ay az) (Point bx by bz) (Point cx cy cz)
    = ((ax * by * cz - ax * bz * cy) +
       (az * bx * cy - ay * bx * cz) +
       (ay * bz * cx - az * by * cx))
      * signum (az * bz * cz)
--      / (az * bz * cz)


--a2 ((Point ax ay), (Point bx by), (Point cx cy))
--    = ax * (by - cy) +
--    bx * (cy - ay) +
--    cx * (ay - by)

isCCW p0 p1 p = ccw p0 p1 p

prop_isCCW = isCCW (m 0 1 1) (m 0 0 1) (m 1 0 1) > 0

m = Point

-- -> lineare Optimierung! 
-- aber einfach 2d spezialfall!
-- nur interessiert am rechtesten element (max x)
vertices :: [Halbebene] -> [Point]
vertices hs = filter (swing all (map contains hs)) .  map meet2L $ subSet 2 hs
    where meet2L [h1, h2] = meet2 h1 h2
          contains = flip inside

prop_vertices2 p0 p1 = (vertices $ zipWith makeH [p0,p1] [p1,p0]) \\ [p0,p1]
                       == []
prop_vertices3 p0 p1 p2 = (vertices $ zipWith makeH [p0,p1,p2] [p1,p2,p0]) \\ [p0,p1,p2]
                          == []

stdP = normalize

prop_inside3 a b c = (isFinite a3 && isFinite b3 && isFinite c3) ==>
--                     (ccw a3 b3 c3 >= 0) ==
                     ((inside m ha == inside m hb) &&
                      (inside m hb == inside m hc))
    where a3 = 3 ~* stdP a
          b3 = 3 ~* stdP b
          c3 = 3 ~* stdP c
          m = stdP a + stdP b + stdP c
          ha = makeH b3 c3
          hb = makeH c3 a3
          hc = makeH a3 b3
        

cross (v1x,v1y,v1z) (v2x,v2y,v2z) = (vx,vy,vz)
    where
      vx = v1y * v2z - v1z * v2y
      vy = v1z * v2x - v1x * v2z
      vz = v1x * v2y - v1y * v2x
                