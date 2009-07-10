module Main where
import Data.List.Split
import Data.Ratio
import Control.Monad.Writer
import Control.Monad
import Data.List
import Data.Foldable (toList)
import Data.Function
import Debug.Trace
import System
import qualified Data.Set as S
import Control.Monad.State.Strict

type X = Float -- (Ratio Integer)
data Y = Unten | Y !(Float) | Oben deriving (Show, Eq, Ord)

data Point = Point !X !Y deriving (Eq, Ord)

--instance Ord Point where
--    compare = compare `on` x
--        where x (Point x0 _) = x0

instance Show Point where
    show (Point x (Y y)) = "("++show x' ++ " " ++ show y' ++ ") "
        where x', y' :: Float
              x' = x
              y' = y

data Ray = Ray { anfang :: !Point, ende :: !Point}
           deriving (Show, Ord, Eq)

areaL :: Lines -> Float
areaL [] = 0
areaL [_] = 0
areaL (p1:ps@(p2:_)) = areaTrapez p1 p2 + areaL ps
areaTrapez (Point (x0) (Y y0)) (Point ( x1) (Y y1))
    = (x1 - x0) * (y0+y1) / 2

areaCPoly :: CPoly -> Float
areaCPoly (CPoly lower upper) = areaL lower - areaL upper

range = (-1000,1000)
(rI,rS) = range
maxA = (rS-rI) ^ 2

areaSup :: Poly -> Float
areaSup p@(Poly lower@(l@(Point lx ly):_) upper@(u@(Point ux uy):_) inf sup)
    = if ly >= uy
      then areaCPoly (close p)
      else case meet inf sup of
             Nothing -> maxA
             Just (m@(Point mx _))
                 -> if mx <= lx || mx <= ux
                    then maxA
                    else areaCPoly (CPoly (m:lower) (m:upper))



meetScan :: X -> Ray -> Y
meetScan _ (Ray _ (Point _ Unten))
    = Unten
meetScan _ (Ray _ (Point _ Oben))
    = Oben
meetScan ( scan) (Ray (Point ( x0) (Y y0))
                       (Point ( x1) (Y y1)))
    = if dxRay == 0
      then if dyRay > 0
           then Oben
           else Unten
      else (Y (dyEcht + y0))
    where dxRay = x1 - x0
          dyRay = y1 - y0

          dxEcht = scan - x0
          dyEcht = --trace ("dxRay: " ++ show dxRay ++ "\n") $
                   (dyRay * dxEcht / dxRay)

type Lines = [Point]

--data Poly = Poly {lower :: Lines ,upper :: Lines
--                 ,inf   :: Ray   ,sup   :: Ray}
data Poly = Poly !Lines !Lines
                 !Ray !Ray deriving (Ord, Eq)

instance Show CPoly where
    show (CPoly l u) = "CPoly " ++ (concat . map show . reverse $ l) ++ "\n\t" ++ (concat . map show . reverse $ u)


instance Show Poly where
    show (Poly l u rInf rSup) = "Poly " ++ (concat . map show . reverse $ l) ++ "\n\t" ++ (concat . map show . reverse $ u) ++
                                "\n\t"++show rInf ++ "\n\t" ++ show rSup ++
                                "\n\t"++show (meet rInf rSup)
                        
meet :: Ray -> Ray -> Maybe Point
meet (Ray (Point ( x1) (Y y1))
          (Point ( x2) (Y y2)))
         (Ray (Point ( x3) (Y y3))
          (Point ( x4) (Y y4)))
    = if det == 0
      then Nothing
      else Just $ mkPoint ((x1*y2-y1*x2)*(x3-x4)-(x1-x2)*(x3*y4-y3*x4) / det
                          ,(x1*y2-y1*x2)*(y3-y4)-(y1-y2)*(x3*y4-y3*x4) / det)
    where det = ((x1-x2)*(y3-y4)-(y1-y2)*(x3-x4))
meet (Ray _ _) (Ray _ _) = Nothing



data CPoly = CPoly {l :: !Lines, u :: !Lines}
           deriving (Ord, Eq)

openNew :: Point -> Poly
openNew links@(Point x y) = (Poly [links] [links]
                             (Ray links (Point x Unten))
                             (Ray links (Point x Oben)))


-- WriterT Trace (State z) a

treatScan1o1 :: Point -> Poly -> WriterT [CPoly] (State Float) (S.Set Poly)
treatScan1o1 pN@(Point xN yN) polyO@(Poly lower@(l@(Point lx ly):_) 
                                          upper@(u@(Point ux uy):_)
                                          inf         sup)
    = if infNy < supNy
      then do bestSeen <- get 
              return . S.fromList . filter (((bestSeen-1)<= ) . areaSup) $ polyN
      else do let c = close polyO
              tell [] -- [c]
              modify (max (areaCPoly c))
              return $ S.empty
    where infNy = meetScan xN inf
          supNy = meetScan xN sup

          polyN = if yN <= infNy || supNy <= yN
                  then [Poly lower upper inf sup]
                  else [Poly (pN:lower) upper (Ray l pN) sup
                       ,Poly lower upper (Ray l pN) sup
                             
                       ,Poly lower (pN:upper) inf (Ray u pN)
                       ,Poly lower upper inf (Ray u pN)]

--           poly1 = if infNy > yN
--                   then Poly lower upper inf sup
--                   else Poly (pN:lower) upper (Ray l pN) sup
-- --                  Poly lower upper (max infY yN) sup
--           poly2 = if supNy < yN
--                   then Poly lower upper inf sup
--                   else Poly lower (pN:upper) inf (Ray u pN)
-- --                  Poly lower upper inf           (min supY yN)

close :: Poly -> CPoly
close polyO@(Poly lower@(l:_)
                  upper@(u:_)
                  _         _)
    = case compare l u of
        LT -> CPoly (u:lower) upper
        EQ -> CPoly lower upper
        GT -> CPoly lower (l:upper)

prop_close left lo up = l == u
    where (CPoly (l:_) (u:_)) = close (Poly (map mkPoint lo ++ [mkPoint left]) (map mkPoint up ++ [mkPoint left]) undefined undefined)

mkPoint (x,y) = Point ( x) (Y y)

run1p :: Point -> S.Set Poly -> WriterT [CPoly] (State Float) (S.Set Poly)
                             -- Writer [Poly] (S.Set Poly)
run1p pN polys = liftM (S.insert (openNew pN) . S.unions)
                 . sequence . fmap (treatScan1o1 pN) . S.toList
                 . trace ("#polys:\t" ++ show (S.size polys) ++ "\tpn:\t"++show pN)
                 $ polys

--                 >>= liftM ((:) (openNew pN))


run :: [Point] -> ((S.Set Poly, [CPoly]), Float)
run ps = (runState (runWriterT (doAll ps)) 0)
    --runWriter . doAll
      where doAll :: [Point] -> WriterT [CPoly] (State Float) (S.Set Poly)
                     -- Writer [Poly] (S.Set Poly)
            doAll = foldl (>>=) (return $ S.empty) . map run1p . sort

allClose = map close . uncurry (++)

bestCPoly :: [CPoly] -> (Float) --[Float]
bestCPoly cp = maximum . map (areaCPoly) $ cp


assoc f l = zip l (map f l)
assoc1 f l = zip (map f l) l

s0 :: Integer
s0 = 290797

f s = (s^2) `mod` 50515093



t = map p . splitEvery 2 . map c . tail . iterate f
    where c s = (s `mod` 2000) - 1000
          p [a,b] = Point (id . fromIntegral $ a) (Y . fromIntegral $ b)

t1 = map mkPoint $
     [(0,0)
     ,(1,1)
     ,(1,-1)
     ,(2,0)
     ,(2,0)
     ,(3,0)
     ]

t2 = map mkPoint $
     [
--      (-883, -38) 
--     (-827, -607) 
--     ,(-811, 286) 
--     (-754, 70) 
     (-665, -726) 
     ,(-646, -422) 
     ,(-488, 732) -- kaputt
     ,(-477, -23) 
     ,(-454, -947) 
     ,(-389, 938) 
     ,(-178, 540)  -- kaputt
     ,(-60, 138) 
     ,(273, -175) 
     ,(913, -860) ]
--     [(-883, -38) ,(-827, -607) ,(-811, 286) ,(-754, 70) ,(-665, -726) ,(-646, -422) ,(-488, 732) ,(-477, -23) ,(-454, -947) ,(-389, 938) ,(-178, 540) ,(-60, 138) ,(54, 316) ,(273, -175) ,(331, 698) ,(491, 368) ,(527, 144) ,(806, 528) ,(913, -860) ]
     

main = do {- putStrLn $ unlines $ map show $ c
          putStrLn "---"
          
          putStrLn $ unlines $ map show $ o
          putStrLn "---"
          putStrLn $ unlines $ map show $ allClose (o,c)-}
--          putStrLn "---"
          args <- getArgs
          let n = read (args !! 0)
              ((o,c),bestClosed) = run $  take n (t s0)
              bestOpen = bestCPoly $ map close (toList o)
          print bestClosed
          print bestOpen
          putStrLn $ show $ max bestClosed bestOpen 
--          print $ sort t'
          
    where 
          t' = undefined
          _ = t2