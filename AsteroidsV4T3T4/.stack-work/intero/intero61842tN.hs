module Main where
import Graphics.Gloss --Olin ilmesesti importannut myös Monadit (join) ja Bifunktorit (bimap)
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Pure.Simulate
import Graphics.Gloss.Interface.Pure.Display
import Debug.Trace

{-

--b) ohjelma vaikuttaa hieman raskaalta ja parametrien kuljettaminen ainakin tässä mallissa tuntuu vielä aika kömpelöltä.
--Olipa hommaa saada melkein toimimaan. Keksisin lisää mitä ongelmia listojen kanssa on mutta taidan mennä nukkumaan.
--bugeja olis vielä aika läjä korjata.

-}

data AsteroidWorld = Play [Rock] Ship [Bullet] UFO | GameOver deriving (Eq,Show) --lisäsin tähän UFO tyypin        
data Ship = Ship PointInSpace Velocity deriving (Eq,Show)
data Bullet = Bullet PointInSpace Velocity Age deriving (Eq,Show)
data Rock = Rock PointInSpace Size Velocity deriving (Eq,Show)
data UFO = UFO PointInSpace Size Velocity (UfoPhase Action) deriving (Eq,Show)--lisäsin uuden ufo datatyypin.

data Action = Scan [Rock] PointInSpace Int | Move Int | Shoot Rock Int deriving (Eq,Show)
data UfoPhase a = Phase (a, UfoPhase a)| Empty deriving (Eq,Show)
type Velocity = (Float, Float)
type Size = Float
type Age = Float

bulletVelocity = -150
waitAfterShot = 120
scannerLength = 130
scannerTimer = 60
spotArea = 0.1
moveTime = 60
ufoVelocity = (10,12)

initialWorld :: AsteroidWorld
initialWorld = Play
                   [Rock (150,150)  45 (2,6)    
                   ,Rock (-45,201)  45 (13,-8) 
                   ,Rock (45,22)    25 (-2,8)  
                   ,Rock (-210,-15) 30 (-2,-8) 
                   ,Rock (-45,-201) 25 (8,2)   
                   ] -- The default rocks
                   (Ship (0,0) (0,5)) -- The initial ship
                   --Annoin tässä playn construktorille oman ufoni.x
                   [] -- The initial bullets (none)
                   (UFO (41,-100) 20 (0,0) (Phase(Move moveTime,Empty)))

simulateWorld :: Float -> (AsteroidWorld -> AsteroidWorld)
simulateWorld _ GameOver = GameOver  
simulateWorld timeStep (Play rocks (Ship shipPos shipV) bullets ufo)
  |any (collidesWith shipPos) rocks = GameOver
  |otherwise = Play
   (concatMap updateRock rocks) 
   (Ship newShipPos shipV)
   (concat (map updateBullet (didUfoShoot ufo bullets)))
   (doUfoAction ufo)
  where
    doUfoAction :: UFO -> UFO --TOD
    doUfoAction (UFO point size v Empty)
      = UFO point size (0,0) (Phase (Move moveTime, (Phase (Scan rocks (1.0,0.0) scannerTimer, Empty))))
    doUfoAction (UFO point size v (Phase (x, xs))) = case x of
      Scan (r:rs) direction timer ->  if sameDirection r point direction
        then (UFO point size v (Phase (Shoot r 0, xs)))
        else doUfoAction $ UFO point size v (Phase (Scan rs direction timer, xs))
      Scan [] direction timer -> UFO point size v (Phase (Scan rocks (rotateV 0.1 direction) (timer-1), xs))
      Move time -> if time>0
        then UFO (restoreToScreen (point .+ timeStep .* ufoVelocity)) size ufoVelocity (Phase (Move (time - 1),xs))
        else UFO point size 0 xs
      Shoot rock timer -> if timer==waitAfterShot
        then (UFO point size v xs)
        else (UFO point size v (Phase ((Shoot rock (timer+1)),xs)))

    sameDirection (Rock p s v) ufoPoint scanner
      = if (distance<scannerLength+s) && spotArea>magV(norm scanner .- rockDirection)
      then True
      else False
      where
        rockDirectionVector = p .- ufoPoint
        distance = magV rockDirectionVector
        rockDirection = norm rockDirectionVector
        
    didUfoShoot :: UFO -> [Bullet]->[Bullet]
    didUfoShoot (UFO point size v (Empty)) bullets = []
    didUfoShoot (UFO point size v (Phase (Shoot rock@(Rock locationR sizeR vR) 0, xs))) bullets
      =(Bullet point (bulletVector) 0):bullets
      where
        normDist = norm (locationR.-point)
        vYL = innerProduct normDist vR
        vXL = vR - vYL .* normDist
        vBYL = sqrt (bulletVelocity * bulletVelocity + magV vXL * magV vXL)
        bulletVector = (-bulletVelocity) .* norm (vBYL .* normDist .+ vXL)
        
    didUfoShoot (UFO point size v _ ) bullets = bullets
    
    newRockLocation (Rock locationR sizeR vR) point = (-bulletVelocity .* (norm (locationR .- point))) .+ (vR)
                                                                 
    collidesWith :: PointInSpace -> Rock -> Bool
    collidesWith p (Rock rp s _) = magV (rp .- p) < s 
 
    collidesWithBullet :: Rock -> Bool
    collidesWithBullet r = any (\(Bullet bp _ _) -> collidesWith bp r) bullets

    updateRock :: Rock -> [Rock]
    updateRock r@(Rock p s v) 
      |collidesWithBullet r && s < 7 = []
      |collidesWithBullet r && s > 7 = splitRock r
      |otherwise = [Rock (restoreToScreen (p .+ timeStep .* v)) s v]
      
    updateBullet :: Bullet -> [Bullet] 
    updateBullet (Bullet p v a) 
      |a > 5 = []
      |any (collidesWith p) rocks = [] 
      |otherwise = [Bullet (restoreToScreen (p .+ timeStep .* v)) v (a + timeStep)] 
      
    newShipPos :: PointInSpace
    newShipPos = restoreToScreen (shipPos .+ timeStep .* shipV)

splitRock :: Rock -> [Rock]
splitRock (Rock p s v) = [Rock p (s/2) (3 .* rotateV (pi/3)  v)
                         ,Rock p (s/2) (3 .* rotateV (-pi/3) v) ]

restoreToScreen :: PointInSpace -> PointInSpace
restoreToScreen (x,y) = (cycleCoordinates x, cycleCoordinates y)

cycleCoordinates :: (Ord a, Num a) => a -> a
cycleCoordinates x 
  | x < (-400) = 800+x
  | x > 400    = x-800
  | otherwise  = x

drawWorld :: AsteroidWorld -> Picture
drawWorld GameOver 
  = scale 0.3 0.3 
    . translate (-400) 0 
    . color red 
    . text 
    $ "Game Over!"
     
drawWorld (Play rocks (Ship (x,y) (vx,vy)) bullets dataUFO@(UFO (xUFO,yUFO) s v exe))
  = pictures $[ship, asteroids, shots, ufo, scanner] where
  
     ship = color red (pictures [translate x y (circle 10)])
     
     asteroids = pictures [translate x y (color orange (circle s)) 
                          | Rock   (x,y) s _ <- rocks]
                 
     shots = pictures [translate x y (color red (circle 2)) 
                      | Bullet (x,y) _ _ <- bullets]
             
     ufo = translate xUFO yUFO (color violet (circle s))
                    
                
     scanner = case (trace "yritettiin piirtaa jotakin skannaukseen liittyen" scanning) of
       Nothing -> blank
       Just direction -> (color green (line ((scannerLength .* direction) .+ (xUFO,yUFO) :( xUFO, yUFO) : [])))
                         
     scanning = case dataUFO of
       UFO _ _ _ (Phase (Scan rocks direction timer, _)) -> Just direction
       UFO _ _ _ _ -> Nothing
    
handleEvents :: Event -> AsteroidWorld -> AsteroidWorld
handleEvents (EventKey (MouseButton LeftButton) Down _ clickPos) GameOver = initialWorld
handleEvents _ GameOver = GameOver
handleEvents (EventKey (MouseButton LeftButton) Down _ clickPos)
  (Play rocks (Ship shipPos shipVel) bullets ufo)
  = Play rocks (Ship shipPos newVel) (newBullet : bullets) ufo
  where 
    newBullet = Bullet shipPos (bulletVelocity .* norm (shipPos .- clickPos)) 0
    newVel = shipVel .+ (50 .* norm (shipPos .- clickPos))  
handleEvents _ w = w



type PointInSpace = (Float, Float)
(.-) , (.+) :: PointInSpace -> PointInSpace -> PointInSpace
(x,y) .- (u,v) = (x-u,y-v)
(x,y) .+ (u,v) = (x+u,y+v)

(.*) :: Float -> PointInSpace -> PointInSpace
s .* (u,v) = (s*u,s*v)

infixl 6 .- , .+
infixl 7 .*

norm :: PointInSpace -> PointInSpace
norm (x,y) = let m = magV (x,y) in (x/m,y/m)

magV :: PointInSpace -> Float
magV (x,y) = sqrt (x**2 + y**2) 

limitMag :: Float -> PointInSpace -> PointInSpace
limitMag n pt = if (magV pt > n) then n .* (norm pt) else pt

rotateV :: Float -> PointInSpace -> PointInSpace
rotateV r (x,y) = (x * cos r - y * sin r
                  ,x * sin r + y * cos r)

innerProduct ::PointInSpace -> PointInSpace -> Float
innerProduct (x1,x2) (y1,y2)=x1*y1+y2*y1


main = play
         (InWindow "Asteroids!" (550,550) (20,20)) 
         black 
         60
         initialWorld 
         drawWorld
         handleEvents
         simulateWorld
