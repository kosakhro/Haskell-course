module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Pure.Simulate
import Graphics.Gloss.Interface.Pure.Display

data AsteroidWorld = Play [Rock] Ship Ufo [Bullet] -- Extended with new Ufo type
                   | GameOver
                   deriving (Eq,Show)

type Velocity     = (Float, Float)  
type Size         = Float          
type Age          = Float
type Health       = Float  
type Exploding    = Float        


data State = Fleeing Velocity Health
           | Hunting Velocity Health
           | Exploding 
    deriving (Eq,Show)

data Ship   = Ship   PointInSpace Velocity
    deriving (Eq,Show)
data Ufo    = Ufo  PointInSpace State -- New type for Ufo,  upgraded with health 
    deriving (Eq, Show)
data Bullet = Bullet PointInSpace Velocity Age
    deriving (Eq,Show)
data Rock   = Rock   PointInSpace Size Velocity
    deriving (Eq,Show)



initialWorld :: AsteroidWorld
initialWorld = Play
                   [Rock (150,150)  45 (2,6)
                   ,Rock (-45,201)  45 (13,-8)
                   ,Rock (45,22)    25 (-2,8)
                   ,Rock (-210,-15) 30 (-2,-8)
                   ,Rock (-45,-201) 25 (8,2)
                   ] -- The default rocks
                   (Ship (0,0) (0,0)) -- The initial ship
                   (Ufo  (20, 20) (20, 20) 2.2) -- The initial Ufo
                   [] -- The initial bullets (none)


simulateWorld :: Float -> (AsteroidWorld -> AsteroidWorld)

simulateWorld _        GameOver          = GameOver

simulateWorld timeStep (Play rocks (Ship shipPos shipV) (Ufo ufoPos ufoV 2.2) bullets) -- Ufo added
  | any (collidesWith shipPos) rocks = GameOver
  | otherwise = Play (concatMap updateRock rocks)
                              (Ship newShipPos shipV)
                              (Ufo newUfoPos ufoV 2.2) -- Ufo added
                              (concat (map updateBullet bullets))
  where
      collidesWith :: PointInSpace -> Rock -> Bool
      collidesWith p (Rock rp s _)
       = magV (rp .- p) < s

      collidesWithBullet :: Rock -> Bool
      collidesWithBullet r
       = any (\(Bullet bp _ _) -> collidesWith bp r) bullets

      updateRock :: Rock -> [Rock]
      updateRock r@(Rock p s v)
       | collidesWithBullet r && s < 7
            = []
       | collidesWithBullet r && s > 7
            = splitRock r
       | otherwise
            = [Rock (restoreToScreen (p .+ timeStep .* v)) s v]

      updateBullet :: Bullet -> [Bullet]
      updateBullet (Bullet p v a)
        | a > 5
             = []
        | any (collidesWith p) rocks
             = []
        | otherwise
             = [Bullet (restoreToScreen (p .+ timeStep .* v)) v
                       (a + timeStep)]

      newShipPos :: PointInSpace
      newShipPos = restoreToScreen (shipPos .+ timeStep .* shipV)
  
      newUfoPos :: PointInSpace -- calculating Ufo position
      newUfoPos = restoreToScreen (ufoPos .+ timeStep .* (ufoV + (rotateV (pi/3) (2,2))))
  
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

drawWorld GameOver  -- changed to handle restart option
   = pictures 
   [scale 0.3 0.3 
   . translate (-400) 0 
   . color red 
   . text 
   $ "Game Over!",
    scale 0.1 0.1 
   . translate (-100) (-200)
   . color black 
   . text 
   $ "Push Key Up to restart"]

drawWorld (Play rocks (Ship (x,y) (vx,vy)) (Ufo (fx,fy) (fvx, fvy) 2.2) bullets) -- Ufo added
  = pictures [ship, asteroids, ufo, shots]
   where
    ship      = color red (pictures [translate x y (circle 10)])
    asteroids = pictures [(color orange (polygon (asteroidPolygon x y s))) -- change rock shape
                         | Rock   (x,y) s _ <- rocks]
    ufo       = color green (pictures [translate fx fy (circle 10)]) -- Ufo added
    shots     = pictures [translate x y (color red (circle 2))
                         | Bullet (x,y) _ _ <- bullets]
                         
asteroidPolygon :: Float -> Float -> Float -> [Point]
asteroidPolygon x y s = [ (x+1,y+s-2),(x-1+s,y+2), (x,y+1),(x+s,y-s),(x-s,y+s),(x+0.8*s,y+0.1*s),(x-0.5*s,y-0.5*s),(x-0.8*s,y+0.2*s) ]

handleEvents :: Event -> AsteroidWorld -> AsteroidWorld

handleEvents (EventKey (SpecialKey KeyUp) Down _ _) GameOver -- handle restart
             = initialWorld


handleEvents (EventKey (MouseButton LeftButton) Down _ clickPos)
             (Play rocks (Ship shipPos shipVel) ufo bullets) 
             = Play rocks (Ship shipPos newVel) ufo
                          (newBullet : bullets)
 where
     newBullet = Bullet shipPos
                        (negate 150 .* norm (shipPos .- clickPos))
                        0
     newVel    = shipVel .+ (50 .* norm (shipPos .- clickPos))

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
limitMag n pt = if (magV pt > n)
                  then n .* (norm pt)
                  else pt

rotateV :: Float -> PointInSpace -> PointInSpace
rotateV r (x,y) = (x * cos r - y * sin r
                  ,x * sin r + y * cos r)


main = play
         (InWindow "Asteroids!" (550,550) (20,20))
         blue
         24
         initialWorld
         drawWorld
         handleEvents
         simulateWorld
