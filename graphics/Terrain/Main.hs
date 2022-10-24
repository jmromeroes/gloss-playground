{-# LANGUAGE ParallelListComp #-}
-- Implementation of unstable terrain animation inspired in
-- Coding Challenge #11: 3D Terrain Generation with Perlin Noise in Processing
-- https://www.youtube.com/watch?v=IKB1hWWedMk

module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Simulate
import qualified Data.Matrix as M
import qualified Data.Vector as V

data World
  = World
  { width     :: Float
  , height    :: Float
  , wScale     :: Float }
  deriving Show

data ThreeDPoint
  = ThreeDPoint
  { x :: Float
  , y :: Float
  , z :: Float
  } deriving Show

main :: IO ()
main = do
      let w = World { width = 600
                    , height = 600
                    , wScale = 20
                    }
      let scl = wScale w
      let cols = round (width w / scl) :: Integer
      let rows = round (height w / scl) :: Integer
      let pts = (getPointsFromRowsAndCols w) :: [[ThreeDPoint]]
      
      simulate (InWindow "Gloss Playground" (round (width w) :: Int, round (height w) :: Int) (10, 10))
        black 30 ( pts) (renderTriangles w) iteration

getPointsFromRowsAndCols :: World -> [[ThreeDPoint]]
getPointsFromRowsAndCols w = do
  let cols = round (width' / scl) :: Integer
  let rows = round (height' / scl) :: Integer
  [[createPoint col row | col <- [0..cols]] | row <- [0..rows]]

  where
    scl = wScale w
    width' = width w
    height' = height w
    
    createPoint :: Integer -> Integer -> ThreeDPoint
    createPoint col row =
      ThreeDPoint { x = (fromIntegral col * scl) - width'/2
                  , y = ((fromIntegral row * scl) - (height'/2))
                  , z = 0
                  }
      
  
projectionMatrix :: M.Matrix Float
projectionMatrix = M.fromLists [[1, 0, 0], [0, 1, 0]]

fromMatrixToPoint :: M.Matrix Float -> Point
fromMatrixToPoint m = (vector V.! 0, vector V.! 1)
  where
    vector :: V.Vector Float
    vector = M.getRow 0 m

getPointsFrom3D :: ThreeDPoint -> Point
getPointsFrom3D pt = (x pt, y pt)

rotateX :: M.Matrix Float -> Float -> M.Matrix Float
rotateX points angle = undefined
  
renderTriangles :: World -> [[ThreeDPoint]] -> Picture
renderTriangles w pts =
  color white $ Pictures polygons
  where
    coordinates = do
      y <- [0..(length pts - 2)]
      x <- [0..(length pts - 2)]
      return (x, y)

    pointFromCoordinate :: Int -> Int -> ThreeDPoint
    pointFromCoordinate x y = pts!!y!!x
      
    polygons =
      do map getTriangleFromPoints coordinates

    getTriangleFromPoints (y', x') =
           Pictures [ line [x $ pts!!y'!!x', y $ pts!!(y'+1)!!x']
                    , line [pts!!(y+1)!!x, pts!!(y+1)!!(x+1)]
                    , line [pts!!(y+1)!!(x+1), pts!!y!!x]
                    ]

           where
             mappedPts =
               map (\row -> map )
             
           

iteration :: ViewPort -> Float -> [[ThreeDPoint]] -> [[ThreeDPoint]]
iteration _vp _step pts = pts
