{-# LANGUAGE RecordWildCards #-}

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
      let pts = getPointsFromRowsAndCols w
      
      simulate (InWindow "Gloss Playground" (round (width w) :: Int, round (height w) :: Int) (10, 10))
        black 30 pts renderTriangles iteration

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
                  , y = (fromIntegral row * scl) - (height'/2)
                  , z = 0
                  }
  
projectionMatrix :: M.Matrix Float
projectionMatrix = M.fromLists [[1, 0, 0], [0, 1, 0]]

projectedMatrix :: M.Matrix Float -> M.Matrix Float
projectedMatrix = M.multStd2 projectionMatrix

fromMatrixToPoint :: M.Matrix Float -> Point
fromMatrixToPoint m = (vector V.! 0, vector V.! 1)
  where
    vector :: V.Vector Float
    vector = M.getCol 1 m
getPointsFrom3D :: ThreeDPoint -> Point
getPointsFrom3D pt = (x pt, y pt)

rotationMatrixX :: Float -> M.Matrix Float
rotationMatrixX angle =
  M.fromLists [[1, 0, 0],
               [0, cos angle, -sin angle ],
               [0, sin angle, cos angle ]]

rotationMatrixY :: Float -> M.Matrix Float
rotationMatrixY angle =
  M.fromLists [[0, cos angle, -sin angle ],
               [1, 0, 0],
               [0, sin angle, cos angle ]]

rotationMatrixZ :: Float -> M.Matrix Float
rotationMatrixZ angle =
  M.fromLists [[0, cos angle, -sin angle ],
               [0, sin angle, cos angle ],
               [1, 0, 0]]

rotateX :: M.Matrix Float -> Float -> M.Matrix Float
rotateX points angle =
  M.multStd2 (rotationMatrixX angle) points

rotateY :: M.Matrix Float -> Float -> M.Matrix Float
rotateY points angle =
  M.multStd2 (rotationMatrixY angle) points

rotateZ :: M.Matrix Float -> Float -> M.Matrix Float
rotateZ points angle =
  M.multStd2 (rotationMatrixZ angle) points

from3DPoint :: ThreeDPoint -> M.Matrix Float
from3DPoint ThreeDPoint{..} = M.fromLists [[x],
                                           [y],
                                           [z]]

rotatedPts :: ThreeDPoint -> ThreeDPoint
rotatedPts point =
  let rotatedM = rotateZ (rotateY (from3DPoint point) 400) 400
      row = M.getCol 1 rotatedM
  in ThreeDPoint (row V.! 0) (row V.! 1) (row V.! 2)

transformPoint :: ThreeDPoint -> Point
transformPoint = fromMatrixToPoint . projectedMatrix . from3DPoint . rotatedPts

renderTriangles :: [[ThreeDPoint]] -> Picture
renderTriangles pts =
  color white $ Pictures polygons

  where
    coordinates = do
      y' <- [0..(length pts - 2)]
      x' <- [0..(length pts - 2)]
      return (x', y')
  
    polygons =
      do map getTriangleFromPoints coordinates

    getTriangleFromPoints (y', x') =
           Pictures [ line [mappedPts!!y'!!x', mappedPts!!(y'+1)!!x']
                    , line [mappedPts!!(y'+1)!!x', mappedPts!!(y'+1)!!(x'+1)]
                    , line [mappedPts!!(y'+1)!!(x'+1), mappedPts!!y'!!x']
                    ]
           where
             mappedPts :: [[Point]]
             mappedPts =
               (map . map) transformPoint pts         
               
iteration :: ViewPort -> Float -> [[ThreeDPoint]] -> [[ThreeDPoint]]
iteration _vp _step pts = pts
