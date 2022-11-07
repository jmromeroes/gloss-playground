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
  , wScale    :: Float
  , nearZ     :: Float
  , farZ      :: Float
  , fovy      :: Float
  , points    :: [[ThreeDPoint]]}
  deriving Show

data ThreeDPoint
  = ThreeDPoint
  { x :: Float
  , y :: Float
  , z :: Float
  } deriving Show

main :: IO ()
main = do
      let w = World { width = 800
                    , height = 600
                    , wScale = 20
                    , nearZ = 1
                    , farZ = 10
                    , fovy = pi/2
                    , points = getPointsFromRowsAndCols w
                    }

      simulate (InWindow "Gloss Playground" (round (width w) :: Int, round (height w) :: Int) (10, 10))
        black 30 w renderTriangles iteration

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
  
projectionMatrixOrtho :: M.Matrix Float
projectionMatrixOrtho = M.fromLists [[1, 0, 0], [0, 1, 0]]

projectedMatrixOrtho :: M.Matrix Float -> M.Matrix Float
projectedMatrixOrtho = M.multStd2 projectionMatrixOrtho

projectionMatrixPers :: World -> M.Matrix Float
projectionMatrixPers World{..} =
  M.fromLists [[d/(width/height), 0, 0 , 0],
               [0, d, 0, 0],
               [0, 0, (-nearZ - farZ)/(nearZ-farZ), (2*farZ*nearZ)/(nearZ-farZ)],
               [0, 0, 1, 0]]

  where
    d = 1/tan(fovy/2)

toRadians :: Float -> Float
toRadians = (*) (pi/180)

transformedMatrixPers :: Float -> World -> M.Matrix Float -> M.Matrix Float
transformedMatrixPers angle w = M.multStd2 (M.multStd2 (projectionMatrixPers w) ((transformationMatrix . toRadians) angle))
  where
    transformationMatrix radians =
      M.multStd (M.multStd2 (rotationMatrixPersZ radians) (rotationMatrixPersX radians)) $ rotationMatrixPersY 30
    
fromMatrixToPoint :: M.Matrix Float -> Point
fromMatrixToPoint m = (vector V.! 0, vector V.! 1)
  where
    vector :: V.Vector Float
    vector = M.getCol 1 m
    
getPointsFrom3D :: ThreeDPoint -> Point
getPointsFrom3D pt = (x pt, y pt)

rotationMatrixOrthoX :: Float -> M.Matrix Float
rotationMatrixOrthoX angle =
  M.fromLists [[1, 0, 0],
               [0, cos angle, -sin angle ],
               [0, sin angle, cos angle ]]

rotationMatrixOrthoY :: Float -> M.Matrix Float
rotationMatrixOrthoY angle =
  M.fromLists [[0, cos angle, -sin angle ],
               [1, 0, 0],
               [0, sin angle, cos angle ]]

rotationMatrixOrthoZ :: Float -> M.Matrix Float
rotationMatrixOrthoZ angle =
  M.fromLists [[0, cos angle, -sin angle ],
               [0, sin angle, cos angle ],
               [1, 0, 0]]


rotationMatrixPersX :: Float -> M.Matrix Float
rotationMatrixPersX angle =
  M.fromLists [[1, 0, 0, 0],
               [0, cos angle, -sin angle, 0],
               [0, sin angle, cos angle, 0],
               [0, 0, 0, 1]]

rotationMatrixPersY :: Float -> M.Matrix Float
rotationMatrixPersY angle =
  M.fromLists [[cos angle, 0, sin angle, 0],
               [0, 1, 0, 0],
               [-sin angle, 0, cos angle, 0],
               [0, 0, 0, 1]]

rotationMatrixPersZ :: Float -> M.Matrix Float
rotationMatrixPersZ angle =
  M.fromLists [[cos angle, -sin angle, 0, 0],
               [sin angle, cos angle, 0, 0],
               [0, 0, 1, 0],
               [0, 0, 0, 1]]  

rotateXOrtho :: M.Matrix Float -> Float -> M.Matrix Float
rotateXOrtho points angle =
  M.multStd2 (rotationMatrixOrthoX angle) points

rotateYOrtho :: M.Matrix Float -> Float -> M.Matrix Float
rotateYOrtho points angle =
  M.multStd2 (rotationMatrixOrthoY angle) points

rotateZOrtho :: M.Matrix Float -> Float -> M.Matrix Float
rotateZOrtho points angle =
  M.multStd2 (rotationMatrixOrthoZ angle) points

rotateXPers :: M.Matrix Float -> Float -> M.Matrix Float
rotateXPers points angle =
  M.multStd2 (rotationMatrixPersX angle) points

rotateYPers :: M.Matrix Float -> Float -> M.Matrix Float
rotateYPers points angle =
  M.multStd2 (rotationMatrixPersY angle) points

rotateZPers :: M.Matrix Float -> Float -> M.Matrix Float
rotateZPers points angle =
  M.multStd2 (rotationMatrixPersZ angle) points


from3DPoint :: ThreeDPoint -> M.Matrix Float
from3DPoint ThreeDPoint{..} = M.fromLists [[x],
                                           [y],
                                           [z]]

from3DPointPers :: ThreeDPoint -> M.Matrix Float
from3DPointPers ThreeDPoint{..} = M.fromLists [[x],
                                               [y],
                                               [z],
                                               [1]]

rotatedPtsOrtho :: ThreeDPoint -> ThreeDPoint
rotatedPtsOrtho point =
  let rotatedM = rotateZOrtho (rotateYOrtho (from3DPoint point) 400) 400
      row = M.getCol 1 rotatedM
  in ThreeDPoint (row V.! 0) (row V.! 1) (row V.! 2)

rotatedPtsPers :: ThreeDPoint -> ThreeDPoint
rotatedPtsPers point =
  let rotatedM = rotateZPers (rotateXPers (from3DPointPers point) 45) 45
      row = M.getCol 1 rotatedM
  in ThreeDPoint (row V.! 0) (row V.! 1) (row V.! 2)

transformPointOrtho :: ThreeDPoint -> Point
transformPointOrtho = fromMatrixToPoint . projectedMatrixOrtho . from3DPoint . rotatedPtsOrtho

transformPointPers :: World -> ThreeDPoint -> Point
transformPointPers w = fromMatrixToPoint . transformedMatrixPers 45 w . from3DPointPers

renderTriangles :: World -> Picture
renderTriangles w =
  color white $ Pictures polygons

  where
    coordinates = do
      y' <- [0..(length (points w) - 2)]
      x' <- [0..(length (points w) - 2)]
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
               (map . map) (transformPointPers w) (points w)
               
iteration :: ViewPort -> Float -> World -> World
iteration _vp _step w = w  
    
