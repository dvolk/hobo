module Main where

import Debug.Trace

import Linear
import SpatialMath
import Vis

import Robot
import Arm

-- axes: Z - negative is up
--       Y - negative is in
--       X - negative is left

cur :: Float -> VisObject Float
cur t =
   let x = case () of
            _
             | t >= 0     && t <= 13.15 -> t / 15
	     | t >= 13.15 && t <= 39.45 -> (26.30 - t) / 15
	     | otherwise                -> -13.15/15

       y = case () of
            _
	     | t >= 0     && t <= 13.15 -> 0
	     | t >= 13.15 && t <= 39.45 -> t - 13.15
	     | otherwise                -> 39.45 - 13.15

       arm1 = Arm 2   0.5  red    (Euler y 0 0)
       arm2 = Arm 1.8 0.25 white  (rot arm1 `addE` Euler 0 x 0)
       arm3 = Arm 1.6 0.12 yellow (rot arm2 `addE` Euler 0 x 0)
       arm4 = Arm 1.6 0.09 white  (rot arm3 `addE` Euler 0 x 0)
       arm5 = Arm 1.4 0.06 blue   (rot arm4 `addE` Euler 0 x 0)

       arm1start = V3 0 0 0
       arm1end = armEndPoint arm1start arm1
       arm2end = armEndPoint arm1end arm2
       arm3end = armEndPoint arm2end arm3
       arm4end = armEndPoint arm3end arm4
       arm5end = armEndPoint arm4end arm5

       joint1 = Trans arm1end $ Sphere 0.45 Solid red
       joint2 = Trans arm2end $ Sphere 0.20 Solid white
       joint3 = Trans arm3end $ Sphere 0.10 Solid yellow
       joint4 = Trans arm4end $ Sphere 0.07 Solid white
       joint5 = Trans arm5end $ Sphere 0.20 Solid blue

       da1 = drawArm arm1start arm1
       da2 = drawArm arm1end arm2
       da3 = drawArm arm2end arm3
       da4 = drawArm arm3end arm4
       da5 = drawArm arm4end arm5

       ground = Plane (V3 0 0 1) yellow cyan

       text1 = Text2d ("naklon: " ++ show (round $ x * 180/pi) ++ "\176") (50, 50)  TimesRoman24 white
       text2 = Text2d ("rotacija: "  ++ show (round $ y * 180/pi) ++ "\176") (50, 100) TimesRoman24 white

       robo = RotEulerDeg (Euler 0 180 0) $ VisObjects [da5, da4, da3, da2, da1, joint1, joint2, joint3, joint4, joint5, text1, text2]

   in VisObjects [ ground
                 , robo
		 ]

main :: IO ()
main = animate (Just ((800, 600), (0,0)))
	 	"Hobo robo"
		cur
