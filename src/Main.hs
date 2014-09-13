{-# OPTIONS_GHC -Wall #-}

module Main where

import Linear
import SpatialMath
import Vis

data Robot = Robot
     { start :: V3 Float
     , arms :: [Arm]
     }

data Arm = Arm
     { length :: Float
     , radius :: Float
     , color  :: Color
     , rot :: Euler Float
     }

drawArm :: V3 Float -> Arm -> VisObject Float
drawArm from (Arm length radius color rot) =
	Trans from $ RotEulerRad rot $ Cylinder (-length, radius) color

testArm :: Arm
testArm = Arm 1 0.5 red (Euler 0 0 0)

armEndPoint :: M33 Float -> V3 Float -> Arm -> (V3 Float, M33 Float)
armEndPoint prevrot prev (Arm len rad col rot) =
	   let rot33 = dcmOfEuler321 rot !*! prevrot
	       hom = prev
	       from = point $ prev
	   in (normalizePoint ((mkTransformationMat rot33 hom) !* from), rot33)

robot = Robot (V3 0 0 0) [testArm, testArm2]
	   
testArm2 :: Arm	   
testArm2 = Arm 3 0.3 white (Euler 0 1 0)

helloWorld :: VisObject Float
helloWorld = 
	(Text2d "Hello World" (100, 100)
	Helvetica12
	white)

{-
   Trans V3 parameters: Z - up down, negative is up
                        Y - in out, negative is in
                        X - left right, negative is left
-}

cyl = Trans (V3 0 (-10) (-5)) $ RotEulerRad (Euler 0 1 0) $ Cylinder (-5, 0.5) white

cyl2 = Trans (V3 0 (-10) (-5)) (Cylinder (5, 1) red)

ground = Trans (V3 0 0 0) (Plane (V3 0 0 1) yellow cyan)

sph = Trans (V3 0 (-10) (-5)) $ Sphere 1 Solid red

cur =
   let arm1start = (V3 0 0 0)
       arm1 = Arm 2 0.5 red (Euler 0 0 0)
       (arm1end, arm1rot) = armEndPoint eye3 arm1start arm1
       arm2 = Arm 3 0.3 white (Euler 0 0.6 0)
       (arm2end, arm2rot) = armEndPoint arm1rot arm1end arm2
       arm3 = Arm 2 0.1 yellow (Euler 0 0.35 0)
       (arm3end, arm3rot) = armEndPoint arm2rot arm2end arm3
       arm4 = Arm 1 0.1 blue (Euler 0 0.2 0)

       da1 = drawArm arm1start arm1
       da2 = drawArm arm1end arm2
       da3 = drawArm arm2end arm3
       da4 = drawArm arm3end arm4

   in VisObjects [ cyl, sph, cyl2, helloWorld, ground --,drawArm (V3 0 0 0) testArm, drawArm (armEndPoint (V3 0 0 0) testArm) testArm2
              , da1, da2, da3, da4]

main :: IO ()
main = display Nothing
	 	"Hello World"
		cur
