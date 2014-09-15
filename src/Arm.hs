module Arm where

import Linear
import SpatialMath
import Vis

data Arm = Arm
     { length :: Float
     , radius :: Float
     , color  :: Color
     , rot :: Euler Float
     }

drawArm :: V3 Float
	-> Arm
	-> VisObject Float
drawArm from (Arm len rad col r) =
        Trans from $ RotEulerRad r $ Cylinder (len, rad) col

armEndPoint :: V3 Float
            -> Arm
            -> V3 Float
armEndPoint prev (Arm len _ _ r) =
           let newpos = prev ^+^ ((V3 0 0 len) *! (dcmOfEuler321 r))
           in newpos

addE :: Euler Float -> Euler Float -> Euler Float
addE (Euler a b c) (Euler d e f) = Euler (a + d) (b + e) (c + f)
