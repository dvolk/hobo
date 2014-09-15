module Robot where

import Linear

import Arm

data Robot = Robot
     { start :: V3 Float
     , arms :: [Arm]
     }
