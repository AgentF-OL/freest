module Conditional where

{-
method                   | expects             | receives            | from
#########################|#####################|#####################|###################
tripledFromDistanceThree | DistanceThree       | DistanceThree       | main
tripledFromDistanceFive  | DistanceFive        | DistanceThree       | tripledFromDistanceThree
tripledFromDistanceThree | DistanceFiveTripled | DistanceFiveTripled | tripledFromDistanceFive
main                     | DistanceFiveTripled | DistanceFiveTripled | tripledFromDistanceThree

where
receives      <: expects
DistanceThree <: DistanceFive
...
-}

type DistanceFive, DistanceThree, DistanceFiveTripled : *T

type DistanceFive = -- between -5 and 5 ...
  {x: Int | 0 <= (if (if x < 0 then -1 * x else x) > 0 then -1 * x else x) && 2 * (if x < 0 then -1 * x else x) + 1 <= 11}

type DistanceFiveTripled = -- between -15 and 15 ...
  {y: Int | (y < 0 => 0 <= -1 * y && -1 * y <= 15) && (y >= 0 => 0 <= y && y <= 15)}

type DistanceThree = {z: Int | -3 <= z && z <= 3}

tripledFromDistanceFive : DistanceFive -> DistanceFiveTripled
tripledFromDistanceFive x = 3 * x

tripledFromDistanceThree : DistanceThree -> DistanceFiveTripled
tripledFromDistanceThree x = tripledFromDistanceFive x

main : DistanceFiveTripled
main = tripledFromDistanceThree 2