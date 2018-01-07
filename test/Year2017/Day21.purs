module Test.Year2017.Day21 where

-- http://adventofcode.com/2017/day/21

import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Either (Either(..))
import Data.Functor (map)

import Data.Show (show)
import Data.Tuple.Nested ((/\))
import Prelude (Unit, ($), (<>), (==))
import Year2017.Day21 (solution, splitArray, flattenArray)
import Year2017.Day21.Input (testInput, input)
import Year2017.Day21.Types (Pixel(..))

day21Tests :: forall e. Eff (console :: CONSOLE | e) Unit
day21Tests = foreachE (tests) (\(f /\ inputs /\ iterations /\ expected) ->
                              let
                                res = f iterations inputs
                              in do
                                log $ show (res == expected)
                                  <> " ---"
                                  <> " should be: "
                                  <> show expected
                                  <> ", was "
                                  <> show res) where

  tests = map (solution /\ _) [ testInput /\ 2  /\ (Right 12)
                              , input     /\ 5  /\ (Right 197)
                              , input     /\ 18 /\ (Right 3081737)
                              ]

day21Tests2 :: forall e. Eff (console :: CONSOLE | e) Unit
day21Tests2 = foreachE (tests) (\(f /\ inputs /\ expected) ->
                              let
                                res = f inputs
                              in do
                                log $ show (res == expected)
                                  <> " ---"
                                  <> " should be: "
                                  <> show expected
                                  <> ", was "
                                  <> show res) where

  tests = [ flattenArray /\ array6By2Res /\ array6
          , flattenArray /\ array6By3Res /\ array6
          , flattenArray /\ array8By4Res /\ array8
          , flattenArray /\ array8By2Res /\ array8
          , flattenArray /\ array12By1Res /\ array12Int
          , flattenArray /\ array12By2Res /\ array12Int
          , flattenArray /\ array12By3Res /\ array12Int
          , flattenArray /\ array12By4Res /\ array12Int
          , flattenArray /\ array12By6Res /\ array12Int
          ]


day21Tests3 :: forall e. Eff (console :: CONSOLE | e) Unit
day21Tests3 = foreachE (tests) (\(f /\ inputs) ->
                              let
                                res = f inputs
                              in do
                                log $ show (res == inputs)
                                  <> " ---"
                                  <> " should be: "
                                  <> show inputs
                                  <> ", was "
                                  <> show res) where

  array81 = [[1, 2, 3, 4, 5, 6, 7, 8, 9 ],
             [10,11,12,13,14,15,16,17,18],
             [19,20,21,22,23,24,25,26,27],
             [28,29,30,31,32,33,34,35,36],
             [37,38,39,40,41,42,43,44,45],
             [56,47,48,49,50,51,52,53,54],
             [55,56,57,58,59,60,61,62,63],
             [64,65,66,67,68,69,70,71,72],
             [73,74,75,76,78,78,79,80,81]]
  tests = [ (\i -> flattenArray (splitArray 2 i)) /\ array6
          , (\i -> flattenArray (splitArray 3 i)) /\ array6
          , (\i -> flattenArray (splitArray 3 i)) /\ array81
          ]

array6 :: Array (Array Int)
array6 = [[1, 2, 3, 4, 5, 6],
          [7, 8, 9,10,11,12 ],
          [13,14,15,16,17,18],
          [19,20,21,22,23,24],
          [25,26,27,28,29,30],
          [31,32,33,34,35,36]]

array8 :: Array (Array Int)
array8 = [[1,  2,  3,  4,  5,  6,  7,  8 ],
          [9,  10, 11, 12, 13, 14, 15, 16],
          [17, 18, 19, 20, 21, 22, 23, 24],
          [25, 26, 27, 28, 29, 30, 31, 32],
          [33, 34, 35, 36, 37, 38, 39, 40],
          [41, 42, 43, 44, 45, 46, 47, 48],
          [49, 50, 51, 52, 53, 54, 55, 56],
          [57, 58, 59, 60, 61, 62, 63, 64]]

array12Int :: Array (Array Int)
array12Int = [[1,   2,   3,   4,   5,   6,   7,   8,   9,   10,  11,  12 ],
              [13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24 ],
              [25,  26,  27,  28,  29,  30,  31,  32,  33,  34,  35,  36 ],
              [37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,  48 ],
              [49,  50,  51,  52,  53,  54,  55,  56,  57,  58,  59,  60 ],
              [61,  62,  63,  64,  65,  66,  67,  68,  69,  70,  71,  72 ],
              [73,  74,  75,  76,  77,  78,  79,  80,  81,  82,  83,  84 ],
              [85,  86,  87,  88,  89,  90,  91,  92,  93,  94,  95,  96 ],
              [97,  98,  99,  100, 101, 102, 103, 104, 105, 106, 107, 108],
              [109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120],
              [121, 122, 123, 124, 125, 126, 127, 128, 129, 130, 131, 132],
              [133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143, 144]]

array12By1Res :: Array (Array (Array (Array Int)))
array12By1Res = [[[[1]],[[2]],[[3]],[[4]],[[5]],[[6]],[[7]],[[8]],[[9]],[[10]],[[11]],[[12]]],[[[13]],[[14]],[[15]],[[16]],[[17]],[[18]],[[19]],[[20]],[[21]],[[22]],[[23]],[[24]]],[[[25]],[[26]],[[27]],[[28]],[[29]],[[30]],[[31]],[[32]],[[33]],[[34]],[[35]],[[36]]],[[[37]],[[38]],[[39]],[[40]],[[41]],[[42]],[[43]],[[44]],[[45]],[[46]],[[47]],[[48]]],[[[49]],[[50]],[[51]],[[52]],[[53]],[[54]],[[55]],[[56]],[[57]],[[58]],[[59]],[[60]]],[[[61]],[[62]],[[63]],[[64]],[[65]],[[66]],[[67]],[[68]],[[69]],[[70]],[[71]],[[72]]],[[[73]],[[74]],[[75]],[[76]],[[77]],[[78]],[[79]],[[80]],[[81]],[[82]],[[83]],[[84]]],[[[85]],[[86]],[[87]],[[88]],[[89]],[[90]],[[91]],[[92]],[[93]],[[94]],[[95]],[[96]]],[[[97]],[[98]],[[99]],[[100]],[[101]],[[102]],[[103]],[[104]],[[105]],[[106]],[[107]],[[108]]],[[[109]],[[110]],[[111]],[[112]],[[113]],[[114]],[[115]],[[116]],[[117]],[[118]],[[119]],[[120]]],[[[121]],[[122]],[[123]],[[124]],[[125]],[[126]],[[127]],[[128]],[[129]],[[130]],[[131]],[[132]]],[[[133]],[[134]],[[135]],[[136]],[[137]],[[138]],[[139]],[[140]],[[141]],[[142]],[[143]],[[144]]]]

array12By2Res :: Array (Array (Array (Array Int)))
array12By2Res = [[[[1,2],[13,14]],[[3,4],[15,16]],[[5,6],[17,18]],[[7,8],[19,20]],[[9,10],[21,22]],[[11,12],[23,24]]],[[[25,26],[37,38]],[[27,28],[39,40]],[[29,30],[41,42]],[[31,32],[43,44]],[[33,34],[45,46]],[[35,36],[47,48]]],[[[49,50],[61,62]],[[51,52],[63,64]],[[53,54],[65,66]],[[55,56],[67,68]],[[57,58],[69,70]],[[59,60],[71,72]]],[[[73,74],[85,86]],[[75,76],[87,88]],[[77,78],[89,90]],[[79,80],[91,92]],[[81,82],[93,94]],[[83,84],[95,96]]],[[[97,98],[109,110]],[[99,100],[111,112]],[[101,102],[113,114]],[[103,104],[115,116]],[[105,106],[117,118]],[[107,108],[119,120]]],[[[121,122],[133,134]],[[123,124],[135,136]],[[125,126],[137,138]],[[127,128],[139,140]],[[129,130],[141,142]],[[131,132],[143,144]]]]

array12By3Res :: Array (Array (Array (Array Int)))
array12By3Res = [[[[1,2,3],[13,14,15],[25,26,27]],[[4,5,6],[16,17,18],[28,29,30]],[[7,8,9],[19,20,21],[31,32,33]],[[10,11,12],[22,23,24],[34,35,36]]],[[[37,38,39],[49,50,51],[61,62,63]],[[40,41,42],[52,53,54],[64,65,66]],[[43,44,45],[55,56,57],[67,68,69]],[[46,47,48],[58,59,60],[70,71,72]]],[[[73,74,75],[85,86,87],[97,98,99]],[[76,77,78],[88,89,90],[100,101,102]],[[79,80,81],[91,92,93],[103,104,105]],[[82,83,84],[94,95,96],[106,107,108]]],[[[109,110,111],[121,122,123],[133,134,135]],[[112,113,114],[124,125,126],[136,137,138]],[[115,116,117],[127,128,129],[139,140,141]],[[118,119,120],[130,131,132],[142,143,144]]]]

array12By4Res :: Array (Array (Array (Array Int)))
array12By4Res = [[[[1,2,3,4],[13,14,15,16],[25,26,27,28],[37,38,39,40]],[[5,6,7,8],[17,18,19,20],[29,30,31,32],[41,42,43,44]],[[9,10,11,12],[21,22,23,24],[33,34,35,36],[45,46,47,48]]],[[[49,50,51,52],[61,62,63,64],[73,74,75,76],[85,86,87,88]],[[53,54,55,56],[65,66,67,68],[77,78,79,80],[89,90,91,92]],[[57,58,59,60],[69,70,71,72],[81,82,83,84],[93,94,95,96]]],[[[97,98,99,100],[109,110,111,112],[121,122,123,124],[133,134,135,136]],[[101,102,103,104],[113,114,115,116],[125,126,127,128],[137,138,139,140]],[[105,106,107,108],[117,118,119,120],[129,130,131,132],[141,142,143,144]]]]

array12By6Res :: Array (Array (Array (Array Int)))
array12By6Res = [[[[1,2,3,4,5,6],[13,14,15,16,17,18],[25,26,27,28,29,30],[37,38,39,40,41,42],[49,50,51,52,53,54],[61,62,63,64,65,66]],[[7,8,9,10,11,12],[19,20,21,22,23,24],[31,32,33,34,35,36],[43,44,45,46,47,48],[55,56,57,58,59,60],[67,68,69,70,71,72]]],[[[73,74,75,76,77,78],[85,86,87,88,89,90],[97,98,99,100,101,102],[109,110,111,112,113,114],[121,122,123,124,125,126],[133,134,135,136,137,138]],[[79,80,81,82,83,84],[91,92,93,94,95,96],[103,104,105,106,107,108],[115,116,117,118,119,120],[127,128,129,130,131,132],[139,140,141,142,143,144]]]]

array8By4Res :: Array (Array (Array (Array Int)))
array8By4Res = [[[[1,  2,  3,  4 ],
                  [9,  10, 11, 12],
                  [17, 18, 19, 20],
                  [25, 26, 27, 28]],
                 [[5,  6,  7,  8],
                  [13, 14, 15, 16],
                  [21, 22, 23, 24],
                  [29, 30, 31, 32]]],
                [[[33, 34, 35, 36],
                  [41, 42, 43, 44],
                  [49, 50, 51, 52],
                  [57, 58, 59, 60]],
                 [[37, 38, 39, 40],
                  [45, 46, 47, 48],
                  [53, 54, 55, 56],
                  [61, 62, 63, 64]]]]

array8By2Res :: Array (Array (Array (Array Int)))
array8By2Res = [[[[1,2],[9,10]],
                 [[3,4],[11,12]],
                 [[5,6],[13,14]],
                 [[7,8],[15,16]]],
                [[[17,18],[25,26]],
                 [[19,20],[27,28]],
                 [[21,22],[29,30]],
                 [[23,24],[31,32]]],
                [[[33,34],[41,42]],
                 [[35,36],[43,44]],
                 [[37,38],[45,46]],
                 [[39,40],[47,48]]],
                [[[49,50],[57,58]],
                 [[51,52],[59,60]],
                 [[53,54],[61,62]],
                 [[55,56],[63,64]]]]

array6By2Res :: Array (Array (Array (Array Int)))
array6By2Res = [[[[1, 2 ],
                  [7, 8]],
                 [[3, 4 ],
                  [9, 10]],
                 [[5, 6 ],
                  [11,12]]],
                [[[13,14],
                  [19,20]],
                 [[15,16],
                  [21,22]],
                 [[17,18],
                  [23,24]]],
                [[[25,26],
                  [31,32]],
                 [[27,28],
                  [33,34]],
                 [[29,30],
                  [35,36]]]]

array6By3Res :: Array (Array (Array (Array Int)))
array6By3Res = [[[[1, 2, 3 ],
                  [7, 8, 9 ],
                  [13,14,15]],
                 [[4, 5, 6 ],
                  [10,11,12],
                  [16,17,18]]],
                [[[19,20,21],
                  [25,26,27],
                  [31,32,33]],
                 [[22,23,24],
                  [28,29,30],
                  [34,35,36]]]]

day21Tests4 :: forall e. Eff (console :: CONSOLE | e) Unit
day21Tests4 = foreachE (tests) (\(f /\ inputs /\ expected) ->
                              let
                                res = f inputs
                              in do
                                log $ show (res == expected)
                                  <> " ---"
                                  <> " should be: "
                                  <> show expected
                                  <> ", was "
                                  <> show res) where

  tests = [ (\i -> (splitArray 2 i)) /\ array6 /\ array6By2Res
          , (\i -> (splitArray 3 i)) /\ array6 /\ array6By3Res
          ]

array12Res :: Array (Array (Array (Array Pixel)))
array12Res =
  [[[[Off,On ], [On ,On ]], [[Off,Off], [On ,Off]], [[Off,On ], [On ,On ]], [[Off,Off], [On ,Off]], [[On ,On ], [Off,Off]], [[On ,Off], [On ,On ]]],
   [[[On ,On ], [Off,Off]], [[Off,On ], [Off,On ]], [[On ,On ], [Off,Off]], [[Off,On ], [Off,On ]], [[On ,On ], [Off,On ]], [[On ,Off], [On ,Off]]],
   [[[On ,On ], [Off,Off]], [[On ,Off], [On ,On ]], [[Off,On ], [On ,On ]], [[Off,Off], [On ,Off]], [[Off,On ], [On ,On ]], [[Off,Off], [On ,Off]]],
   [[[On ,On ], [Off,On ]], [[On ,Off], [On ,Off]], [[On ,On ], [Off,Off]], [[Off,On ], [Off,On ]], [[On ,On ], [Off,Off]], [[Off,On ], [Off,On ]]],
   [[[On ,On ], [On ,On ]], [[On ,Off], [On ,Off]], [[Off,On ], [On ,On ]], [[Off,Off], [On ,Off]], [[On ,On ], [On ,Off]], [[On ,Off] ,[On ,On ]]],
   [[[Off,On ], [On ,On ]], [[Off,On ], [On ,Off]], [[On ,On ], [Off,Off]], [[Off,On ], [Off,On ]], [[On ,On ], [On ,On ]], [[On ,Off], [On ,On ]]]]

array12 :: Array (Array Pixel)
array12 = [[Off,On ,Off,Off,Off,On ,Off,Off,On ,On ,On ,Off],
           [On ,On ,On ,Off,On ,On ,On ,Off,Off,Off,On ,On ],
           [On ,On ,Off,On ,On ,On ,Off,On ,On ,On ,On ,Off],
           [Off,Off,Off,On ,Off,Off,Off,On ,Off,On ,On ,Off],
           [On ,On ,On ,Off,Off,On ,Off,Off,Off,On ,Off,Off],
           [Off,Off,On ,On ,On ,On ,On ,Off,On ,On ,On ,Off],
           [On ,On ,On ,Off,On ,On ,Off,On ,On ,On ,Off,On ],
           [Off,On ,On ,Off,Off,Off,Off,On ,Off,Off,Off,On ],
           [On ,On ,On ,Off,Off,On ,Off,Off,On ,On ,On ,Off],
           [On ,On ,On ,Off,On ,On ,On ,Off,On ,Off,On ,On ],
           [Off,On ,Off,On ,On ,On ,Off,On ,On ,On ,On ,Off],
           [On ,On ,On ,Off,Off,Off,Off,On ,On ,On ,On ,On ]]


day21Tests5 :: forall e. Eff (console :: CONSOLE | e) Unit
day21Tests5 = foreachE (tests) (\(f /\ inputs /\ expected) ->
                              let
                                res = f inputs
                              in do
                                log $ show (res == expected)
                                  <> " ---"
                                  <> " should be: "
                                  <> show expected
                                  <> ", was "
                                  <> show res) where

  tests = [(\i -> (splitArray 2 i)) /\ array12 /\ array12Res
          ]

day21Tests6 :: forall e. Eff (console :: CONSOLE | e) Unit
day21Tests6 = foreachE (tests) (\(f /\ inputs /\ expected) ->
                              let
                                res = f inputs
                              in do
                                log $ show (res == expected)
                                  <> " ---"
                                  <> " should be: "
                                  <> show expected
                                  <> ", was "
                                  <> show res) where

  tests = [flattenArray /\ array12Res /\ array12
          ]


day21Tests7 :: forall e. Eff (console :: CONSOLE | e) Unit
day21Tests7 = foreachE (tests) (\(f /\ by /\ inputs /\ expected) ->
                              let
                                res = f by inputs
                              in do
                                log $ show (res == expected)
                                  <> " ---"
                                  <> " should be: "
                                  <> show expected
                                  <> ", was "
                                  <> show res) where

  tests = [ splitArray /\ 2 /\ array6 /\ array6By2Res
          , splitArray /\ 3 /\ array6 /\ array6By3Res
          , splitArray /\ 4 /\ array8 /\ array8By4Res
          ]
