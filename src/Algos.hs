module Algos
  ( tdevs
  , mtiesY
  , mtiesXY
  ) where

import Control.Exception (assert)
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)
import qualified Data.Vector.Unboxed as VU
import Utils (ordNub, isSorted)


mtiesY :: NanoSeconds -> VU.Vector Y -> [(Interval, Mtie)]
mtiesY = mtiesY2

mtiesXY :: VU.Vector (X, Y) -> [(Interval, Mtie)]
mtiesXY = mtiesXY1

tdevs :: Double -> Int -> VU.Vector Double -> [(X, Y)]
tdevs = tdevs9

-- Indication of performance:
-- mtiesXY1 = 29:38.96
-- mtiesXY2 = 0:26.27

type X = Double
type Y = Double
data SampleMinMax = SMin (Position, Sample) | SMax (Position, Sample) deriving (Show)

type Offset = Int
type Window = (Offset, VU.Vector Sample)
type WindowMinMax = (SampleMinMax, SampleMinMax)
type SampleTime = Double
type Interval = Double
type MinimumY = Double
type MaximumY = Double
type Mtie = Double
type DiffY = Double
type Sample = Double
type Position = Int
type NanoSeconds = Double

-- mtie intervals for output (these values were found in a reference program, but no idea why these specific values)
-- this could possibly be replaced by a logarithmic function with pointsPerDecade
mties_intervals :: [Double]
mties_intervals =
  [ 0.001, 0.012, 0.016, 0.020, 0.025, 0.032, 0.040, 0.050, 0.063, 0.080, 0.1, 0.12, 0.16, 0.2, 0.25
  , 0.32, 0.4, 0.5, 0.63, 0.8, 1, 1.2, 1.6, 2, 2.5, 3.2, 4, 5, 6.3, 8, 10, 12, 16, 20, 25, 32, 40, 50
  , 63, 80, 100, 120, 160, 200, 250, 320, 400, 500, 630, 800, 1000, 1200, 1500, 2000, 2500, 3200, 4000
  , 5000, 6300, 8000, 10000, 12000, 15000, 20000, 25000, 32000, 40000, 50000, 63000, 80000, 100000.0
  ]

valid_mties_intervals_points :: SampleTime -> [Double] -> Int -> [Int]
valid_mties_intervals_points st intervals veclen = assert (isSorted intervals) $ filter (<= veclen) (ordNub $ map intervalToPoints intervals)
  where intervalToPoints :: Interval -> Int
        intervalToPoints int = if int < (st * 2) then 2 else (round (int / st) + 1)
        -- need at least 2 samples in the window to calculate a difference

-- TDEV(n * \tau0) = \sqrt{ \frac {1} {6n^2(N-3n+1))} \sum_{j=1}^{N-3n+1} \left( \sum_{i=j}^{n+j-1} (x_{i+2n}-2x_{i+n}+x_i) \right)^2 }
--                        _____________________________________________________________________________
--                      /         1         __ N - 3n + 1 /  __ n + j - 1                         \2  
-- TDEV(n * tau0) = |  /  ---------------- \              | \             (x       - 2x      + x )|   
--                  | /     2              /__ j = 1      \ /__ i = j       i + 2n     i + n    i /   
--                  |/    6n (N - 3n + 1))                                                            
-- ascii from: http://www.sciweavers.org/free-online-latex-equation-editor
--
-- This is version 9 of tdev which uses the improved lookup algorithm and expects sum to be done earlier in conduit
tdevs9 :: Double -> Int -> VU.Vector Double -> [(X, Y)]
tdevs9 sampleTime pointsPerDecade points = map formula points_list
  where ɴ = VU.length points
        -- pointsPerDecade = 10
        sum_1_lower_bound = 1

        -- To increase the confidence in the estimate and to maximise the data utilisation Wallis and Allan [23] substitute N-3n+1 for N
        -- in their expression for the Allan Variance and therefore limit n to values up to $\mathrm{int}(\frac{N}{3})$.
        n_max n = ɴ - 3 * n + 1

        -- generate a list of X-values for our data points (this relates to the frequency)
        -- N.B. 5 is chosen because at this index numbers are not duplicates anymore when pointsPerDecade = 10
        points_list :: [Int]
        points_list = ordNub $ takeWhile (\n -> n_max n >= sum_1_lower_bound) $ map (\p -> round $ 10 ** (p / fromIntegral pointsPerDecade)) [1..]

        formula n = ( (fromIntegral n) * sampleTime, sqrt (equation_1 n * sum_1 n) )
          where equation_1 :: Int -> Double
                equation_1 n = 1 / (fromIntegral $ (6 * n ^ 2) * (ɴ - 3 * n + 1))

                sum_1 n = equation_2_0 + (foldl' (+) 0 $ map (\j -> equation_2 (j - 1) ** 2) [(sum_1_lower_bound + 1).. n_max n]) -- formula may start indexing at 1, but our vector is 0-indexed
                          where lu start end = (points VU.! end) - (points VU.! (start - 1))
                                equation_2   j = (lu (j+n*2) (j+n*3-1)) - 2 * (lu (j+n) (j+n*2-1)) + (lu j (j+n-1))
                                equation_2_0   = ((lu (n*2) (n*3-1)) - 2 * (lu n (n*2-1)) + (points VU.! (n-1))) ** 2 -- this handles the special case where j = 0


{-
Calculates the Mtie given sampleTime and samples

Algorithm description:
take smallest interval and calculate per window the min/max
then the next interval only has to look at the interval of previous ones
example: intervals 2, 3, 4

for 5 points the needed windows are:
2 = (min, max), (min, max), (min, max), (min, max)
3 = (min, max), (min, max), (min, max)
4 = (min, max), (min, max)

now first interval of 4 is the same as first + second from 3

points: x x x x x
4 (1) : x x x x    <- first window (position 0) of interval 4  -- w4_0
3 (1) : x x x      <- first window (position 0) of interval 3  -- w3_0
3 (2) :   x x x    <- second window (position 1) of interval 3 -- w3_1

now window w4_0 can be calculated with:
(min (fst w3_0) (fst w3_1), max (fst w3_0) (fst w3_1))

----
Possible improvements for further speedup (not implemented)
* For small intervals the original function is actually FASTER than the improved algorithm
  But the overal time is greatly reduced 838,31 s to 13.02 s for a test run of the complete program on the same dataset
  However the original algorithm could be applied for small intervals and then later switch to the improved version
* If the smallest interval is (significantly) larger than 2 points (samples) one could keep track of the min/max location
  inside that window, and reuse this information to easily determine the window of the next start location. Example:
  x x x x x x x x
      \min  \max
    x x x x x x x x  <- next window is 1 sample further
  now only the newest sample has to be compared with the min/max found in the previous window and not all the samples
* Same as last point but instead of creating 1 window for each startpoint skip the window start point the the min or max
  the new startpoint is the left-most min or max
  this is incompatible with the algorithm below because it requires access to (min, max) vector of all startpoints of previous interval
  One big plus of this algorithm is that it requires a lot less allocation for intermediate results
-}
mtiesY2 :: NanoSeconds -> VU.Vector Y -> [(Interval, Mtie)]
mtiesY2 st points = let (x:xs) = (valid_mties_intervals_points st mties_intervals len)
                        firstMinMaxVec = intervalToMinMaxVec x
                    in (minMaxVecToMtie firstMinMaxVec) : (memFold lastToMinMaxVec minMaxVecToMtie firstMinMaxVec xs)
  where len = VU.length points

        -- this function calculates all the (min, max) of an interval
        intervalToMinMaxVec :: Int -> (Int, VU.Vector (MinimumY, MaximumY))
        intervalToMinMaxVec ip = (ip, VU.map (windowMinMax . (window ip)) (start_indices_window ip))
          where 
                windowMinMax :: VU.Vector Y -> (MinimumY, MaximumY)
                windowMinMax window = (VU.minimum window, VU.maximum window)
                window :: Int -> Int -> VU.Vector Y
                window len start = VU.take len (VU.drop start points)

        -- same as intervalToMinMaxVec but much faster because it reuses information from the last (min, max) vector
        lastToMinMaxVec :: (Int, VU.Vector (MinimumY, MaximumY)) -> Int -> (Int, VU.Vector (MinimumY, MaximumY))
        lastToMinMaxVec (last_ip, vec) ip = (ip, VU.map (\x -> minmin_maxmax $ minMaxVecs x) (start_indices_window ip))
               -- trace (show (VU.take 5 $ start_indices_window ip) ++ " len startindces: "  ++ show (VU.length $ start_indices_window ip) ++ ", ip: " ++ show ip ++ ", last ip: " ++ show last_ip) $

          where 
                minMaxVecs :: Int -> VU.Vector (MinimumY, MaximumY)
                minMaxVecs ip_start = let startPoints ip_start = VU.enumFromStepN ip_start last_ip (floor $ fromIntegral ip / fromIntegral last_ip) -- calculates the index positions of the smaller windows to fill the bigger window
                                          starters = VU.map (VU.unsafeIndex vec) (startPoints ip_start) -- (min, max) of smaller windows at the start of big window
                                          fromEnd = VU.unsafeIndex vec (ip - last_ip + ip_start) -- (min, max) from smaller window at the end of big window
                                      in if rem ip last_ip > 0 then VU.snoc starters fromEnd else starters
                minmin_maxmax :: VU.Vector (MinimumY, MaximumY) -> (MinimumY, MaximumY)
                minmin_maxmax minmax = VU.foldl1' (\(mi, ma) (mi2, ma2) -> (min mi mi2, max ma ma2)) minmax

        -- gets mtie from (min, max) vector
        minMaxVecToMtie :: (Int, VU.Vector (MinimumY, MinimumY)) -> (Interval, MinimumY)
        minMaxVecToMtie (ip, vec) = (fromIntegral (ip - 1) * st, VU.maximum $ VU.map (\(vecMin, vecMax) -> vecMax - vecMin) vec)

        -- helper function to make start indices of windows for a given interval (in points)
        start_indices_window :: Int -> VU.Vector Int
        start_indices_window interval_points = VU.enumFromN 0 ((VU.length points) - interval_points - 1)

        -- foldl' while still being able to access the intermediate result of the last iteration
        -- iteration applies 2 functions f1 and f2 where the result of f1 is available for the next iteration
        memFold :: (Show a, Foldable t) => (b -> a -> b) -> (b -> c) -> b -> t a -> [c]
        memFold f1 f2 beginState lst = fst $ foldl' go ([], beginState) lst
          where go (acc, last') ip' = let y1 = f1 last' ip'
                                          y2 = f2 y1
                                      in (acc ++ [y2], y1)


type SampleXY = (X, Y)
data SampleMinMaxXY = SMinXY (Position, SampleXY) | SMaxXY (Position, SampleXY) deriving (Show)

data MinMaxError
  = NotEnoughSamples  -- interval is too big, sample longer
  | IntervalTooShort  -- interval was too short, increase samples/s
  deriving (Show)
type Percentage = Double

--               start     stop      interval  min              max
type WindowXY = (Position, Position, Interval, (SampleMinMaxXY, SampleMinMaxXY))


-- These algorithms (mtiesXY2 and mtiesY3) work off the fact that there are at least 2 samples in a window
-- Without 2 samples it's not possible to calculate an mtie, which is the difference between the min and max value of a window
-- Therefor it's guaranteed that the min and max sample are not the first sample at the same time.
-- In case of equal samples the right-most sample will be chosen.
-- Example: input (2,2)
--                   \__-> is min and also max sample
-- When the first sample is a min or a max we drop this sample and take as many new samples as needed (# window C size # below)
-- In case of mtiesY3 this is always 1 sample, in case of mtiesXY2 this is at least 1 sample.
-- Now we will have to find a new min and max, suppose that the first sample was a minimum value we can find the min and max as follows:
--   max: compare window A max with window C max
--   min: compare window B min with window C min
-- in case the first sample is a maximum value:
--   max: compare window B max with window C max
--   min: compare window A min with window C min
-- in case the first sample is not a minimum or maximum value:
--   max: compare window A max with window C max
--   min: compare window A min with window C min

-- Notes:
-- * when the first sample is a minimum or maximum sample then in mtiesY3 window C is only 1 sample big
-- * window C is called windowA_new in mtiesY3
-- * window E is the result of the calculation
-- * window E ix called window C in mtiesXY2

--    /-- ileft
--    |---- window E ---|
-- |--| window D
--    |- window B --|
-- |--- window A ---|---|
--                  |---| window C
--                  \__ window A end


-- # window C size #
-- mtiesY3: window D size
-- mtiesXY2: at least 1 sample or IntervalTooShort

-- Another version of this algorithm would be to record the 2 lowest and 2 highest samples .. then window B does not need to be searched.
mtiesXY2 :: VU.Vector (X, Y) -> [(Interval, Mtie)]
mtiesXY2 points = filter_mties $ map go valid_mties_intervals
  where valid_mties_intervals = filter (<= last_point) mties_intervals

        last_point = fst $ VU.last points

        len = VU.length points

        -- we have a "requested interval" .. and an actual interval.
        -- for multiple requested interval it could be the same actual interval
        -- we dedup them taking the right most, this one is closest to the requested interval
        filter_mties [] = []
        filter_mties [x] = [x]
        filter_mties (x1@(i1,m1) : x2@(i2,m2) : xs) = if abs (i2 - i1) < 1.0e-6 then filter_mties (x2:xs) else x1 :  filter_mties (x2:xs)

        go :: Interval -> (Interval, Mtie)
        go interval = --trace ("Start with interval: " ++ show interval) $
          case getMinMaxFromOffset 0 interval of
            Left IntervalTooShort -> (interval, -1)
            Left err     -> error $ show err
            Right window -> let allWindows = getAllWindows interval window
                                (summedInterval, mtie) = foldl' windowsToMtie (0, -1) allWindows
                            in (summedInterval / fromIntegral (length allWindows), mtie)
          where windowsToMtie :: (Interval, Mtie) -> WindowXY -> (Interval, Mtie)
                windowsToMtie (interval', mtie) w = let (intervalA, mtieA) = diffMinMax interval w
                                                    in (interval' + intervalA, max mtie mtieA) -- sum all intervals here to get average

        getAllWindows :: Interval -> WindowXY -> [WindowXY]
        getAllWindows interval w = --trace ("getAllWindows: " ++ show w) $
          case nextWindow interval w of
            Nothing -> [w]
            Just w' -> --error (show interval ++ " " ++ show w ++ " " ++ show w')
                       w : getAllWindows interval w'

        -- prefers B
        combineMinOrMax :: SampleMinMaxXY -> SampleMinMaxXY -> SampleMinMaxXY
        combineMinOrMax a_smin@(SMinXY (_, (_, a_ymin))) b_smin@(SMinXY (_, (_, b_ymin))) =
          if b_ymin <= a_ymin then b_smin else a_smin
        combineMinOrMax a_smax@(SMaxXY (_, (_, a_ymax))) b_smax@(SMaxXY (_, (_, b_ymax))) =
          if b_ymax >= a_ymax then b_smax else a_smax
        combineMinOrMax _ _ = error "can not compare"

        nextWindow :: Interval -> WindowXY -> Maybe WindowXY
        nextWindow interval input@(a_istart, a_istop, _, (a_smin@(SMinXY (a_imin, (a_xmin, a_ymin))), a_smax@(SMaxXY (a_imax, (a_xmax, a_ymax))))) =
          let (ileft, _) = if ileftIsMin then (a_imin, a_imax) else (a_imax, a_imin)
              ileftIsMin = a_imin < a_imax
              windowC_istart = if ileft == a_istart then ileft + 1 else ileft
          in --trace "next window starting here"

             case getMinMaxFromOffset windowC_istart interval of
               Left err -> Nothing
               Right debug@(_, c_istop, windowC_interval, (c_smin, c_smax)) ->
                 -- trace ("\ninput: "
                 --  ++ show input ++ "\ninter: "
                 --  ++ show debug ++ "\n"
                 --  ++ show a_istop ++ " "
                 --  ++ show interval ++ " " ++ show (ileft == a_istart) ++ " " ++ show ileft ++ " " ++ show a_istart
                 --  ++ " " ++ show windowC_istart ++ " " ++ show ileftIsMin
                 -- )

                 Just (windowC_istart, c_istop, windowC_interval, (c_smin, c_smax))

        -- takes a wanted interval and returns the actual interval
        -- this function can fail in two ways (not implemented yet):
        --   1.  not enough samples
        --   2.  interval was too short   -- maybe not abort here but just don't include as value ?
        --                                                            start     stop
        getMinMaxFromOffset :: Int -> Interval -> Either MinMaxError (Position, Position, Interval, (SampleMinMaxXY, SampleMinMaxXY))
        getMinMaxFromOffset start interval =
          if (start + 1) >= len then Left NotEnoughSamples else
            if x_second > x_stop then Left IntervalTooShort else -- Right (start, start, 0, ((SMinXY (start, s_first)), (SMaxXY (start, s_first))))
              go (start + 1) s_first (SMinXY (start, s_first)) (SMaxXY (start, s_first))
          where s_first@(x_first, _) = points VU.! start
                s_second@(x_second, _) = points VU.! (start + 1)

                x_stop = x_first + interval

                go :: Int
                   -> (X, Y)
                   -> SampleMinMaxXY
                   -> SampleMinMaxXY
                   -> Either MinMaxError (Position, Position, Interval, (SampleMinMaxXY, SampleMinMaxXY))
                go idx (x_previous, _) smin@(SMinXY (_, (_, ymin))) smax@(SMaxXY (_, (_, ymax))) =
                  let s@(x, y) = points VU.! idx
                      n_smin = if y <= ymin then SMinXY (idx, s) else smin
                      n_smax = if y >= ymax then SMaxXY (idx, s) else smax
                  in -- trace (show x_stop ++ " " ++ show s_first ++ " " ++ show interval ++ " " ++ show len ++ " " ++ show s ++ " " ++ show smin ++ " " ++ show smax) $
                    --  63     64
                     if idx >= (len - 1) then Left IntervalTooShort else
                       if x > x_stop then Right (start, idx, x_previous - x_first, (smin, smax)) else
                          go (succ idx) s n_smin n_smax
                go a b c d = error $ show a ++ "\n" ++ show b ++ "\n" ++ show c ++ "\n" ++ show d


        diffMinMax :: Interval -> (Position, Position, Interval, (SampleMinMaxXY, SampleMinMaxXY)) -> (Interval, Mtie)
        diffMinMax interval val@(_, _, actual_interval, (SMinXY (_, (_, ymin)), SMaxXY (_, (_, ymax)))) = --trace (show val ++ " " ++ show interval) $
          assert (compareIntervals interval actual_interval) $ (actual_interval, ymax - ymin)

        compareIntervals interval actual_interval = abs (interval / actual_interval) > 0.7

        minMaxWithinInterval :: Interval -> (Position, Position, Interval, (SampleMinMaxXY, SampleMinMaxXY)) -> Bool
        minMaxWithinInterval interval debug@(_, _, _, (SMinXY (_, (xmin, _)), SMaxXY (_, (xmax, _)))) = trace (show debug ++ " " ++ show interval ++ " " ++ show xmin ++ " " ++ show xmax) $
          (abs (xmin - xmax) - interval) < 1.0e-17



---------------------------------------
---------------------------------------
---------------------------------------
-- Alternative implementations below --
---------------------------------------
---------------------------------------
---------------------------------------



-- This is the original version (slow) version of tdevs
-- it is unused but left in the source code to better understand the improved version
--
-- it expects the actual samples and not the accumulated ones
-- to try this function change accConduit to dummyConduit elsewhere in the code
tdevs_original :: Double -> Int -> VU.Vector Double -> [(X, Y)]
tdevs_original sampleTime pointsPerDecade points = map (\n -> ((fromIntegral n) * sampleTime, sqrt (equation_1 n * sum_1 n))) points_list
  where sum' = foldl' (+) (0 :: Double)

        ɴ = VU.length points

        n_max n = ɴ - 3 * n + 1
        sum_1_lower_bound = 1

        points_list :: [Int]
        points_list = takeWhile (\n -> n_max n >= sum_1_lower_bound) $ map (\p -> round $ 10 ** (p / fromIntegral pointsPerDecade)) [5..]

        equation_1 :: Int -> Double
        equation_1 n = 1 / (fromIntegral $ (6 * n ^ 2) * (ɴ - 3 * n + 1))

        sum_1 :: Int -> Double
        sum_1 n = sum' $ map (\j -> sum_2 j ** 2) [sum_1_lower_bound .. n_max n]

          where sum_2 :: Int -> Double
                sum_2 j = sum' $ map equation_2 $ takeWhile (<= n + j - 1) [j..]

                equation_2 :: Int -> Double
                equation_2 i = points VU.! (ii + 2 * n) - 2 * points VU.! (ii + n) + points VU.! ii
                  where ii = i - 1 -- formula may start indexing at 1, but our vector is 0-indexed

-- This is the original version (slow) version of mtiesY
-- it is unused but left in the source code to better understand the improved version
mtiesY1 :: NanoSeconds -> VU.Vector Y -> [(Interval, Mtie)]
mtiesY1 st points = map go (valid_mties_intervals_points st mties_intervals len)
  where len = VU.length points

        go :: Int -> (Interval, Mtie)
        go ip = (fromIntegral (ip - 1) * st, maximum $ map (windowDiff . (window ip)) (start_indices_window ip))
          where 
                windowDiff :: VU.Vector Y -> DiffY -- program spends all the time here !
                windowDiff window = VU.maximum window - VU.minimum window

                window :: Int -> Int -> VU.Vector Y
                window len start = VU.take len (VU.drop start points)

                start_indices_window :: Int -> [Int]
                start_indices_window interval_points = [0 .. ((VU.length points) - interval_points)]

{-
This algorithm doesn't make use of the information gathered by the previous interval length.
Instead it will find the min/max location of the current window and uses that to advance the window further than just 1
Example: a a a a a a a a a a a a a a a a a a a b b b b b b b b b b b b
         \samples to drop    \min  \max            \newMin   \newMax
This drops the leading samples and starts a new searching at either min or max (whatever has the lowest index) -- in this case min
Then it searches the additional samples (denotes y) for new min/max within all b's
The min and max are then compared to the one to the left, if the absolute value is bigger than these positions are used as new indices
Otherwise when the first min is smallar than the newMin the original min needs to be dropped and the remaining a's need to be searched too
a a a a a a a a a a a a a a a a a a a b b b b b b b b b b b b
\samples to drop    \min  \max            \newMin   \newMax
                      \start of a's
                    c c c c c c c c c c c c c c c c c c c c c
Now we have the new min/max of window c without traversing all the points of c

Near the end of the samples the last b can go out of bounds of the remaining samples, in this case the search area for b is truncated
and the algorithm finishes it's current interval
-}
mtiesY3 :: NanoSeconds -> VU.Vector Y -> [(Interval, Mtie)]
mtiesY3 st points = map go (valid_mties_intervals_points st mties_intervals len)
  where len = VU.length points

        go :: Int -> (Interval, Mtie)
        go ip = let firstWindow = windowToMinMaxInfo (getWindow ip 0)
                in (fromIntegral (ip - 1) * st, maximum $ map diffMinMax $ getAllWindows (0, firstWindow))
          where getAllWindows w@(pos, _) =
                  if (pos + ip) < (len - 1) then
                    let wn = nextWindow w
                    in w : getAllWindows wn
                  else
                    [w]

                minMaxWithinInterval (_, (SMin (imin, _), SMax (imax, _))) = (abs (imin - imax) + 1 <= ip)

                diffMinMax :: (Position, (SampleMinMax, SampleMinMax)) -> DiffY
                diffMinMax val@(_, debug@(SMin (_, vmin), SMax (_, vmax))) = -- trace ("diffMinMax -- " ++ show debug) $ 
                                                                             assert (minMaxWithinInterval val) $ vmax - vmin

                -- given a window (ip and windowA_start with it's min and max)
                -- will give back a new window starting at
                --   ileft in windowA  -- in case the extreme is not the first value
                --   windowA_start + 1 -- in case the extreme is on the first value
                --
                -- should run until  new_pos + ip < len
                nextWindow :: (Position, (SampleMinMax, SampleMinMax)) -> (Position, (SampleMinMax, SampleMinMax))
                nextWindow (windowA_start, a_min_max@(a_smin@(SMin (a_imin, _)), a_smax@(SMax (a_imax, _)))) =
                  let startNew = windowA_start + 1
                      (ileft, iright) = if ileftIsMin then (a_imin, a_imax) else (a_imax, a_imin)
                      ileftIsMin = a_imin < a_imax

                      windowA_end = windowA_start + ip - 1

                  in assert (iright >= startNew) $ if ileft < startNew
                    -- the first sample is a min or max so we have to drop this sample and search the next window again
                    then let ileft_new = ileft + 1
                             windowA_new_end = windowA_end + 1
                             windowA_new = getWindow (windowA_new_end - ileft) ileft_new
                             windowA_new_last_point = (windowA_new_end, points VU.! windowA_new_end)
                         in assert (ileft + 1 == startNew) $
                           if ileftIsMin
                             then let an_smin = SMin $ vectorFindRightWithIndex (<) windowA_new
                                      result = (ileft_new, (an_smin, combineMax a_smax (SMax windowA_new_last_point)))
                                  in assert (minMaxWithinInterval result) result
                                     
                             else let an_smax = SMax $ vectorFindRightWithIndex (>) windowA_new
                                      result = (ileft_new, (combineMin a_smin (SMin windowA_new_last_point), an_smax))
                                  in assert (minMaxWithinInterval result) result
                                   
                    -- we can move forward and search extra samples at the end (size of window B depends on ileft)
                    else let  windowB_start = windowA_end + 1
                              windowB_end_maybe_out_of_bounds = ileft + ip
                              windowB_is_out_of_bounds = windowB_end_maybe_out_of_bounds > (len - 1)
                              windowB_end = if windowB_is_out_of_bounds then (len - 1) else windowB_end_maybe_out_of_bounds -- possibly truncated
                              windowB = getWindow (windowB_end - windowB_start) windowB_start
                              b_min_max = windowToMinMaxInfo windowB

                              result = (ileft, combineMinMax a_min_max b_min_max)
                         in assert (minMaxWithinInterval result) result

        windowToMinMaxInfo :: Window -> WindowMinMax
        windowToMinMaxInfo (offset, samples) = let result = VU.ifoldl' ffold (SMin (offset, firstElem), SMax (offset, firstElem)) windowRemain
          in result
          where firstElem = VU.unsafeHead samples
                windowRemain = VU.tail samples
                -- when there are two equal values use the right most value, this is beneficial because the window moves right
                -- and need to keep track of less points on the left side
                ffold acc@(smin@(SMin (_, vmin)), smax@(SMax (_, vmax))) i v = -- trace ("ffold -- acc: " ++ show acc ++ ", i: " ++ show i ++ ", v: " ++ show v)
                   (if v <= vmin then SMin (i+1+offset, v) else smin, if v >= vmax then SMax (i+1+offset, v) else smax)

        vectorFindRightWithIndex :: (Sample -> Sample -> Bool) -> Window -> (Position, Sample)
        vectorFindRightWithIndex f (offset, samples) = VU.ifoldl' ffold (offset, VU.unsafeHead samples) (VU.tail samples)
          where ffold a@(_, av) i e = -- trace ("find: " ++ show a ++ " " ++ show i ++ " " ++ show e ++ " " ++ show offset) $
                                      if f av e then a else (i+1+offset, e)

        -- prefers b when a and b are equal
        combineMinMax (a_smin@(SMin (_, a_vmin)), a_smax@(SMax (_, a_vmax))) (b_smin@(SMin (_, b_vmin)), b_smax@(SMax (_, b_vmax))) =
          let c_smin = if b_vmin <= a_vmin then b_smin else a_smin
              c_smax = if b_vmax >= a_vmax then b_smax else a_smax
          in (c_smin, c_smax)

        -- incomplete pattern matching !
        combineMin a_smin@(SMin (_, a_vmin)) b_smin@(SMin (_, b_vmin)) =
          let c_smin = if b_vmin <= a_vmin then b_smin else a_smin
          in c_smin

        combineMax a_smax@(SMax (_, a_vmax)) b_smax@(SMax (_, b_vmax)) =
          let c_smax = if b_vmax >= a_vmax then b_smax else a_smax
          in c_smax

        -- need to relate everything back to the original vector, that's why the offset is stored
        getWindow :: Int -> Int -> Window
        getWindow len start = (start, VU.take len (VU.drop start points))



-- naive version (slow)
mtiesXY1 :: VU.Vector (X, Y) -> [(Interval, Mtie)]
mtiesXY1 points = map intervalMtie valid_mties_intervals
  where valid_mties_intervals = filter (<= last_point) mties_intervals

        last_point = fst $ VU.last points

        intervalMtie :: Interval -> (Interval, Mtie)
        intervalMtie int = (int, maximum $ map (\i -> windowMinMax $ window $ remainder i) (start_indices int))
          where 
                start_indices :: Interval -> [(Int)] -- list with valid start indices
                start_indices int = [0 .. start_max_i]
                  where start_max = last_point - int
                        start_max_i :: Int
                        start_max_i = let i = (fromMaybe (-1) $ (VU.findIndex (\(x, _) -> x > start_max) points)) - 1
                                      in assert (i > 0) i

                remainder :: Int -> VU.Vector (X, Y) -- get a vector from start window to end of all samples
                remainder start_i = VU.unsafeDrop start_i points

                window :: VU.Vector (X, Y) -> VU.Vector (X, Y) -- get a vector from start window to end window
                window remainder = VU.takeWhile (\(x, _) -> x <= start + int) remainder
                  where start = fst $ VU.head remainder

                windowMinMax :: VU.Vector (X, Y) -> Mtie
                windowMinMax w = VU.maximum ys - VU.minimum ys
                  where ys = VU.map snd w
