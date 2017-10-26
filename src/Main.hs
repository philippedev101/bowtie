{-
Copyright (C) 2017  philippedev101@gmail.com

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as published
by the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}


module Main where

import Algos
import Control.Exception
import Control.Exception.Extra (ignore)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State
import Control.Monad.Trans.Resource
import Data.Conduit hiding ((=$), ($=), (=$=), ($$)) -- first 3 operators have been replaced by .| ,   x $$  y = runConduit (x .| y)
import Data.Conduit.Attoparsec hiding (Position)
import Data.Conduit.Lift
import Data.List (intersperse)
import Data.Text.Encoding
import Data.Version (showVersion)
import Debug.Trace (trace)
import GHC.IO.Exception (ioe_description)
import Options.Applicative hiding (handleParseResult, str)
import Paths_bowtie (version)
import qualified Control.Foldl as F
import qualified Data.Attoparsec.ByteString.Char8 as A hiding (parse, Done, skipWhile)
import qualified Data.Attoparsec.ByteString.Lazy as A hiding (takeWhile)
import qualified Data.Attoparsec.Text as AT
import qualified Data.Binary.IEEE754 as DB
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Internal as BS (c2w)
import qualified Data.ByteString.Lazy as B
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Serialization.Binary as CSB
import qualified Data.Double.Conversion.Text as DT
import qualified Data.Map as Map
import qualified Data.Semigroup as S ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector.Unboxed as VU
import qualified Text.PrettyPrint.ANSI.Leijen as PP hiding ((<$>), sep)
import qualified Text.Show.ByteString
import System.Directory (removeFile, doesFileExist)
import System.IO (stderr, hPutStrLn, stdin, stdout, IOMode(..), openFile, hClose) -- openBinaryFile
import Utils (first, second, maybeErr)


type NanoSeconds = Double
type X = Double
type Y = Double
type Time = NanoSeconds
type Value = NanoSeconds

data ResampleStrategy = LeftNearest | Interpolate
instance Show ResampleStrategy where
  show LeftNearest = "nearest"
  show Interpolate = "interpolate"
  -- could add a nearest here (pick left or right based on what is closer)

data Calculation = Tdev | Mtie | All deriving (Eq, Ord)
instance Show Calculation where
  show Tdev = "tdev"
  show Mtie = "mtie"
  show All = "all"

data Device = OmniBer | SJ300E | SJ300E_BINARY -- SJ300E_ASCII | 
instance Show Device where
  show OmniBer = "omniber"
  show SJ300E  = "sj300e"
  -- show SJ300E_ASCII  = "sj300e_ascii"
  show SJ300E_BINARY  = "sj300e_binary"

-- program options
data Options = Options
  { _calculation :: Calculation
  , _input  :: Maybe String
  , _output :: Maybe String
  , _device :: Maybe Device
  , _valueColumn :: Maybe Int
  , _timeColumn :: Maybe Int
  , _sampleTime :: Maybe NanoSeconds -- should be given by the user and approximate the actual sample time
  , _resampleStrategy :: ResampleStrategy
  , _floatPrecision :: Maybe Int -- -1 for disabled in parsing
  , _pointsPerDecade :: Int
  , _useAllValues :: Bool
  , _verbose :: Bool
  , _version :: Bool -- https://pvp.haskell.org/faq/
  }

parseOptions :: Parser Options
parseOptions = Options
  <$> option (maybeReader parseCalculation)
    ( long "calculate"
    S.<> short 'c'
    S.<> help "Calculate to run on the input file. \"all\" runs all calculations, when an output file is given it will be suffixed with the calculation name, example `-o myfile.dat` will give `myfile.dat.tdev` and `myfile.dat.mtie`"
    S.<> metavar "tdev | mtie | all" )
  <*> optional (strOption
    ( long "input"
    S.<> short 'i'
    S.<> metavar "SOURCE"
    S.<> help "Input file, when omitted will read from stdin." ))
  <*> optional (strOption
    ( long "output"
    S.<> short 'o'
    S.<> metavar "DESTINATION"
    S.<> help "Output file, when omitted will write to stdout." ))
  <*> (optional (option (maybeReader parseDevice)
    ( long "device"
    S.<> short 'd'
    S.<> help "Set settings for specific device, this will set default values when not given for: valuecolumn, timecolumn and sampletime. It will override skipWarmupValues"
    S.<> metavar "omniber | sj300e | sj300e_binary" ))) -- sj300e_ascii | 
  <*> (optional $ option auto
    ( long "valuecolumn"
    S.<> short 'v'
    S.<> help "Column to use for TIE values (first column is index 1)."
    S.<> metavar "INT" ))
  <*> (optional $ option auto
    ( long "timecolumn"
    S.<> short 't'
    S.<> help "Column to use for time values (first column is index 1). When ommitted --sampletime has to be supplied."
    S.<> metavar "INT" ))
  <*> (optional $ option auto
    ( long "sampletime"
    S.<> short 's'
    S.<> help "Sampletime used. When ommitted --timecolumn has to be supplied and sampletime will be automatically calculated by averaging."
    S.<> metavar "Seconds" ))
  <*> option (maybeReader parseResampleStrategy)
    ( long "resample"
    S.<> short 'r'
    S.<> help "Resample strategy to use when the --timecolumn option is set. Nearest picks the sample before t. Interpolate uses the samples before and after t."
    S.<> showDefault
    S.<> value Interpolate
    S.<> metavar "nearest | interpolate" )
  <*>  ((\x -> (if x < 0 then Nothing else Just x)) <$> option auto
    ( long "precision"
    S.<> short 'p'
    S.<> help "Float precision in decimals for output. Use -1 for max precision/"
    S.<> showDefault
    S.<> value 3
    S.<> metavar "INT" ))
  <*> option (parseWithPredicate AT.decimal (>= 1) "pointsPerDecade should be 1 or higher.")
    ( long "pointsPerDecade"
    S.<> help "The desired amount of output data points per decade. It can happen that there are not enough data points available in a decade, especially in the lower intervals."
    S.<> showDefault
    S.<> value 100
    S.<> metavar "INT" )
  <*> switch
    ( long "useAllValues"
    S.<> short 'a'
    S.<> help "Some scripts controlling the OmniBer or SJ300E give a few unreliable values at start. These values have timestamps that increment by 0.001 ns and will be detected and skipped, unless this flag is passed."
    )
  <*> switch
    ( long "verbose"
    S.<> help "Prints extra information."
    )
  <*> switch
    ( long "version"
    S.<> help "Prints the program version."
    )


parseWithPredicate :: AT.Parser a -> (a -> Bool) -> String -> ReadM a
parseWithPredicate p predicate err = eitherReader $ \str ->
  case (AT.parseOnly p (T.pack str)) of
    Left e -> Left e
    Right v -> if predicate v then Right v else Left err

parseCalculation :: String -> Maybe Calculation
parseCalculation "tdev" = Just Tdev
parseCalculation "mtie" = Just Mtie
parseCalculation "all"  = Just All
parseCalculation _      = Nothing

parseResampleStrategy :: String -> Maybe ResampleStrategy
parseResampleStrategy "nearest"     = Just LeftNearest
parseResampleStrategy "interpolate" = Just LeftNearest
parseResampleStrategy _             = Nothing

parseDevice :: String -> Maybe Device
parseDevice "omniber"       = Just OmniBer
parseDevice "sj300e"        = Just SJ300E
-- parseDevice "sj300e_ascii"  = Just SJ300E_ASCII
parseDevice "sj300e_binary" = Just SJ300E_BINARY
parseDevice _               = Nothing



-- https://hackage.haskell.org/package/conduit-extra-0.1.6/docs/src/Data-Conduit-Extra-Foldl.html#sinkFold
sinkFold :: Monad m => F.Fold a b -> Consumer a m b
sinkFold (F.Fold f seed extract) = fmap extract (CL.fold f seed)

getAverageSampleInterval :: Monad m => ConduitM Double o m (Maybe Double)
getAverageSampleInterval = sinkFold $ liftA3 (\start end len -> liftA2 (\s e -> (s - e) / fromIntegral len) start end) F.head F.last F.length

-- variant of https://hackage.haskell.org/package/conduit-1.2.10/docs/Data-Conduit.html#v:awaitForever
awaitTwo :: Monad m => (i -> i -> ConduitM i o m ()) -> ConduitM i o m ()
awaitTwo callback = do
  m1 <- await
  m2 <- await
  case (m1, m2) of
    (Just i1, Just i2) -> do callback i1 i2
                             leftover i2
                             awaitTwo callback
    _                  -> pure ()


skipInvalidValues :: Monad m => Conduit (Time, Value) m (Time, Value)
skipInvalidValues = evalStateC 0 $ awaitTwo $ \i1@(t1, _) (t2, _) -> do
  cnt <- get

  if cnt == 1 then do
    yield i1
  else do
    -- real measurement numbers start when the difference is greater than 0.001
    -- when 0.001, 0.002, 0.55, 0.83 is given this will be true on 0.002 - 0.55
    -- now start yielding 0.002 so that next conduit received (Nothing, 0.002)
    -- and then on next iteration things will go as they should (0.55, 0.83)
    when ((t2 - t1) > 0.001) $ do
      modify $ \c -> c + 1

sampleStateSummary :: AdjustSampleState -> T.Text
sampleStateSummary (AdjustSampleState {_smallestTimeStep = s, _largestTimeStep = l}) = T.concat
  [(T.pack "Smallest interval: "), (DT.toShortest s), (T.pack "\nBiggest interval: "), (DT.toShortest l), (T.pack "\n")]


data AdjustSampleState = AdjustSampleState 
  { _handledStartOffset :: Bool -- R/W when samples start at t > st, t is first incremented by st so that t1 <= t <= t2
                                --     otherwise it would be: t <= t1 <= t2 giving false (often negative) interpolation results
                                --     this is also wrong in the original algorithm
  , _t :: Double                -- R/W the current time, can be used afterwards to get the measurement time
  , _smallestTimeStep :: Double -- W   can be used later to detect anomalies in the measurement
  , _largestTimeStep :: Double  -- W   can be used later to detect anomalies in the measurement, and also detect the error margin
  } deriving (Show)

infinity :: Double
infinity = encodeFloat (floatRadix 0 - 1) (snd $ floatRange 0) -- Infinity, https://stackoverflow.com/questions/2354707/in-haskell-is-there-infinity-num-a-a#comment2332197_2354766

startAdjustSampleState :: AdjustSampleState
startAdjustSampleState = AdjustSampleState
  { _handledStartOffset = False
  , _t = 0
  , _smallestTimeStep = infinity
  , _largestTimeStep = 0
  }


-- replaces fixed_sample_size_filter in original code
--
-- == Consideration about left nearest strategy
-- Because the actual measurements are pretty accurate it's expected that the strategy doesn't matter much for the accuracy of the results
-- * Interpolate has the weakness of creating "fake" values
--   since it's an average over 2 samples it's as if the signal has gone through a small FIR filter (lowpass)
-- * LeftNearest has the weakness of using values at the wrong timestamp
--   The error in relation to the sampletime can be calculated with: (_largestTimeStep / sampleTime) * 100%
-- It's hard to tell which one is best in accuracy of results, but LeftNearest is probably prefered because of speed
-- Further analysis of the results and benchmarks of this program are required to find out.

-- == Consideration about code style relating to the resample strategies
-- It's more difficult to move the resample strategies out of this function because we need to pass
-- * t
-- * the callback function for the conduit, in this case: adjustSampleInterval resampleStrategy
-- * the state in order to update the current time
-- Since there are only 2 strategies at the moment we leave it like is

-- == Considerations about the original program
-- * first index in original program is never assigned and undefined because of malloc (instead of calloc)
-- * original program has out of bound array index because max index happens when  org_sample_nr == number_of_samples - 1
--   but then later index  org_sample_nr + 1  is used
--   This is then later compensated by not using the last sample
adjustSampleInterval :: Monad m => NanoSeconds -> ResampleStrategy -> Conduit (Time, Value) m Value
adjustSampleInterval sampleTime resampleStrategy = evalStateC startAdjustSampleState $ awaitTwo go
  where go (t1, v1) (t2, v2) = do

          AdjustSampleState
            { _t = tState
            , _smallestTimeStep = small
            , _largestTimeStep = large
            , _handledStartOffset = handledStartOffset
            } <- get

          -- this prevents bogus results at start, suppose first values are 0.55 and 0.89
          -- when t = 0 it will yield negative numbers when using interpolation
          -- what's actually happening is that this extrapolates to the left
          -- to correct this t is first offset so that  t >= t1
          -- with sampleTime of 0.33 this would be 0.66
          t <- if handledStartOffset then pure tState else do
            let t' = until (>= t1) (+ sampleTime) tState
            modify $ \s -> s { _handledStartOffset = True, _t = t' }
            pure t'

          -- error $ printf "%f %f %f %f %f\n" tState t t1 t2 sampleTime

          -- uncomment following line to verify a particular calculation
          -- when (t > 0) (error $ printf "%f %f %f %f %f %f\n" t t1 t2 v1 v2 (v1 + ((t - t1) / (t2 - t1)) * (v2 - v1)))

          when (small > t2 - t1) $ modify $ \s -> s { _smallestTimeStep = t2 - t1 }
          when (large < t2 - t1) $ modify $ \s -> s { _largestTimeStep = t2 - t1 }

          case resampleStrategy of
            LeftNearest -> leftPick t
            Interpolate -> interpolation t


          where yieldSample val = do
                  modify $ \s -> s { _t = _t s + sampleTime }
                  yield val

                -- not (t2 < t)  ==  t2 >= t   ,  so:   t1 <= t <= t2

                -- fastest strategy: just pick the first value left of the sample time
                leftPick t = when (not (t2 < t)) $ yieldSample v2

                -- strategy of original program: interpolate the value based on 2 values, 1 left of sampletime, 1 right of sampletime
                interpolation t = when (not (t2 < t)) $ yieldSample $ v1 + ((t - t1) / (t2 - t1)) * (v2 - v1)
                                  -- org_sample_nr = v1
                                  -- org_sample_nr + 1 = v2
                                  -- x[new_sample_nr] = va[org_sample_nr] + (diff_new_sample_time / diff_measurement_time) * diff_tie_ampl;


type AccumResult = Value

-- this should be run before the new implementation of tdev
accConduit :: Monad m => Conduit Value m AccumResult
accConduit = evalStateC 0 $ awaitForever $ \i -> do
  total <- get
  let newTotal = total + i
  put newTotal
  yield newTotal


-- functions for handling input
type SJ300Eheader = (BSC.ByteString, Maybe BSC.ByteString)

parseHeader_SJ300E :: A.Parser [SJ300Eheader]
parseHeader_SJ300E = do
  res <- flip sepBy1 (A.char '\NUL') headerParser
  _ <- A.string $ BSC.pack "\NUL"
  pure res
  --  3-5 look like they can be replaced by many $ flip manyTill (A.char '\NUL') headerParser, allowing you to inline headerParser's definition
  where headerParser = do
          -- ([^=]+)(=[^=]+)? od oa \0
          first <- A.takeWhile $ \c -> c /= '=' && c /= '\NUL'
          second <- A.option Nothing $ A.try $ do
            _ <- A.char '='
            ss <- fmap trimBytestring $ fmap BSC.pack $ A.many1 $ A.notChar '\NUL'

            if BSC.length ss == 0 then
              pure Nothing
            else
              pure $ Just ss

          pure (first, second)
        sepBy1 :: (Monad f, Alternative f) => f (BSC.ByteString, t) -> f a -> f [(BSC.ByteString, t)]
        sepBy1 p s = scan
            where scan = do
                    res@(first, _) <- p
                    if BSC.take 7 first == "REMARKS" then do
                      pure [res]
                    else do
                      sep <- ((s *> scan) <|> pure []) -- trace (BSC.unpack $ BSC.take 7 first) 
                      pure $ res : sep
              

parseNumbers :: A.Parser [Double]
parseNumbers = do
  _ <- skip20
  numbers <- A.sepBy1 A.double skip20
  _ <- skip20
  _ <- A.skipWhile (\c -> c == (BS.c2w '\n') || c == (BS.c2w '\r'))
  pure numbers
  where skip20 = A.skipWhile (== (BS.c2w ' '))

handleParseResult :: MonadIO m => Conduit (Either Data.Conduit.Attoparsec.ParseError (PositionRange, a)) m a
handleParseResult = awaitForever $ \result -> case result of
  Left e -> liftIO $ hPutStrLn stderr (show e)
  Right (_, val) -> yield val

readTwoCols :: MonadIO m => Int -> Int -> Int -> Conduit BSC.ByteString m (Time, Value)
readTwoCols tcol vcol maxcol = conduitParserEither (parseTimeValue tcol vcol) .| handleParseResult
  where 
        parseTimeValue :: Int -> Int -> A.Parser (Time, Value)
        parseTimeValue columnTimeIndex columnValueIndex = do
          numbers <- parseNumbers
          if length numbers < maxcol then
            fail $ "Not enough numbers found to select column " ++ show maxcol ++ " numbers found: " ++ show numbers
          else
            pure ( numbers !! (columnTimeIndex - 1) , numbers !! (columnValueIndex - 1) )

readSingleCol :: MonadIO m => Int -> Conduit BSC.ByteString m Value
readSingleCol col = conduitParserEither parseCol .| handleParseResult
  where 
        parseCol ::A.Parser Value
        parseCol = do
          numbers <- parseNumbers
          if length numbers < col then
            fail $ "Not enough numbers found to select column " ++ show col ++ " numbers found: " ++ show numbers
          else
            pure $ numbers !! (col - 1)

-- functions for formatting output
xyToByteString :: Maybe Int -> Conduit (X, Y) (ResourceT IO) BSC.ByteString
xyToByteString floatPrecision = do
  await >>= \case
    Nothing -> pure ()
    Just (x, y) -> do
      yield $ BSC.concat [floatToByteString floatPrecision x, BSC.pack " ", floatToByteString floatPrecision y, BSC.pack "\n"]
      xyToByteString floatPrecision
  where 
        floatToByteString :: RealFloat a => Maybe Int -> a -> BSC.ByteString
        floatToByteString fp bs = B.toStrict $ Text.Show.ByteString.runPut $ Text.Show.ByteString.showpFFloat fp bs


-- main functions
dummyConduit :: Monad m => Conduit a m a
dummyConduit = awaitForever $ \val -> yield val

divide100 :: Monad m => Conduit Double m Double
divide100 = awaitForever $ \val -> yield (val / 100)

prepareSJ300Ebinary :: (MonadResource m, MonadThrow m) => Source m BSC.ByteString -> m (ResumableSource m Double, [SJ300Eheader])
prepareSJ300Ebinary src = do
  (newSrc, headers) <- src $$+ (sinkParser parseHeader_SJ300E)
  let continuation = newSrc $=+ (CSB.conduitGet DB.getFloat64le) .| divide100
  pure (continuation, headers)


trimBytestring :: BSC.ByteString -> BSC.ByteString
trimBytestring bs = fst $ BSC.spanEnd isSpace (BSC.dropWhile isSpace bs)
  where isSpace :: Char -> Bool
        isSpace word = word == '\r' || word == '\n' || word == ' '

headersToText :: [SJ300Eheader] -> T.Text
headersToText headers = T.concat $ intersperse (T.singleton '\n') $ map headerAsText headers
  where trimDecode = decodeLatin1 . trimBytestring
        headerAsText (key, Nothing) = (trimDecode key)
        headerAsText (key, Just val) = T.concat [(trimDecode key), T.pack " = ", (trimDecode val)]


data ConduitException = CE String deriving (Show)
instance Exception ConduitException


runOutputAlgo :: Maybe Int -> Sink BSC.ByteString (ResourceT IO) () -> [(X, Y)] -> IO ()
runOutputAlgo fp snk values = catch (runResourceT $ runConduit $ CL.sourceList values .| (xyToByteString fp) .| snk) (\(CE ex) -> hPutStrLn stderr ex)


runTwo
  :: (VU.Unbox a, VU.Unbox b) =>
     Bool
     -> Maybe Int
     -> ResourceT IO (ResumableSource (ResourceT IO) fromParser, Maybe T.Text)
     -> (VU.Vector a -> [(X, Y)], Conduit fromParser (ResourceT IO) a, Sink BSC.ByteString (ResourceT IO) ())
     -> (VU.Vector b -> [(X, Y)], Conduit fromParser (ResourceT IO) b, Sink BSC.ByteString (ResourceT IO) ())
     -> IO ()
runTwo vvv fp getValues (algo1, applyCon1, snk1) (algo2, applyCon2, snk2) = do
  let splitter (rsrc, t) = do
        samples1 <- rsrc $=+ applyCon1 $$+- CL.consume
        samples2 <- rsrc $=+ applyCon2 $$+- CL.consume
        pure (samples1, samples2, t)

  run <- runResourceT $ fmap splitter getValues
  (values1, values2, verboseText) <- runResourceT run

  when vvv $ forM_ verboseText T.putStrLn
  let vec1 = VU.fromList values1
  let vec2 = VU.fromList values2
  
  runOutputAlgo fp snk1 $ algo1 vec1
  runOutputAlgo fp snk2 $ algo2 vec2


runOne
  :: (VU.Unbox fromParser, Show fromParser) =>
     Bool
     -> Maybe Int
     -> Sink BSC.ByteString (ResourceT IO) ()
     -> (VU.Vector fromParser -> [(X, Y)]) -- algorithm to apply
     -> ResourceT IO (ResumableSource (ResourceT IO) fromParser, Maybe T.Text) -- source for the values
     -> IO ()
runOne vvv fp snk algo getValues = do
  vec <- runResourceT $ do
    (values, verboseText) <- fmap (\(rsrc, t) -> (rsrc $$+- CL.consume, t)) getValues
    liftIO $ when vvv $ forM_ verboseText T.putStrLn
    vec <- fmap VU.fromList $ values

    ------------- Uncomment this code to assist in debugging input into algorithms (possible after resampling)
    -- let vecfile = "processed_samples.out"
    -- --when (liftIO $ doesFileExist vecfile) (liftIO $ removeFile vecfile)
    -- liftIO $ ignore $ removeFile vecfile
    -- handle <- liftIO $ openFile vecfile WriteMode
    -- VU.mapM_ (\val -> liftIO $ hPutStrLn handle (show val)) vec
    -- liftIO $ hClose handle
    -- --liftIO $ T.appendFile vecfile $ VU.foldl' (\a b -> T.append a (T.pack $ show b ++ "\n")) T.empty $ vec
    -- --VU.mapM_ (\val -> liftIO $ appendFile vecfile (show val ++ "\n")) vec

    pure vec
  runOutputAlgo fp snk $ algo vec


parseWithTime
  :: (Monad m, MonadIO m1) =>
     ConduitM () BSC.ByteString m1 ()
     -> Int
     -> Int
     -> Maybe Device
     -> m (ResumableSource m1 (Time, Value), Maybe T.Text)
parseWithTime src tcol vcol device =
  case device of
    Nothing      -> go
    Just OmniBer -> go
    Just SJ300E  -> go
    _            -> error "error A"
  where go =
          let (continuation, sampleState) = prepareFromTimeValue src tcol vcol
          in pure $ (continuation, Just $ sampleStateSummary sampleState)
        prepareFromTimeValue src tcol vcol =
          let rsrc = newResumableSource $ src .| (readTwoCols tcol vcol (max tcol vcol))
          in (rsrc, startAdjustSampleState)


parseWithoutTime
  :: (MonadBaseControl IO m, MonadThrow m, MonadIO m) =>
     Conduit () (ResourceT m) BSC.ByteString
     -> Int
     -> Maybe Device
     -> ResourceT m (ResumableSource (ResourceT m) Value, Maybe T.Text)
parseWithoutTime src vcol device =
  case device of
    Nothing            -> pure (prepareFromValue src vcol, Nothing)
    Just SJ300E_BINARY -> fmap (second $ Just . headersToText) $ prepareSJ300Ebinary src --fmap (\(continuation, headers) -> (continuation, Just $ headersToText headers)) (prepareSJ300Ebinary2 src)
    _                  -> error $ "The parseWithoutTime function should not be called for device " ++ (show device)
  where prepareFromValue :: (MonadResource m, MonadIO m) => Conduit () m BSC.ByteString -> Int -> ResumableSource m Value
        prepareFromValue src vcol = newResumableSource $ src .| (readSingleCol vcol)


getSampleTime
  :: Conduit () (ResourceT IO) BSC.ByteString
     -> Maybe Int
     -> Maybe NanoSeconds
     -> IO (Either String NanoSeconds)
getSampleTime src tcol st = case st of
  Just st' -> pure $ Right st'
  Nothing  -> case tcol of
    Nothing    -> pure $ Left "Either --sampletime or --timecolumn needs to be set."
    Just tcol' -> do
      runResourceT $ runConduit $ src .| (readSingleCol tcol') .| getAverageSampleInterval
      >>= \case
        Nothing -> pure $ Left "No sampletime was given, and no values were found to calculate the average"
        Just average -> pure $ Right average


checkForDeviceSettings :: (MonadIO m, MonadResource m, MonadBaseControl IO m) => Options -> Either (IO ()) (IO
  ( Source m BSC.ByteString
  , Map.Map Calculation (Conduit BSC.ByteString (ResourceT IO) t)
  , Maybe Int
  , Maybe Int
  , Maybe NanoSeconds
  , ResampleStrategy
  , Maybe Int
  , Int
  , Bool
  , Maybe Device
  , Bool
  , Calculation
  ))
checkForDeviceSettings (Options calc fin fout device vcol tcol st rs fp ppd vvv uav ver) = if ver then Left (putStrLn (showVersion version)) else do
  let deviceDefault val deviceCase = val <|> (deviceCase device)
      tcol' = deviceDefault tcol $ \d -> case d of { Just OmniBer -> Just 1;     Just SJ300E -> Just 1;                                     _ -> Nothing }
      vcol' = deviceDefault vcol $ \d -> case d of { Just OmniBer -> Just 2;     Just SJ300E -> Just 3;    Just SJ300E_BINARY -> Just 3;    _ -> Nothing }
      st'   = deviceDefault st   $ \d -> case d of { Just OmniBer -> Just (1/3); Just SJ300E -> Just 0.02; Just SJ300E_BINARY -> Just 0.02; _ -> Nothing }
      uav' = case device of
               Nothing -> uav
               Just OmniBer       -> False
               Just SJ300E        -> True
               Just SJ300E_BINARY -> True
      src = case fin of
        Nothing    -> CB.sourceHandle stdin `catchC` \ex -> throw $ CE $ "Input stream error: " ++ ioe_description ex
        Just input -> CB.sourceFile input   `catchC` \ex -> throw $ CE $ "Input file error: " ++ ioe_description ex
      stdOutSnk = CB.sinkHandle stdout `catchC` \ex -> throw $ CE $ "Output stream error: " ++ ioe_description ex :: Conduit BSC.ByteString (ResourceT IO) t
      fileSnk filename = CB.sinkFile filename `catchC` \ex -> throw $ CE $ "Output file error: " ++ ioe_description ex :: Conduit BSC.ByteString (ResourceT IO) t
      snks :: Map.Map Calculation (Conduit BSC.ByteString (ResourceT IO) t)
      snks = case (calc, fout) of
               (_, Nothing) -> Map.fromList [(Tdev, stdOutSnk), (Mtie, stdOutSnk)]
               (Tdev, Just output) -> Map.fromList [(Tdev, fileSnk output)]
               (Mtie, Just output) -> Map.fromList [(Mtie, fileSnk output)]
               (All, Just output)  -> Map.fromList [(Tdev, fileSnk (output ++ "_tdev")), (Mtie, fileSnk (output ++ "_mtie"))]

  Right $ pure $ (src, snks, vcol', tcol', st', rs, fp, ppd, uav', device, vvv, calc)


run :: Options -> IO ()
run opts = do
  let eitherSettings = checkForDeviceSettings opts :: Either (IO ()) (IO (Source (ResourceT IO) BSC.ByteString, Map.Map Calculation (Conduit BSC.ByteString (ResourceT IO) t), Maybe Int, Maybe Int, Maybe NanoSeconds, ResampleStrategy, Maybe Int, Int, Bool, Maybe Device, Bool, Calculation))
  case eitherSettings of
    Left version' -> version'
    Right settings -> do
      (src, snks, vcol, tcol, st, rs, fp, ppd, uav, device, vvv, calc) <- settings

      case vcol of
        Nothing    -> hPutStrLn stderr $ "Either --valuecolumn or --device needs to be set."
        Just vcol' -> do

          getSampleTime src tcol st >>= \case
            Left err -> hPutStrLn stderr err
            Right st'' -> do
              let withoutTime    = parseWithoutTime src vcol' device
                  withTime :: Int -> ResourceT IO (ResumableSource (ResourceT IO) (Time, Value), Maybe T.Text)
                  withTime tcol' = parseWithTime src tcol' vcol' device

                  goOne :: (VU.Unbox b, Show b) =>
                      ResourceT IO (ResumableSource (ResourceT IO) fromParser, Maybe T.Text)
                      -> ((VU.Vector b -> [(X, Y)]), Conduit fromParser (ResourceT IO) b, Sink BSC.ByteString (ResourceT IO) ())
                      -> IO ()
                  goOne parser (algo, applyCon, snk) = runOne vvv fp snk algo (fmap (first ($=+ applyCon)) parser)

                  goTwo :: (VU.Unbox a, VU.Unbox b) =>
                      ResourceT IO (ResumableSource (ResourceT IO) fromParser, Maybe T.Text)
                      -> (VU.Vector a -> [(X, Y)], Conduit fromParser (ResourceT IO) a, Sink BSC.ByteString (ResourceT IO) ())
                      -> (VU.Vector b -> [(X, Y)], Conduit fromParser (ResourceT IO) b, Sink BSC.ByteString (ResourceT IO) ())
                      -> IO ()
                  goTwo parser a b = runTwo vvv fp parser a b

                  goTdev = tdevs st'' ppd

                  tdevSnk = maybeErr "Error: Tdev sink should be available" $ Map.lookup Tdev snks
                  mtieSnk = maybeErr "Error: Mtie sink should be available" $ Map.lookup Mtie snks

                  tdevWithoutTime = (goTdev, accConduit, tdevSnk)
                  tdevWithTimeWithUav = (goTdev, (skipInvalidValues .| (adjustSampleInterval st'' rs) .| accConduit), tdevSnk)
                  tdevWithTimeWithoutUav = (goTdev, ((adjustSampleInterval st'' rs) .| accConduit), tdevSnk)

                  mtieWithoutTime = ((mtiesY st''), dummyConduit, mtieSnk)
                  mtieWithTimeWithUav = (mtiesXY, skipInvalidValues, mtieSnk)
                  mtieWithTimeWithoutUav = (mtiesXY, dummyConduit, mtieSnk)
              case calc of
                Tdev -> case (tcol, uav) of
                          (Nothing,    _ )    -> goOne withoutTime tdevWithoutTime
                          (Just tcol', True ) -> goOne (withTime tcol') tdevWithTimeWithUav
                          (Just tcol', False) -> goOne (withTime tcol') tdevWithTimeWithoutUav

                Mtie -> case (tcol, uav) of
                          (Nothing,    _ )    -> goOne withoutTime mtieWithoutTime
                          (Just tcol', True ) -> goOne (withTime tcol') mtieWithTimeWithUav
                          (Just tcol', False) -> goOne (withTime tcol') mtieWithTimeWithoutUav

                All  -> case (tcol, uav) of
                          (Nothing,    _ )    -> goTwo withoutTime tdevWithoutTime mtieWithoutTime
                          (Just tcol', True ) -> goTwo (withTime tcol') tdevWithTimeWithUav mtieWithTimeWithUav
                          (Just tcol', False) -> goTwo (withTime tcol') tdevWithTimeWithoutUav mtieWithTimeWithoutUav


main :: IO ()
main = run =<< execParser opts -- if runBench then doBench else 
  where
    opts = info (parseOptions <**> helper) ( fullDesc
      S.<> header "bowtie ⋈ — a program to perform analysis on Time Interval Error (TIE)\n\n"
      S.<> progDescDoc (Just $ PP.text ("\n"
      ++ "Explanation of tdev options:\n"
      ++ "| timecolumn | sampletime |  resample   | \n"
      ++ "|      ✘     |     ✘      |      -      | Invalid, either timecolumn or sampletime needs to be present.\n"
      ++ "|      ✘     |     ✔      |      -      | Don't use time values, don't resample, use TIE value as is.\n"
      ++ "|      ✔     |     ✔      |   nearest   | Pick left nearest TIE value given a sampletime.\n"
      ++ "|      ✔     |     ✘      |   nearest   | Pick left nearest TIE value based on average sampletime.\n"
      ++ "|      ✔     |     ✔      | interpolate | Interpolate TIE values given a sampletime.\n"
      ++ "|      ✔     |     ✘      | interpolate | Interpolate TIE values based on average sampletime.\n"
      ++ "View ResampleStrategies.png for a graphical explanation of the resampling strategies.\n"
      )))
