
module Main (
  main,
  perfect,dumb,mm,mc
  ) where

import Control.Monad
import Data.List (maximumBy)
import Data.List.NonEmpty (NonEmpty)
import Data.Ord (comparing)
import Data.Ratio ((%))
import System.Random (getStdRandom,randomR)
import qualified Data.List.NonEmpty as NE

import NimRules
  (Size, Move, Pos, Status(Complete,Continue), Outcome(Loss,Draw,Win),
   start, status, apply,
   )
import qualified NimRules


main :: IO ()
main = do
  --tournament size (mm depth) perfect
  tournament size (mc trials) perfect
  where
    size = 8 -- mc is no good for size 10!
    trials = 10_000


-- | run a tournament between 2 strategies; playing 20 rounds of Nim/size
tournament :: Size -> Strategy -> Strategy -> IO ()
tournament size s1 s2 = do
  putStrLn $ "*nim*" <> show size
  let n = 20
  runs <- forM [1..n] $ \_ -> do
    run <- runRand $ playoff size s1 s2
    pr run
    return run
  print (length (filter p1wins runs) % n)
    where
      pr moves = putStrLn $ (if p1wins moves then "(p1) " else "(p2) ") <> show moves
      p1wins moves = length moves `mod` 2 == 0


playoff :: Size -> Strategy -> Strategy -> Rand [Move]
playoff size s1 s2 = play (start size) s1 s2
  where
    play :: Pos -> Strategy -> Strategy -> Rand [Move]
    play pos s1@(Strategy choose) s2 = case status pos of
      Complete _ -> return []
      Continue moves -> do
        m <- choose moves
        (m:) <$> play (apply m) s2 s1


data Strategy = Strategy (NonEmpty Move -> Rand Move)


newtype Trials = Trials Int deriving (Eq,Enum,Num)

-- | monte-carlo strategy; fixed number of trials
mc :: Trials -> Strategy
mc (Trials n) = Strategy $ \moves -> do
  let n' = Trials (n `div` length moves)
  rollouts <- forM (NE.toList moves) $ \m -> do
    score <- rollOuts n' m
    return (m, score)
  return $ fst $ maximumBy (comparing snd) rollouts

rollOuts :: Trials -> Move -> Rand Double
rollOuts (Trials n) move = do
  outcomes <- forM [1..n] $ \_ -> rollOutMove move
  let nWins = length [ () | Win <- outcomes]
  let nDraws = length [ () | Draw <- outcomes]
  return $ fromIntegral (2*nWins + nDraws) / fromIntegral (2*n)

rollOut :: Pos -> Rand Outcome
rollOut pos = case status pos of
  Complete outcome -> return outcome
  Continue moves -> do
    m <- randomPick moves
    rollOutMove m

rollOutMove :: Move -> Rand Outcome
rollOutMove move = invertOutcome <$> rollOut (apply move)



newtype Depth = Depth Int deriving (Eq,Ord,Num)

-- | mini-max strategy; fixed depth
mm :: Depth -> Strategy
mm d = Strategy $ \moves -> do
  if d == 0 then randomPick moves else do
    let choices = flip NE.map moves $ \m -> (m, valueMove d m)
    case NE.nonEmpty [ m | (m, Win) <- NE.toList choices ] of
      Just vics -> randomPick vics
      Nothing ->
        case NE.nonEmpty [ m | (m, Draw) <- NE.toList choices ] of
          Just draws -> randomPick draws
          Nothing -> randomPick moves

valuePos :: Depth -> Pos -> Outcome
valuePos d pos =
  case status pos of
    Complete outcome -> outcome
    Continue moves ->
      if d==0 then Draw else
        maximum [ valueMove d m | m <- NE.toList moves ]

valueMove :: Depth -> Move -> Outcome
valueMove d m = invertOutcome (valuePos (d-1) (apply m))

invertOutcome :: Outcome -> Outcome
invertOutcome = \case Win -> Loss; Loss -> Win; Draw -> Draw


-- | perfect strategy; based on mathmatical structure of Nim
perfect :: Strategy
perfect = Strategy $ \moves ->
  case NE.nonEmpty (filter NimRules.desirable (NE.toList moves)) of
    Just good -> randomPick good
    Nothing -> randomPick moves


-- | pure random strategy
dumb :: Strategy
dumb = Strategy $ randomPick



newtype Rand a = Rand (IO a) deriving (Functor,Applicative,Monad)

runRand :: Rand a -> IO a
runRand (Rand io) = io

randomPick :: NonEmpty a -> Rand a
randomPick ne = Rand $ do
  let xs = NE.toList ne
  i <- getStdRandom (randomR (0,length xs - 1))
  return (xs !! i)
