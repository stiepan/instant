module Jvm.OptimizeAbs where

import qualified Data.Map.Strict as Map
import Control.Monad.Trans.State
import Data.Functor.Identity
import AbsGrammar

data HExp
    = HExpAdd {hashE :: Integer, expl :: HExp, expr :: HExp}
    | HExpSub {hashE :: Integer, expl :: HExp, expr :: HExp}
    | HExpMul {hashE :: Integer, expl :: HExp, expr :: HExp}
    | HExpDiv {hashE :: Integer, expl :: HExp, expr :: HExp}
    | HExpLit {hashE :: Integer, explit :: Integer}
    | HExpVar {hashE :: Integer, expvar :: Ident}
  deriving (Eq, Show)

type Enumerator = StateT Integer Identity

type SwapMap = Map.Map Integer (Map.Map Integer Integer)

type SwapsCache = StateT SwapMap Identity


infinity :: Integer
infinity = 2^30


tick :: Enumerator Integer
tick = do
  i <- get
  put (i + 1)
  return i

enumE :: Exp -> Enumerator HExp
enumE (ExpAdd e1 e2) = enumEBin HExpAdd e1 e2

enumE (ExpMul e1 e2) = enumEBin HExpMul e1 e2

enumE (ExpDiv e1 e2) = enumEBin HExpDiv e1 e2

enumE (ExpSub e1 e2) = enumEBin HExpSub e1 e2

enumE (ExpVar var) = do
  i <- tick
  return $ HExpVar i var

enumE (ExpLit n) = do
   i <- tick
   return $ HExpLit i n


enumEBin :: (Integer -> HExp -> HExp -> HExp) -> Exp -> Exp -> Enumerator HExp
enumEBin c el er = do
  hel <- enumE el
  i <- tick
  her <- enumE er
  return $ c i hel her


enumedE :: Exp -> HExp
enumedE e = evalState (enumE e) 0


swapsOptimized :: Integer -> Exp -> (Integer, SwapMap, HExp)
swapsOptimized h e = (swapsCount, swapsMap, he)
  where
  (swapsCount, swapsMap) = runState optimization Map.empty
  he = enumedE e
  optimization :: SwapsCache Integer
  optimization = do
    prepareState he
    optimizeSwaps h he


optimizeSwaps :: Integer -> HExp -> SwapsCache Integer
optimizeSwaps h e = do
  m <- get
  let he = hashE e
  case Map.lookup h (m Map.! he) of
    Just n -> return n
    Nothing -> do -- no value cached, it needs to be actually computed
      n <- optimizeSwapsS h e
      m <- get
      let arr = Map.insert h n (m Map.! he)
      put $ Map.insert he arr m
      return n


optimizeSwapsS :: Integer -> HExp -> SwapsCache Integer
optimizeSwapsS h (HExpLit _ _) = optimizeSwapsLeaf h

optimizeSwapsS h (HExpVar _ _) = optimizeSwapsLeaf h

optimizeSwapsS h (HExpAdd _ el er) = optimizeSwapsBin h el er 0

optimizeSwapsS h (HExpSub _ el er) = optimizeSwapsBin h el er 1

optimizeSwapsS h (HExpMul _ el er) = optimizeSwapsBin h el er 0

optimizeSwapsS h (HExpDiv _ el er) = optimizeSwapsBin h el er 1


optimizeSwapsLeaf :: Integer -> SwapsCache Integer
optimizeSwapsLeaf h = return $ if h > 0 then 0 else infinity


optimizeSwapsBin :: Integer -> HExp -> HExp -> Integer -> SwapsCache Integer
optimizeSwapsBin h el er swapPenalty =
  if h < 1 then
    return infinity
  else do
    leftFirstR <- optimizeSwaps (h - 1) er
    rightFirstL <- optimizeSwaps (h - 1) el
    leftFirstL <- optimizeSwaps h el
    rightFirstR <- optimizeSwaps h er
    let leftFirst = leftFirstL + leftFirstR
    let rightFirst = rightFirstL + rightFirstR + swapPenalty
    return $ min leftFirst rightFirst


prepareState :: HExp -> SwapsCache ()
prepareState e@(HExpLit _ _) = insertExprToState e

prepareState e@(HExpVar _ _) = insertExprToState e

prepareState e@(HExpAdd _ el er) = prepareStateBinExpr e el er

prepareState e@(HExpSub _ el er) = prepareStateBinExpr e el er

prepareState e@(HExpMul _ el er) = prepareStateBinExpr e el er

prepareState e@(HExpDiv _ el er) = prepareStateBinExpr e el er


prepareStateBinExpr :: HExp -> HExp -> HExp -> SwapsCache ()
prepareStateBinExpr e el er = do
  insertExprToState e
  prepareState el
  prepareState er


insertExprToState :: HExp -> SwapsCache ()
insertExprToState e = do
  m <- get
  put $ Map.insert (hashE e) Map.empty m


minStackE:: Exp -> Integer
minStackE (ExpLit _) = 1

minStackE (ExpVar _) = 1

minStackE (ExpAdd e1 e2) = minBinExp ExpAdd e1 e2

minStackE (ExpMul e1 e2) = minBinExp ExpMul e1 e2

minStackE (ExpDiv e1 e2) = minBinExp ExpDiv e1 e2

minStackE (ExpSub e1 e2) = minBinExp ExpSub e1 e2


minBinExp :: (Exp -> Exp -> Exp) -> Exp -> Exp -> Integer
minBinExp c le re = (max lExpStack rExpStack) + stackUp
  where
  lExpStack = minStackE le
  rExpStack = minStackE re
  stackUp = if lExpStack == rExpStack then 1 else 0


minStack :: [Exp] -> Integer
minStack [] = 0
minStack es = foldr1 max (map minStackE es)
