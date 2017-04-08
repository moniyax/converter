module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Generic
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import Data.Tuple
import Data.Maybe
import Data.Foldable

data CUnit =
    NauticalMile | Mile | Yard | Foot | Inch | CentiMeter | Meter | KiloMeter | MilliMeter | MicroMeter | NanoMeter

derive instance genericCUnit :: Generic CUnit

instance showCUnit :: Show CUnit where
    show = gShow

instance eqCUnit :: Eq CUnit where
    eq = gEq

data Conv = CUnit CUnit | Factor Number

type Qty = Array Conv

lengthQ :: Qty
lengthQ =
    [ CUnit NauticalMile  , Factor 1.15078
    , CUnit Mile          , Factor 1760.0
    , CUnit Yard          , Factor 3.0
    , CUnit Foot          , Factor 12.0
    , CUnit Inch          , Factor 2.54
    , CUnit CentiMeter    , Factor 0.01
    , CUnit Meter         , Factor 0.001
    , CUnit KiloMeter     , Factor 1000000.0
    , CUnit MilliMeter    , Factor 1000.0
    , CUnit MicroMeter    , Factor 1000.0
    , CUnit NanoMeter]

factorsBetween :: CUnit -> CUnit -> Qty -> (Tuple (Tuple CUnit CUnit) (Array Number))
factorsBetween from to qty =
    if from == to then
        (Tuple (Tuple from to) [1.0])
    else
        let f acc val  =
                case acc of
                    (Tuple pair factors) ->
                        case pair of
                            (Tuple Nothing Nothing) ->
                                case val of
                                    -- Use a catchall pattern and guard clause on first branch
                                    CUnit unit ->
                                        if unit == from || unit == to then
                                            (Tuple (Tuple (Just unit) Nothing) factors)
                                        else acc
                                    Factor factor -> acc
                            (Tuple l@(Just _) Nothing) ->
                                case val of
                                    CUnit unit ->
                                        if unit == from || unit == to then
                                            (Tuple (Tuple l (Just unit)) factors)
                                        else acc
                                    Factor factor -> (Tuple pair (factors <> [factor]))
                            (Tuple (Just _) (Just _)) -> acc
                            (Tuple Nothing (Just _)) -> unsafeThrow "Just _ can't come after Nothing"
            (Tuple resUnitMaybes resFactors) = foldl f (Tuple (Tuple Nothing Nothing) []) qty
        in  case resUnitMaybes of
                (Tuple (Just l) (Just r)) -> (Tuple (Tuple l r) resFactors)
                _   -> unsafeThrow "Both units must be Just's"

convert :: CUnit -> CUnit -> Qty -> Number -> Number
convert from to qty val =
    let (Tuple (Tuple lUnit rUnit) factors) = factorsBetween from to qty
        combinedFactor = foldl (*) 1.0 factors
        adjustedCombinedFactor =    if (Tuple from to) == (Tuple lUnit rUnit) then combinedFactor
                                    else if (Tuple to from) == (Tuple lUnit rUnit) then 1.0 / combinedFactor
                                    else unsafeThrow "Both tuples must contain same units."
    in  val * adjustedCombinedFactor






main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"
