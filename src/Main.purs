module Main where

import Prelude
import Data.Generic
import Data.Tuple
import Data.Maybe
import Data.Foldable
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)


data Conv u = CUnit u | Factor Number

class Eq u <= Quantity u where
    formula :: Array (Conv u)


-- Length


data Length =
    NauticalMile | Mile | Yard | Foot | Inch | CentiMeter | Meter | KiloMeter | MilliMeter | MicroMeter | NanoMeter

derive instance genericLength :: Generic Length

instance eqLength :: Eq Length where
    eq = gEq

instance lengthQuantity :: Quantity Length where
    formula = lengthF

lengthF :: Array (Conv Length)
lengthF =
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
    , CUnit NanoMeter ]


-- Mass


data Mass =
    Tonne | ImperialTon | USTon | Stone | Pound | Ounce | Kilogram | Gram | Milligram | Microgram

derive instance genericMass :: Generic Mass

instance eqMass :: Eq Mass where
    eq = gEq

instance massQuantity :: Quantity Mass where
    formula = massF

massF :: Array (Conv Mass)
massF =
    [ CUnit Tonne       , Factor 0.984207
    , CUnit ImperialTon , Factor 1.12
    , CUnit USTon       , Factor 142.857
    , CUnit Stone       , Factor 14.0
    , CUnit Pound       , Factor 16.0
    , CUnit Ounce       , Factor 0.0283495
    , CUnit Kilogram    , Factor 1000.0
    , CUnit Gram        , Factor 1000.0
    , CUnit Milligram   , Factor 1000.0
    , CUnit Microgram ]


-- Plane of Angle

data PlaneOfAngle =
    Degree | Gradian | Milliradian | MinuteOfArc | Radian | SecondOfArc

derive instance genericPlaneOfAngle :: Generic PlaneOfAngle

instance eqPlaneOfAngle :: Eq PlaneOfAngle where
    eq = gEq

instance planeOfAngleQuantity :: Quantity PlaneOfAngle where
    formula = planeOfAngleF

planeOfAngleF :: Array (Conv PlaneOfAngle)
planeOfAngleF =
    [ CUnit Degree      , Factor 1.11111
    , CUnit Gradian     , Factor 15.708
    , CUnit Milliradian , Factor 3.43775
    , CUnit MinuteOfArc , Factor 0.000290888
    , CUnit Radian      , Factor 206265.0
    , CUnit SecondOfArc ]



convert :: forall u. Quantity u => u -> u -> Number -> Number
convert from to val =
    val * adjustedCombinedFactor
    where
        (Tuple (Tuple lUnit rUnit) factors) = factorsBetween from to
        combinedFactor = foldl (*) 1.0 factors
        adjustedCombinedFactor =    if (Tuple from to) == (Tuple lUnit rUnit) then combinedFactor
                                    else if (Tuple to from) == (Tuple lUnit rUnit) then 1.0 / combinedFactor
                                    else unsafeThrow "Both tuples must contain same units."

factorsBetween :: forall u. Quantity u => u -> u -> (Tuple (Tuple u u) (Array Number))
factorsBetween from to =
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
            (Tuple resUnitMaybes resFactors) = foldl f (Tuple (Tuple Nothing Nothing) []) formula
        in  case resUnitMaybes of
                (Tuple (Just l) (Just r)) -> (Tuple (Tuple l r) resFactors)
                _   -> unsafeThrow "Both units must be Just's"


main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"
