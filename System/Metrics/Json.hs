{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module defines an encoding of ekg metrics as JSON. This
-- encoding is used by the ekg web UI. This encoding is also
-- standardized so that other web servers and frameworks can also expose
-- ekg metrics.
module System.Metrics.Json
    ( -- * Converting metrics to JSON values
      sampleToJson
    , valueToJson

      -- ** Newtype wrappers with instances
    , Sample(..)
    , Value(..)
    ) where

import Data.Aeson ((.=))
import qualified Data.Aeson.Types as A
import qualified Data.HashMap.Strict as M
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified System.Metrics as Metrics
import qualified System.Metrics.Distribution as Distribution

------------------------------------------------------------------------
-- * Converting metrics to JSON values


-- | Encode metrics as nested JSON objects. Each "." in the metric name
-- introduces a new level of nesting.
--
-- For example, the metric set
--
-- > ("foo.bar", {}, "label")
-- > ("foo.baz", {"tag","a"}, 10)
-- > ("foo.baz", {"tag","b"}, 11)
--
-- is encoded as
--
-- > {
-- >   "foo": {
-- >     "bar": [
-- >       { "tags": {},
-- >         "value": {
-- >           "type": "l",
-- >           "val": "label"
-- >         }
-- >       }
-- >     ],
-- >     "baz": [
-- >       { "tags": {
-- >           "tag": "a"
-- >         },
-- >         "value": {
-- >           "type": "c",
-- >           "val": 10
-- >         }
-- >       },
-- >       { "tags": {
-- >           "tag": "b"
-- >         },
-- >         "value": {
-- >           "type": "c",
-- >           "val": 11
-- >         }
-- >       }
-- >     ]
-- >   }
-- > }
--
sampleToJson :: Metrics.Sample -> A.Value
sampleToJson metrics =
    M.foldlWithKey' build A.emptyObject (groupMetrics metrics)
  where
    -- Group a set of metrics by metric name.
    groupMetrics
      :: M.HashMap Metrics.Identifier Metrics.Value
      -> M.HashMap T.Text [(M.HashMap T.Text T.Text, Metrics.Value)]
    groupMetrics = M.foldlWithKey' f M.empty
      where
        f m (Metrics.Identifier name tags) value =
            let !x = (tags, value)
            -- Info: If inserting at an existing key,
            -- `Data.HashMap.Strict.insertWith f key value` calls
            -- `f value existingValue`.
            in  M.insertWith (++) name [x] m

    build
      :: A.Value
      -> T.Text
      -> [(M.HashMap T.Text T.Text, Metrics.Value)]
      -> A.Value
    build json name values = go json (T.splitOn "." name)
      where
        valuesArray :: A.Value
        valuesArray = A.Array $ V.fromList $ map taggedValueToJson values

        taggedValueToJson
          :: (M.HashMap T.Text T.Text, Metrics.Value) -> A.Value
        taggedValueToJson (tags, value) = A.object
            [ "tags" .= tags
            , "value" .= valueToJson value
            ]

        go :: A.Value -> [T.Text] -> A.Value
        go (A.Object m) (str:rest) = A.Object $ M.insert str goRest m
          where
            goRest = case rest of
                [] -> valuesArray
                (_:_) -> go (fromMaybe A.emptyObject $ M.lookup str m) rest
        go v _ = typeMismatch "Object" v

typeMismatch :: String   -- ^ The expected type
             -> A.Value  -- ^ The actual value encountered
             -> a
typeMismatch expected actual =
    error $ "when expecting a " ++ expected ++ ", encountered " ++ name ++
    " instead"
  where
    name = case actual of
        A.Object _ -> "Object"
        A.Array _  -> "Array"
        A.String _ -> "String"
        A.Number _ -> "Number"
        A.Bool _   -> "Boolean"
        A.Null     -> "Null"

-- | Encodes a single metric as a JSON object. Example:
--
-- > {
-- >   "type": "c",
-- >   "val": 89460
-- > }
--
valueToJson :: Metrics.Value -> A.Value
valueToJson (Metrics.Counter n)      = scalarToJson n CounterType
valueToJson (Metrics.Gauge n)        = scalarToJson n GaugeType
valueToJson (Metrics.Label l)        = scalarToJson l LabelType
valueToJson (Metrics.Distribution l) = distrubtionToJson l

-- | Convert a scalar metric (i.e. counter, gauge, or label) to a JSON
-- value.
scalarToJson :: A.ToJSON a => a -> MetricType -> A.Value
scalarToJson val ty = A.object
    ["val" .= val, "type" .= metricType ty]
{-# SPECIALIZE scalarToJson :: Int64 -> MetricType -> A.Value #-}
{-# SPECIALIZE scalarToJson :: T.Text -> MetricType -> A.Value #-}

data MetricType =
      CounterType
    | GaugeType
    | LabelType
    | DistributionType

metricType :: MetricType -> T.Text
metricType CounterType      = "c"
metricType GaugeType        = "g"
metricType LabelType        = "l"
metricType DistributionType = "d"

-- | Convert a distribution to a JSON value.
distrubtionToJson :: Distribution.Stats -> A.Value
distrubtionToJson stats = A.object
    [ "mean" .= Distribution.mean stats
    , "variance" .= Distribution.variance stats
    , "count" .= Distribution.count stats
    , "sum" .= Distribution.sum stats
    , "min" .= Distribution.min stats
    , "max" .= Distribution.max stats
    , "type" .= metricType DistributionType
    ]

------------------------------------------------------------------------
-- ** Newtype wrappers with instances

-- | Newtype wrapper that provides a 'A.ToJSON' instances for the
-- underlying 'Metrics.Sample' without creating an orphan instance.
newtype Sample = Sample Metrics.Sample
    deriving Show

-- | Uses 'sampleToJson'.
instance A.ToJSON Sample where
    toJSON (Sample s) = sampleToJson s

-- | Newtype wrapper that provides a 'A.ToJSON' instances for the
-- underlying 'Metrics.Value' without creating an orphan instance.
newtype Value = Value Metrics.Value
    deriving Show

-- | Uses 'valueToJson'.
instance A.ToJSON Value where
    toJSON (Value v) = valueToJson v
