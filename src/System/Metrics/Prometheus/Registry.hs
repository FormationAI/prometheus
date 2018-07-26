{-# LANGUAGE DeriveDataTypeable #-}

module System.Metrics.Prometheus.Registry
       ( Registry
       , RegistrySample (..)
       , new
       , registerCounter
       , registerGauge
       , registerHistogram
       , sample
       ) where

import           Control.Exception                          (Exception, throw)
import           Data.Map                                   (Map)
import qualified Data.Map                                   as Map
import           Data.Typeable                              (Typeable)

import           System.Metrics.Prometheus.Metric           (Metric (..),
                                                             MetricSample (..))
import           System.Metrics.Prometheus.Metric.Counter   (Counter)
import qualified System.Metrics.Prometheus.Metric.Counter   as Counter
import           System.Metrics.Prometheus.Metric.Gauge     (Gauge)
import qualified System.Metrics.Prometheus.Metric.Gauge     as Gauge
import           System.Metrics.Prometheus.Metric.Histogram (Histogram,
                                                             UpperBound)
import qualified System.Metrics.Prometheus.Metric.Histogram as Histogram
import           System.Metrics.Prometheus.MetricId         (Labels (..),
                                                             MetricId (MetricId),
                                                             Name (..))

newtype Registry = Registry { unRegistry :: Map MetricId Metric }
newtype RegistrySample = RegistrySample { unRegistrySample :: Map MetricId MetricSample }

newtype KeyError
    = TypeMismatch MetricId deriving (Show, Typeable)
instance Exception KeyError


new :: Registry
new = Registry Map.empty


registerCounter :: Name -> Labels -> Registry -> IO (Counter, Registry)
registerCounter name labels registry =
    case Map.lookup mid (unRegistry registry) of
        Nothing -> do
            counter <- Counter.new
            return (counter, Registry $ Map.insert mid (CounterMetric counter) (unRegistry registry))
        Just (CounterMetric x) -> return (x, registry)
        _ -> throw $ TypeMismatch mid
  where
      mid = MetricId name labels


registerGauge :: Name -> Labels -> Registry -> IO (Gauge, Registry)
registerGauge name labels registry =
    case Map.lookup mid (unRegistry registry) of
        Nothing -> do
            gauge <- Gauge.new
            return (gauge, Registry $ Map.insert mid (GaugeMetric gauge) (unRegistry registry))
        Just (GaugeMetric x) -> return (x, registry)
        _ -> throw $ TypeMismatch mid
  where
      mid = MetricId name labels


registerHistogram :: Name -> Labels -> [UpperBound] -> Registry -> IO (Histogram, Registry)
registerHistogram name labels buckets registry =
    case Map.lookup mid (unRegistry registry) of
        Nothing -> do
            histogram <- Histogram.new buckets
            return (histogram, Registry $ Map.insert mid (HistogramMetric histogram) (unRegistry registry))
        Just (HistogramMetric x) -> return (x, registry)
        _ -> throw $ TypeMismatch mid
  where
      mid = MetricId name labels


sample :: Registry -> IO RegistrySample
sample = fmap RegistrySample . mapM sampleMetric . unRegistry
  where
    sampleMetric :: Metric -> IO MetricSample
    sampleMetric (CounterMetric count) = CounterMetricSample <$> Counter.sample count
    sampleMetric (GaugeMetric gauge) = GaugeMetricSample <$> Gauge.sample gauge
    sampleMetric (HistogramMetric histogram) = HistogramMetricSample <$> Histogram.sample histogram
