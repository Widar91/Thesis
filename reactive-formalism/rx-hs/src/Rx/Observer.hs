module Rx.Observer where

import Rx.Types

import Control.Exception
import Data.Functor.Contravariant


instance Contravariant Observer where
    contramap f obr = Observer (onNext obr . f)
                               (onError obr)
                               (onCompleted obr)
                               (subscription obr)


