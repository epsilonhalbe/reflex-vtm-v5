{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Backend where

import Common.Route
import Obelisk.Backend
import Data.Dependent.Sum (DSum (..))
import Data.Functor.Identity

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> serve \case
      BackendRoute_Missing :=> Identity () -> pure ()
  , _backend_routeEncoder = routeEncoder
  }
