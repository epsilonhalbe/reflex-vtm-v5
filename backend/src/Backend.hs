{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Backend where

import Common.Route
import Obelisk.Backend

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = backend_run
  , _backend_routeEncoder = fullRouteEncoder
  }

backend_run
  :: Applicative m
  => ((b -> m ()) -> t) -> t
backend_run serve = serve $ const $ pure  ()
