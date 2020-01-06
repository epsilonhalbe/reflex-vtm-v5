{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Frontend.Home (home) where

import Common.Route
import Control.Monad (forM_)
import Data.Universe (universe)
import Data.Text (pack)
import Obelisk.Route
import Obelisk.Route.Frontend
import Reflex.Dom

home
  :: forall m t
  .( DomBuilder t m
   , SetRoute t (R FrontendRoute) m
   , RouteToUrl (R FrontendRoute) m
   )
  => m ()
home = do
  examplesList

examplesList
  :: forall m t
  .( DomBuilder t m
   , SetRoute t (R FrontendRoute) m
   , RouteToUrl (R FrontendRoute) m
   )
  => m ()
examplesList = el "ul" $ do
  -- Iterate over all the top-level routes
  forM_ universe $ \section -> el "li" $ do
    el "h4" $ routeLink (ExampleR :/ (Just $ sectionHomepage section)) $
      text $ pack $ show section
