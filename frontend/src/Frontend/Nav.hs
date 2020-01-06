{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Frontend.Nav (nav) where

import Common.Route
import Obelisk.Route.Frontend
import Obelisk.Generated.Static
import Reflex.Dom

nav
  :: forall t m
  . ( DomBuilder t m
    , SetRoute t (R FrontendRoute) m
    , RouteToUrl (R FrontendRoute) m
    )
  => m ()
nav = do
  elClass "div" "mui-appbar" do
    elAttr "table" [("width", "100%")] do
      elClass "tr" "middle" do
        routeLink (HomeR :/ ()) do
          elAttr "img" [ ("class", "mui--appbar-height")
                      , ("src", static @"habito.svg")
                      , ("alt", "Habito")
                      ] blank


