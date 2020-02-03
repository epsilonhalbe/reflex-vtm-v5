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
    routeLink (HomeR :/ ()) do
      elClass "span" "mui--appbar-height mui--text-dark" $ text "Home"
      elClass "div" "mui--appbar-height" do
        elAttr "img" [ ("class", "mui--appbar-height")
                      , ("src", static @"MalkavianV5.webp")
                      , ("alt", "Home")
                      ] blank
