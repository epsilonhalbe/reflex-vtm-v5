{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Frontend where

import Common.Route as Route
import Control.Monad.Fix (MonadFix)
import Obelisk.Configs as Cfg
import Obelisk.Frontend
import Obelisk.Generated.Static
import Obelisk.Route.Frontend (R, RoutedT, subRoute_, maybeRoute_, askRoute
                              )
import Reflex.Dom.Core

import Frontend.Home (home)
import Frontend.Nav (nav)
import Frontend.App.CharacterBuilder as CharacterBuilder (app)

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" (text "Vampire the Masquerade: V5 - Character Sheet")
      styleSheet $ static @"mui-0.10.0/css/mui.css"
      styleSheet $ static @"fontawesome-free-5.12.0-web/css/all.css"
      styleSheet $ static @"custom.css"
  , _frontend_body = frontend_body
  }
 where
    styleSheet href =
      elAttr "link"
        [ ("href", href)
        , ("type","text/css")
        , ("rel","stylesheet")
        ] blank


frontend_body
  :: ( ObeliskWidget js t (R FrontendRoute) m
     )
    => RoutedT t (R FrontendRoute) m ()
frontend_body = do
  r <- getConfig "config/common"
  el "header" $ nav
  el "main" $ subRoute_ \case
    HomeR -> home
    ExampleR -> maybeRoute_ home
      (examples r =<< askRoute)
  blank


examples
  :: forall js m t str.
     ( DomBuilder t m
     , MonadFix m
     , MonadHold t m
     , Prerender js t m
     )
  => Maybe str
  -> Dynamic t (R Example)
  -> RoutedT t (R Example) m ()
examples _route _ = subRoute_ $ \case
  Route.CharacterBuilder -> CharacterBuilder.app
