{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

-- import Common.Api
import Common.Route
import Common.Types
import Control.Monad.Fix (MonadFix)
import Data.Proxy
import Data.Text (Text, pack)
import Frontend.Dropdown
import Frontend.Range
-- import Obelisk.Configs
import Obelisk.Frontend
import Obelisk.Generated.Static
import Obelisk.Route
import Reflex.Dom.Core


frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" (text "Vampire the Masquerade: V5 - Character Sheet")
      styleSheet (static @"mui-0.10.0/css/mui.css")
      styleSheet (static @"fontawesome-free-5.12.0-web/css/all.css")
      styleSheet (static @"custom.css")

  , _frontend_body = bodyBod
  }
 where
    styleSheet href =
      elAttr "link" [("href", href)
                    ,("type","text/css")
                    ,("rel","stylesheet")
                    ] blank

bodyBod
  :: forall t m.
  ( DomBuilder t m
  , PostBuild t m
  , MonadHold t m
  , MonadFix m
  ) => m ()
bodyBod = do
  elAttr "img" [("src", static @"logo.png")
                ,("alt", "vampire the masquerade logo")
                ,("class", "logo")
                ] blank
  elClass "div" "mui-container-fluid" do
    el "h1" $ text "Details"
    elClass "div" "mui-row" do
      elClass "div" "mui-col-md-4" do
        _name <- inputWidget "Name"
        _concept <- inputWidget "Concept"
        _chronicle <- inputWidget "Chronicle"
        blank

      elClass "div" "mui-col-md-4" do
        _ambition <- inputWidget "Ambition"
        _desire <- inputWidget "Desire"
        _sire <- inputWidget "Sire"
        blank

      elClass "div" "mui-col-md-4" do
        _clan <- dropdownWidget (Proxy @Clan)
        _generation <- dropdownWidget (Proxy @Generation)
        _predator <- dropdownWidget_v1 (Proxy @Predator)
        blank
    blank
  elClass "div" "mui-container-fluid" do
    elClass "div" "mui-row" do
      elClass "div" "mui-col-md-4" do
        el "h2" $ text "Physical"
        _strength <- dotWidget "Strength"
        _dexterity <- dotWidget "Dexterity"
        _stamina <- dotWidget "Stamina"
        blank
      elClass "div" "mui-col-md-4" do
        el "h2" $ text "Social"
        _charisma <- dotWidget "Charisma"
        _manipulation <- dotWidget "Manipulation"
        _composure <- dotWidget "Composure"
        blank
      elClass "div" "mui-col-md-4" do
        el "h2" $ text "Mental"
        _intelligence <- dotWidget "Intelligence"
        _wits <- dotWidget "Wits"
        _resolve <- dotWidget "Resolve"
        blank
  blank

inputWidget
  :: forall t m
  .  DomBuilder t m
  => Text
  -> m (Dynamic t Text)
inputWidget name
  = fmap value
  $ elClass "div" "mui-textfield"
  $ inputElement
  $ def & inputElementConfig_elementConfig
        . elementConfig_initialAttributes .~ [("placeholder", name)]

dotWidget
  :: forall t m
  .  ( DomBuilder t m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     )
  => Text
  -> m (Dynamic t Word)
dotWidget name = el "mui-row" mdo
  elClass "span" "mui-col-md-6 mui--text-headline" $ dynText $ (\n -> name <> " (" <> pack (show n) <> "):") <$> val
  val <- elClass "span" "mui-col-md-6  mui--text-headline mui--text-right" $ rangeWidget 5
  pure val
