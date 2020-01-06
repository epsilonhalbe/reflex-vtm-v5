{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecursiveDo #-}
module Frontend.App.CharacterBuilder where

import Common.Types
import Control.Monad.Fix (MonadFix)
import Data.Proxy
import Data.Text (Text, pack)
import Obelisk.Generated.Static
import Reflex.Dom.Core

import Frontend.Widget.Dropdown
import Frontend.Widget.Range
import Frontend.Widget.SimpleTextInput

app
  :: forall t m.
  ( DomBuilder t m
  , PostBuild t m
  , MonadHold t m
  , MonadFix m
  ) => m ()
app = elClass "div" "app" do
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
        _predator <- dropdownWidget (Proxy @Predator)
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
  elClass "span" "mui-col-md-6 mui--text-headline" $
    dynText $ (\n -> name <> " (" <> pack (show n) <> "):") <$> val
  val <- elClass "span" "mui-col-md-6  mui--text-headline mui--text-right" $
            rangeWidget 5
  pure val
