{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RecordWildCards #-}
module Frontend.App.CharacterBuilder where

import Common.Types
import Control.Monad (void)
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
  _details <- elClass "div" "mui-container-fluid" do
    el "h1" $ text "Details"
    elClass "div" "mui-row" do
      (_name, _concept, _chronicle) <-
        elClass "div" "mui-col-md-4" $ (,,)
          <$> inputWidget "Name"
          <*> inputWidget "Concept"
          <*> inputWidget "Chronicle"

      (_ambition, _desire, _sire) <-
        elClass "div" "mui-col-md-4" $ (,,)
          <$> inputWidget "Ambition"
          <*> inputWidget "Desire"
          <*> inputWidget "Sire"

      (_clan, _generation, _predator) <-
        elClass "div" "mui-col-md-4" $ (,,)
          <$> dropdownWidget (Proxy @Clan)
          <*> dropdownWidget (Proxy @Generation)
          <*> dropdownWidget (Proxy @Predator)

      pure $ Details
        <$> _name
        <*> _concept
        <*> _chronicle
        <*> _ambition
        <*> _desire
        <*> _sire
        <*> _clan
        <*> _generation
        <*> _predator
  _attributes <- elClass "div" "mui-container-fluid" do
    elClass "div" "mui-row" do
      (_strength, _dexterity, _stamina) <-
        elClass "div" "mui-col-md-4" do
          el "h2" $ text "Physical"
          (,,)
            <$> dotWidget "Strength"
            <*> dotWidget "Dexterity"
            <*> dotWidget "Stamina"
      (_charisma, _manipulation, _composure) <-
        elClass "div" "mui-col-md-4" do
          el "h2" $ text "Social"
          (,,)
            <$> dotWidget "Charisma"
            <*> dotWidget "Manipulation"
            <*> dotWidget "Composure"
      (_intelligence, _wits, _resolve) <-
        elClass "div" "mui-col-md-4" do
          el "h2" $ text "Mental"
          (,,)
            <$> dotWidget "Intelligence"
            <*> dotWidget "Wits"
            <*> dotWidget "Resolve"
      pure $ Attributes
          <$> _strength
          <*> _dexterity
          <*> _stamina
          <*> _charisma
          <*> _manipulation
          <*> _composure
          <*> _intelligence
          <*> _wits
          <*> _resolve
  (e, _) <- elClass' "div" "mui-btn" $ text "save"
  let char = Character <$> _details <*> _attributes
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
