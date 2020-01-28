{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RecordWildCards #-}
module Frontend.App.CharacterBuilder where

import Common.Types
import Control.Monad (void)
import Control.Monad.Fix (MonadFix)
import Data.Proxy
import Data.Map (Map)
import Data.Functor (($>))
import Data.Text (Text, pack)
import Obelisk.Generated.Static
import Reflex.Dom.Core

import Frontend.Widget.Dropdown
import Frontend.Widget.Range
import Frontend.Widget.SimpleTextInput
import LocalStorage as LS

app
  :: forall js m t.
  ( DomBuilder t m
  -- , MonadFix m
  -- , MonadHold t m
  -- , PostBuild t m
  , Prerender js t m
  ) => m ()
app = void $ prerender blank $ elClass "div" "app" do
  elAttr "img" [("src", static @"logo.png")
                ,("alt", "vampire the masquerade logo")
                ,("class", "logo")
                ] blank

  ((_name,_concept,_chronicle),
   (_ambition, _desire, _sire),
   (_clan, _generation, _predator)) <-
    elClass "div" "mui-container-fluid" do
      el "h1" $ text "Details"
      elClass "div" "mui-row" $ (,,)
        <$> ( elClass "div" "mui-col-md-4" $ (,,)
              <$> inputWidget "Name"
              <*> inputWidget "Concept"
              <*> inputWidget "Chronicle"
            )
        <*> ( elClass "div" "mui-col-md-4" $ (,,)
              <$> inputWidget "Ambition"
              <*> inputWidget "Desire"
              <*> inputWidget "Sire"
            )
        <*> ( elClass "div" "mui-col-md-4" $ (,,)
              <$> dropdownWidget (Proxy @Clan)
              <*> dropdownWidget (Proxy @Generation)
              <*> dropdownWidget (Proxy @Predator)
            )

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
  storage <- getLocalStorage
  -- let char = Character <$> _details <*> _attributes
  _delete :: () <- do
    (e, _) <- elClass' "div" "mui-btn" $ text "remove"
    performEvent_ (domEvent Click e $> (removeItem storage "blip"))
  _store :: () <- do
    (e, _) <- elClass' "div" "mui-btn" $ text "save"
    performEvent_ (domEvent Click e $> (setItem storage "blip" ([("blap", "blup")] :: Map Text Text)))
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
