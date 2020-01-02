{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import Control.Monad.Fix (MonadFix)
import Data.Functor (($>), (<&>))
import Data.Text (Text, pack)
-- import qualified Data.Text.Encoding as Tx
import Obelisk.Frontend
-- import Obelisk.Configs
import Obelisk.Route
import Reflex.Dom.Core
import Data.Proxy

-- import Common.Api
import Common.Route
import Common.Types
import Obelisk.Generated.Static

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" (text "Vampire the Masquerade: V5 - Character Sheet")
      elAttr "link" [("href", static @"mui-0.10.0/css/mui.css")
                    ,("type","text/css")
                    ,("rel","stylesheet")
                    ] blank
      elAttr "link" [("href", static @"custom.css")
                    ,("type","text/css")
                    ,("rel","stylesheet")
                    ] blank
  , _frontend_body = bodyBod
  }

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
  el "h1" $ text "Details"
  elAttr "div" [("class", "details")] do
    name <- inputWidget "Name"
    _player <- inputWidget "Player"
    _chronicle <- inputWidget "Chronicle"
    _concept <- inputWidget "Concept"
    _ambition <- inputWidget "Ambition"
    _predator <- dropdownWidget (Proxy @Predator)
    dynText name
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
        . elementConfig_initialAttributes .~ ("placeholder" =: name)

dropdownWidget
  :: forall t m a
  . ( DomBuilder t m
    , MonadHold t m
    , PostBuild t m
    , MonadFix m
    , Bounded a
    , Show a
    , Enum a
    )
  => Proxy a -> m (Dynamic t (Maybe a))
dropdownWidget _ =
  elClass "div" "mui-dropdown" $ mdo
    ddClick <- dropdownButton selected
    open <- clicked $ leftmost [ddClick $> Nothing, updated selected]
    selected <-
       elDynClass "ul" open $ holdDyn Nothing =<< selectItems
    pure selected

  where
    selectItems :: m (Event t (Maybe a))
    selectItems = fmap Just . leftmost <$> traverse li ([minBound .. maxBound] :: [a])

    clicked :: Event t b -> m (Dynamic t Text)
    clicked click = do
      opn <- (foldDyn (const not) False click)
      pure $ opn
       <&> ("mui-dropdown__menu" <>)
        . \case True  -> " mui--is-open"
                False -> ""

    dropdownButton :: Dynamic t (Maybe a) -> m (Event t ())
    dropdownButton mCurrentElement = do
      (e,_) <- elAttr' "button"
        [("class", "mui-btn mui-btn--primary")
        ,("data-mui-toggle", "dropdown")
        ] $ dynText $ maybe "(select)" (pack . show) <$> mCurrentElement
      pure $ domEvent Click e

    li :: a -> m (Event t a)
    li x =
      el "li" do
        (e, _) <- elAttr' "a" ("href" =: "#") . text . pack $ show x
        pure $ domEvent Click e $> x




