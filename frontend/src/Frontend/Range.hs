{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Frontend.Range where

import Control.Monad.Fix (MonadFix)
import Data.Functor (($>), (<&>))
import Data.Text (Text)
import Data.Bool (bool)
import Reflex.Dom.Core

-- import Common.Api

rangeWidget
  :: forall t m
  .  ( DomBuilder t m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     )
  => Text -> Word
  -> m (Dynamic t Word)
rangeWidget faIcon maxdots = mdo
  selected <- holdDyn 0 . leftmost
          =<< traverse (item selected faIcon) ([1 .. maxdots] :: [Word])
  pure selected

item
  :: forall t m
  .  ( DomBuilder t m
     , PostBuild t m
     )
  => Dynamic t Word -> Text -> Word -> m (Event t Word)
item dyN faIcon n = do
  let filled = dyN <&> \n' -> bool "far" "fas" (n <= n') <> " " <> faIcon
  (e,_) <- elDynClass' "i" filled blank
  pure $ (tag (current dyN) (domEvent Click e)) <&> \n' ->
    if n == n' then pred n else n

rangeWidget_v1
  :: forall t m
  .  ( DomBuilder t m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     )
  => Text -> Word
  -> m (Dynamic t Word)
rangeWidget_v1 faIcon maxdots = mdo
  selected <- holdDyn 0 . leftmost
          =<< traverse (item_v1 selected faIcon) ([1 .. maxdots] :: [Word])
  pure selected

item_v1
  :: forall t m
  .  ( DomBuilder t m
     , PostBuild t m
     )
  => Dynamic t Word -> Text -> Word -> m (Event t Word)
item_v1 dyN faIcon n = do
  let filled = dyN <&> \n' -> bool "far" "fas" (n <= n') <> " " <> faIcon
  (e,_) <- elDynClass' "i" filled blank
  pure $ domEvent Click e $> n
