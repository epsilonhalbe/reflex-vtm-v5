{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecursiveDo #-}
module Frontend.Widget.Range where

import Control.Monad (void)
import Control.Monad.Fix (MonadFix)
import Data.Functor (($>), (<&>))
import Data.Bool (bool)
import Reflex.Dom.Core

-- import Common.Api

app
  :: forall t m.
  ( DomBuilder t m
  , PostBuild t m
  , MonadHold t m
  , MonadFix m
  ) => m ()
app = elClass "div" "app" do
  el "h1" $ text "Prototype 1 - item 1"
  void $ rangeWidget_v1 5 item_v1
  el "h1" $ text "Prototype 1 - item 2"
  void $ rangeWidget_v1 5 item_v2
  el "h1" $ text "Prototype 1 - item 3"
  void $ rangeWidget_v1 5 item_v3
  el "h1" $ text "Prototype 1 - item 4"
  void $ rangeWidget_v1 5 item_v4
  el "h1" $ text "Prototype 2 - item 5"
  void $ rangeWidget_v2 5 item_v5
  el "h1" $ text "Test Range Widget"
  void $ rangeWidget 5
  blank


rangeWidget
  :: forall t m
  .  ( DomBuilder t m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     )
  => Word
  -> m (Dynamic t Word)
rangeWidget maxdots = mdo
  (sel,hov) <- unzip <$> traverse (item selected hovered) ([1 .. maxdots] :: [Word])
  selected <- holdDyn 0 $ leftmost sel
  hovered  <- holdDyn Nothing $ leftmost hov
  pure selected

rangeWidget_v2
  :: forall t m
  .  ( DomBuilder t m
     , MonadFix m
     , MonadHold t m
     )
  => Word
  -> (Dynamic t Word -> Dynamic t (Maybe Word) -> Word -> m (Event t Word, Event t (Maybe Word)))
  -> m (Dynamic t Word)
rangeWidget_v2 maxdots itm = mdo
  (sel,hov) <- unzip <$> traverse (itm selected hovered) ([1 .. maxdots] :: [Word])
  selected <- holdDyn 0 $ leftmost sel
  hovered  <- holdDyn Nothing $ leftmost hov
  pure selected

rangeWidget_v1
  :: forall t m
  .  ( DomBuilder t m
     , MonadFix m
     , MonadHold t m
     )
  => Word
  -> (Dynamic t Word -> Word -> m (Event t Word))
  -> m (Dynamic t Word)
rangeWidget_v1 maxdots itm = mdo
  selected <- holdDyn 0 . leftmost
          =<< traverse (itm selected) ([1 .. maxdots] :: [Word])
  pure selected

item_v1
  :: forall t m
  .  ( DomBuilder t m
     , PostBuild t m
     )
  => Dynamic t Word -> Word -> m (Event t Word)
item_v1 dyN n = do
  let filled = dyN <&> \n' -> bool "far" "fas" (n <= n') <> " fa-circle"
  (e,_) <- elDynClass' "i" filled blank
  pure $ domEvent Click e $> n

item_v2
  :: forall t m
  .  ( DomBuilder t m
     , PostBuild t m
     )
  => Dynamic t Word -> Word -> m (Event t Word)
item_v2 dyN n = do
  let filled = dyN <&> \n' -> bool "far" "fas" (n <= n') <> " fa-circle"
  (e,_) <- elDynClass' "i" filled blank
  pure $ (tag (current dyN) (domEvent Click e)) <&> \n' ->
    if n == n' then pred n else n

item_v3
  :: forall t m
  .  ( DomBuilder t m
     , MonadFix m
     , MonadHold t m
     , PostBuild t m
     )
  => Dynamic t Word -> Word -> m (Event t Word)
item_v3 dyN n = mdo
  let filled = dyN <&> (n <=)
  let class_ = (zipDyn filled hovered) <&>
        \case (False, False) -> "far fa-circle"
              (False, True ) -> "far fa-dot-circle"
              (True,  True ) -> "fas fa-dot-circle"
              (True,  False) -> "fas fa-circle"
  (e,_) <- elDynClass' "i" class_ blank
  let over = domEvent Mouseover e $> True
  let out  = domEvent Mouseout  e $> False
  hovered <- holdDyn False $ leftmost [over, out]
  pure $ domEvent Click e $> n

item_v4
  :: forall t m
  .  ( DomBuilder t m
     , MonadFix m
     , MonadHold t m
     , PostBuild t m
     )
  => Dynamic t Word -> Word -> m (Event t Word)
item_v4 dyN n = mdo
  let filled = dyN <&> (n <=)
  let class_ = (zipDyn filled hovered) <&>
        \case (False, False) -> "far fa-circle"
              (False, True ) -> "far fa-dot-circle"
              (True,  True ) -> "fas fa-dot-circle"
              (True,  False) -> "fas fa-circle"
  (e,_) <- elDynClass' "i" class_ blank
  let over = domEvent Mouseover e $> True
  let out  = domEvent Mouseout  e $> False
  hovered <- holdDyn False $ leftmost [over, out]
  pure $ (tag (current dyN) (domEvent Click e)) <&> bool n (pred n) . (== n)

item_v5
  :: forall t m
  .  ( DomBuilder t m
     , PostBuild t m
     )
  => Dynamic t Word -> Dynamic t (Maybe Word) -> Word -> m (Event t Word, Event t (Maybe Word))
item_v5 dyN hovered n = mdo
  let class_ = (zipDyn (compare n <$> dyN) (fmap (compare n) <$> hovered)) <&> \case
        (GT, Just LT) -> "fas fa-dot-circle"
        (GT, Just GT) -> "far fa-circle"
        (GT, Nothing) -> "far fa-circle"
        (_ , Just EQ) -> "fas fa-dot-circle"
        (_ , Just GT) -> "far fa-dot-circle"
        (_ , _      ) -> "fas fa-circle"
  (e,_) <- elDynClass' "i" class_ blank
  let over = domEvent Mouseover e $> Just n
  let out  = domEvent Mouseout  e $> Nothing
  pure ((tag (current dyN) (domEvent Click e)) <&> bool n (pred n) . (== n)
       , leftmost [over, out]
       )
item
  :: forall t m
  .  ( DomBuilder t m
     , PostBuild t m
     )
  => Dynamic t Word -> Dynamic t (Maybe Word) -> Word -> m (Event t Word, Event t (Maybe Word))
item dyN hovered n = mdo
  let class_ = (zipDyn (compare n <$> dyN) (fmap (compare n) <$> hovered)) <&> \case
        (GT, Just LT) -> "fas fa-dot-circle"
        (GT, Just GT) -> "far fa-circle"
        (GT, Nothing) -> "far fa-circle"
        (EQ, Just EQ) -> "mui--text-dark-secondary fas fa-dot-circle"
        (_ , Just EQ) -> "fas fa-dot-circle"
        (_ , Just GT) -> "far fa-dot-circle"
        (_ , _      ) -> "fas fa-circle"
  (e,_) <- elDynClass' "i" class_ blank
  let over = domEvent Mouseover e $> Just n
  let out  = domEvent Mouseout  e $> Nothing
  pure ((tag (current dyN) (domEvent Click e)) <&> bool n (pred n) . (== n)
       , leftmost [over, out]
       )


