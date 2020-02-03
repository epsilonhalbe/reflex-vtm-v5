{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecursiveDo #-}
module Frontend.Widget.Range where

import Control.Monad.Fix (MonadFix)
import Data.Functor (($>), (<&>))
import Data.Bool (bool)
import Reflex.Dom.Core

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


