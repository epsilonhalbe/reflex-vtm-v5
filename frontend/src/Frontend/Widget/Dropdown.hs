{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecursiveDo #-}
module Frontend.Widget.Dropdown where

import Control.Monad.Fix (MonadFix)
import Data.Bool (bool)
import Data.Functor (($>))
import Data.Proxy (Proxy)
import Data.Text (Text, pack)
import Reflex.Dom.Core

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
dropdownWidget prx =
  elAttr "div"
      [("class", "mui-dropdown block")
      ,("required", "")
      ] mdo
    open <- clicked $ leftmost [ddClick $> Nothing, updated selected]
    ddClick <- dropdownButton selected
    selected <- elDynClass "ul" open $ holdDyn Nothing =<< selectItems prx
    pure selected

selectItems
  :: forall t m a
  . ( DomBuilder t m
    , Bounded a
    , Show a
    , Enum a
    )
  => Proxy a -> m (Event t (Maybe a))
selectItems _
  =   fmap Just . leftmost
  <$> traverse dropdownLi ([minBound .. maxBound] :: [a])

dropdownButton
  :: forall a m t
  .  ( DomBuilder t m
     , PostBuild t m
     , Show a
     )
  => Dynamic t (Maybe a) -> m (Event t ())
dropdownButton mCurrentElement = do
  (e,_) <- elClass' "button" "mui-btn mui-btn--large mui-btn--primary block"
            $ dynText $ maybe "(select)" (pack . show) <$> mCurrentElement
  pure $ domEvent Click e

dropdownLi
  :: forall a m t
  .  ( DomBuilder t m
     , Show a
     )
  => a -> m (Event t a)
dropdownLi x =
  el "li" do
    (e, _) <- elClass' "a" "dropdown-link" $ text . pack $ show x
    pure $ domEvent Click e $> x


clicked
  :: forall b m t
  .  ( MonadHold t m
     , MonadFix m
     , Reflex t
     )
  => Event t b -> m (Dynamic t Text)
clicked click
  =  fmap (("mui-dropdown__menu fullwidth" <>) . bool "" " mui--is-open")
 <$> foldDyn (const not) False click
