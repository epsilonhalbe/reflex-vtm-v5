{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedLists #-}
module Frontend.Widget.SimpleTextInput where

import Control.Monad.Fix (MonadFix)
import Control.Monad (void)
import Data.Functor ((<&>))
import Data.Text (Text)
import Reflex.Dom.Core

app
  :: forall t m
  .  ( DomBuilder t m
     , PostBuild t m
     , MonadFix m
     )
  => m ()
app = elClass "div" "app" mdo
  el "h1" $ text "Prototype 1"
  void $ inputWidget "Test"

  el "h1" $ text "Prototype 2"
  txt2 <- inputWidget "Test"
  el "br" blank
  dynText (txt2 <&> ("Test: " <>))

  el "h1" $ text "Input Widget"
  dynText (txt <&> ("Test: " <>))
  el "br" blank
  txt <- inputWidget "Test"
  blank

inputWidget
  :: forall t m
  .  DomBuilder t m
  => Text
  -> m (Dynamic t Text)
inputWidget name
  = fmap value
  $ elAttr "div"
      [("class","mui-textfield")
      ,("required","")
      ]
  $ inputElement
  $ def & inputElementConfig_elementConfig
        . elementConfig_initialAttributes .~ [("placeholder", name)]

