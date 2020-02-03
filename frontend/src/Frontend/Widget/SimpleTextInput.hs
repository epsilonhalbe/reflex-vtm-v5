{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedLists #-}
module Frontend.Widget.SimpleTextInput where

import Data.Text (Text)
import Reflex.Dom.Core

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

