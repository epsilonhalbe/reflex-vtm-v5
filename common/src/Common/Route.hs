{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module Common.Route where

{- -- You will probably want these imports for composing Encoders.
import Prelude hiding (id, (.))
import Control.Category
-}

import Data.Text (Text)
import Data.Functor.Identity
import Data.Some (Some(..))

import Obelisk.Route
import Obelisk.Route.TH

data BackendRoute :: * -> * where
  BackendRoute_Missing :: BackendRoute ()

data FrontendRoute :: * -> * where
  HomeR :: FrontendRoute ()
  ExampleR :: FrontendRoute (Maybe (R Example))

data Example :: * -> * where
  CharacterBuilder :: Example ()

routeEncoder
  :: Encoder (Either Text) Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
routeEncoder = mkFullRouteEncoder
  (FullRoute_Backend BackendRoute_Missing :/ ())
  (\case
      BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty
  )
  (\case
      HomeR    -> PathEnd (unitEncoder mempty)
      ExampleR -> PathSegment "example" $ maybeEncoder (unitEncoder mempty) $
         pathComponentEncoder \case
            CharacterBuilder -> PathSegment "vtm" $ unitEncoder mempty
  )

sectionHomepage :: Some Example -> R Example
sectionHomepage (Some sec) = sec :/ case sec of
  CharacterBuilder  -> ()


concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  , ''Example
  ]
