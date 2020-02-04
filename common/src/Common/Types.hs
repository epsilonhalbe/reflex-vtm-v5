{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
module Common.Types where

import GHC.Generics (Generic)
import Data.Generic.HKD
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)

data Predator
 = Alleycat
 | Bagger
 | BloodLeech
 | Cleaver
 | Consensualist
 | Farmer
 | Osiris
 | Sandman
 | SceneQueen
 | Siren
 deriving stock (Enum, Bounded, Generic)
 deriving anyclass (FromJSON, ToJSON)

instance Show Predator where
 show Alleycat      = "Alleycat"
 show Bagger        = "Bagger"
 show BloodLeech    = "Blood Leech"
 show Cleaver       = "Cleaver"
 show Consensualist = "Consensualist"
 show Farmer        = "Farmer"
 show Osiris        = "Osiris"
 show Sandman       = "Sandman"
 show SceneQueen    = "Scene Queen"
 show Siren         = "Siren"

data Clan
 = BanuHaqim
 | Brujah
 | Gangrel
 | Hecata
 | Lasombra
 | Malkavian
 | Nosferatu
 | TheMinistry
 | Toreador
 | Tremere
 | Ventrue
 deriving (Enum, Bounded, Generic)
 deriving anyclass (FromJSON, ToJSON)

instance Show Clan where
  show BanuHaqim   = "Banu Haqim"
  show Brujah      = "Brujah"
  show Gangrel     = "Gangrel"
  show Hecata      = "Hecata"
  show Lasombra    = "Lasombra"
  show Malkavian   = "Malkavian"
  show Nosferatu   = "Nosferatu"
  show TheMinistry = "The Ministry"
  show Toreador    = "Toreador"
  show Tremere     = "Tremere"
  show Ventrue     = "Ventrue"

data Generation
 = Caine
 | SecondGen
 | ThirdGen
 | FourthGen
 | FifthGen
 | SixthGen
 | SeventhGen
 | EigthGen
 | NinethGen
 | TenthGen
 | EleventhGen
 | TwelfthGen
 | ThirteenthGen
 | FourteenthGen
 | FifteenthGen
 deriving (Enum, Bounded, Generic)
 deriving anyclass (FromJSON, ToJSON)

instance Show Generation where
 show Caine         = "Caine"
 show SecondGen     = "2nd Gen"
 show ThirdGen      = "3rd Gen"
 show FourthGen     = "4th Gen"
 show FifthGen      = "5th Gen"
 show SixthGen      = "6th Gen"
 show SeventhGen    = "7th Gen"
 show EigthGen      = "8th Gen"
 show NinethGen     = "9th Gen"
 show TenthGen      = "10th Gen"
 show EleventhGen   = "11th Gen"
 show TwelfthGen    = "12th Gen"
 show ThirteenthGen = "13th Gen"
 show FourteenthGen = "14th Gen"
 show FifteenthGen  = "15th Gen"

data Character f = Character
 { _details :: f (Details f)
 , _attributes :: f (Attributes f)
 }
 deriving stock (Generic)
 -- deriving anyclass (FromJSON, ToJSON)

data Details f = Details
 { _name       :: f Text
 , _concept    :: f Text
 , _chronicle  :: f Text
 , _ambition   :: f Text
 , _desire     :: f Text
 , _sire       :: f Text
 , _clan       :: f Clan
 , _generation :: f Generation
 , _predator   :: f Predator
 }
 deriving stock (Generic)
 -- deriving anyclass (FromJSON, ToJSON)

data Attributes f = Attributes
 { _strength     :: f Word
 , _dexterity    :: f Word
 , _stamina      :: f Word
 , _charisma     :: f Word
 , _manipulation :: f Word
 , _composure    :: f Word
 , _intelligence :: f Word
 , _wits         :: f Word
 , _resolve      :: f Word
 }
 deriving stock (Generic)
 -- deriving anyclass (FromJSON, ToJSON)

