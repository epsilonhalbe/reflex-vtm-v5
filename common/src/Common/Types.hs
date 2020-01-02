module Common.Types where

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
 deriving (Enum, Bounded)

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
 deriving (Enum, Bounded)

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
 deriving (Enum, Bounded)


instance Show Generation where
 show Caine         = "Caine"
 show SecondGen     = "2nd Gen (Antediluvian)"
 show ThirdGen      = "3rd Gen (Antediluvian)"
 show FourthGen     = "4th Gen (Metuselah)"
 show FifthGen      = "5th Gen (Metuselah)"
 show SixthGen      = "6th Gen (Elder)"
 show SeventhGen    = "7th Gen (Elder)"
 show EigthGen      = "8th Gen"
 show NinethGen     = "9th Gen"
 show TenthGen      = "10th Gen"
 show EleventhGen   = "11th Gen"
 show TwelfthGen    = "12th Gen"
 show ThirteenthGen = "13th Gen"
 show FourteenthGen = "14th Gen (Thin-Blooded)"
 show FifteenthGen  = "15th Gen (Thin-Blooded)"
