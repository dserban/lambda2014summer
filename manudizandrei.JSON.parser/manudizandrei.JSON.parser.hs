{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Lens
import GHC.Generics ( Generic )
import Data.Aeson   ( FromJSON
                    , ToJSON
                    , decode
                    )

import qualified Data.ByteString.Lazy.Char8 as BL

data Location = Location {_at_floor :: Int} deriving (Generic, Show)

data Whereabouts = Whereabouts {_location :: Location, _destination :: Int} deriving (Generic, Show)

data TypicalOfficeHours = TypicalOfficeHours {_typical_office_hours :: [Whereabouts]} deriving (Generic, Show)

data Struct = Struct { _number_of_floors :: Int
                     , _number_of_elevators :: Int
                     , _carrying_capacity_of_one_elevator :: Int
                     , _traffic_patterns :: TypicalOfficeHours
                     } deriving (Generic, Show)

instance FromJSON Location
instance ToJSON Location

instance FromJSON Whereabouts
instance ToJSON Whereabouts

instance FromJSON TypicalOfficeHours
instance ToJSON TypicalOfficeHours

instance FromJSON Struct
instance ToJSON Struct

decodeJSONToStruct :: BL.ByteString -> Maybe Struct
decodeJSONToStruct = decode

makeLenses ''Struct
makeLenses ''TypicalOfficeHours
makeLenses ''Whereabouts
makeLenses ''Location

main :: IO ()
main = do
  input2 <- BL.readFile "input2.json"
  print $ decodeJSONToStruct input2
  let Just output = decodeJSONToStruct input2
  print output
  let Just interesting = output ^? traffic_patterns . typical_office_hours . ix 1 . location . at_floor
  print interesting

