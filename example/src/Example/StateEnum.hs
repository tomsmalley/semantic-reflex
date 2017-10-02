{-# LANGUAGE OverloadedStrings #-}

module Example.StateEnum where

import Prelude hiding (GT, LT)

import Data.Text (Text)

data StateEnum
  = AL | AK | AZ | AR | CA | CO | CT | DE | DC | FL | GA | HI | ID | IL | IN
  | IA | KS | KY | LA | ME | MD | MA | MI | MN | MS | MO | MT | NE | NV | NH
  | NJ | NM | NY | NC | ND | OH | OK | OR | PA | RI | SC | SD | TN | TX | UT
  | VT | VA | WA | WV | WI | WY
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

stateText :: StateEnum -> Text
stateText AL = "Alabama"
stateText AK = "Alaska"
stateText AZ = "Arizona"
stateText AR = "Arkansas"
stateText CA = "California"
stateText CO = "Colorado"
stateText CT = "Connecticut"
stateText DE = "Delaware"
stateText DC = "District Of Columbia"
stateText FL = "Florida"
stateText GA = "Georgia"
stateText HI = "Hawaii"
stateText ID = "Idaho"
stateText IL = "Illinois"
stateText IN = "Indiana"
stateText IA = "Iowa"
stateText KS = "Kansas"
stateText KY = "Kentucky"
stateText LA = "Louisiana"
stateText ME = "Maine"
stateText MD = "Maryland"
stateText MA = "Massachusetts"
stateText MI = "Michigan"
stateText MN = "Minnesota"
stateText MS = "Mississippi"
stateText MO = "Missouri"
stateText MT = "Montana"
stateText NE = "Nebraska"
stateText NV = "Nevada"
stateText NH = "New Hampshire"
stateText NJ = "New Jersey"
stateText NM = "New Mexico"
stateText NY = "New York"
stateText NC = "North Carolina"
stateText ND = "North Dakota"
stateText OH = "Ohio"
stateText OK = "Oklahoma"
stateText OR = "Oregon"
stateText PA = "Pennsylvania"
stateText RI = "Rhode Island"
stateText SC = "South Carolina"
stateText SD = "South Dakota"
stateText TN = "Tennessee"
stateText TX = "Texas"
stateText UT = "Utah"
stateText VT = "Vermont"
stateText VA = "Virginia"
stateText WA = "Washington"
stateText WV = "West Virginia"
stateText WI = "Wisconsin"
stateText WY = "Wyoming"
