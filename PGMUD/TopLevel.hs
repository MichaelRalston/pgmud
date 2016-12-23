{-# LANGUAGE OverloadedStrings #-}

module PGMUD.TopLevel
    ( module PGMUD.Skills
    , module PGMUD.Weapons
    , module PGMUD.Adjectives
    , testBody
    , decodedAdjectiveList
    ) where
    
import PGMUD.Skills
import PGMUD.Weapons
import PGMUD.Adjectives
import PGMUD.Types
import PGMUD.Prelude

import qualified Data.Vector as Vector
import Data.Csv (decodeByName)

testBody :: ByteString
testBody = "id,adjective,sort order,context,excludes,interaction-air,interaction-earth,interaction-fire,interaction-water,interaction-order,interaction-chaos,interaction-light,interaction-metal,interaction-blood,interaction-decay,interaction-staff,interaction-book,interaction-fist,interaction-axe,interaction-orb,interaction-wand,interaction-rapier,interaction-spear,interaction-bow,interaction-dagger,interaction-whip,interaction-twohanded,interaction-thrown,ilvl,weapon class,air,earth,fire,water,order,chaos,light,metal,blood,decay,hp,pd,sd,sp,ap,ev,acc,pa,sa,stamina,willpower,health,defense,offense,mental,physical,mind,body,dexterity,skill,energy,maneuver,technique,cleverness\ntest,test Adjective,5,weapon,test3;test5,+,-,y,,,,n,,,,-,,+,-,,,,+,,,,,,15,staff,,,,0.3,,,,0.1,,,,,10,,-10,,,,,,,10,,,,13,,,,,,,-14,"

decodedAdjectiveList :: Either String [Adjective]
decodedAdjectiveList = (Vector.toList . snd) <$> (decodeByName $ traceTagged "body" $ fromStrict testBody)