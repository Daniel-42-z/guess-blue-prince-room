module RoomList where

import Data.Set (empty, fromList, singleton)
import Room

-- ============================================================================
-- Blueprints (001-046)
-- ============================================================================

-- 001
foundation :: Room
foundation =
  Room
    { name = "Foundation",
      shape = TShaped,
      colors = singleton Blue,
      gemCost = 0,
      poolCat = Starting,
      specialCats = singleton Permanent,
      resources = noResourceGain
    }

-- 002
entranceHall :: Room
entranceHall =
  Room
    { name = "Entrance Hall",
      shape = TShaped,
      colors = singleton Blue,
      gemCost = 0,
      poolCat = Starting,
      specialCats = singleton Permanent,
      resources = noResourceGain
    }

-- 003
spareRoom :: Room
spareRoom =
  Room
    { name = "Spare Room",
      shape = Straight,
      colors = singleton Blue,
      gemCost = 0,
      poolCat = Upgradable,
      specialCats = empty,
      resources = noResourceGain
    }

-- 004
rotunda :: Room
rotunda =
  Room
    { name = "Rotunda",
      shape = Corner,
      colors = singleton Blue,
      gemCost = 3,
      poolCat = Starting,
      specialCats = empty,
      resources = noResourceGain
    }

-- 005
parlor :: Room
parlor =
  Room
    { name = "Parlor",
      shape = Corner,
      colors = singleton Blue,
      gemCost = 0,
      poolCat = Upgradable,
      specialCats = singleton Puzzle,
      resources = resourceGains [(Gems, Fixed 2)]
    }

-- 006
billiardRoom :: Room
billiardRoom =
  Room
    { name = "Billiard Room",
      shape = Corner,
      colors = singleton Blue,
      gemCost = 0,
      poolCat = Upgradable,
      specialCats = singleton Puzzle,
      resources = resourceGains [(Keys, Fixed 2)]
    }

-- 007
gallery :: Room
gallery =
  Room
    { name = "Gallery",
      shape = Straight,
      colors = singleton Blue,
      gemCost = 0,
      poolCat = Starting,
      specialCats = singleton Puzzle,
      resources = resourceGains [(Gems, Fixed 2), (Coins, Fixed 4), (Other, Fixed 1)]
    }

-- 008
room8 :: Room
room8 =
  Room
    { name = "Room 8",
      shape = Corner,
      colors = singleton Blue,
      gemCost = 0,
      poolCat = Starting,
      specialCats = singleton Puzzle,
      resources = resourceGains [(Other, Fixed 1)]
    }

-- 009
closet :: Room
closet =
  Room
    { name = "Closet",
      shape = DeadEnd,
      colors = singleton Blue,
      gemCost = 0,
      poolCat = Upgradable,
      specialCats = empty,
      resources = resourceGains [(Other, Fixed 2)]
    }

-- 010
walkInCloset :: Room
walkInCloset =
  Room
    { name = "Walk-In Closet",
      shape = DeadEnd,
      colors = singleton Blue,
      gemCost = 1,
      poolCat = Starting,
      specialCats = empty,
      resources = resourceGains [(Other, Fixed 4)]
    }

-- 011
attic :: Room
attic =
  Room
    { name = "Attic",
      shape = DeadEnd,
      colors = singleton Blue,
      gemCost = 3,
      poolCat = Starting,
      specialCats = empty,
      resources = resourceGains [(Other, Fixed 8)]
    }

-- 012
storeRoom :: Room
storeRoom =
  Room
    { name = "Storeroom",
      shape = DeadEnd,
      colors = singleton Blue,
      gemCost = 0,
      poolCat = Upgradable,
      specialCats = empty,
      resources = resourceGains [(Coins, Fixed 1), (Gems, Fixed 1), (Keys, Fixed 1)]
    }

-- 013
nook :: Room
nook =
  Room
    { name = "Nook",
      shape = Corner,
      colors = singleton Blue,
      gemCost = 0,
      poolCat = Upgradable,
      specialCats = empty,
      resources = resourceGains [(Keys, Fixed 1)]
    }

-- 014
garage :: Room
garage =
  Room
    { name = "Garage",
      shape = DeadEnd,
      colors = singleton Blue,
      gemCost = 1,
      poolCat = Starting,
      specialCats = empty,
      resources = resourceGains [(Keys, Fixed 3)]
    }

-- 015
musicRoom :: Room
musicRoom =
  Room
    { name = "Music Room",
      shape = Corner,
      colors = singleton Blue,
      gemCost = 2,
      poolCat = Starting,
      specialCats = empty,
      resources = resourceGains [(Keys, Fixed 1), (Other, Fixed 1)]
    }

-- 016
lockerRoom :: Room
lockerRoom =
  Room
    { name = "Locker Room",
      shape = Straight,
      colors = singleton Blue,
      gemCost = 1,
      poolCat = Starting,
      specialCats = singleton Spread,
      resources = resourceGains [(Keys, SpreadGain)]
    }

-- 017
den :: Room
den =
  Room
    { name = "Den",
      shape = TShaped,
      colors = singleton Blue,
      gemCost = 0,
      poolCat = Starting,
      specialCats = empty,
      resources = resourceGains [(Gems, Fixed 1)]
    }

-- 018
wineCellar :: Room
wineCellar =
  Room
    { name = "Wine Cellar",
      shape = DeadEnd,
      colors = singleton Blue,
      gemCost = 0,
      poolCat = Starting,
      specialCats = empty,
      resources = resourceGains [(Gems, Fixed 3)]
    }

-- 019
trophyRoom :: Room
trophyRoom =
  Room
    { name = "Trophy Room",
      shape = Corner,
      colors = singleton Blue,
      gemCost = 5,
      poolCat = Starting,
      specialCats = empty,
      resources = resourceGains [(Gems, Fixed 8)]
    }

-- 020
ballroom :: Room
ballroom =
  Room
    { name = "Ballroom",
      shape = Straight,
      colors = singleton Blue,
      gemCost = 2,
      poolCat = Starting,
      specialCats = singleton Entry,
      resources = resourceGains [(Gems, Mixed)]
    }

-- 021
pantry :: Room
pantry =
  Room
    { name = "Pantry",
      shape = Corner,
      colors = singleton Blue,
      gemCost = 0,
      poolCat = Starting,
      specialCats = empty,
      resources = resourceGains [(Coins, Fixed 4), (Steps, VarGain)]
    }

-- 022
rumpusRoom :: Room
rumpusRoom =
  Room
    { name = "Rumpus Room",
      shape = Straight,
      colors = singleton Blue,
      gemCost = 1,
      poolCat = Starting,
      specialCats = empty,
      resources = resourceGains [(Coins, Fixed 8)]
    }

-- 023
vault :: Room
vault =
  Room
    { name = "Vault",
      shape = DeadEnd,
      colors = singleton Blue,
      gemCost = 3,
      poolCat = Starting,
      specialCats = empty,
      resources = resourceGains [(Coins, Fixed 40)]
    }

-- 024
office :: Room
office =
  Room
    { name = "Office",
      shape = Corner,
      colors = singleton Blue,
      gemCost = 2,
      poolCat = Starting,
      specialCats = singleton Spread,
      resources = resourceGains [(Coins, SpreadGain)]
    }

-- 025
drawingRoom :: Room
drawingRoom =
  Room
    { name = "Drawing Room",
      shape = TShaped,
      colors = singleton Blue,
      gemCost = 1,
      poolCat = Starting,
      specialCats = singleton Drafting,
      resources = noResourceGain
    }

-- 026
study :: Room
study =
  Room
    { name = "Study",
      shape = DeadEnd,
      colors = singleton Blue,
      gemCost = 0,
      poolCat = Starting,
      specialCats = singleton Drafting,
      resources = noResourceGain
    }

-- 027
library :: Room
library =
  Room
    { name = "Library",
      shape = Corner,
      colors = singleton Blue,
      gemCost = 0,
      poolCat = Starting,
      specialCats = singleton Drafting,
      resources = noResourceGain
    }

-- 028
chamberOfMirrors :: Room
chamberOfMirrors =
  Room
    { name = "Chamber of Mirrors",
      shape = DeadEnd,
      colors = singleton Blue,
      gemCost = 0,
      poolCat = Starting,
      specialCats = singleton Drafting,
      resources = noResourceGain
    }

-- 029
thePool :: Room
thePool =
  Room
    { name = "The Pool",
      shape = TShaped,
      colors = singleton Blue,
      gemCost = 1,
      poolCat = Starting,
      specialCats = singleton Drafting,
      resources = noResourceGain
    }

-- 030
draftingStudio :: Room
draftingStudio =
  Room
    { name = "Drafting Studio",
      shape = Straight,
      colors = singleton Blue,
      gemCost = 2,
      poolCat = Starting,
      specialCats = singleton Drafting,
      resources = noResourceGain
    }

-- 031
utilityCloset :: Room
utilityCloset =
  Room
    { name = "Utility Closet",
      shape = DeadEnd,
      colors = singleton Blue,
      gemCost = 0,
      poolCat = Starting,
      specialCats = singleton Mechanical,
      resources = noResourceGain
    }

-- 032
boilerRoom :: Room
boilerRoom =
  Room
    { name = "Boiler Room",
      shape = TShaped,
      colors = singleton Blue,
      gemCost = 1,
      poolCat = Starting,
      specialCats = singleton Mechanical,
      resources = noResourceGain
    }

-- 033
pumpRoom :: Room
pumpRoom =
  Room
    { name = "Pump Room",
      shape = Corner,
      colors = singleton Blue,
      gemCost = 0,
      poolCat = Starting,
      specialCats = singleton Mechanical,
      resources = noResourceGain
    }

-- 034
security :: Room
security =
  Room
    { name = "Security",
      shape = TShaped,
      colors = singleton Blue,
      gemCost = 1,
      poolCat = Starting,
      specialCats = singleton Mechanical,
      resources = noResourceGain
    }

-- 035
workshop :: Room
workshop =
  Room
    { name = "Workshop",
      shape = Straight,
      colors = singleton Blue,
      gemCost = 0,
      poolCat = Starting,
      specialCats = singleton Mechanical,
      resources = noResourceGain
    }

-- 036
laboratory :: Room
laboratory =
  Room
    { name = "Laboratory",
      shape = Corner,
      colors = singleton Blue,
      gemCost = 1,
      poolCat = Starting,
      specialCats = singleton Mechanical,
      resources = noResourceGain
    }

-- 037
sauna :: Room
sauna =
  Room
    { name = "Sauna",
      shape = DeadEnd,
      colors = singleton Blue,
      gemCost = 0,
      poolCat = Starting,
      specialCats = singleton Tomorrow,
      resources = resourceGains [(Steps, Fixed 20)]
    }

-- 038
coatCheck :: Room
coatCheck =
  Room
    { name = "Coat Check",
      shape = DeadEnd,
      colors = singleton Blue,
      gemCost = 0,
      poolCat = Starting,
      specialCats = singleton Tomorrow,
      resources = noResourceGain
    }

-- 039
mailRoom :: Room
mailRoom =
  Room
    { name = "Mail Room",
      shape = DeadEnd,
      colors = singleton Blue,
      gemCost = 0,
      poolCat = Upgradable,
      specialCats = singleton Tomorrow,
      resources = resourceGains [(Other, VarGain)]
    }

-- 040
freezer :: Room
freezer =
  Room
    { name = "Freezer",
      shape = DeadEnd,
      colors = singleton Blue,
      gemCost = 0,
      poolCat = Starting,
      specialCats = singleton Tomorrow,
      resources = noResourceGain
    }

-- 041
diningRoom :: Room
diningRoom =
  Room
    { name = "Dining Room",
      shape = TShaped,
      colors = singleton Blue,
      gemCost = 0,
      poolCat = Starting,
      specialCats = empty,
      resources = resourceGains [(Steps, VarGain)]
    }

-- 042
observatory :: Room
observatory =
  Room
    { name = "Observatory",
      shape = Corner,
      colors = singleton Blue,
      gemCost = 1,
      poolCat = Starting,
      specialCats = empty,
      resources = resourceGains [(Other, VarGain)]
    }

-- 043
conferenceRoom :: Room
conferenceRoom =
  Room
    { name = "Conference Room",
      shape = TShaped,
      colors = singleton Blue,
      gemCost = 0,
      poolCat = Starting,
      specialCats = singleton Spread,
      resources = noResourceGain
    }

-- 044
aquarium :: Room
aquarium =
  Room
    { name = "Aquarium",
      shape = TShaped,
      colors = fromList [Blue, Red, GreenRoom, Hallway, Bedroom, Shop, Blackprint],
      gemCost = 1,
      poolCat = Upgradable,
      specialCats = empty,
      resources = noResourceGain
    }

-- 045
antechamber :: Room
antechamber =
  Room
    { name = "Antechamber",
      shape = Crossroad,
      colors = singleton Blue,
      gemCost = 0,
      poolCat = Starting,
      specialCats = singleton Permanent,
      resources = resourceGains [(Other, Fixed 1)]
    }

-- 046
room46 :: Room
room46 =
  Room
    { name = "Room 46",
      shape = DeadEnd,
      colors = singleton Blue,
      gemCost = 0,
      poolCat = Starting,
      specialCats = singleton Permanent,
      resources = resourceGains [(Other, Fixed 1)]
    }

-- ============================================================================
-- Bedrooms
-- ============================================================================

bedroom :: Room
bedroom =
  Room
    { name = "Bedroom",
      shape = Corner,
      colors = singleton Bedroom,
      gemCost = 0,
      poolCat = Starting,
      specialCats = singleton Entry,
      resources = resourceGains [(Steps, PerEntryGain)]
    }

boudoir :: Room
boudoir =
  Room
    { name = "Boudoir",
      shape = Corner,
      colors = singleton Bedroom,
      gemCost = 0,
      poolCat = Upgradable,
      specialCats = empty,
      resources = noResourceGain
    }

guestBedroom :: Room
guestBedroom =
  Room
    { name = "Guest Bedroom",
      shape = DeadEnd,
      colors = singleton Bedroom,
      gemCost = 0,
      poolCat = Upgradable,
      specialCats = empty,
      resources = resourceGains [(Steps, Fixed 10)]
    }

nursery :: Room
nursery =
  Room
    { name = "Nursery",
      shape = DeadEnd,
      colors = singleton Bedroom,
      gemCost = 1,
      poolCat = Upgradable,
      specialCats = singleton Drafting,
      resources = resourceGains [(Steps, VarGain), (Other, Fixed 1)]
    }

servantsQuarters :: Room
servantsQuarters =
  Room
    { name = "Servant's Quarters",
      shape = DeadEnd,
      colors = singleton Bedroom,
      gemCost = 1,
      poolCat = Starting,
      specialCats = empty,
      resources = resourceGains [(Keys, VarGain)]
    }

bunkRoom :: Room
bunkRoom =
  Room
    { name = "Bunk Room",
      shape = DeadEnd,
      colors = singleton Bedroom,
      gemCost = 0,
      poolCat = Upgradable,
      specialCats = empty,
      resources = resourceGains [(Other, Fixed 2)]
    }

herLadyshipsChamber :: Room
herLadyshipsChamber =
  Room
    { name = "Her Ladyship's Chamber",
      shape = DeadEnd,
      colors = singleton Bedroom,
      gemCost = 0,
      poolCat = Starting,
      specialCats = empty,
      resources = resourceGains [(Steps, VarGain), (Gems, VarGain)]
    }

masterBedroom :: Room
masterBedroom =
  Room
    { name = "Master Bedroom",
      shape = DeadEnd,
      colors = singleton Bedroom,
      gemCost = 2,
      poolCat = Starting,
      specialCats = empty,
      resources = resourceGains [(Steps, VarGain)]
    }

-- ============================================================================
-- Hallways
-- ============================================================================

hallway :: Room
hallway =
  Room
    { name = "Hallway",
      shape = TShaped,
      colors = singleton Hallway,
      gemCost = 0,
      poolCat = Upgradable,
      specialCats = empty,
      resources = noResourceGain
    }

westWingHall :: Room
westWingHall =
  Room
    { name = "West Wing Hall",
      shape = TShaped,
      colors = singleton Hallway,
      gemCost = 0,
      poolCat = Starting,
      specialCats = empty,
      resources = noResourceGain
    }

eastWingHall :: Room
eastWingHall =
  Room
    { name = "East Wing Hall",
      shape = TShaped,
      colors = singleton Hallway,
      gemCost = 0,
      poolCat = Starting,
      specialCats = empty,
      resources = noResourceGain
    }

corridor :: Room
corridor =
  Room
    { name = "Corridor",
      shape = Straight,
      colors = singleton Hallway,
      gemCost = 0,
      poolCat = Starting,
      specialCats = empty,
      resources = noResourceGain
    }

passageway :: Room
passageway =
  Room
    { name = "Passageway",
      shape = Crossroad,
      colors = singleton Hallway,
      gemCost = 2,
      poolCat = Starting,
      specialCats = empty,
      resources = noResourceGain
    }

secretPassage :: Room
secretPassage =
  Room
    { name = "Secret Passage",
      shape = Straight,
      colors = singleton Hallway,
      gemCost = 1,
      poolCat = Starting,
      specialCats = empty,
      resources = noResourceGain
    }

foyer :: Room
foyer =
  Room
    { name = "Foyer",
      shape = Straight,
      colors = singleton Hallway,
      gemCost = 2,
      poolCat = Starting,
      specialCats = empty,
      resources = noResourceGain
    }

greatHall :: Room
greatHall =
  Room
    { name = "Great Hall",
      shape = Crossroad,
      colors = singleton Hallway,
      gemCost = 0,
      poolCat = Starting,
      specialCats = empty,
      resources = noResourceGain
    }

-- ============================================================================
-- Green Rooms
-- ============================================================================

terrace :: Room
terrace =
  Room
    { name = "Terrace",
      shape = DeadEnd,
      colors = singleton GreenRoom,
      gemCost = 0,
      poolCat = Starting,
      specialCats = empty,
      resources = resourceGains [(Other, Fixed 1)]
    }

patio :: Room
patio =
  Room
    { name = "Patio",
      shape = Corner,
      colors = singleton GreenRoom,
      gemCost = 1,
      poolCat = Starting,
      specialCats = singleton Spread,
      resources = resourceGains [(Gems, SpreadGain)]
    }

courtyard :: Room
courtyard =
  Room
    { name = "Courtyard",
      shape = TShaped,
      colors = singleton GreenRoom,
      gemCost = 1,
      poolCat = Upgradable,
      specialCats = empty,
      resources = noResourceGain
    }

cloister :: Room
cloister =
  Room
    { name = "Cloister",
      shape = Crossroad,
      colors = singleton GreenRoom,
      gemCost = 3,
      poolCat = Upgradable,
      specialCats = empty,
      resources = noResourceGain
    }

veranda :: Room
veranda =
  Room
    { name = "Veranda",
      shape = Straight,
      colors = singleton GreenRoom,
      gemCost = 2,
      poolCat = Starting,
      specialCats = empty,
      resources = noResourceGain
    }

greenhouse :: Room
greenhouse =
  Room
    { name = "Greenhouse",
      shape = DeadEnd,
      colors = singleton GreenRoom,
      gemCost = 1,
      poolCat = Starting,
      specialCats = singleton Drafting,
      resources = resourceGains [(Other, Fixed 1)]
    }

morningRoom :: Room
morningRoom =
  Room
    { name = "Morning Room",
      shape = Corner,
      colors = singleton GreenRoom,
      gemCost = 0,
      poolCat = Starting,
      specialCats = singleton Tomorrow,
      resources = resourceGains [(Gems, Fixed 2)]
    }

secretGarden :: Room
secretGarden =
  Room
    { name = "Secret Garden",
      shape = TShaped,
      colors = singleton GreenRoom,
      gemCost = 0,
      poolCat = Starting,
      specialCats = singleton Spread,
      resources = resourceGains [(Steps, SpreadGain)]
    }

-- ============================================================================
-- Shops
-- ============================================================================

commissary :: Room
commissary =
  Room
    { name = "Commissary",
      shape = Corner,
      colors = singleton Shop,
      gemCost = 1,
      poolCat = Starting,
      specialCats = empty,
      resources = resourceGains [(Steps, Purchase), (Gems, Purchase), (Keys, Purchase), (Other, Purchase)]
    }

kitchen :: Room
kitchen =
  Room
    { name = "Kitchen",
      shape = Corner,
      colors = singleton Shop,
      gemCost = 1,
      poolCat = Starting,
      specialCats = empty,
      resources = resourceGains [(Steps, Purchase)]
    }

locksmith :: Room
locksmith =
  Room
    { name = "Locksmith",
      shape = DeadEnd,
      colors = singleton Shop,
      gemCost = 1,
      poolCat = Starting,
      specialCats = empty,
      resources = resourceGains [(Keys, Purchase), (Other, Purchase)]
    }

showroom :: Room
showroom =
  Room
    { name = "Showroom",
      shape = Straight,
      colors = singleton Shop,
      gemCost = 2,
      poolCat = Starting,
      specialCats = empty,
      resources = resourceGains [(Other, Purchase)]
    }

laundryRoom :: Room
laundryRoom =
  Room
    { name = "Laundry Room",
      shape = DeadEnd,
      colors = singleton Shop,
      gemCost = 1,
      poolCat = Starting,
      specialCats = empty,
      resources = resourceGains [(Steps, Purchase), (Coins, Purchase), (Gems, Purchase), (Keys, Purchase), (Other, Purchase)]
    }

bookshop :: Room
bookshop =
  Room
    { name = "Bookshop",
      shape = Corner,
      colors = singleton Shop,
      gemCost = 1,
      poolCat = Starting,
      specialCats = empty,
      resources = noResourceGain
    }

theArmory :: Room
theArmory =
  Room
    { name = "The Armory",
      shape = Corner,
      colors = fromList [Shop, Blackprint],
      gemCost = 0,
      poolCat = Starting,
      specialCats = empty,
      resources = resourceGains [(Other, Purchase)]
    }

giftShop :: Room
giftShop =
  Room
    { name = "Gift Shop",
      shape = TShaped,
      colors = singleton Shop,
      gemCost = 0,
      poolCat = Starting,
      specialCats = empty,
      resources = noResourceGain
    }

-- ============================================================================
-- Red Rooms
-- ============================================================================

lavatory :: Room
lavatory =
  Room
    { name = "Lavatory",
      shape = DeadEnd,
      colors = singleton Red,
      gemCost = 0,
      poolCat = Starting,
      specialCats = empty,
      resources = noResourceGain
    }

chapel :: Room
chapel =
  Room
    { name = "Chapel",
      shape = TShaped,
      colors = singleton Red,
      gemCost = 0,
      poolCat = Starting,
      specialCats = singleton Entry,
      resources = resourceGains [(Coins, PerEntryLoss)]
    }

maidsChamber :: Room
maidsChamber =
  Room
    { name = "Maid's Chamber",
      shape = Corner,
      colors = fromList [Bedroom, Red],
      gemCost = 0,
      poolCat = Starting,
      specialCats = empty,
      resources = noResourceGain
    }

archives :: Room
archives =
  Room
    { name = "Archives",
      shape = Crossroad,
      colors = singleton Red,
      gemCost = 0,
      poolCat = Starting,
      specialCats = empty,
      resources = noResourceGain
    }

gymnasium :: Room
gymnasium =
  Room
    { name = "Gymnasium",
      shape = TShaped,
      colors = singleton Red,
      gemCost = 0,
      poolCat = Starting,
      specialCats = singleton Entry,
      resources = resourceGains [(Steps, PerEntryLoss)]
    }

darkroom :: Room
darkroom =
  Room
    { name = "Darkroom",
      shape = TShaped,
      colors = singleton Red,
      gemCost = 0,
      poolCat = Starting,
      specialCats = empty,
      resources = noResourceGain
    }

weightRoom :: Room
weightRoom =
  Room
    { name = "Weight Room",
      shape = Crossroad,
      colors = singleton Red,
      gemCost = 0,
      poolCat = Starting,
      specialCats = empty,
      resources = resourceGains [(Steps, VarLoss)]
    }

furnace :: Room
furnace =
  Room
    { name = "Furnace",
      shape = DeadEnd,
      colors = singleton Red,
      gemCost = 0,
      poolCat = Starting,
      specialCats = empty,
      resources = noResourceGain
    }

-- ============================================================================
-- Studio Additions
-- ============================================================================

dovecote :: Room
dovecote =
  Room
    { name = "Dovecote",
      shape = Corner,
      colors = singleton Blue,
      gemCost = 0,
      poolCat = Studio,
      specialCats = empty,
      resources = noResourceGain
    }

theKennel :: Room
theKennel =
  Room
    { name = "The Kennel",
      shape = Straight,
      colors = singleton Blue,
      gemCost = 0,
      poolCat = Studio,
      specialCats = empty,
      resources = noResourceGain
    }

clockTower :: Room
clockTower =
  Room
    { name = "Clock Tower",
      shape = DeadEnd,
      colors = singleton Blue,
      gemCost = 1,
      poolCat = Studio,
      specialCats = singleton Tomorrow,
      resources = resourceGains [(Keys, VarGain)]
    }

classroom :: Room
classroom =
  Room
    { name = "Classroom",
      shape = Corner,
      colors = singleton Blue,
      gemCost = 1,
      poolCat = Studio,
      specialCats = singleton Drafting,
      resources = resourceGains [(Steps, Fixed 2)]
    }

solarium :: Room
solarium =
  Room
    { name = "Solarium",
      shape = DeadEnd,
      colors = singleton GreenRoom,
      gemCost = 1,
      poolCat = Studio,
      specialCats = empty,
      resources = noResourceGain
    }

dormitory :: Room
dormitory =
  Room
    { name = "Dormitory",
      shape = DeadEnd,
      colors = singleton Bedroom,
      gemCost = 0,
      poolCat = Studio,
      specialCats = singleton Entry,
      resources = resourceGains [(Steps, PerEntryGain)]
    }

vestibule :: Room
vestibule =
  Room
    { name = "Vestibule",
      shape = Crossroad,
      colors = singleton Hallway,
      gemCost = 2,
      poolCat = Studio,
      specialCats = singleton Entry,
      resources = noResourceGain
    }

casino :: Room
casino =
  Room
    { name = "Casino",
      shape = Corner,
      colors = singleton Shop,
      gemCost = 2,
      poolCat = Studio,
      specialCats = empty,
      resources = resourceGains $ (Coins, Mixed) : (zip [Steps, Gems, Keys, Other] (repeat VarGain))
    }

-- ============================================================================
-- Found Floorplans
-- ============================================================================

planetarium :: Room
planetarium =
  Room
    { name = "Planetarium",
      shape = DeadEnd,
      colors = singleton Blue,
      gemCost = 0,
      poolCat = Found,
      specialCats = singleton Tomorrow,
      resources = resourceGains [(Other, VarGain)]
    }

mechanarium :: Room
mechanarium =
  Room
    { name = "Mechanarium",
      shape = Crossroad,
      colors = singleton Blue,
      gemCost = 0,
      poolCat = Found,
      specialCats = singleton Mechanical,
      resources = noResourceGain
    }

treasureTrove :: Room
treasureTrove =
  Room
    { name = "Treasure Trove",
      shape = Corner,
      colors = singleton Blackprint,
      gemCost = 1,
      poolCat = Found,
      specialCats = empty,
      resources = resourceGains [(Coins, VarGain)]
    }

throneRoom :: Room
throneRoom =
  Room
    { name = "Throne Room",
      shape = TShaped,
      colors = singleton Blackprint,
      gemCost = 5,
      poolCat = Found,
      specialCats = empty,
      resources = noResourceGain
    }

tunnel :: Room
tunnel =
  Room
    { name = "Tunnel",
      shape = Straight,
      colors = singleton Hallway,
      gemCost = 0,
      poolCat = Found,
      specialCats = singleton Drafting,
      resources = noResourceGain
    }

conservatory :: Room
conservatory =
  Room
    { name = "Conservatory",
      shape = Corner,
      colors = singleton GreenRoom,
      gemCost = 1,
      poolCat = Found,
      specialCats = singleton Drafting,
      resources = noResourceGain
    }

lostAndFound :: Room
lostAndFound =
  Room
    { name = "Lost & Found",
      shape = Corner,
      colors = singleton Red,
      gemCost = 0,
      poolCat = Found,
      specialCats = singleton Entry,
      resources = resourceGains [(Other, Mixed)]
    }

closedExhibit :: Room
closedExhibit =
  Room
    { name = "Closed Exhibit",
      shape = TShaped,
      colors = singleton Red,
      gemCost = 0,
      poolCat = Found,
      specialCats = empty,
      resources = noResourceGain
    }

-- ============================================================================
-- Outer Rooms
-- ============================================================================

toolshed :: Room
toolshed =
  Room
    { name = "Toolshed",
      shape = DeadEnd,
      colors = singleton Blue,
      gemCost = 0,
      poolCat = Outer,
      specialCats = empty,
      resources = resourceGains [(Other, Fixed 2)]
    }

shelter :: Room
shelter =
  Room
    { name = "Shelter",
      shape = DeadEnd,
      colors = singleton Blue,
      gemCost = 0,
      poolCat = Outer,
      specialCats = empty,
      resources = noResourceGain
    }

schoolhouse :: Room
schoolhouse =
  Room
    { name = "Schoolhouse",
      shape = DeadEnd,
      colors = singleton Blue,
      gemCost = 0,
      poolCat = Outer,
      specialCats = singleton Drafting,
      resources = noResourceGain
    }

shrine :: Room
shrine =
  Room
    { name = "Shrine",
      shape = DeadEnd,
      colors = singleton Blue,
      gemCost = 0,
      poolCat = Outer,
      specialCats = empty,
      resources = resourceGains [(Coins, VarLoss), (Other, VarGain)]
    }

rootCellar :: Room
rootCellar =
  Room
    { name = "Root Cellar",
      shape = DeadEnd,
      colors = singleton GreenRoom,
      gemCost = 0,
      poolCat = Outer,
      specialCats = empty,
      resources = noResourceGain
    }

hovel :: Room
hovel =
  Room
    { name = "Hovel",
      shape = DeadEnd,
      colors = singleton Bedroom,
      gemCost = 0,
      poolCat = Outer,
      specialCats = empty,
      resources = resourceGains [(Steps, VarLoss)]
    }

tradingPost :: Room
tradingPost =
  Room
    { name = "Trading Post",
      shape = DeadEnd,
      colors = singleton Shop,
      gemCost = 0,
      poolCat = Outer,
      specialCats = empty,
      resources = noResourceGain
    }

tomb :: Room
tomb =
  Room
    { name = "Tomb",
      shape = DeadEnd,
      colors = singleton Blackprint,
      gemCost = 0,
      poolCat = Outer,
      specialCats = singleton Spread,
      resources = resourceGains [(Coins, SpreadGain)]
    }

-- ============================================================================
-- All Rooms (Room Directory Order)
-- ============================================================================

allRooms :: [Room]
allRooms =
  [ foundation,
    entranceHall,
    spareRoom,
    rotunda,
    parlor,
    billiardRoom,
    gallery,
    room8,
    closet,
    walkInCloset,
    attic,
    storeRoom,
    nook,
    garage,
    musicRoom,
    lockerRoom,
    den,
    wineCellar,
    trophyRoom,
    ballroom,
    pantry,
    rumpusRoom,
    vault,
    office,
    drawingRoom,
    study,
    library,
    chamberOfMirrors,
    thePool,
    draftingStudio,
    utilityCloset,
    boilerRoom,
    pumpRoom,
    security,
    workshop,
    laboratory,
    sauna,
    coatCheck,
    mailRoom,
    freezer,
    diningRoom,
    observatory,
    conferenceRoom,
    aquarium,
    antechamber,
    room46,
    bedroom,
    boudoir,
    guestBedroom,
    nursery,
    servantsQuarters,
    bunkRoom,
    herLadyshipsChamber,
    masterBedroom,
    hallway,
    westWingHall,
    eastWingHall,
    corridor,
    passageway,
    secretPassage,
    foyer,
    greatHall,
    terrace,
    patio,
    courtyard,
    cloister,
    veranda,
    greenhouse,
    morningRoom,
    secretGarden,
    commissary,
    kitchen,
    locksmith,
    showroom,
    laundryRoom,
    bookshop,
    theArmory,
    giftShop,
    lavatory,
    chapel,
    maidsChamber,
    archives,
    gymnasium,
    darkroom,
    weightRoom,
    furnace,
    dovecote,
    theKennel,
    clockTower,
    classroom,
    solarium,
    dormitory,
    vestibule,
    casino,
    planetarium,
    mechanarium,
    treasureTrove,
    throneRoom,
    tunnel,
    conservatory,
    lostAndFound,
    closedExhibit,
    toolshed,
    shelter,
    schoolhouse,
    shrine,
    rootCellar,
    hovel,
    tradingPost,
    tomb
  ]
