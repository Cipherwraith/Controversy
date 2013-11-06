module Main where

import Control.Arrow
import Data.List

-- Build a datatype to hold episode score data
data EpisodeScore = EpisodeScore {
                        _episodeName :: String,
                        _1 :: Integer,
                        _2 :: Integer,
                        _3 :: Integer,
                        _4 :: Integer,
                        _5 :: Integer,
                        _average :: Double
                        } deriving (Show, Eq, Ord)

-- This gets the average of the average scores in a list
listAverage = (\x -> (sum $ map _average x) / (fromIntegral $ length x :: Double))

-- Calculate the average for the controversial data list
cDataAverage = listAverage controversialData

-- Give weight to the votes.
-- Voted number:   1  2 3 4 5 
-- Number weight: -5 -3 1 3 5
-- A vote for 3 is equal to 1 point, vote for 1 is equal to -5 points
voteWeight :: EpisodeScore -> Integer
voteWeight = (\x -> sum [ _1 x * (-5)
                        , _2 x * (-3)
                        , _3 x * 1
                        , _4 x * 3
                        , _5 x * 5])

-- Calculate a weighted average score for each episode
weightedAverage :: EpisodeScore -> Double
weightedAverage = (\x -> fromIntegral (voteWeight x) / (fromIntegral $ sumOfVotes x))

-- Find the average amount of votes per episode
calculateAverageAmtOfVotes :: [EpisodeScore] -> Double
calculateAverageAmtOfVotes = (\x -> (fromIntegral . sum . map sumOfVotes $ x) / (fromIntegral . length $ x))

-- Add up the amount of votes for an episode
sumOfVotes = (\x -> sum [_1 x, _2 x, _3 x, _4 x, _5 x])

-- Calculate the bayesian average for the weighted data
c = calculateAverageAmtOfVotes controversialData
m = cDataAverage
rating :: EpisodeScore -> Double
rating x = ( (m * c) + (weightedAverage x) * (fromIntegral $ sumOfVotes x) ) / ( c + (fromIntegral $ sumOfVotes x) )

-- Apply the controversy formula
addControversy :: (Double, EpisodeScore) -> (Double, EpisodeScore)
addControversy (x, y) = ( ( (fromIntegral $ sumOfVotes y) / x) , y)

-- Do our calculations. Less controversial at the top, more controversial at the bottom
calculateAndSort :: [EpisodeScore] -> [(Double, EpisodeScore)]
calculateAndSort = sort . map ( addControversy . (rating &&& id))

-- Pretty Print Function
prettyPrint :: (Double, EpisodeScore) -> String
prettyPrint x = concat  [ _episodeName epDat
                        , " | "
                        , show $ sumOfVotes epDat
                        , " | ("
                        , intercalate "," $ map show [ _1 epDat
                                                     , _2 epDat
                                                     , _3 epDat
                                                     , _4 epDat
                                                     , _5 epDat] 
                        , ") | " 
                        , ( take 8 . show $ fst x)
                        , "\n" 
                        ]
  where
    epDat = snd x

main = putStrLn . concat $ header : (map prettyPrint $ calculateAndSort controversialData)
  

header = "Episode | Total Responses | Vote Spread | Controversy Rating (least to most controversial)\n------- | --------------- | ----------- | ------------------------------------------------\n"

-- Need to calculate the bayesian average for each episode.
-- Then do the (totalVotes / bayesianAverage) to get a controversial score
--EpisodeName 1 2 3 4 5 Average rating
controversialData :: [EpisodeScore]
controversialData = [
  EpisodeScore "S08E25 Inverted Underwater Car" 0 0 1 4 5 4.40,
  EpisodeScore "S08E24 Reverse Engineering" 0 0 2 3 6 4.36,
  EpisodeScore "S04E07 Bullets Fired Up" 0 0 1 3 3 4.29,
  EpisodeScore "S04E01 Paper Crossbow" 0 1 1 2 4 4.13,
  EpisodeScore "S08E26 Bug Special" 0 0 4 3 5 4.08,
  EpisodeScore "S03E14 Jet Pack" 0 1 0 5 2 4.00,
  EpisodeScore "S04E13 Killer Whirlpool" 0 1 2 1 4 4.00,
  EpisodeScore "S06E09 Exploding Steak" 0 0 2 3 2 4.00,
  EpisodeScore "S08E22 Arrow Machine Gun" 0 0 4 2 4 4.00,
  EpisodeScore "S07E21 Unarmed and Unharmed" 0 1 3 2 4 3.90,
  EpisodeScore "S01E18 Best Electric Myths" 0 1 2 3 3 3.89,
  EpisodeScore "S04E17 Earthquake Machine" 0 1 2 1 3 3.86,
  EpisodeScore "S04E20 Killer Cable Snaps" 0 1 1 3 2 3.86,
  EpisodeScore "S05E01 Hindenburg Mystery" 0 1 0 5 1 3.86,
  EpisodeScore "S09E19 Location, Location, Location" 1 1 3 2 6 3.85,
  EpisodeScore "S09E20 Wet and Wild" 1 1 3 2 6 3.85,
  EpisodeScore "S01E08 Buried Alive" 0 1 4 2 4 3.82,
  EpisodeScore "S08E20 Cold Feet" 0 0 5 3 3 3.82,
  EpisodeScore "S01E17 Best Animal Myths" 0 1 2 4 2 3.78,
  EpisodeScore "S06E16 Alcohol Myths" 0 1 3 2 3 3.78,
  EpisodeScore "S07E14 Dirty vs. Clean Car" 0 1 4 0 4 3.78,
  EpisodeScore "S03E13 Breaking Glass" 1 1 1 1 4 3.75,
  EpisodeScore "S04E02 Shredded Plane" 0 2 1 2 3 3.75,
  EpisodeScore "S05E05 Dog Myths" 0 0 3 4 1 3.75,
  EpisodeScore "S06E18 Coffin Punch" 0 0 4 2 2 3.75,
  EpisodeScore "S03E25 Steel Toe Amputation" 0 2 1 1 3 3.71,
  EpisodeScore "S04E16 Crimes and Myth-demeanors 2" 1 0 1 3 2 3.71,
  EpisodeScore "S05E25 Air Plane Hour" 0 1 2 2 2 3.71,
  EpisodeScore "S06E14 Blind Driving" 0 0 4 1 2 3.71,
  EpisodeScore "S03E26 Seasickness - Kill or Cure" 0 2 0 2 2 3.67,
  EpisodeScore "S05E11 Big Rig Myths" 0 1 2 1 2 3.67,
  EpisodeScore "S07E16 Hurricane Windows" 0 1 3 3 2 3.67,
  EpisodeScore "S01E06 Barrel of Bricks" 0 3 2 2 4 3.64,
  EpisodeScore "S02E09 Pingpong Rescue" 0 2 2 5 2 3.64,
  EpisodeScore "S07E12 Knock Your Socks Off" 0 2 1 3 2 3.63,
  EpisodeScore "S08E23 Mini Myth Madness" 0 1 4 3 2 3.60,
  EpisodeScore "S01E05 Cell Phone Destroys Gas Station" 1 2 2 3 4 3.58,
  EpisodeScore "S03E24 Vodka Myths" 0 1 2 3 1 3.57,
  EpisodeScore "S04E15 Shattering Subwoofer" 1 0 2 2 2 3.57,
  EpisodeScore "S04E21 Air Cylinder Rocket" 0 1 2 3 1 3.57,
  EpisodeScore "S02E10 Boom Lift Catapult" 0 2 3 1 3 3.56,
  EpisodeScore "S03E23 Confederate Rocket" 0 1 2 6 0 3.56,
  EpisodeScore "S02E06 Beat the Radar Detector" 0 3 3 1 4 3.55,
  EpisodeScore "S01E03 Poppy-Seed Drug Test" 1 1 4 3 3 3.50,
  EpisodeScore "S02E01 Myths Revisited" 0 1 5 2 2 3.50,
  EpisodeScore "S03E16 Bullet Proof Water" 0 2 2 2 2 3.50,
  EpisodeScore "S05E21 Super Sized Myths!" 1 0 1 3 1 3.50,
  EpisodeScore "special Common Car Myths Special" 0 0 5 2 1 3.50,
  EpisodeScore "S07E18 Myth Evolution 2" 0 1 5 2 2 3.50,
  EpisodeScore "S07E24 Boomerang Bullet" 0 2 3 3 2 3.50,
  EpisodeScore "S02E05 Elevator of Death, Levitation Machine" 0 3 4 0 4 3.45,
  EpisodeScore "S07E22 Hidden Nasties" 0 2 3 2 2 3.44,
  EpisodeScore "S04E08 Myths Reopened" 1 0 3 1 2 3.43,
  EpisodeScore "S04E25 Firearms Folklore" 0 2 1 3 1 3.43,
  EpisodeScore "S04E28 22,000 Foot Fall" 0 1 2 4 0 3.43,
  EpisodeScore "S05E06 More Myths Reopened" 1 0 2 3 1 3.43,
  EpisodeScore "S05E12 Grenades and Guts" 0 1 3 2 1 3.43,
  EpisodeScore "S05E13 Snow Special" 0 1 3 2 1 3.43,
  EpisodeScore "S08E27 President's Challenge" 2 1 1 3 3 3.40,
  EpisodeScore "S04E24 Concrete Glider" 1 0 4 1 2 3.38,
  EpisodeScore "S04E26 Anti-Gravity Device" 1 1 2 2 2 3.38,
  EpisodeScore "S07E09 Prison Escape" 0 3 1 2 2 3.38,
  EpisodeScore "S02E12 Ming Dynasty Astronaut" 1 1 4 3 2 3.36,
  EpisodeScore "S09E10 Planes, Trains and Automobiles" 2 1 3 1 4 3.36,
  EpisodeScore "S01E09 Lightning Strikes / Tongue Piercing" 0 2 4 1 2 3.33,
  EpisodeScore "S03E17 MythBusters Jaws Special" 1 1 0 3 1 3.33,
  EpisodeScore "S03E20 Escape Slide Parachute" 0 0 4 2 0 3.33,
  EpisodeScore "S04E19 Mega Movie Myths" 1 0 1 4 0 3.33,
  EpisodeScore "S05E16 Red Flag to a Bull" 0 2 1 2 1 3.33,
  EpisodeScore "S05E19 Trail Blazers" 0 1 3 1 1 3.33,
  EpisodeScore "S05E23 Pirates 2" 1 0 2 2 1 3.33,
  EpisodeScore "S06E11 Viral Hour" 0 0 4 2 0 3.33,
  EpisodeScore "S06E15 Ninjas 2" 0 0 4 2 0 3.33,
  EpisodeScore "S05E02 Pirate Special" 1 0 3 2 1 3.29,
  EpisodeScore "S05E04 Speed Cameras" 0 1 3 3 0 3.29,
  EpisodeScore "S07E20 Antacid Jail Break" 0 4 2 3 2 3.27,
  EpisodeScore "S03E02 Ultimate MythBusters" 1 0 4 2 1 3.25,
  EpisodeScore "S05E24 Confederate Steam Gun" 0 2 4 0 2 3.25,
  EpisodeScore "S07E06 Exploding Bumper" 0 2 4 0 2 3.25,
  EpisodeScore "S10E13 Jawsome Shark Special" 2 3 1 2 4 3.25,
  EpisodeScore "S02E11 Exploding House" 0 3 3 1 2 3.22,
  EpisodeScore "S03E08 Cooling a Six-Pack" 0 1 5 3 0 3.22,
  EpisodeScore "S07E15 Greased Lightning" 0 2 4 2 1 3.22,
  EpisodeScore "S07E23 Mini Myth Mayhem" 0 2 5 0 2 3.22,
  EpisodeScore "S02E03 Scuba Diver, Car Capers" 0 3 4 1 2 3.20,
  EpisodeScore "S02E07 Quicksand, Bathtub Electrocution, MRI Rays and Tattoos" 0 2 5 2 1 3.20,
  EpisodeScore "S05E10 Western Myths" 0 2 2 1 1 3.17,
  EpisodeScore "S05E18 Myth Evolution!" 1 0 3 1 1 3.17,
  EpisodeScore "S03E21 MythBusters Revisited" 1 1 2 2 1 3.14,
  EpisodeScore "S04E12 Steam Cannon" 1 0 4 1 1 3.14,
  EpisodeScore "S04E18 Deadly Straw" 1 2 1 1 2 3.14,
  EpisodeScore "S05E20 Exploding Water Heater" 1 1 3 0 2 3.14,
  EpisodeScore "S03E22 Chinese Invasion Alarm" 1 2 2 1 2 3.13,
  EpisodeScore "S06E20 Viewer Special Threequel" 1 1 3 2 1 3.13,
  EpisodeScore "S07E17 Crash and Burn" 0 2 5 1 1 3.11,
  EpisodeScore "S02E13 Christmas Special" 2 1 3 1 2 3.00,
  EpisodeScore "S03E04 Salsa Escape" 1 4 2 0 3 3.00,
  EpisodeScore "S03E07 MythBusters Outtakes" 0 3 3 1 1 3.00,
  EpisodeScore "S03E11 MythBusters: Revealed" 2 1 1 1 2 3.00,
  EpisodeScore "S03E12 Hollywood On Trial" 2 0 2 2 1 3.00,
  EpisodeScore "S03E15 Killer Brace Position" 1 2 3 0 2 3.00,
  EpisodeScore "S03E18 Border Slingshot" 0 1 4 1 0 3.00,
  EpisodeScore "S04E10 Exploding Pants" 0 3 2 1 1 3.00,
  EpisodeScore "S05E17 Superhero Hour" 1 0 3 2 0 3.00,
  EpisodeScore "S07E13 Duct Tape Hour" 1 2 3 2 1 3.00,
  EpisodeScore "special Buster's Cut: Duct Tape Hour 1" 3 1 1 3 2 3.00,
  EpisodeScore "S08E28 Green Hornet Special" 3 2 3 2 3 3.00,
  EpisodeScore "special Buster's Cut: Spy Car Escape" 3 1 2 2 2 2.90,
  EpisodeScore "S03E10 Shop 'til You Drop" 1 1 4 0 1 2.86,
  EpisodeScore "S04E05 Franklin's Kite" 0 2 4 1 0 2.86,
  EpisodeScore "special MythBusters Young Scientists Special" 2 2 0 1 2 2.86,
  EpisodeScore "S03E19 Killer Tissue Box" 1 0 4 1 0 2.83,
  EpisodeScore "S04E11 Crimes and Myth-demeanors" 1 0 4 1 0 2.83,
  EpisodeScore "S05E15 Viewer Special" 1 0 4 1 0 2.83,
  EpisodeScore "S03E06 Is Yawning Contagious?" 2 1 4 3 0 2.80,
  EpisodeScore "S07E10 Curving Bullets" 2 3 1 1 2 2.78,
  EpisodeScore "S11E08 Explosions A to Z" 4 2 2 1 3 2.75,
  EpisodeScore "S04E09 Mind Control" 2 1 2 1 1 2.71,
  EpisodeScore "S03E09 Son of a Gun" 2 2 4 1 1 2.70,
  EpisodeScore "S05E14 Baseball Myths" 1 1 3 1 0 2.67,
  EpisodeScore "S05E22 Shooting Fish in a Barrel" 1 1 3 1 0 2.67,
  EpisodeScore "S03E03 Brown Note" 2 1 3 2 0 2.63,
  EpisodeScore "S04E04 Helium Football" 0 5 1 2 0 2.63,
  EpisodeScore "special Buster's Cut: Unarmed and Unharmed" 3 3 1 1 2 2.60,
  EpisodeScore "S04E27 Holiday Special" 2 1 2 2 0 2.57,
  EpisodeScore "S05E09 Walking On Water" 2 1 2 2 0 2.57,
  EpisodeScore "S02E02 Best Explosions" 3 3 0 1 2 2.56,
  EpisodeScore "S03E05 Exploding Port-a-Potty" 1 2 5 0 0 2.50,
  EpisodeScore "S04E06 Cell Phones on Planes" 1 3 1 0 1 2.50,
  EpisodeScore "S08E17 Buster's Cut: Bottle Bash" 3 2 3 1 1 2.50,
  EpisodeScore "special Buster's Cut: Phone Book Friction" 3 3 1 2 1 2.50,
  EpisodeScore "S08E13 Buster's Cut: Alcohol Myths" 4 3 1 1 2 2.45,
  EpisodeScore "S03E01 Buster Special" 3 1 3 2 0 2.44,
  EpisodeScore "special Buster's Cut: Curving Bullets" 3 3 0 2 1 2.44,
  EpisodeScore "S05E07 Voice Flame Extinguisher" 2 0 5 0 0 2.43,
  EpisodeScore "S08E15 Buster's Cut: Knock Your Socks Off" 3 3 2 1 1 2.40,
  EpisodeScore "S08E16 Buster's Cut: Viewer Special Threequel" 3 3 2 1 1 2.40,
  EpisodeScore "S06E19 End With A Bang" 4 0 1 3 0 2.38
  ]
