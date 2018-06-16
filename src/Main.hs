{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Random
import System.Random.Shuffle
import Lucid
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.Text as Text
import qualified Data.Set as Set

deluxe = ["Creation and Control"
         ,"Honor and Profit"
         ,"Data and Destiny"
         ,"Order and Chaos"
         ,"Democracy and Dogma"
         ,"Reign and Reverie"
         ]

cycles = ["Genesis","Spin","Lunar","SanSan","Mumbad","Flashpoint","Red Sand","Kitara","Original Core Set","Terminal Directive"]

cycleOfPack pack =
  case pack of
    "What Lies Ahead" -> "Genesis"
    "Trace Amount" -> "Genesis"
    "Cyber Exodus" -> "Genesis"
    "A Study in Static" -> "Genesis"
    "Humanity's Shadow" -> "Genesis"
    "Future Proof" -> "Genesis"
    "Opening Moves" -> "Spin"
    "Second Thoughts" -> "Spin"
    "Mala Tempora" -> "Spin"
    "True Colors" -> "Spin"
    "Double Time" -> "Spin"
    "Fear and Loathing" -> "Spin"
    "Upstalk" -> "Lunar"
    "The Spaces Between" -> "Lunar"
    "First Contact" -> "Lunar"
    "Up and Over" -> "Lunar"
    "All That Remains" -> "Lunar"
    "The Source" -> "Lunar"
    "The Valley" -> "SanSan"
    "Breaker Bay" -> "SanSan"
    "Chrome City" -> "SanSan"
    "The Underway" -> "SanSan"
    "Old Hollywood" -> "SanSan"
    "The Universe of Tomorrow" -> "SanSan"
    "Kala Ghoda" -> "Mumbad"
    "Business First" -> "Mumbad"
    "Salsette Island" -> "Mumbad"
    "The Liberated Mind" -> "Mumbad"
    "Fear the Masses" -> "Mumbad"
    "Democracy and Dogma" -> "Mumbad"
    "23 Seconds" -> "Flashpoint"
    "Blood Money" -> "Flashpoint"
    "Escalation" -> "Flashpoint"
    "Intervention" -> "Flashpoint"
    "Martial Law" -> "Flashpoint"
    "Quorum" -> "Flashpoint"
    "Daedalus Complex" -> "Red Sand"
    "Station One" -> "Red Sand"
    "Earth's Scion" -> "Red Sand"
    "Blood and Water" -> "Red Sand"
    "Free Mars" -> "Red Sand"
    "Crimson Dust" -> "Red Sand"
    "Sovereign Sight" -> "Kitara"
    "Down the White Nile" -> "Kitara"
    "Council of the Crest" -> "Kitara"
    "The Devil and the Dragon" -> "Kitara"
    "Whispers in Nalubaale" -> "Kitara"
    "Kampala Ascendent" -> "Kitara"
    "Original Core Set" -> ""
    missing -> error missing

packs = ["Original Core Set",

         "What Lies Ahead",
         "Trace Amount",
         "Cyber Exodus",
         "A Study in Static",
         "Humanity's Shadow",
         "Future Proof",

         "Opening Moves",
         "Second Thoughts",
         "Mala Tempora",
         "True Colors",
         "Double Time",
         "Fear and Loathing",

         "Upstalk",
         "The Spaces Between",
         "First Contact",
         "Up and Over",
         "All That Remains",
         "The Source",

         "The Valley",
         "Breaker Bay",
         "Chrome City",
         "The Underway",
         "Old Hollywood",
         "The Universe of Tomorrow",

         "Kala Ghoda",
         "Business First",
         "Salsette Island",
         "The Liberated Mind",
         "Fear the Masses",
         "Democracy and Dogma",

         "23 Seconds",
         "Blood Money",
         "Escalation",
         "Intervention",
         "Martial Law",
         "Quorum",

         "Daedalus Complex",
         "Station One",
         "Earth's Scion",
         "Blood and Water",
         "Free Mars",
         "Crimson Dust",

         "Sovereign Sight",
         "Down the White Nile",
         "Council of the Crest",
         "The Devil and the Dragon",
         "Whispers in Nalubaale",
         "Kampala Ascendent"
        ]

combinations =
  do a <- deluxe
     c <- packs
     b <- (removeItem (cycleOfPack c) cycles) -- Prevent choosing the same cycle that the pack is in
     return (Set.fromList [a, b, c])

uniqueCombinations =
  Set.toList (Set.fromList combinations)

splitEvery _ [] = []
splitEvery n list = first : (splitEvery n rest)
  where
    (first,rest) = splitAt n list

removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys

page :: [(String, Set.Set String)] -> Html ()
page gameFormats =
  do html_ $
       do head_ $
            do meta_ [charset_ "UTF-8"]
               meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=0"]
               link_ [rel_ "stylesheet", href_ "style.css"]
          body_ $
            do mapM_ (\(date, format) -> (p_ (toHtml (date ++ ": " ++ (show (Set.toList format)))))) gameFormats

dates = do
  year <- [2018..]
  month <- [1..]
  day <- [1..31]
  return (show year ++ "-" ++ show month ++ "-" ++ show day)

main :: IO ()
main = do
  gen <- newStdGen
  let --xs = deluxe ++ cycles
      xs = uniqueCombinations
      shuffled = shuffle' xs (length xs) gen
      --gameFormat = map (\xs -> (Set.fromList xs)) (splitEvery 3 shuffled)
      gameFormat = zip dates shuffled
  putStrLn ("Total count: " ++ show (length gameFormat))
  writeFile "index.html" (T.unpack (renderText (page gameFormat)))
