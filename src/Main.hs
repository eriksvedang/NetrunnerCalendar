{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Random
import System.Random.Shuffle
import Lucid
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.Text as Text

sets = ["Original Core Set",
        "Genesis",
        "What Lies Ahead",
        "Trace Amount",
        "Cyber Exodus",
        "A Study in Static",
        "Humanity's Shadow",
        "Future Proof",
        "Creation and Control",
        "Spin",
        "Opening Moves",
        "Second Thoughts",
        "Mala Tempora",
        "True Colors",
        "Fear and Loathing",
        "Double Time",
        "Honor and Profit",
        "Lunar",
        "Upstalk",
        "The Spaces Between",
        "First Contact",
        "Up and Over",
        "All That Remains",
        "The Source",
        "Order and Chaos",
        "SanSan",
        "The Valley",
        "Breaker Bay",
        "Chrome City",
        "The Underway",
        "Old Hollywood",
        "The Universe of Tomorrow",
        "Data and Destiny",
        "Mumbad",
        "Kala Ghoda",
        "Business First",
        "Democracy and Dogma",
        "Salsette Island",
        "The Liberated Mind",
        "Fear the Masses",
        "Flashpoint",
        "23 Seconds",
        "Blood Money",
        "Escalation",
        "Intervention",
        "Martial Law",
        "Quorum",
        "Red Sand",
        "Daedalus Complex",
        "Station One",
        "Earth's Scion",
        "Blood and Water",
        "Free Mars",
        "Crimson Dust",
        "Terminal Directive",
        "Revised Core Set",
        "Kitara",
        "Sovereign Sight",
        "Down the White Nile",
        "Council of the Crest",
        "The Devil and the Dragon",
        "Whispers in Nalubaale",
        "Kampala Ascendent"
        -- "Reign and Reverie"
       ]

combinations =
  do a <- sets
     b <- sets
     c <- sets
     return (a, b, c)

page :: String -> Html ()
page gameFormat =
  do html_ $
       do head_ $
            do meta_ [charset_ "UTF-8"]
               meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=0"]
               link_ [rel_ "stylesheet", href_ "carp_style.css"]
          body_ $
            do h1_ (toHtml (Text.pack gameFormat))

main :: IO ()
main = do
  gen <- newStdGen
  let shuffled = shuffle' combinations (length combinations) gen
      gameFormat = (show (head shuffled))
  writeFile "index.html" (T.unpack (renderText (page gameFormat)))
