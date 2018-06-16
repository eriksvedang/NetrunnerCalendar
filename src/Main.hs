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
         ]

pack = ["Original Core Set",

        "Genesis",
        "What Lies Ahead",
        "Trace Amount",
        "Cyber Exodus",
        "A Study in Static",
        "Humanity's Shadow",
        "Future Proof",

        "Spin",
        "Opening Moves",
        "Second Thoughts",
        "Mala Tempora",
        "True Colors",
        "Double Time",
        "Fear and Loathing",

        "Lunar",
        "Upstalk",
        "The Spaces Between",
        "First Contact",
        "Up and Over",
        "All That Remains",
        "The Source",

        "SanSan",
        "The Valley",
        "Breaker Bay",
        "Chrome City",
        "The Underway",
        "Old Hollywood",
        "The Universe of Tomorrow",

        "Mumbad",
        "Kala Ghoda",
        "Business First",
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

        --"Reign and Reverie"
       ]

combinations =
  do a <- deluxe
     b <- pack
     c <- pack
     return (Set.fromList [a, b, c])

uniqueCombinations =
  Set.toList (Set.fromList combinations)

page :: [Set.Set String] -> Html ()
page gameFormats =
  do html_ $
       do head_ $
            do meta_ [charset_ "UTF-8"]
               meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=0"]
               link_ [rel_ "stylesheet", href_ "style.css"]
          body_ $
            do mapM_ (\format -> (p_ (toHtml (show (Set.toList format))))) gameFormats

main :: IO ()
main = do
  gen <- newStdGen
  let shuffled = shuffle' uniqueCombinations (length uniqueCombinations) gen
      gameFormat = shuffled
  putStrLn ("Total count: " ++ show (length shuffled))
  writeFile "index.html" (T.unpack (renderText (page gameFormat)))
