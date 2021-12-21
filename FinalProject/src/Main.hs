module Main where

import           Control.Monad               (void)
import           Graphics.UI.Threepenny      (set, attr, title, text, value, style,
                                              (#), (#+), (<@), (#.),
                                              Event, Element, UI, Window,
                                              sink, accumB)
import           Graphics.UI.Threepenny      hiding (map, start, Color, color )
import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core (startGUI, defaultConfig)

import           Calculator
import           Data.Char                   (toLower)


--  entry point from main.js launch script
main :: IO ()
main = startGUI defaultConfig setup

-- setup window layout and import css file

setup :: Window -> UI ()
setup win = void $ do
  -- define page
  _ <- return win # set title "Haskell Calculator"
  UI.addStyleSheet win "layout.css"

  -- UI controls
  outputBox <- UI.input
                # set (attr "readonly") "true"
                # set (attr "style") "text-align: right; min-width: 400px; min-height: 40px; font-size:24px"
                # set (attr "align") "center"

  -- button grid
  buttons   <- mapM (mapM mkButton) buttonDefinitions

  -- define page DOM with html combinators
  _ <- getBody win # set (attr "style") "overflow: hidden; align: center" #+
    [ UI.div #. "ui raised very padded text container segment" #+
      [UI.table #+ [UI.row [UI.div #. "ui input" #+ [element outputBox]]] #+ 
                    map (UI.row . map element) buttons]
    ]

  let  
      buttonMap = zip (concat buttons) (concatMap (map fst) buttonDefinitions)

      -- register mouse click events to all buttons
      clicks  = buttonClicks buttonMap

      -- use processCommand function to build the equation
      commands  = fmap processCommand clicks

  -- calculate behaviour by accumulating all commands  
  calcBehaviour <- accumB def commands

  let outText  = fmap display calcBehaviour

  -- output textbox
  element outputBox # sink value outText

  -- button UI
  where
    mkButton :: (Command, Color) -> UI Element
    mkButton (cmd, clr) =
      let btnLabel = labels cmd -- get the button text
      in  UI.button #. ("ui " ++ color clr ++ " button")
                    # set text btnLabel # set value btnLabel
                    # set (attr "type")  "button"
                    # set (attr "style") "min-width: 80px; min-height: 60px; font-size: 24px"
                    # set style [("color","blue")]

    color :: Color -> String
    color = map toLower . show

    -- buttons on the app
    buttonDefinitions :: [[(Command, Color)]]
    buttonDefinitions =
      [ [(Digit Seven, Grey), (Digit Eight, Grey), (Digit Nine,  Grey), (Operation Add, Grey), (Operation Sub, Grey)]
      , [(Digit Four,  Grey), (Digit Five,  Grey), (Digit Six,   Grey), (Operation Mul, Grey), (Operation Div, Grey)]
      , [(Digit One,   Grey), (Digit Two,   Grey), (Digit Three, Grey), (Operation Exp, Grey), (Operation Mod, Grey)]
      , [(Dot,  Grey),        (Digit Zero,  Grey), (ClearError,   Grey), (Clear,        Grey), (Equal, Grey)] ]

    -- function to handle when the button is clicked
    buttonClicks :: [(Element, Command)] -> Event Command
    buttonClicks = foldr1 (UI.unionWith const) . map makeClick
      where
        makeClick (elmnt, cmd) = UI.pure cmd <@ UI.click elmnt

-- button background color
data Color = Grey deriving (Show, Eq)

