# cpsc354 Report and Final Project

This repository contains the Latex file of my report, a PDF, and the final project code. 

## How to run the Final project
To run the final project code, run the following commands:
* cd FinalProject
* cabal install threepenny-gui -fbuildExamples
* cabal new-build
* cabal new-run threep

The Calculator interface will then be viewable from: http://127.0.0.1:8023/

### Please Note
You may see some warnings from the terminal relating to import statements for Graphics.UI.Threepenny, this should not effect the operation of the code, this just means that the UI graphics were not imported correctly, but the base layout you see in Figure 7 of the report should be there. 

## Sources
Sources used for the final project include:
* https://hackage.haskell.org/package/threepenny-gui
* http://wiki.haskell.org/Threepenny-gui
* https://github.com/HeinrichApfelmus/threepenny-gui
* https://github.com/astynax/threep
* https://github.com/thma/ThreepennyElectron
