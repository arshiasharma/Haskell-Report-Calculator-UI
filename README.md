# cpsc354_report

This repository contains the Latex file of my report, a PDF, and the final project code. To run the final project code, cd into the FinalProject folder and run the following commands:

* cabal new-build
* cabal new-run threep

The Calculator interface will then be viewable from: http://127.0.0.1:8023/

Note: You may see some warnings from the terminal relating to import statements for Graphics.UI.Threepenny, this should not effect the operation of the code, this just means that the UI graphics were not imported correctly, but the base layout you see in Figure 7 of the report should be there. 


Sources used include:
https://hackage.haskell.org/package/threepenny-gui
http://wiki.haskell.org/Threepenny-gui
https://github.com/thma/ThreepennyElectron
https://github.com/HeinrichApfelmus/threepenny-gui
https://github.com/astynax/threep
