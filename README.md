# StarFinder: Identifying Constellations from a Subset of Stars


Input cartesian coordinates of stars, and the program will output possible constellations where the stars belong. Although there is no limitations on what the input can be, the ideal use case is to in put locations of actual stars. With random input, the program would still spit out the possible constellations but with low confidence. Therefore, one set of possible inputs for better results is in input.txt.

To run the program on input.txt, simply run
ghc −threaded −eventlog −rtsopts −−make parse.hs
./parse +RTS −ls −N4 < input.txt
