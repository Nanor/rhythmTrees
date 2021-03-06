**Usage:**

`cabal run compare <file1> <file2>`
Compares the two midi files, returns a numerical representation of the different between the two rhythm trees

`cabal run tree <file1>`
Prints the data structure representing the rhythm tree of the midi file

`cabal run draw <file1>`
Prints an ascii representation of the data structure representing the rhythm tree of the midi file

`cabal run play <file1>`
Plays the midi file

`cabal run markov <comparison method> <file1> <file2> ...`
Uses markov chains to generate 10 bars of rhythms based on the supplied midi files. Plays the rhythm and prints the data structure.

Comparison Methods: 
- `siblings` Decides on transition probabilites based on note depth, how many siblings is has, and its position in those siblings
- `path` Decides on transition probabilites based on its absoulte position in the bar's rhythm tree

**Mac users**
To allow midi playback a midi player must be running, the simplest method to do this is to install simpleSynth. (`brew cask install simplesynth`)