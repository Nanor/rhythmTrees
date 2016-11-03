module EuterpeaTest where

import Euterpea

cMajScale = Modify (Tempo 2)
           (line [c 4 qn, d 4 en, e 4 en, f 4 en,
           g 4 en, a 4 en, b 4 en, c 5 en])


main = play cMajScale