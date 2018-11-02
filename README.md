Place the csd folder in your Racket collects. Run markovinator.rkt and markovinator.maxpat
Connect both programs to the same ports, and start inputting midi into Max' midiin 
Output will be generated from Max' midiout

racket -e '(load "markovinator_v2.rkt") (connect 12121 21212) (markovinator x)' will do the same job as well. 
Replace x with the desired markov order (x=0 will be first order)
