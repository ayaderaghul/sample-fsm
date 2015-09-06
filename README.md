# sample-fsm

this is sample code of finite state automata playing a repeated game.

the population evolves over time.

time is counted by cycles.

details are written in file fsm0.rkt

# how to run

in terminal, run `rlwrap racket`

in racket, 

```
(load "fsm0.rkt") ; there is already a population of fsm generated named A
(evolve A 1000 10 20)  
;; evolve population A over 1000 cycles, with the learning speed of 10, rounds per match 20
;; learning speed = 10 means that at the end of each cycle 10 out of 100 are killed
;; these 10 are replaced by randomising over fitness vector.
;; interperetation : at the end of each cycle, 10 agents observe the population (fitness vector)
;; and decide to change their automata with propensity equal fitness.
;; a stronger automaton will be more likely to be ressurrect 
;; hence over time, the stronger will increase in size
;; the weaker dies away

(plot-mean population-mean) ; plot the population mean over 1000 cycles
```
