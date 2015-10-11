#lang racket

;; INTRODUCTION
;; I generate a population of finite state automata randomly
;; in each cycle, they are pair-matched to play a repeated game
;; their *fitness* is their relative payoff.
;; At the end of the cycle, I kill randomly 10%
;; and resurrect an equivalent amount to keep the population constant.
;; Automata are ressurrect based on their *fitness*.

;; Technically, the fitness vector is a vector that sums up to 1,
;; and we randomised over this vector to choose which automata to ressurrect
;; independently.

;; The game is a 2 player game,
;; each player has 2 possible stragies {Cooperate, Defect},
;; and they move simultaneously.
;; Here is the payoff matrix:
;;               Cooperate      Defect
;; Cooperate        3,3           0,4
;; Defect           4,0           1,1
;; This is a very familiar game in social science.
;; If both players "cooperate", they both get the reward of 3.
;; If one cooperates and the other defects, the cooperator is called "sucker"
;; because he gets 0 and the other gets 4. The payoff 4 is called "temptation".
;; If both defect, they both get the "punishment" payoff of 1.
;; This game captures the tradeoff between self interest (temptation)
;; and social welfare (both get reward).

(module automaton racket
  (provide (all-defined-out))

  ;; AUTOMATON
  (struct automaton (current-state states) #:transparent)
  (struct state (name result0 result1) #:transparent)
  ;; mutable data structure seems very hard to handle later on
  ;; Automaton = (automaton-current-state-id [list of states])
  ;; State = (state-label result-of-action0 result-of-action1)

  ;; in this example, there are 2 possible strategies (actions) for each player
  ;; 2 actions are {cooperate, defect}
  ;; let cooperate = 0; defect = 1
  ;; the action set is {0,1}

  ;; hence in each state, we only need to prescribe the action
  ;; of the automaton which is 0 or 1 (state-name)
  ;; plus the result of the opponent action 0 and 1 (result0 and result1).

  ;; coincidently, in this example, each automaton has 2 states
  ;; WARNING:
  ;; the automaton-current-state-id =/= the name of the current state
  ;; the automaton-current-state gives us the id of the state
  ;; we have to call the state-name to see what its current action is

  (define (automaton-current-strat an-auto)
    (define result-state-id (automaton-current-state an-auto))
    (define states (automaton-states an-auto))
    (define result-state (list-ref states result-state-id))
    (state-name result-state))

  (define (create-automaton* init-state
                             state0 state1
                             result00 result01
                             result10 result11)
    (automaton init-state
               (list (state state0 result00 result01)
                     (state state1 result10 result11)
                     )))

  (define (create-automaton)
    (create-automaton* (random 2) ;; number 2 here is 2 states
                       (random 2) (random 2) ;; number 2 here is 2 strats
                       (random 2) (random 2) ;; number 2 here is 2 states
                       (random 2) (random 2) ;; number 2 here is 2 states
                       ))

  ;; POPULATION
  (define (create-population N)
    (for/list ([n N])
      (create-automaton)))

  ;; AUTOMATON BEHAVIOR
  (define (jump-to-state an-event an-auto)
    (define current-state-id (automaton-current-state an-auto))
    (define states (automaton-states an-auto))
    (define result-state (list-ref states current-state-id))
    (if (zero? an-event)
        (state-result0 result-state)
        (state-result1 result-state)))

  (define (react an-event an-auto)
    (define states (automaton-states an-auto))
    (define next-state-id (jump-to-state an-event an-auto))
    (state-name (list-ref states next-state-id)))

  (define (update old-auto new-state)
    (struct-copy automaton old-auto
                 [current-state new-state]))

  ;; CLASSIC AUTOMATA
  ;; let 0 = cooperate, 1 = defect
  ;; some classic automata
  ;; the all defector has 2 states of cooperate and defect
  ;; but it always defects, no matter what
  ;; the opponent may play cooperate or defect
  ;; it doesnt care, it always stay in the state defect
  (define all-defects (create-automaton* 1 0 1 1 1 1 1))
  (define all-cooperates (create-automaton* 0 0 1 0 0 0 0))
  ;; the tit for tat starts out optimistic, it cooperates initially
  ;; however, if the opponent defects, it punishes by switching to defecting
  ;; if the opponent cooperates, it returns to play cooperate again
  (define tit-for-tat (create-automaton* 0 0 1 0 1 0 1))
  ;; the grim trigger also starts out optimistic,
  ;; but the opponent defects for just once then
  ;; it jumps to defect forever
  ;; it doesnt forgive, and doesnt forget
  (define grim-trigger (create-automaton* 0 0 1 0 1 1 1)))

;; TEST
(require 'automaton)
(module+ test
  (require rackunit)
  (check-equal? (automaton-current-state all-defects) 1)
  (check-equal? (automaton-current-strat all-defects) 1)
  (check-equal? (jump-to-state 1 all-cooperates) 0)
  (check-equal? (react 1 tit-for-tat) 1)
  (check-equal?
   (update grim-trigger 1)
   (automaton 1 (list (state 0 0 1) (state 1 1 1)))))



(module evolution racket
  (provide (all-defined-out))
  (require (submod ".." automaton))

  ;; MATCHING
  ;; strategy n -> payoff n
  (define (match-strategies strat1 strat2)
    (match (list strat1 strat2)
      [(list 0 0) (list 3 3)]
      [(list 0 1) (list 0 4)]
      [(list 1 0) (list 4 0)]
      [(list 1 1) (list 1 1)]))

  (define (match-pair auto1 auto2 rounds-per-match)
    (define round-results
      (for/fold ([round-result '()])
                ([i rounds-per-match])
        [define current-strat1 (automaton-current-strat auto1)]
        [define current-strat2 (automaton-current-strat auto2)]
        [define next-state1 (jump-to-state current-strat2 auto1)]
        [define next-state2 (jump-to-state current-strat1 auto2)]
        [define result (match-strategies current-strat1 current-strat2)]
        (set! auto1 (update auto1 next-state1))
        (set! auto2 (update auto2 next-state2))
        (cons result round-result)))
    (reverse round-results))

  (define (match-population population rounds-per-match)
    (define population-result
      (for/fold ([population-result '()])
                ([i (/ (length population) 2)])
        [define round-result
          (match-pair
           (list-ref population (* i 2))
           (list-ref population (add1 (* i 2)))
           rounds-per-match)]
        (cons
         (list
          (apply + (map first round-result))
          (apply + (map second round-result)))
         population-result)))
    (flatten (reverse population-result)))

  ;; FITNESS
  ;; from matching result, calculate the fitness

  (define (accumulated-payoff-percentages payoff-list)
    (define payoff-sum (apply + payoff-list))
    (define-values (accumulated _)
      (for/fold ([accumulated (list 0)]
                 [init 0])
                ([y (in-list payoff-list)])
        [define next-init (+ init (/ y payoff-sum))]
        (values (cons next-init accumulated) next-init)))
    (reverse accumulated))

  (define (randomise-over-fitness accumulated-payoff-percentage
                                  population speed)
    (for/list ([ n speed])
      [define r (random)]
      (for/and ([p (in-list population)]
                [a (in-list accumulated-payoff-percentage)]
                #:break (< r a))
        p)))

  ;; EVOLVE THE POPULATION OVER CYCLES
  (define (evolve population cycles speed rounds-per-match)
    (define-values (result _)
      (for/fold ([result '()]
                 [population population])
                ([_ cycles])
        [define round-results (match-population population rounds-per-match)]
        [define sum (apply + round-results)]
        [define average-payoff (/ sum (* rounds-per-match (length population)))]
        [define accum-fitness (accumulated-payoff-percentages round-results)]
        [define survivors (drop population speed)]
        [define successors (randomise-over-fitness accum-fitness population speed)]
        [define new-population (shuffle (append survivors successors))]
        (values (cons average-payoff result) new-population)))
    (reverse result)))

;; TEST
(require 'automaton 'evolution)
(module+ test
  (require rackunit)
  (check-equal? (match-strategies 0 1) '(0 4))
  (check-equal? (match-strategies 1 0) '(4 0))
  (check-equal? (match-strategies 0 0) '(3 3))
  (check-equal? (match-strategies 1 1) '(1 1))
  (check-equal?
   (match-pair all-defects all-cooperates 10)
   '((4 0) (4 0) (4 0) (4 0) (4 0) (4 0) (4 0) (4 0) (4 0) (4 0)))
  (check-equal?
   (match-pair all-defects tit-for-tat 10)
   '((4 0) (1 1) (1 1) (1 1) (1 1) (1 1) (1 1) (1 1) (1 1) (1 1)))
  (check-equal?
   (match-pair tit-for-tat all-defects 10)
   '((0 4) (1 1) (1 1) (1 1) (1 1) (1 1) (1 1) (1 1) (1 1) (1 1)))
  (check-equal?
   (accumulated-payoff-percentages (list 1 3 5 2 9))
   '(0 1/20 1/5 9/20 11/20 1)))



(module tv racket
  (provide (all-defined-out))
  (require (submod ".." automaton)
           (submod ".." evolution)
           plot) ; to plot result
  (plot-new-window? #t)

  ;; the result is the population average:
  ;; how much an average automaton gets in the game in each cycle
  ;; if the average is 3, the society is in a prosperous period
  ;; in which all are cooperators
  ;; if the average gets down to 1, the society is in a state
  ;; of everybody defecting everybody

  (define (run)
    (time
     (plot-mean
      (evolve (create-population 100) 1000 10 20))))

  (define (plot-mean data)
    (define coors (map list (build-list (length data) values) data))
    (plot (lines coors)))

  (module* main #f
    (run))
  )

;; ACKNOWLEDGEMENT
;; Thanks to the blog post of Tim Thornton,
;; http://timthornton.net/blog/id/538fa6f2f09a16ba0674813d
;; i know where to start
;; Thanks to Racket mailing list
;; https://groups.google.com/forum/#!topic/racket-users/4o1goSwrLHA
;; and IRC #racket for all the discussions
;; http://pastebin.com/sxrCnwRV
