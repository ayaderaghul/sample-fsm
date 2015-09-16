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


;; CODE
(require plot) ; this is to plot the result
;; the result is the population average:
;; how much an average automaton gets in the game in each cycle
;; if the average is 3, the society is in a prosperous period
;; in which all are cooperators
;; if the average gets down to 1, the society is in a state
;; of everybody defecting everybody
(plot-new-window? #t)

;; AUTOMATON
(struct action (event result) #:transparent #:mutable)
; a transition rule: an event and the result state
(struct state (name actions) #:transparent #:mutable)
; a state: name and many transition rules
(struct automaton (current-state states) #:transparent #:mutable)
; the machine itself: current state + states

;; REACT TO OPPONENT MOVE
;; when an event happens, the right action needs to be chosen
(define (this-action? an-event an-action)
  (equal? an-event (action-event an-action)))
;; in a state, there are many actions, filter out the right action
;; given the event
(define (filter-action an-event actions)
  (filter
   (lambda (an-action)
     (this-action? an-event an-action))
   actions))
; after the right action has been chosen,
; extract the result of that action, given an event
(define (act an-event actions)
  (let ([result (filter-action an-event actions)])
    (if (null? result)
        null
        (action-result (car result)))))

; given a name, the right state needs to be found
(define (this-state? a-name a-state)
  (equal? a-name (state-name a-state)))
; in an automaton, there are many states,
; filter out the right state given the name
(define (filter-state a-name states)
  (filter
   (lambda (a-state)
     (this-state? a-name a-state))
   states))
; extract the result of the needed action, given an event
(define (react an-event an-auto)
  (let ([result-state (filter-state (automaton-current-state an-auto)
                                    (automaton-states an-auto))])
    (if (null? result-state)
        an-auto
        (act an-event
             (state-actions
              (car result-state))))))
; update the state of the auto, return the auto
(define (update old-auto new-state)
  (begin (set-automaton-current-state! old-auto new-state)
         old-auto))

;; CLASSIC AUTOMATA
;; let 0 = cooperate, 1 = defect
;; some classic automata
;; the all defector has 2 states of cooperate and defect
;; but it always defects, no matter what
;; the opponent may play cooperate or defect
;; it doesnt care, it always stay in the state defect
(define all-defects
  (automaton 1
             (list (state 0 (list (action 0 1)
                                  (action 1 1)))
                   (state 1 (list (action 0 1)
                                  (action 1 1))))))
(define all-cooperates
  (automaton 0
             (list (state 0 (list (action 0 0)
                                  (action 1 0)))
                   (state 1 (list (action 0 0)
                                  (action 1 0))))))
;; the tit for tat starts out optimistic, it cooperates initially
;; however, if the opponent defects, it punishes by switching to defecting
;; if the opponent cooperates, it returns to play cooperate again
(define tit-for-tat
  (automaton 0
             (list (state 0 (list (action 0 0)
                                  (action 1 1)))
                   (state 1 (list (action 0 0)
                                  (action 1 1))))))
;; the grim trigger also starts out optimistic,
;; but the opponent defects for just once then
;; it jumps to defect forever
;; it doesnt forgive, and doesnt forget
(define grim-trigger
  (automaton 0
             (list (state 0 (list (action 0 0)
                                  (action 1 1)))
                   (state 1 (list (action 0 1)
                                  (action 1 1))))))

;; generate random automaton (random current state,
;; random result state after each event
(define (generate-auto)
  (automaton (random 2)
             (list (state 0 (list (action 0 (random 2))
                                  (action 1 (random 2))))
                   (state 1 (list (action 0 (random 2))
                                  (action 1 (random 2)))))))

;; PAYOFF MATRIX
(define (match-strategies strategies)
  (cond [(equal? strategies (list 0 0)) (list 3 3)]
        [(equal? strategies (list 0 1)) (list 0 4)]
        [(equal? strategies (list 1 0)) (list 4 0)]
        [(equal? strategies (list 1 1)) (list 1 1)]))

;; MATCH PAIR
(define (match-pair* auto1 auto2 results previous-strategies countdown)
  (if (zero? countdown)
      results
      (let ([reaction1 (react (last previous-strategies) auto1)]
            [reaction2 (react (car previous-strategies) auto2)])
        (match-pair* (update auto1 reaction1)
                     (update auto2 reaction2)
                     (append results (list
                                      (match-strategies previous-strategies)))
                     (list reaction1 reaction2)
                     (sub1 countdown)))))

;; match a pair of automaton for n rounds
;; return a list of round results
(define (match-pair automaton-pair rounds-per-match)
  (match-pair* (car automaton-pair)
               (last automaton-pair)
               '()
               (map automaton-current-state automaton-pair)
               rounds-per-match))

;; GENERATE POPULATION
(define A
  (for/list
      ([n 100])
    (generate-auto)))

;; in each match, take the sum of round results for each automaton
;; returns a pair of sums
(define (take-sums round-results)
  (map (lambda (f) (apply +  (map f round-results)))
       (list first second)))

;; MATCH POPULATION
(define (match-population population rounds-per-match)
  (for/list ([i (/ (length population)
                   2)])
    (take-sums
     (match-pair (list
                  (list-ref population (* 2 i))
                  (list-ref population (add1 (* 2 i))))
                 rounds-per-match))))

;; FITNESS CALCULATION
;; from the matching result, calculate the fitness
(define (reductions-h f accumulated init a-list)
  (if (null? a-list)
      accumulated
      (let ((next-init (f init (first a-list))))
        (reductions-h f
                      (append accumulated (list next-init))
                      next-init
                      (rest a-list)))))
(define (reductions f init a-list)
  (if (null? a-list)
      accumulated
      (reductions-h f '() init a-list)))
(define (reductions* f a-list)
  (let ([init (first a-list)])
    (reductions-h f (list init) init (rest a-list))))

(define (accumulate a-list)
  (reductions* + (cons 0 a-list)))

(define (payoff-percentages payoff-list)
  (let ([s (apply + payoff-list)])
    (for/list ([i (length payoff-list)])
      (/ (list-ref payoff-list i)
         s))))

(define (accumulated-fitness population rounds-per-match)
  (accumulate
   (payoff-percentages
    (flatten
     (match-population population rounds-per-match)))))

;; REGENERATE FITTEST AUTOMATA
;; at the end of the cycle, i kill 10%
;; so i resurrect automata by randomising over the fitness vector
(define (randomise-over-fitness accumulated-payoff-percentage population speed)
  (let
      ([len (length population)])
    (for/list
        ([n speed])
      (let ([r (random)])
        (for/and ([i len])
          #:break (< r (list-ref accumulated-payoff-percentage i))
          (list-ref population i))))))

(define population-mean '())

;; EVOLVE THE POPULATION OVER CYCLES
(define (evolve population cycles speed rounds-per-match)
  (let* ([round-results (match-population population rounds-per-match)]
         [average-payoff (exact->inexact (/ (apply + (flatten round-results))
                                            (* rounds-per-match 100)))]
         [accum-fitness (accumulate (payoff-percentages (flatten round-results)))]
         [survivors (drop population speed)]
         [successors
          (randomise-over-fitness accum-fitness population speed)]
         [new-population (shuffle (append survivors successors))])
    (set! population-mean
          (append population-mean (list average-payoff)))
    (if (zero? cycles)
        "done"
        (evolve new-population (sub1 cycles) speed rounds-per-match))))

;; TV
(define (plot-mean data)
  (let* ([l (length data)]
         [coors (map list (build-list l values)
                     data)])
    (plot (lines coors))))

;; ACKNOWLEDGEMENT
;; Thanks to the blog post of Tim Thornton,
;; http://timthornton.net/blog/id/538fa6f2f09a16ba0674813d
;; i know where to start
;; Thanks to Racket mailing list
;; https://groups.google.com/forum/#!topic/racket-users/4o1goSwrLHA
;; and IRC #racket for all the discussions
;; http://pastebin.com/sxrCnwRV
