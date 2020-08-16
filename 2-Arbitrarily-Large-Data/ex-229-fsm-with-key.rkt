;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-229-fsm-with-key) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; An FSM is one of:
;   – '()
;   – (cons Transition FSM)

(define-struct ktransition [current key next])
; A Transition.v2 is a structure:
;   (make-ktransition FSM-State KeyEvent FSM-State)
 
; FSM-State is a Color.
 
; interpretation An FSM represents the transitions that a
; finite state machine can take from one state to another 
; in reaction to keystrokes

; FSM-State FSM-State -> Boolean
; Checks if two given states are equal
(check-expect (state=? "blue" "blue") #true)
(check-expect (state=? "pink" "green") #false)
(define (state=? s1 s2)
  (string=? s1 s2))

(define fsm-traffic
  (list (make-ktransition "red" "g" "green")
        (make-ktransition "green" "y" "yellow")
        (make-ktransition "yellow" "r" "red")))
(define fsm-bw
  (list (make-ktransition "white" "b" "black")
        (make-ktransition "black" "w" "white")))

(define-struct fs [fsm current])
; A SimulationState.v2 is a structure: 
;   (make-fs FSM FSM-State)
 
; SimulationState.v2 KeyEvent -> SimulationState.v2
; finds the next state from an-fsm and ke
(check-expect
  (find-next-state (make-fs fsm-traffic "red") "g")
  (make-fs fsm-traffic "green"))
(check-expect
  (find-next-state (make-fs fsm-traffic "red") "a")
  (make-fs fsm-traffic "red"))
(check-expect
  (find-next-state (make-fs fsm-traffic "green") "y")
  (make-fs fsm-traffic "yellow"))
(check-expect
  (find-next-state (make-fs fsm-traffic "yellow") "r")
  (make-fs fsm-traffic "red"))
(define (find-next-state an-fsm ke)
  (make-fs
   (fs-fsm an-fsm)
   (find (fs-fsm an-fsm) ke (fs-current an-fsm))))

; FSM KeyEvent FSM-State -> FSM-State
; finds the state representing current in transitions and switch if given correct key
; and retrieves the next field 
(check-expect (find fsm-traffic "g" "red") "green")
(check-expect (find fsm-traffic "y" "green") "yellow")
(check-expect (find fsm-traffic "a" "green") "green")
(check-error (find fsm-traffic "b" "black")
             "not found: black")
(define (find transitions key current)
  (cond
    [(empty? transitions) (error "not found: " current)]
    [else (if
           (transition-matches? (first transitions) current)
           (if
            (key-matches? (first transitions) key)
            (ktransition-next (first transitions))
            (ktransition-current (first transitions)))
           (find (rest transitions) key current))]))

; Transition.v2 FSM-State -> Boolean
; Checks if a current state matches the beginning of a transition
(check-expect (transition-matches? (make-ktransition "blue" "a" "green") "blue") #t)
(check-expect (transition-matches? (make-ktransition "blue" "b" "green") "pink") #f)
(define (transition-matches? t current)
  (state=? (ktransition-current t) current))

; Transition.v2 KeyEvint -> Boolean
; Checks if a key is correct for advancing a given transition
(check-expect (key-matches? (make-ktransition "blue" "a" "green") "a") #t)
(check-expect (key-matches? (make-ktransition "blue" "b" "green") "a") #f)
(define (key-matches? t ke)
  (key=? (ktransition-key t) ke))

; SimulationState.v2 -> Image 
; renders current world state as a colored square 
(check-expect (state-as-colored-square
                (make-fs fsm-traffic "red"))
              (square 100 "solid" "red"))
(define (state-as-colored-square an-fsm)
  (square 100 "solid" (fs-current an-fsm)))

; FSM FSM-State -> SimulationState.v2 
; match the keys pressed with the given FSM 
(define (simulate an-fsm s0)
  (big-bang (make-fs an-fsm s0)
    [to-draw state-as-colored-square]
    [on-key find-next-state]))

