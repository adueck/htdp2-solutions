;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-230-fsm-full) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define-struct fsm [initial transitions final])
(define-struct transition [current key next])
; An FSM.v2 is a structure: 
;   (make-fsm FSM-State LOT FSM-State)
; A LOT is one of: 
; – '() 
; – (cons Transition.v3 LOT)
; A Transition.v3 is a structure: 
;   (make-transition FSM-State KeyEvent FSM-State)

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

(define lot-traffic (list (make-transition "red" "g" "green")
                          (make-transition "green" "y" "yellow")
                          (make-transition "yellow" "r" "red")))

(define fsm-traffic
  (make-fsm "red" lot-traffic "yellow"))
(define fsm-109
  (make-fsm
   "white"
   (list (make-transition "white" "a" "yellow")
         (make-transition "yellow" "b" "yellow")
         (make-transition "yellow" "c" "yellow")
         (make-transition "yellow" "d" "green"))
   "green"))

; FSM.v2 KeyEvent -> FSM.v2
; finds the next state from an-fsm and ke
(check-expect
  (find-next-state (make-fsm "green" lot-traffic "yellow") "y")
  (make-fsm "yellow" lot-traffic "yellow"))
(check-expect
  (find-next-state (make-fsm "green" lot-traffic "yellow") "b")
  (make-fsm "green" lot-traffic "yellow"))
(define (find-next-state an-fsm ke)
  (make-fsm
   (find (fsm-initial an-fsm) (fsm-transitions an-fsm) ke)
   (fsm-transitions an-fsm)
   (fsm-final an-fsm)))

; FSM-State LOT KeyEvent -> FSM-State
; finds the state representing current in transitions and switch if given correct key
; and retrieves the next field 
(check-expect (find "red" lot-traffic "g") "green")
(check-expect (find "red" lot-traffic "c") "red")
(check-error (find "black" lot-traffic "b") "not found: black")
(define (find current transitions key)
  (cond
    [(empty? transitions) (error "not found: " current)]
    [else (if
           (transition-matches? (first transitions) current)
           (if
            (key-matches? (first transitions) key)
            (transition-next (first transitions))
            (transition-current (first transitions)))
           (find current (rest transitions) key))]))

; Transition FSM-State -> Boolean
; Checks if a current state matches the beginning of a transition
(check-expect (transition-matches? (make-transition "blue" "a" "green") "blue") #t)
(check-expect (transition-matches? (make-transition "blue" "b" "green") "pink") #f)
(define (transition-matches? t current)
  (state=? (transition-current t) current))

; Transition KeyEvent -> Boolean
; Checks if a key is correct for advancing a given transition
(check-expect (key-matches? (make-transition "blue" "a" "green") "a") #t)
(check-expect (key-matches? (make-transition "blue" "b" "green") "a") #f)
(define (key-matches? t ke)
  (key=? (transition-key t) ke))

; FSM.v2 -> Image 
; renders current world state as a colored square 
(check-expect (state-as-colored-square
                (make-fsm "red" lot-traffic "yellow"))
              (square 100 "solid" "red"))
(define (state-as-colored-square an-fsm)
  (square 100 "solid" (fsm-initial an-fsm)))

; FSM.v2 -> Image
; renders the final screen of a finished FSM
(check-expect (render-final (make-fsm "yellow" lot-traffic "yellow"))
              (place-image
               (text "DONE" 16 "black")
               50 50
               (square 100 "solid" "yellow")))
(define (render-final an-fsm)
  (place-image
   (text "DONE" 16 "black")
   50 50
   (state-as-colored-square an-fsm)))

; FSM.v2 -> Boolean
; checks if the final state of an FSM.v2 has been reached
(check-expect (hit-final? fsm-traffic) #false)
(check-expect (hit-final? (make-fsm "yellow" lot-traffic "yellow")) #true)
(define (hit-final? an-fsm)
  (state=? (fsm-initial an-fsm) (fsm-final an-fsm)))

; FSM.v2 -> FSM.v2 
; match the keys pressed with the given FSM 
(define (simulate an-fsm)
  (big-bang an-fsm
    [to-draw state-as-colored-square]
    [on-key find-next-state]
    [stop-when hit-final? render-final]))

