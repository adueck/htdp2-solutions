;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-381) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)
; An FSM is a [List-of 1Transition]
; A 1Transition is a list of three items:
;   (cons FSM-State (cons FSM-State (cons KeyEvent '())))
; An FSM-State is a String that specifies a color
; A KeyEvent is a string that specifies the keystroke needed to make
; the transition happen

; An XMachine is a nested list of this shape:
;   (list 'machine (list (list initial FSM-State))  [List-of X1T]))
; An X1T is a nested list of this shape:
;   ('action (list (list state FSM-State) (list next FSM-State)))
; using cons
; An XMachine is a nested list of this shape:
;   (cons 'machine
;     (cons
;       (cons (cons initial (cons FSM-State '())) '())
;       XIT*))
; An X1T is a nested list of this shape:
;(cons 'action
;        (cons
;         (cons (cons state (cons FSM-State '()))
;               (cons (cons next (cons FSM-State '())) '()))
;         '()))
; AN XIT* is one of the following
; - '()
; - (cons XIT XIT*)

; data examples 
(define fsm-traffic
  '(("red" "green" "g") ("green" "yellow" "y") ("yellow" "red" "r")))
 
; FSM FSM-State -> FSM-State 
; matches the keys pressed by a player with the given FSM 
(define (simulate state0 transitions)
  (big-bang state0 ; FSM-State
    [to-draw
      (lambda (current)
        (overlay/align/offset
         "middle"
         "top"
         (text current 14 "black")
         0 -10
         (square 100 "solid" current)))]
    [on-key
      (lambda (current key-event)
        (local ((define req-key (find-key transitions current)))
          (if (key=? key-event req-key) 
              (find-next transitions current)
              current)))]))
 
; [X Y] [List-of [List X Y ...]] X -> Y
; finds the matching Y for the given X in alist
(check-expect (find-next '(("a" 3 8) ("b" 4 1)) "b") 4)
(check-expect (find-next '((4 1) (5 3)) 4) 1)
(check-error (find-next '((1 2) (3 4)) 5))
(check-error (find-next '() "a"))
(check-error (find-next 23 "a"))
(define (find-next alist x)
  (local ((define fm (assoc x alist)))
    (if (cons? fm) (second fm) (error "not found"))))

; [X Y] [List-of [List X Y Z]] X -> Y
; finds the matching Z for the given X in alist
(check-expect (find-key '(("a" 3 8) ("b" 4 1)) "b") 1)
(check-expect (find-key '((4 1 6) (5 3 9)) 4) 6)
(check-error (find-key '((1 2) (3 4)) 5))
(check-error (find-key '() "a"))
(check-error (find-key 23 "a"))
(define (find-key alist x)
  (local ((define fm (assoc x alist)))
    (if (cons? fm) (third fm) (error "not found"))))