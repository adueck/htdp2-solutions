;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-383) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)
; An FSM is a [List-of 1Transition]
; A 1Transition is a list of two items:
;   (cons FSM-State (cons FSM-State '()))
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
  '(("red" "green") ("green" "yellow") ("yellow" "red")))
(define fsm-bw
  '(("white" "black") ("black" "white")))

;<machine initial="red">
;  <action state="red"    next="green" />
;  <action state="green"  next="yellow" />
;  <action state="yellow" next="red" />
;</machine>

(define xm0
  '(machine ((initial "red"))
     (action ((state "red") (next "green")))
     (action ((state "green") (next "yellow")))
     (action ((state "yellow") (next "red")))))

; <machine initial="white">
;   <action state="white" next="black" />
;   <action state="black" next="white" />
; </machine>

(define xmbw
  '(machine ((initial "white"))
            (action ((state "white") (next "black")))
            (action ((state "black") (next "white")))))

; XMachine -> FSM-State
; simulates an FSM via the given configuration 
;(define (simulate-xmachine xm)
;  (simulate ... ...))

; XMachine -> FSM-State
(check-expect (xm-state0 xm0) "red")
(check-expect (xm-state0 xmbw) "white")
(define (xm-state0 xm) "")

; XMachine -> FSM
(check-expect (xm->transitions xm0) fsm-traffic)
(check-expect (xm->transitions xmbw) fsm-bw)
(define (xm->transitions xm) '())

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
        ;(local ((define req-key (find-key transitions current)))
          ;(if (key=? key-event req-key) 
              (find-next transitions current))]))
              ;current)))]))
 
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

; Xexpr -> [List-of Attribute]
; retrieves the list of attributes of xe
(define (xexpr-attr xe)
  (local ((define x-info (rest xe)))
    (cond
      [(empty? x-info) '()]
      [else (if
             (list-of-attributes? (first x-info))
             (first x-info)
             '())])))

; LOA-or-X -> boolean
; determines whether x is an element of [List-of Attribute]
; #false otherwise
(define (list-of-attributes? x)
  (cond
    [(empty? x) #true]
    [else (local
            ((define neloa-or-x (first x)))
            (cons? neloa-or-x))]))

; Xexpr -> Symbol
; returns the name of an Xexpr
(define (xexpr-name x)
  (first x))

; Xexpr -> [List-of Xexpr]
; returns the content of an Xexpr
(define (xexpr-content x)
  (local
    ((define x-info (rest x)))
    (cond
      [(empty? x-info) '()]
      [else
       (if (list-of-attributes? (first x-info))
           (rest x-info)
           x-info)])))

; [List-of Attributes] Symbol -> String or #false
; If the attributes list associates the symbol with a string,
; the function retrieves this string; otherwise it returns #false
(define (find-attr loa s)
  (local
    ((define res (assq s loa)))
    (cond
      [(false? res) #false]
      [else (second res)])))