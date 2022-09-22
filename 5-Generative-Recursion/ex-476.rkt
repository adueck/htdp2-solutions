;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-476) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define-struct transition [current key next])
(define-struct fsm [initial transitions final])
; An FSM is a structure:
;   (make-fsm FSM-State [List-of 1Transition] FSM-State)
; A 1Transition is a structure:
;   (make-transition FSM-State 1String FSM-State)
; An FSM-State is String.
(define fsm-a-bc*-d
  (make-fsm
   "AA"
   (list (make-transition "AA" "a" "BC")
         (make-transition "BC" "b" "BC")
         (make-transition "BC" "c" "BC")
         (make-transition "BC" "d" "DD"))
   "DD"))

; FSM String -> Boolean
(check-expect (fsm-match fsm-a-bc*-d "abcd") #true)
(check-expect (fsm-match fsm-a-bc*-d "aac") #false)
(define (fsm-match fsm s)
  (local (; FSM-State [List-of 1String] -> Boolean
          (define (fmtch state los)
            (cond
              [(string=? state (fsm-final fsm)) #true]
              [(empty? los) #false]
              [else (fmtch
                     (advance state (first los) fsm)
                     (rest los))])))
    (fmtch (fsm-initial fsm) (explode s))))

; FSM-State 1String FSM -> FSM-State
; Advances to next FSM-State given a keystroke 1String
(check-expect (advance "AA" "a" fsm-a-bc*-d)
              "BC")
(check-expect (advance "AA" "f" fsm-a-bc*-d)
              "AA")
(define (advance curr char fsm)
  (local
    ((define next-transition (findf
                              (lambda (t) (and
                                           (string=? curr (transition-current t))
                                           (string=? char (transition-key t))))
                              (fsm-transitions fsm))))
     (if (boolean? next-transition)
         curr
         (transition-next next-transition))))


; X [X -> Boolean] -> [Maybe [List-of X]]
; Returns the first element of l that satisfies the predicate p?
; otherwise #false
(define (findf p? l)
  (cond
    [(empty? l) #false]
    [else (if (boolean=? #t (p? (first l)))
              (first l)
              (findf p? (rest l)))]))
   