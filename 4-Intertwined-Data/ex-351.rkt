;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-351) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; An Atom is one of:
;; – Number
;; – String
;; – Symbol

;; An S-expr is one of:
;; – Atom
;; – SL

;; An SL is a [List-of S-expr].

(define-struct add [left right])
;; An Add is a structure:
;;    (make-add BSL-expr BSL-expr)

(define-struct mul [left right])
;; A Mul is a structure:
;;    (make-mul BSL-expr BSL-expr)

;; A BSL-expr is one of:
;; - Number
;; - Add
;; - Mul

(define WRONG "Invalid expression.")

; S-expr -> BSL-expr
(check-expect (parse 3) 3)
(check-expect (parse '(+ 3 1)) (make-add 3 1))
(check-expect (parse '(* 4 2)) (make-mul 4 2))
(check-expect (parse '(+ (* 2 (+ 1 2)) (+ 2 4)))
              (make-add (make-mul 2
                                  (make-add 1 2))
                        (make-add 2 4)))
(check-error (parse "cool"))
(check-error (parse +))
(check-error (parse '+))
(check-error (parse '(- 4 2)))
(define (parse s)
  (cond
    [(atom? s) (parse-atom s)]
    [else (parse-sl s)]))
 
; SL -> BSL-expr 
(define (parse-sl s)
  (cond
    [(and (consists-of-3 s) (symbol? (first s)))
     (cond
       [(symbol=? (first s) '+)
        (make-add (parse (second s)) (parse (third s)))]
       [(symbol=? (first s) '*)
        (make-mul (parse (second s)) (parse (third s)))]
       [else (error WRONG)])]
    [else (error WRONG)]))

; BSL-Expression -> BSL-Value
; Evaluates a BSL-Explession and outputs it's value
(check-expect (eval-expression 3) 3)
(check-expect (eval-expression
               (make-add 1 1)) 2)
(check-expect (eval-expression
               (make-mul 3 10)) 30)
(check-expect (eval-expression
               (make-add (make-mul 1 1) 10))
               11)
(define (eval-expression ex)
  (cond
    [(number? ex) ex]
    [(add? ex) (+
                (eval-expression (add-left ex))
                (eval-expression (add-right ex)))]
    [(mul? ex) (*
                (eval-expression (mul-left ex))
                (eval-expression (mul-right ex)))]))

; S-Expr -> BSL-Value
(check-expect (interpreter-expr 3) 3)
(check-expect (interpreter-expr
               '(+ 3 1)) 4)
(check-expect (interpreter-expr
               '(+ 3 (* 2 3))) 9)
(check-error (parse "cool"))
(define (interpreter-expr sexp)
  (eval-expression (parse sexp)))

; Atom -> BSL-expr 
(define (parse-atom s)
  (cond
    [(number? s) s]
    [(string? s) (error WRONG)]
    [(symbol? s) (error WRONG)]))
 
; SL -> Boolean
(define (consists-of-3 s)
  (and (cons? s) (cons? (rest s)) (cons? (rest (rest s)))
       (empty? (rest (rest (rest s))))))

;; Any -> Boolean
(define (atom? s)
  (or (number? s) (string? s) (symbol? s)))
