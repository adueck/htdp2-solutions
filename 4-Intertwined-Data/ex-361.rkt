;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-361) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define WRONG "Invalid expression.")

(define-struct add [left right])
;; An Add is a structure:
;;    (make-add BSL-expr BSL-expr)

(define-struct mul [left right])
;; A Mul is a structure:
;;    (make-mul BSL-expr BSL-expr)

(define-struct fun [name ex])
;; A Fun is a structure
;;   (make-fun Symbol BSL-expr)

; A BSL-value is a Number

;; A BSL-expr is one of:
;; - Number
;; - Symbol
;; - Fun
;; - Add
;; - Mul

(define-struct BSL-con-def [name value])
;; A BSL-con-dev is a structure
;    (make-BSL-con-def Symbol BSL-expr)

(define-struct BSL-fun-def [name param body])
;; A BSL-fun-def is a structure
;    (make-BSL-fun-def Symbol Symbol BSL-expr

;; A DA-Item is one of:
;; - BSL-expr
;; - BSL-fun-def
;; - BSL-con-def

;; A BSL-da-all is a [List-of DA-Item]
(define da1 (list 3
                  (make-mul 2 1)
                  (make-BSL-fun-def 'f 'x (make-add 3 'x))
                  (make-BSL-fun-def 'add5 'x (make-add 5 'x))
                  (make-BSL-con-def 'a 5)
                  (make-BSL-con-def 'b (make-add 3 5))
                  (make-BSL-con-def 'g 23)))

; BSL-expr BSL-da-all -> BSL-Value
; This evaluates an expression ex with a definitions area da
(check-expect (eval-all (make-add 'a 1) da1) 6)
(check-expect (eval-all
               (make-mul (make-add (make-add 'g 2)
                                   (make-add 'b 2))
                         'a)
               da1)
               (* 35 5))
(check-expect (eval-all (make-fun 'f (make-mul 3 10)) da1) 33)
(check-expect (eval-all (make-fun 'f (make-fun 'add5 10)) da1) 18) 
(check-expect (eval-all 'g da1) 23)
(check-error (eval-all (make-fun 'nothere (make-mul 3 10)) da1))
(check-error (eval-all (make-mul 'y 3) da1))
(define (eval-all ex da)
  (cond
    [(number? ex) ex]
    [(symbol? ex) (eval-all
                   (BSL-con-def-value (lookup-def da "con" ex))
                   da)]
    [(fun? ex) (local
                 ; 1. evaluate the argument of the function
                 ((define value (eval-all (fun-ex ex) da))
                  ; 2. look up the definition of the function mentioned
                  (define fdef (lookup-def da "fun" (fun-name ex)))
                  ; 3. substitute the value of the argument for the paramater
                  ; in the function that we looked up
                  (define plugd (subst (BSL-fun-def-body fdef) (BSL-fun-def-param fdef) value)))
                 ;4. evaluate the new expression via recursion
                 (eval-all plugd da))]
    [(add? ex) (+
                (eval-all (add-left ex) da)
                (eval-all (add-right ex) da))]
    [(mul? ex) (*
                (eval-all (mul-left ex) da)
                (eval-all (mul-right ex) da))]))

;; HELPER FUNCTIONS BEING USED

; BSL-exp Symbol N -> BSL-fun-exp
(check-expect (subst 'x 'x 3) 3)
(check-expect (subst (make-add 'x 3) 'x 3)
                     (make-add 3 3))
(check-expect (subst
               (make-add (make-mul 'x 'x)
                         (make-mul 'y 'y))
               'x 4)
              (make-add (make-mul 4 4)
                         (make-mul 'y 'y)))
(check-expect (subst (make-fun 'b 'c) 'c 2)
              (make-fun 'b 2))
(define (subst ex x v)
  (cond
    [(number? ex) ex]
    [(symbol? ex) (if (symbol=? ex x)
                      v
                      ex)]
    [(add? ex) (make-add
                (subst (add-left ex) x v)
                (subst (add-right ex) x v))]
    [(mul? ex) (make-mul
                (subst (mul-left ex) x v)
                (subst (mul-right ex) x v))]
    [(fun? ex) (make-fun
                (fun-name ex)
                (subst (fun-ex ex) x v))]))

; A Def-Type is one of the following strings:
; - "fun"
; - "con"

; BSL-da-all Def-Type Symbol -> BSL-expr
; When looking for a "fun" it returns a function definition with that name
; When looking for a "con" it returns a constand definition with that name
; Otherwise, if nothing is found it signals an error
(check-expect (lookup-def da1 "con" 'a) (make-BSL-con-def 'a 5))
(check-expect (lookup-def da1 "con" 'b) (make-BSL-con-def 'b (make-add 3 5)))
(check-error (lookup-def da1 "fun" 'a))
(check-expect (lookup-def da1 "fun" 'f) (make-BSL-fun-def 'f 'x (make-add 3 'x)))
(define (lookup-def da t s)
  (local
    ; DA-Item Symbol -> Boolean
    ((define (is-matching? d)
       (cond
         [(string=? t "fun") (and (BSL-fun-def? d) (symbol=? (BSL-fun-def-name d) s))]
         [(string=? t "con") (and (BSL-con-def? d) (symbol=? (BSL-con-def-name d) s))])))
    (cond
      [(empty? da) (error WRONG)]
      [else (if
             (is-matching? (first da))
             (first da)
             (lookup-def (rest da) t s))])))
