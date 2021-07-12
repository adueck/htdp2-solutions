;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-360) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define WRONG "Invalid expression.")
5
(define-struct add [left right])
;; An Add is a structure:
;;    (make-add BSL-expr BSL-expr)

(define-struct mul [left right])
;; A Mul is a structure:
;;    (make-mul BSL-expr BSL-expr)

; A BSL-value is a Number

;; A BSL-expr is one of:
;; - Number
;; - Symbol
;; - Fun
;; - Add
;; - Mul

; An AL (short for association list) is [List-of Association].
; An Association is a list of two items:
;   (cons Symbol (cons Number '())).
(define al1
  (list (cons 'x (cons 2 '()))
        (cons 'y (cons 5 '()))
        (cons 'z (cons 1 '()))))

(define-struct fun [name ex])
;; A Fun is a structure
;;   (make-fun Symbol BSL-expr)
(make-fun 'k (make-add 1 1))

(define-struct BSL-con-def [name value])
;; A BSL-con-dev is a structure
;    (make-BSL-con-def Symbol BSL-expr)
(make-BSL-con-def 'x 23)
(make-BSL-con-def 'y (make-mul 2 4))

(define-struct BSL-fun-def [name param body])
;; A BSL-fun-def is a structure
;    (make-BSL-fun-def Symbol Symbol BSL-expr
(define fadd2 (make-BSL-fun-def 'add2 'x (make-add 'x 2)))
(define ff (make-BSL-fun-def 'f 'x (make-add 3 'x)))
(define fg (make-BSL-fun-def 'g 'y (make-fun 'f
                                             (make-add 2 'y))))
(define fh (make-BSL-fun-def 'h 'v (make-add
                                    (make-fun 'f 'v)
                                    (make-fun 'g 'v))))

;; A DA-Item is one of:
;; - BSL-expr
;; - BSL-fun-def
;; - BSL-con-def

;; A BSL-da-all is a [List-of DA-Item]
(define da1 (list 3
                  (make-mul 2 1)
                  (make-BSL-fun-def 'f 'x (make-add 3 'x))
                  (make-BSL-con-def 'a 5)
                  (make-BSL-con-def 'b (make-add 3 5))
                  (make-BSL-con-def 'g 23)))

; BSL-da-all Symbol -> BSL-expr
; Returns the definition of a constant with the name s, otherwise
; signals an error if not found
(check-expect (lookup-con-def da1 'a) 5)
(check-expect (lookup-con-def da1 'g) 23)
(check-expect (lookup-con-def da1 'b) (make-add 3 5))
(check-error (lookup-con-def da1 'f))
(define (lookup-con-def da s)
  (local
    ; DA-Item Symbol -> Boolean
    ((define (is-matching-con? d s)
       (cond
         [(BSL-con-def? d) (symbol=? (BSL-con-def-name d) s)]
         [else #false])))
    (cond
      [(empty? da) (error WRONG)]
      [else (if
             (is-matching-con? (first da) s)
             (BSL-con-def-value (first da))
             (lookup-con-def (rest da) s))])))

;; A BLS-fun-def* is a [List-of BSL-fun-def]
(define da-fgh
  (list
   (make-BSL-fun-def 'add2 'x (make-add 'x 2))
   (make-BSL-fun-def 'f 'x (make-add 3 'x))
   (make-BSL-fun-def 'g 'y (make-fun 'f
                                     (make-add 2 'y)))
   (make-BSL-fun-def 'h 'v (make-add
                            (make-fun 'f 'v)
                            (make-fun 'g 'v)))))

; BSL-fun-def* Symbol -> BSL-fun-def
; retrieves the definition of f in da
; signals an error if there is none
(check-expect (lookup-def da-fgh 'g) fg)
(check-expect (lookup-def da-fgh 'h) fh)
(check-error (lookup-def da-fgh 'a))
(check-error (lookup-def '() 'g))
(define (lookup-def da f)
  (cond
    [(empty? da) (error WRONG)]
    [else (if (symbol=? f (BSL-fun-def-name (first da)))
              (first da)
              (lookup-def (rest da) f))]))

; BSL-expr BSL-fun-def* -> BSL-value
; produces the result that DrRacket shows if you evaluate ex
; in the interactions area, assuming the definitions area contains da.
(check-expect (eval-function*
               (make-add 3 (make-fun 'add2 1))
               da-fgh)
              6)
(check-expect (eval-function* (make-fun 'g 10) da-fgh)
              15)
(check-expect (eval-function*
               (make-mul (make-fun 'g 10) 2)
               da-fgh)
              30)
(check-error (eval-function* (make-add 2 'sy) da-fgh))
(check-error (eval-function* (make-add 2 (make-fun 'gone 3)) da-fgh))
(define (eval-function* ex da)
  (cond
    [(number? ex) ex]
    [(symbol? ex) (error WRONG)]
    [(add? ex) (+
                (eval-function* (add-left ex) da)
                (eval-function* (add-right ex) da))]
    [(mul? ex) (*
                (eval-function* (mul-left ex) da)
                (eval-function* (mul-right ex) da))]
    [(fun? ex) (local
                 ; when encountering a function:
                  ; 1. evaluate the argument of the function
                 ((define value (eval-function* (fun-ex ex) da))
                  ; 2. look up the definition of the function mentioned
                  (define fdef (lookup-def da (fun-name ex)))
                  ; 3. substitute the value of the argument for the paramater
                  ; in the function that we looked up
                  (define plugd (subst (BSL-fun-def-body fdef) (BSL-fun-def-param fdef) value)))
                 ;4. evaluate the new expression via recursion
                 (eval-function* plugd da))]))
                 
; BSL-expr Symbol Symbol BSL-expr -> BSL-value
(check-expect (eval-definition1
               (make-add 3 5)
               'f
               'x
               (make-add 2 'x))
              8)
(check-expect (eval-definition1
               (make-fun 'g (make-mul 2 10))
               'g
               'x
               (make-add 5 'x))
              25)
(check-expect (eval-definition1
               (make-add 3 (make-fun 'f (make-add 5 1)))
               'f
               'x
               (make-add 2 'x))
              11)
(check-error (eval-definition1
               (make-add 3 (make-fun 'm 5))
               'f
               'x
               (make-add 2 'x)))
(check-error (eval-definition1
               'p
               'f
               'x
               (make-add 2 'x)))
(define (eval-definition1 ex f x b)
  (cond
    [(number? ex) ex]
    [(symbol? ex) (error WRONG)]
    [(add? ex) (+
                (eval-definition1 (add-left ex) f x b)
                (eval-definition1 (add-right ex) f x b))]
    [(mul? ex) (*
                (eval-definition1 (mul-left ex) f x b)
                (eval-definition1 (mul-right ex) f x b))]
    [(fun? ex) (if (symbol=? (fun-name ex) f)
                   (local
                     ((define value (eval-definition1 (fun-ex ex) f x b))
                      (define plugd (subst b x value)))
                     (eval-definition1 plugd f x b))
                   (error WRONG))]))

; Symbol AL -> Number
; returns the number to be substituded for a variable
; given an AL of variables. Errors if the variable is not found in the lookup
; table
(check-expect (lookup 'x al1) 2)
(check-expect (lookup 'z al1) 1)
(check-error (lookup 'a al1))
(check-error (lookup 'a '()))
(define (lookup s da)
  (local
    ((define found (assq s da)))
    (if (cons? found)
        (second found)
        (error WRONG))))

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
