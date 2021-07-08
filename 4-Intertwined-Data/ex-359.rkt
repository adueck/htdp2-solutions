;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-359) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define WRONG "Invalid expression.")

(define-struct add [left right])
;; An Add is a structure:
;;    (make-add BSL-var-expr BSL-var-expr)

(define-struct mul [left right])
;; A Mul is a structure:
;;    (make-mul BSL-var-expr BSL-var-expr)

;; A BSL-var-expr is one of:
;; - Number
;; - Symbol
;; - Add
;; - Mul

;; A BSL-fun-expr is one of:
;; - Number
;; - Symbol
;; - Fun
;; - Add
;; - Mul

(define-struct fun [name ex])
;; A Fun is a structure
;;   (make-fun Symbol BSL-fun-expr)
(make-fun 'k (make-add 1 1))

(define-struct BSL-fun-def [name param body])
;; A BSL-fun-def is a structure
;    (make-BSL-fun-def Symbol Symbol BSL-fun-expr
(define fadd2 (make-BSL-fun-def 'add2 'x (make-add 'x 2)))
(define ff (make-BSL-fun-def 'f 'x (make-add 3 'x)))
(define fg (make-BSL-fun-def 'g 'y (make-fun 'f
                                             (make-add 2 'y))))
(define fh (make-BSL-fun-def 'h 'v (make-add
                                    (make-fun 'f 'v)
                                    (make-fun 'g 'v))))

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

; BSL-fun-expr BSL-fun-def* -> BSL-value
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
(check-error (eval-variable* (make-add 2 'sy) da-fgh))
(check-error (eval-variable* (make-add 2 (make-fun 'gone 3)) da-fgh))
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
                 
                 

;; A BSL-expr is one of:
;; - Number
;; - Add (w/out Symbols)
;; - Mul (w/out Symbols)

; A BSL-value is a Number

; An AL (short for association list) is [List-of Association].
; An Association is a list of two items:
;   (cons Symbol (cons Number '())).
(define al1
  (list (cons 'x (cons 2 '()))
        (cons 'y (cons 5 '()))
        (cons 'z (cons 1 '()))))

; BSL-fun-expr Symbol Symbol BSL-fun-expr -> BSL-value
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
;(check-expect (eval-definition1 7) 7)
;(check-expect (eval-definition1 (make-mul 3 (make-add 1 2))) 9)
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

; BSL-var-expr AL -> Number
; evaluates a BSL-var-expr by substituting a set of provided
; values for the variables. If all variables are not provided
; it will error               
(check-expect (eval-var-lookup (make-add 2 2) '())
              4)
(check-expect (eval-var-lookup (make-add 'x 'y) al1)
              7)
(check-expect (eval-var-lookup (make-mul 'z
                                        (make-add 3 'y))
                              al1)
              8)
(check-error (eval-var-lookup (make-add 'x 'y) '()))
(define (eval-var-lookup e da)
  (cond
    [(number? e) e]
    [(symbol? e) (lookup e da)]
    [(add? e) (+
               (eval-var-lookup (add-left e) da)
               (eval-var-lookup (add-right e) da))]
    [(mul? e) (*
               (eval-var-lookup (mul-left e) da)
               (eval-var-lookup (mul-right e) da))]))

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

; BSL-var-expr AL -> BSL-value
; evaluates a BSL-var-expr by substituting a set of provided
; values for the variables. If all variables are not provided
; it will error
(check-expect (eval-variable* (make-add 2 2) '())
              4)
(check-expect (eval-variable* (make-add 'x 'y) al1)
              7)
(check-expect (eval-variable* (make-mul 'z
                                        (make-add 3 'y))
                              al1)
              8)
(check-error (eval-variable* (make-add 'x 'y) '()))
(define (eval-variable* ex da)
  (local
    ((define w-subs
       (foldr (lambda (v e)
                (subst e (first v) (second v)))
              ex
              da)))
    (if (numeric? w-subs)
        (eval-expression w-subs)
        (error WRONG))))

; BSL-var-expr -> BSL-value
; consumes a BSL-var-expr and determines its value if
; numeric? yields true for the input. Otherwise it signals an error.
(check-expect (eval-variable (make-add 3 4)) 7)
(check-expect (eval-variable (make-mul (make-add 2 3) 3)) 15)
(check-error (eval-variable (make-add 4 'x)))
(define (eval-variable ex)
  (if (numeric? ex)
      (eval-expression ex)
      (error WRONG)))
      

; BSL-var-exp -> Boolean
; Determines whether a BSL-var-exp is also a BSL-expr
(check-expect (numeric? (make-add 3 4)) #t)
(check-expect (numeric? 'y) #f)
(check-expect (numeric? (make-add 'x 4)) #f)
(check-expect (numeric? (make-mul
                         (make-add 2 4)
                         (make-mul 4
                                   (make-add 2 3))))
              #true)
(check-expect (numeric? (make-mul
                         (make-add 2 4)
                         (make-mul 4
                                   (make-add 'y 3))))
              #false)
(define (numeric? ex)
  (cond
    [(number? ex) #t]
    [(add? ex) (and
                (numeric? (add-right ex))
                (numeric? (add-left ex)))]
    [(mul? ex) (and
                (numeric? (mul-right ex))
                (numeric? (mul-left ex)))]
    [else #f]))

; BSL-fun-exp Symbol N -> BSL-fun-exp
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
