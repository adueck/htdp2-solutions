;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-516) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; A Lam is one of: 
; – a Symbol
; – (list 'λ (list Symbol) Lam)
; – (list Lam Lam)

(define ex1 '(λ (x) x))
(define ex2 '(λ (x) y))
(define ex3 '(λ (y) (λ (x) y)))
(define ex4 '((λ (x) (x x)) (λ (x) (x x))))

; A LamS is one of:
; - a Symbol
; - λS
; - AppS

(define-struct λS [para body])
; A λS is a Structure
;  (make-λS [List-of Symbol] LamS)

(define-struct appS [fun arg])
; A AppS is a Structure
;  (make-AppS LamS LamS)

(define ex1S (make-λS 'x 'x))
(define ex2S (make-λS 'x 'y))
(define ex3S (make-λS 'y (make-λS 'x 'y)))

; LamS -> LamS 
(define (undeclareds le0)
  (local (; Lam [List-of Symbol] -> Lam
          ; accumulator declareds is a list of all λ 
          ; parameters on the path from le0 to le
          (define (undeclareds/a le declareds)
            (cond
              [(symbol? le)
               (if (member? le declareds) le '*undeclared)]
              [(λS? le)
               (local ((define para (λS-para le))
                       (define body (λS-body le))
                       (define newd (cons para declareds)))
                 (make-λS (list para)
                   (undeclareds/a body newd)))]
              [(appS? le)
               (local ((define fun (appS-fun le))
                       (define arg (appS-arg le)))
               (list (undeclareds/a fun declareds)
                     (undeclareds/a arg declareds)))])))
    (undeclareds/a le0 '())))

; Any -> Boolean
; Says if a given Lam is a variable (a Symbol)
(check-expect (is-var? 'A) #t)
(check-expect (is-var? '(λ (x) x)) #f)
(check-expect (is-var? '(A B)) #f)
(define (is-var? lam)
  (symbol? lam))

; Any -> Boolean
; Says if a given Lam is a λ ((list 'λ (list Symbol) Lam))
(check-expect (is-λ? 'A) #f)
(check-expect (is-λ? "foo") #f)
(check-expect (is-λ? '(λ (x) x)) #t)
(check-expect (is-λ? '(A B)) #f)
(define (is-λ? lam)
  (and (cons? lam)
       (symbol? (first lam))
       (symbol=? (first lam) 'λ)))

; Any -> Boolean
; Says if a given Lam is an application ((list Lam Lam))
(check-expect (is-app? 'A) #f)
(check-expect (is-app? '(λ (x) x)) #f)
(check-expect (is-app? '(A B)) #t)
(check-expect (is-app? '((λ (x) x) B)) #t)
(define (is-app? lam)
  (and (cons? lam)
       (not (and
             (symbol? (first lam))
             (symbol=? (first lam) 'λ)))))

; Lam[is-λ?] -> Lam
(check-expect (λ-para '(λ (x) y)) 'x)
(check-error (λ-para '(A B)))
(define (λ-para lam)
  (if (not (is-λ? lam))
      (error "is not λ")
      (first (second lam))))

; Lam[is-λ?] -> Lam
(check-expect (λ-body '(λ (x) y)) 'y)
(check-expect (λ-body '(λ (x) (λ (y) x))) '(λ (y) x))
(check-error (λ-body '(A B)))
(define (λ-body lam)
  (if (not (is-λ? lam))
      (error "is not λ")
      (third lam)))

; Lam[is-app?] -> Lam
(check-expect (app-fun '(A B) )'A)
(define (app-fun lam)
  (if (not (is-app? lam))
      (error "is not app")
      (first lam)))

; Lam[is-app?] -> Lam
(check-expect (app-arg '(A B) ) 'B)
(define (app-arg lam)
  (if (not (is-app? lam))
      (error "is not app")
      (second lam)))

; List[is-λ?] -> [List-of Symbol]
(check-expect (declareds '(λ (x y z) x)) '(x y z))
(define (declareds lam)
  (if (not (is-λ? lam))
      (error "is not λ")
      (second lam)))
