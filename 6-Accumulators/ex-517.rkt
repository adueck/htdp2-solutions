;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-517) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; A Lam is one of: 
; – a Symbol
; – (list 'λ (list Symbol) Lam)
; – (list Lam Lam)

(define ex1 '(λ (x) x))
(define ex2 '(λ (x) y))
(define ex3 '(λ (y) (λ (x) y)))
(define ex4 '((λ (x) (x x)) (λ (x) (x x))))

; Lam -> Lam[w nums for distance]
(check-expect (static-distance '((λ (x) ((λ (y) (y x)) x)) (λ (z) z)))
              '((λ (x) ((λ (y) (1 3)) 1)) (λ (z) 0)))
(check-expect (static-distance '(λ (x) x))
              '(λ (x) 0))
(check-expect (static-distance '(λ (x) (λ (y) x)))
              '(λ (x) (λ (y) 1)))
(define (static-distance le0)
  (local
    (; Lam [List-of [List-of Symbol]] -> Lam[w nums for distance]
     ; accumulator a is a list of all parameters on path
     ; from le to le0 in reverse order—the last one seen
     ; is first on the list
     (define (static-distance/a le a)
       (cond
         [(is-var? le) (get-distance le a)]
         [(is-λ? le) (list 'λ
                           (declareds le)
                           (static-distance/a (λ-body le) (cons (declareds le) a)))]
         [(is-app? le) (list
                        (static-distance/a (app-fun le) (cons '() a))
                        (static-distance/a (app-arg le) (cons '() a)))])))
  (static-distance/a le0 '())))

; Symbol -> [List-of [List-of Symbol]] or '*undeclared
(define (get-distance s decs0)
  (local
    ((define (get-distance/a decs n)
       (cond
         [(empty? decs) '*undeclared]
         [else (if (member s (first decs))
                   n
                   (get-distance/a (rest decs) (add1 n)))])))
    (get-distance/a decs0 0)))
   

; Lam -> Lam 
(define (undeclareds le0)
  (local (; Lam [List-of Symbol] -> Lam
          ; accumulator declareds is a list of all λ 
          ; parameters on the path from le0 to le
          (define (undeclareds/a le declareds)
            (cond
              [(is-var? le)
               (if (member? le declareds) le '*undeclared)]
              [(is-λ? le)
               (local ((define para (λ-para le))
                       (define body (λ-body le))
                       (define newd (cons para declareds)))
                 (list 'λ (list para)
                   (undeclareds/a body newd)))]
              [(is-app? le)
               (local ((define fun (app-fun le))
                       (define arg (app-arg le)))
               (list (undeclareds/a fun declareds)
                     (undeclareds/a arg declareds)))])))
    (undeclareds/a le0 '())))

; Yes, this works properly

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
