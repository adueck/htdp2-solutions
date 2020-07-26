;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-188-sort-emails) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct email [from date message])
; An Email Message is a structure: 
;   (make-email String Number String)
; interpretation (make-email f d m) represents text m 
; sent by f, d seconds after the beginning of time

(define em1 (make-email "john" 200 "Hi"))
(define em2 (make-email "bill" 210 "Bye"))
(define em3 (make-email "frank" 220 "Ciao"))

; List-of-emails -> List-of-emails
; Sorts emails by date starting with the most recent
(check-expect (sort-by-date '()) '())
(check-expect (sort-by-date (list em2 em1 em3)) (list em3 em2 em1))
(define (sort-by-date l)
  (cond
    [(empty? l) '()]
    [(cons? l) (insert-by-date
                (first l)
                (sort-by-date (rest l)))]))

; Sorts emails by sender name
(check-expect (sort-by-name '()) '())
(check-expect (sort-by-name (list em2 em1 em3)) (list em2 em3 em1))
(define (sort-by-name l)
  (cond
    [(empty? l) '()]
    [(cons? l) (insert-by-name
                (first l)
                (sort-by-name (rest l)))]))


; Email -> List-of-emails
; inserts an email into an existing list of emails
; sorted from most recent to oldest
(check-expect (insert-by-date em1 '()) (list em1))
(check-expect (insert-by-date em2 (list em3 em1)) (list em3 em2 em1))
(define (insert-by-date e l)
  (cond
    [(empty? l) (list e)]
    [(cons? l) (if (newer? e (first l))
                   (cons e l)
                   (cons (first l) (insert-by-date e (rest l))))]))

; Email -> List-of-emails
; inserts an email into an existing list of emails sorted by sender name
(check-expect (insert-by-name em1 '()) (list em1))
(check-expect (insert-by-name em3 (list em2 em1)) (list em2 em3 em1))
(define (insert-by-name e l)
  (cond
    [(empty? l) (list e)]
    [(cons? l) (if (sender<? e (first l))
                   (cons e l)
                   (cons (first l) (insert-by-name e (rest l))))]))

; Email Email -> Boolean
; checks to see if e1's sender comes before e2's sender alphabetically
(check-expect (sender<? em2 em3) #true)
(check-expect (sender<? em3 em2) #false)
(define (sender<? e1 e2)
  (string<? (email-from e1) (email-from e2)))

; Email Email -> Boolean
; checks to see if e1 is newer or the same age as e2
(check-expect (newer? em1 em2) #f)
(check-expect (newer? em2 em1) #t)
(define (newer? e1 e2)
  (>= (email-date e1) (email-date e2)))    
