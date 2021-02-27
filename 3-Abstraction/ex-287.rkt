;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-287) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct ir [name price])
; An IR is a structure:
;   (make-ir String Number)

(check-expect (eliminate-exp 20 (list (make-ir "dog" 30)
                                      (make-ir "cat" 19)
                                      (make-ir "frog" 20)))
              (list (make-ir "cat" 19)))
(check-expect (eliminate-exp 30 '()) '())
;[List-of IR] -> [List-of IR]
(define (eliminate-exp ua loi)
  (filter (lambda (x) (< (ir-price x) ua)) loi))

(check-expect (recall "pen" '()) '())
(check-expect (recall "pen" (list (make-ir "pen" 10)
                                  (make-ir "table" 5)
                                  (make-ir "pen" 10)))
              (list (make-ir "table" 5)))
;[List-of IR] -> [List-of IR]
(define (recall ty loi)
  (filter (lambda (x) (not (string=? (ir-name x) ty))) loi))


(check-expect (selection (list (make-ir "pen" 5)
                               (make-ir "table" 10)
                               (make-ir "dog" 3))
                         (list (make-ir "pen" 4)
                               (make-ir "dog" 2)
                               (make-ir "apple" 7)))
              (list (make-ir "pen" 4)
                    (make-ir "dog" 2)))
;[List-of IR] -> [List-of IR]
(define (selection loi1 loi2)
  (filter (lambda (x)
            (ormap (lambda (y) (string=? (ir-name y) (ir-name x))) loi1))
            loi2))
