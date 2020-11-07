;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-261) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct ir [name price])
; An IR is a structure:
;   (make-ir String Number)

; An Inventory is a [List-of IR]

; Inventory -> Inventory
; creates an Inventory from an-inv for all
; those items that cost less than a dollar
(check-expect (extract1
               (list (make-ir "book" 5)
                     (make-ir "pencil" 0.3)
                     (make-ir "staple" 0.1)))
              (list (make-ir "pencil" 0.3)
                    (make-ir "staple" 0.1)))
(define (extract1 an-inv)
  (cond
    [(empty? an-inv) '()]
    [else
     (local
       ((define extract1-rest (extract1 (rest an-inv))))
       (cond
         [(<= (ir-price (first an-inv)) 1.0)
          (cons (first an-inv) extract1-rest)]
         [else extract1-rest]))]))

; pre refactor - 40 steps

; post refactor using local - 46 steps

; It didn't help performance at all to save the results of
; extract1-rest in a variable, because in each run of the condition
; the (extract1 (rest an-inv)) would only get run once anyways.

