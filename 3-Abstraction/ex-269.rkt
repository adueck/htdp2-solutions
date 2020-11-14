;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-269) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct ir [name description acq-price sales-price])
; An ir is a structure:
;   (make-ir String String Number Number)

; [List-of ir] -> [List-of ir]
; sorts a list of inventory records by the
; difference between the acquisition and sales prices
(check-expect (sort-by-profit '()) '())
(check-expect (sort-by-profit
               (list
                (make-ir "cup" "holds water" 1 4)
                (make-ir "fork" "stabs food" 3 5)
                (make-ir "plate" "holds food" 2 10)))
              (list
                (make-ir "fork" "stabs food" 3 5)
                (make-ir "cup" "holds water" 1 4)
                (make-ir "plate" "holds food" 2 10)))              
(define (sort-by-profit loi)
  (local (; ir ir -> Boolean
          ; determines if the first ir has smaller difference
          ; in acquisition and sales prices than the second
          (define (compare-profit i j)
            (local
              (; ir -> Number
               ; gives the difference in acquisition and sales prices
               ; for one inventory item
               (define (price-dif k)
                 (abs (- (ir-sales-price k) (ir-acq-price k)))))
              (< (price-dif i) (price-dif j)))))
    (sort loi compare-profit)))

; Number [List-of ir] -> [List-of ir]
; produces a list of all those structures whose sales price is below ua
(check-expect (eliminate-expensive 5 '()) '())
(check-expect (eliminate-expensive 6 (list
                (make-ir "cup" "holds water" 1 4)
                (make-ir "fork" "stabs food" 3 5)
                (make-ir "plate" "holds food" 2 10)))
              (list
                (make-ir "cup" "holds water" 1 4)
                (make-ir "fork" "stabs food" 3 5)))
(define (eliminate-expensive ua loi)
  (local
    (; ir -> Boolean
     ; checks if a price is below or equal to ua
     (define (cheap-enough? i)
       (<= (ir-sales-price i) ua)))
    (filter cheap-enough? loi)))

; String [List-of ir] -> [List of ir]
; produces a list of inventory records that do not use the name ty
(check-expect (recall "cup" '()) '())
(check-expect (recall "plate" (list
                (make-ir "cup" "holds water" 1 4)
                (make-ir "fork" "stabs food" 3 5)
                (make-ir "plate" "holds food" 2 10)))
              (list
                (make-ir "cup" "holds water" 1 4)
                (make-ir "fork" "stabs food" 3 5)))
(define (recall ty loi)
  (local
    (; ir -> Boolean
     (define (will-recall? i)
       (not (string=? (ir-name i) ty))))
    (filter will-recall? loi)))

; A Name is a String

; [List-of Name] [List-of Name] -> [List-of Name]
; selects all those from the second one that are also on the first
(check-expect (selection
               (list "bill" "frank" "joe")
               (list "bill" "joe" "george"))
              (list "bill" "joe"))
(define (selection lon1 lon2)
  (local
    (; Name -> Boolean
     (define (is-on-first-list? n)
       (member? n lon1)))
  (filter is-on-first-list? lon2)))
