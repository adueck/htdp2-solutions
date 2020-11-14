;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-268-inventory-price-comparison) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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