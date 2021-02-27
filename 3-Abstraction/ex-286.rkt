;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-286) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct ir [name desc cost sell-at])
; An IR is a structure:
;   (make-ir String String Number Number)
(check-expect (sort-by-profit '()) '())
(check-expect (sort-by-profit (list (make-ir "pen" "writes" 2 3)
                                   (make-ir "table" "holds" 20 25)
                                   (make-ir "book" "written" 10 12)))
              (list (make-ir "pen" "writes" 2 3)
                    (make-ir "book" "written" 10 12)
                    (make-ir "table" "holds" 20 25)))
(define (sort-by-profit loi)
  (sort
   loi
   (lambda (ir1 ir2) (local
                       ((define (profit ir)
                         (- (ir-sell-at ir) (ir-cost ir))))
                         (< (profit ir1) (profit ir2))))))

