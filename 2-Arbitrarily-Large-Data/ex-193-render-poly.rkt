;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-193-render-poly) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; A Polygon is one of:
; – (list Posn Posn Posn)
; – (cons Posn Polygon)
(define triangle-p
  (list
    (make-posn 20 10)
    (make-posn 20 20)
    (make-posn 30 20)))
 

(define square-p
  (list
    (make-posn 10 10)
    (make-posn 20 10)
    (make-posn 20 20)
    (make-posn 10 20)))


; An NELoP is one of: 
; – (cons Posn '())
; – (cons Posn NELoP)

; a plain background image 
(define MT (empty-scene 50 50))

; Image Polygon -> Image
; renders the given polygon p into img
(check-expect
  (render-poly MT triangle-p)
  (scene+line
    (scene+line
      (scene+line MT 20 10 20 20 "red")
      20 20 30 20 "red")
    30 20 20 10 "red"))
(check-expect
  (render-poly MT square-p)
  (scene+line
    (scene+line
      (scene+line
        (scene+line MT 10 10 20 10 "red")
        20 10 20 20 "red")
      20 20 10 20 "red")
    10 20 10 10 "red"))
; variation a
;(define (render-poly img p)
;  (connect-dots img (cons (last p) p)))
; variation b
(define (render-poly img p)
  (connect-dots img (add-at-end (first p) p)))

; NELoP -> Posn
; extracts the last item from p
(check-expect (last (list
                     (make-posn 2 2)
                     (make-posn 3 4)
                     (make-posn 5 10)))
                    (make-posn 5 10))
(define (last p)
  (cond
    [(empty? (rest p)) (first p)]
    [else (last (rest p))]))

; NELoP -> Posn
; adds a posn onto a list of posns
(check-expect (add-at-end (make-posn 2 3)
                          (list
                           (make-posn 1 1)
                           (make-posn 2 2)))
              (list (make-posn 1 1)
                    (make-posn 2 2)
                    (make-posn 2 3)))
(define (add-at-end p lop)
  (cond
    [(empty? (rest lop)) (list
                          (first lop)
                          p)]
    [else (cons (first lop)
                (add-at-end p (rest lop)))]))


; Image Posn Posn -> Image 
; draws a red line from Posn p to Posn q into im
(check-expect (render-line MT
                (make-posn 10 5)
                (make-posn 20 20))
              (scene+line
               MT 10 5 20 20 "red"))
(define (render-line im p q)
  (scene+line
    im
    (posn-x p) (posn-y p) (posn-x q) (posn-y q)
    "red"))

; Image NELoP -> Image
; connetcs the dots in a p by rendering lines in img
(check-expect (connect-dots MT triangle-p)
              (scene+line
               (scene+line MT 20 20 30 20 "red")
               20 10 20 20 "red"))
(check-expect (connect-dots MT square-p)
              (scene+line
               (scene+line
                (scene+line MT 20 20 10 20 "red")
                20 10 20 20 "red")
               20 10 10 10 "red"))
(define (connect-dots img p)
  (cond
    [(empty? (rest p)) img]
    [else (render-line
           (connect-dots img (rest p))
           (first p)
           (second p))]))
