;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-220-tetris-render) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define WIDTH 10) ; # of blocks, horizontally
(define HEIGHT WIDTH) ; # of blocks, vertically
(define SIZE 10) ; blocks are squares
(define SCENE-SIZE (* WIDTH SIZE))
(define BACKGROUND (empty-scene SCENE-SIZE SCENE-SIZE))
 
(define BLOCK ; red squares with black rims
  (overlay
    (square (- SIZE 1) "solid" "red")
    (square SIZE "outline" "black")))

(define-struct tetris [block landscape])
(define-struct block [x y])
 
; A Tetris is a structure:
;   (make-tetris Block Landscape)
; A Landscape is one of: 
; – '() 
; – (cons Block Landscape)
; A Block is a structure:
;   (make-block N N)
 
; interpretations
; (make-block x y) depicts a block whose left 
; corner is (* x SIZE) pixels from the left and
; (* y SIZE) pixels from the top;
; (make-tetris b0 (list b1 b2 ...)) means b0 is the
; dropping block, while b1, b2, and ... are resting

(define landscape0 '())
(define block-dropping (make-block 2 3))
(define tetris0 (make-tetris (make-block -1 0) '()))
(define tetris0-drop (make-tetris block-dropping '()))
(define block-landed (make-block 0 (- HEIGHT 1)))
(define block-on-block (make-block 0 (- HEIGHT 2)))
(define landscape2 (list block-landed block-on-block))
(define tetris-stacked (make-tetris block-dropping
                                    landscape2))

; Tetris -> Image
; Renders the state of a simple tetris game
(check-expect (tetris-render tetris-stacked)
              (render-block (tetris-block tetris-stacked)
                            (render-landscape (tetris-landscape tetris-stacked)
                                              BACKGROUND)))
(define (tetris-render t)
  (render-block (tetris-block t)
                (render-landscape (tetris-landscape t)
                                  BACKGROUND)))

; Landscape Image -> Image
; Renders a Landscape of blocks onto a given background
(check-expect (render-landscape landscape0 BACKGROUND) BACKGROUND)
(check-expect (render-landscape landscape2 BACKGROUND)
              (render-block (first landscape2)
                            (render-block (second landscape2)
                                          BACKGROUND)))
(define (render-landscape l im)
  (cond
    [(empty? l) BACKGROUND]
    [else (render-block (first l)
                        (render-landscape (rest l) im))]))

; Block Image -> Image
; Renders a block onto a im background Image
(check-expect (render-block block-dropping BACKGROUND)
              (place-image/align BLOCK
                                 (* 2 SIZE)
                                 (* 3 SIZE)
                                 "left" "top"
                                 BACKGROUND))
(define (render-block b im)
  (place-image/align BLOCK
                     (* (block-x b) SIZE)
                     (* (block-y b) SIZE)
                     "left" "top"
                     im))
