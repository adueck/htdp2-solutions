;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-221-tetris-main) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; Tetris -> Tetris
; drops the block and handles landing etc
(check-expect (tock tetris-stacked)
              (make-tetris (make-block 2 4)
                           landscape2))
(define (tock t)
  (if (landed? (tetris-block t) (tetris-landscape t))
      (make-tetris
       ; start a new block dropping at random x coord
       (make-block (random WIDTH) 0)
       ; add the landed block to the landscape
       (cons (tetris-block t) (tetris-landscape t)))
      (make-tetris
       (drop1 (tetris-block t))
       (tetris-landscape t))))

; Block Landscape -> Boolean
; Checks if a block has landed
(define (landed? b l)
  (or
   (hit-bottom? b)
   (hit-landscape? b l)))

; Block -> Boolean
; Checks if the block has hit the bottom
(check-expect (hit-bottom? block-dropping) #false)
(check-expect (hit-bottom? block-landed) #true)
(define (hit-bottom? b)
  (= (block-y b) (sub1 HEIGHT)))

; Block Landscape -> Boolean
; Checks if the block has hit the landscape
(define (hit-landscape? b l) #false)

; Block -> Block
; Drops a block down one position
(check-expect (drop1 (make-block 3 4)) (make-block 3 5))
(define (drop1 b)
  (make-block
   (block-x b) (add1 (block-y b))))
