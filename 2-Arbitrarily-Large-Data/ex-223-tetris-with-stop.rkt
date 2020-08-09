;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-223-tetris-with-stop) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; A Direction is one of the following Strings:
; - "left"
; - "right"
; interpratation - the two directions that a block can be moved

(define landscape0 '())
(define block-dropping (make-block 2 3))
(define tetris0 (make-tetris (make-block -1 0) '()))
(define tetris0-drop (make-tetris block-dropping '()))
(define block-landed (make-block 0 (- HEIGHT 1)))
(define block-on-block (make-block 0 (- HEIGHT 2)))
(define landscape2 (list block-landed block-on-block))
(define tetris-stacked (make-tetris block-dropping
                                    landscape2))

; Tetris KeyEvent -> Tetris
; Moves dropping block left and right on key presses
(check-expect (handle-key (make-tetris (make-block 0 0) '()) "right")
              (make-tetris (make-block 1 0) '()))
(check-expect (handle-key (make-tetris (make-block 0 0) '()) "left")
              (make-tetris (make-block 0 0) '()))
(check-expect (handle-key (make-tetris (make-block (sub1 WIDTH) 0) '()) "left")
              (make-tetris (make-block (- WIDTH 2) 0) '()))
(check-expect (handle-key (make-tetris (make-block (sub1 WIDTH) 0) '()) "right")
              (make-tetris (make-block (sub1 WIDTH) 0) '()))
(check-expect (handle-key (make-tetris (make-block 2 2) '()) "a")
              (make-tetris (make-block 2 2) '()))
(check-expect (handle-key (make-tetris (make-block 2 (- HEIGHT 2))
                                       (list (make-block 1 (- HEIGHT 1))
                                             (make-block 1 (- HEIGHT 2))))
                          "left")
              (make-tetris (make-block 2 (- HEIGHT 2))
                           (list (make-block 1 (- HEIGHT 1))
                                 (make-block 1 (- HEIGHT 2)))))
              
(define (handle-key t ke)
  (cond
    [(or
      (key=? ke "left")
      (key=? ke "right")) (make-tetris
                           (if
                            (is-clear? 
                             (mv-block ke (tetris-block t))
                             (tetris-landscape t))
                            (mv-block ke (tetris-block t))
                            (tetris-block t))
                           (tetris-landscape t))]
    [else t]))

; Block Landscape -> Boolean
; Checks that a block does not collide with an existing landscape
(check-expect (is-clear? (make-block 2 3)
                        (list (make-block 10 11))) #true)
(check-expect (is-clear? (make-block 1 1) '()) #true)
(check-expect (is-clear? (make-block 10 10) (list (make-block 2 2)
                                                 (make-block 10 10)))
              #false)
(define (is-clear? b l)
  (not (member? b l)))

; Direction Block -> Block
; Moves a block in d Direction where possible
(check-expect (mv-block "right" (make-block 0 0))
              (make-block 1 0))
(check-expect (mv-block "left" (make-block 0 0))
              (make-block 0 0))
(check-expect (mv-block "right" (make-block (sub1 WIDTH) 0))
              (make-block (sub1 WIDTH) 0))
(check-expect (mv-block "left" (make-block 3 0))
              (make-block 2 0))
(define (mv-block d b)
  (cond
    [(string=? "right" d) (make-block
                           (if (= (block-x b) (sub1 WIDTH))
                               (sub1 WIDTH)
                               (add1 (block-x b)))
                           (block-y b))]
    [(string=? "left" d) (make-block
                           (if (= (block-x b) 0)
                               0
                               (sub1 (block-x b)))
                           (block-y b))]))

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
(check-expect (tock (make-tetris (make-block 0 (sub1 HEIGHT)) '()))
              (make-tetris (make-block 1 0)
                           (list (make-block 0 (sub1 HEIGHT)))))
(define (tock t)
  (if (landed? (tetris-block t) (tetris-landscape t))
      (make-tetris
       ; start a new block dropping at random x coord
       (new-drop (tetris-block t)) 
       ; add the landed block to the landscape
       (cons (tetris-block t) (tetris-landscape t)))
      (make-tetris
       (drop1 (tetris-block t))
       (tetris-landscape t))))

; Block -> Block
; Creates a new block for dropping one row over
(check-expect (new-drop block-landed) (make-block 1 0))
(check-expect (new-drop (make-block (sub1 WIDTH) 5))
                        (make-block 0 0))
(define (new-drop b)
  (make-block
   (modulo (add1 (block-x b)) WIDTH) 0))

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
(check-expect (hit-landscape? (make-block 0 (- HEIGHT 2)) '())
              #false)
(check-expect (hit-landscape? (make-block 0 (- HEIGHT 2))
                              (list (make-block 0 (- HEIGHT 1))))
              #true)
(define (hit-landscape? b l)
  (cond
    [(empty? l) #false]
    [else (or
           (landed-on? b (first l))
           (hit-landscape? b (rest l)))]))

; Block Block -> Boolean
(check-expect (landed-on? block-on-block block-landed) #true)
(check-expect (landed-on? block-dropping block-landed) #false)
(define (landed-on? b1 b2)
  (and
   (= (block-x b1) (block-x b2))
   (= (add1 (block-y b1)) (block-y b2))))

; Block -> Block
; Drops a block down one position
(check-expect (drop1 (make-block 3 4)) (make-block 3 5))
(define (drop1 b)
  (make-block
   (block-x b) (add1 (block-y b))))

; Tetris -> Boolean
; Checks to see if the game is over
(check-expect (game-over? tetris-stacked) #f)
(check-expect (game-over? (make-tetris (make-block 0 0)
                                       (list
                                        (make-block 3 1)
                                        (make-block 3 0))))
                          #t)
(define (game-over? t)
  (touches-top? (tetris-landscape t)))

; Landscape -> Boolean
; Check if a landscape touches the top
(define (touches-top? l)
  (cond
    [(empty? l) #f]
    [else (or
           (block-touches-top? (first l))
           (touches-top? (rest l)))]))

; Block -> Boolean
; Check if a block hits the top
(check-expect (block-touches-top? (make-block 4 0)) #t)
(check-expect (block-touches-top? (make-block 4 3)) #f)
(define (block-touches-top? b)
  (= (block-y b) 0))

(define start-t (make-tetris (make-block 2 0) '()))
(define (tetris-main rate)
         (big-bang start-t
           [to-draw tetris-render]
           [on-key handle-key]
           [stop-when game-over?]
           [on-tick tock rate]))
