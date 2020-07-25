;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-109) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(define WIDTH 100)

; ExpectsToSee is one of
; - AA
; - BB
; - DD
; - ER

(define AA "start, expect an 'a'")
(define BB "expect 'b', 'c', or 'd'")
(define DD "finished")
(define ER "error, illegal key")

; ExpectsToSee -> Image
(check-expect (render AA) (square WIDTH "solid" "white"))
(check-expect (render BB) (square WIDTH "solid" "yellow"))
(check-expect (render DD) (square WIDTH "solid" "green"))
(check-expect (render ER) (square WIDTH "solid" "red"))
(define (render e)
  (square WIDTH "solid"
          (cond
            [(string=? e AA) "white"]
            [(string=? e BB) "yellow"]
            [(string=? e DD) "green"]
            [(string=? e ER) "red"])))

; ExpectsToSee KeyEvent -> ExpectsToSee
(check-expect (handle-key AA "s") ER)
(check-expect (handle-key AA "b") ER)
(check-expect (handle-key AA "a") BB)
(check-expect (handle-key BB "c") BB)
(check-expect (handle-key BB "b") BB)
(check-expect (handle-key BB "a") ER)
(check-expect (handle-key BB "w") ER)
(check-expect (handle-key BB "d") DD)
(check-expect (handle-key DD "s") DD)
(check-expect (handle-key ER "a") ER)
(define (handle-key e ke)
  (cond
    [(string=? e AA) (if (string=? "a" ke) BB ER)]
    [(string=? e BB) (cond
                       [(or (string=? "b" ke) (string=? "c" ke)) BB]
                       [(string=? "d" ke) DD]
                       [else ER])]
    [(string=? e DD) e]
    [(string=? e ER) e]))

(define (pattern-puzzle e)
  (big-bang e
    [to-draw render]
    [on-key handle-key]))
