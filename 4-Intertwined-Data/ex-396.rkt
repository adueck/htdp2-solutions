;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-396) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require 2htdp/batch-io)

(define LOCATION "/usr/share/dict/words") ; on OS X
(define AS-LIST (read-lines LOCATION))
(define SIZE (length AS-LIST))
(define LETTERS (explode "abcdefghijklmnopqrstuvwxyz"))

; An HM-Word is a [List-of Letter or "_"]
; interpretation "_" represents a letter to be guessed 
 
; HM-Word N -> String
; runs a simplistic hangman game, produces the current state
(define (play the-pick time-limit)
  (local ((define the-word  (explode the-pick))
          (define the-guess (make-list (length the-word) "_"))
          ; HM-Word -> HM-Word
          (define (do-nothing s) s)
          ; HM-Word KeyEvent -> HM-Word 
          (define (checked-compare current-status ke)
            (if (member? ke LETTERS)
                (compare-word the-word current-status ke)
                current-status)))
    (implode
     (big-bang the-guess ; HM-Word
       [to-draw render-word]
       [on-tick do-nothing 1 time-limit]
       [on-key  checked-compare]))))
 
; HM-Word -> Image
(define (render-word w)
  (text (implode w) 22 "black"))

; HM-Word HM-Word Key -> HM-Word
(check-expect (compare-word '("b" "a" "g") '("_" "_" "_") "b")
              '("b" "_" "_"))
(check-expect (compare-word '("b" "a" "g") '("_" "_" "_") "f")
              '("_" "_" "_"))
(check-expect (compare-word '("l" "a" "t" "t" "e") '("_" "a" "_" "_" "_") "t")
              '("_" "a" "t" "t" "_"))
(define (compare-word w status k)
  (cond
    [(empty? w) '()]
    [else (cons
           (if (string=? (first w) k) k (first status))
           (compare-word (rest w) (rest status) k))]))

(play (list-ref AS-LIST (random SIZE)) 100)
