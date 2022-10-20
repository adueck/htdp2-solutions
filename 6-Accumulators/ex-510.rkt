;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-510) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/batch-io)
(require racket/string)

; File -> File
; formats a File at in-f to have a maximum line width of w
; without breaking words
; outputs the formatted File at out-f
; LIMITATION - no words allowed longer that the line length limit w
(define (fmt in-f out-f w)
  (local
    ((define lines (read-lines in-f))
     (define formatted (limit-lines lines w)))
  (write-file out-f
              (string-join formatted "\n"))))

; A Line is a String
; A Word is a String

; [List-of Line] -> [List-of Line]
; limits lines to width of w
(check-expect (limit-lines
               (list "Hello, how are you my friend"
                     "This is a message")
               19)
              (list "Hello, how are you"
                    "my friend"
                    "This is a message"))
(check-expect (limit-lines
               (list "Hello, how are you my friend I haven't seen in so very long now"
                     "This is a message")
               19)
              (list "Hello, how are you"
                    "my friend I haven't"
                    "seen in so very"
                    "long now"
                    "This is a message"))
(define (limit-lines lines0 w)
  (local
    (; [List-of Line] [List-of Line] -> [List-of Line]
     ; accumulator a is the lines that have already been
     ; width limited
     (define (limit-lines/a lines a)
       (cond
         [(empty? lines) a]
         [else (limit-lines/a
                (rest lines)
                (append a
                        (limit-line (first lines) w)))])))
    (limit-lines/a lines0 '())))

; Line -> [List-of Line]
(define (limit-line l w)
  (local
    (; Line Line -> [List-of Line]
     (define (limit-line/a line wrapped)
       (if (<= (string-length line) w)
           (if (string=? "" wrapped)
               (list line)
               (append (list line)
                       (limit-line wrapped w)))
           (limit-line/a (remove-last-word line)
                         (add-word (get-last-word line) wrapped)))))
    (limit-line/a l "")))

; Line -> Line
(check-expect (remove-last-word "This is cool")
              "This is")
(define (remove-last-word line)
  (string-join
   (reverse (rest (reverse (string-split line))))))

; Line -> Word
(check-expect (get-last-word "foo bar baz")
              "baz")
(check-expect (get-last-word "cool")
              "cool")
(define (get-last-word line)
  (first (reverse (string-split line))))

; Word Line -> Line
(check-expect (add-word "candy" "is yummy")
              "candy is yummy")
(check-expect (add-word "foo" "")
              "foo")
(define (add-word word line)
  (string-join (cons word (string-split line))))
    