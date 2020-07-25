;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-178-editor-kh) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define HEIGHT 20) ; the height of the editor 
(define WIDTH 200) ; its width 
(define FONT-SIZE 16) ; the font size 
(define FONT-COLOR "black") ; the font color 
 
(define MT (empty-scene WIDTH HEIGHT))
(define CURSOR (rectangle 1 HEIGHT "solid" "red"))

(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor Lo1S Lo1S) 
; An Lo1S is one of: 
; – '()
; – (cons 1String Lo1S)

(define good
  (cons "g" (cons "o" (cons "o" (cons "d" '())))))
(define all
  (cons "a" (cons "l" (cons "l" '()))))
(define lla
  (cons "l" (cons "l" (cons "a" '()))))

; data example 1: 
(make-editor all good)
 
; data example 2:
(make-editor lla good)

; Lo1s -> Lo1s 
; produces a reverse version of the given list 
 
(check-expect
  (rev (cons "a" (cons "b" (cons "c" '()))))
  (cons "c" (cons "b" (cons "a" '()))))
 
(define (rev l)
  (cond
    [(empty? l) '()]
    [(cons? l) (add-at-end (rev (rest l)) (first l))]))

; Lo1s -> 1String
; adds a 1String to the end of a list of strings

(check-expect (add-at-end '() "a")
              (cons "a" '()))
(check-expect (add-at-end (cons "a" (cons "b" '())) "c")
              (cons "a" (cons "b" (cons "c" '()))))

(define (add-at-end l s)
  (cond
    [(empty? l) (cons s '())]
    [(cons? l) (cons (first l) (add-at-end (rest l) s))]))

; String String -> Editor
; produces an editor starting with the pre and post strings

(check-expect (create-editor "" "")
              (make-editor '() '()))
(check-expect (create-editor "foo" "bar")
              (make-editor
               (cons "o" (cons "o" (cons "f" '())))
               (cons "b" (cons "a" (cons "r" '())))))

(define (create-editor pres posts)
  (make-editor
   (rev (explode pres))
   (explode posts)))

; Editor -> Image
; renders an editor as an image of the two texts 
; separated by the cursor 
(define (editor-render e) MT)

; Editor KeyEvent -> Editor
; deals with a key event, given some editor
; check adding character
(check-expect (editor-kh (create-editor "" "") "e")
              (create-editor "e" ""))
(check-expect
  (editor-kh (create-editor "cd" "fgh") "e")
  (create-editor "cde" "fgh"))
(check-expect
  (editor-kh (create-editor "cdef" "") "g")
  (create-editor "cdefg" ""))
; check backspace
(check-expect (editor-kh (create-editor "" "") "\b")
              (create-editor "" ""))
(check-expect
  (editor-kh (create-editor "cd" "fgh") "\b")
  (create-editor "c" "fgh"))
(check-expect
  (editor-kh (create-editor "cdef" "") "\b")
  (create-editor "cde" ""))
; check right
(check-expect (editor-kh (create-editor "" "") "right")
              (create-editor "" ""))
(check-expect
  (editor-kh (create-editor "cd" "fgh") "right")
  (create-editor "cdf" "gh"))
(check-expect
  (editor-kh (create-editor "cdef" "") "right")
  (create-editor "cdef" ""))
; check left
(check-expect (editor-kh (create-editor "" "") "left")
              (create-editor "" ""))
(check-expect
  (editor-kh (create-editor "cd" "fgh") "left")
  (create-editor "c" "dfgh"))
(check-expect
  (editor-kh (create-editor "cdef" "") "left")
  (create-editor "cde" "f"))
; check tab
(check-expect (editor-kh (create-editor "" "") "\t")
              (create-editor "" ""))
(check-expect
  (editor-kh (create-editor "cd" "fgh") "\t")
  (create-editor "cd" "fgh"))
(check-expect
  (editor-kh (create-editor "cdef" "") "\t")
  (create-editor "cdef" ""))
; check return
(check-expect (editor-kh (create-editor "" "") "\r")
              (create-editor "" ""))
(check-expect
  (editor-kh (create-editor "cd" "fgh") "\r")
  (create-editor "cd" "fgh"))
(check-expect
  (editor-kh (create-editor "cdef" "") "\r")
  (create-editor "cdef" ""))

(define (editor-kh ed k)
  (cond
    [(key=? k "left") ...]
    [(key=? k "right") ...]
    [(key=? k "\b") ...]
    [(key=? k "\t") ...]
    [(key=? k "\r") ...]
    [(= (string-length k) 1) ...]
    [else ...]))

; the template for editor-kh deals with "\t" and "\r" before
; it checks for strings of length 1 because "\t" and "\r" are
; considered strings with a length of one and they need to
; be treated differently before getting lumped in with the others

; main : String -> Editor
; launches the editor given some initial string 
(define (main s)
   (big-bang (create-editor s "")
     [on-key editor-kh]
     [to-draw editor-render]))
