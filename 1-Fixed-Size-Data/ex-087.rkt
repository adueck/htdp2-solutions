;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-087) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define HEIGHT 20)
(define WIDTH 200)
(define CURSOR (rectangle 1 20 "solid" "red"))
(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define FONTSIZE 16)

(define-struct editor [text index])
; An Editor is a structure:
;   (make-editor String Number)
; interpretation (make-editor s i) describes an editor
; whose visible text is s with 
; the cursor displayed at position i

;; RENDERING SECTION
;;
; Editor -> Image
; renders an image of the editor
(check-expect (render (make-editor "hello world" 0))
  (overlay/align "left" "center"
    (beside
      CURSOR
      (render-text "hello world"))
    BACKGROUND))
(check-expect (render (make-editor "hello world" 5))
  (overlay/align "left" "center"
    (beside
      (render-text "hello")
      CURSOR
      (render-text " world"))
    BACKGROUND))
(define (render ed)
  (overlay/align "left" "center"
   (beside
     (render-text (pre-text (editor-text ed) (editor-index ed)))
     CURSOR
     (render-text (post-text (editor-text ed) (editor-index ed))))
   BACKGROUND))

; String -> Image
; renders an image block of text
(check-expect (render-text "hello")
              (text "hello" FONTSIZE "black"))
(define (render-text t)
  (text t FONTSIZE "black"))

; Editor 1String -> Boolean
; checks to see if a given new character will fit in the
; GUI space if rendered
(check-expect (will-fit?
               (make-editor "not much" 1) "a")
              #true)
(check-expect (will-fit?
               (make-editor "way too much text this will not fit at all" 1) "a")
              #false)
(define (will-fit? ed c)
  (< (image-width
   (render-text
    (string-append
     (editor-text ed) c))) WIDTH))
;;
;; END OF RENDERING SECTION

(define (run ed)
  (big-bang ed
    [to-draw render]
    [on-key edit]))

; Editor KeyEvent -> Editor
(check-expect (edit (make-editor "ello" 0) "h")
              (make-editor "hello" 1))
(check-expect (edit (make-editor "hello" 1) "left")
              (make-editor "hello" 0))
(check-expect (edit (make-editor "hello" 0) "left")
              (make-editor "hello" 0))
(check-expect (edit (make-editor "hello" 5) "right")
              (make-editor "hello" 5))
(check-expect (edit (make-editor "hello" 2) "right")
              (make-editor "hello" 3))
(check-expect (edit (make-editor "hello" 1) "\b")
              (make-editor "ello" 0))
(check-expect (edit (make-editor "ello" 0) "\b")
              (make-editor "ello" 0))
(check-expect (edit (make-editor "" 0) "\b")
              (make-editor "" 0))
(check-expect (edit (make-editor "hello" 2) "\r")
              (make-editor "hello" 2))
(check-expect (edit (make-editor "hello" 2) "\t")
              (make-editor "hello" 2))
(check-expect (edit
               (make-editor "hello this is a lot of text here oh boy" 3)
               "a")
               (make-editor "hello this is a lot of text here oh boy" 3))

(define (edit ed ke)
  (cond
    [(string=? "\b" ke) (backspace ed)]
    [(string=? "\t" ke) ed]
    [(string=? "\r" ke) ed]
    [(= (string-length ke) 1)
     (if (will-fit? ed ke) (add-char ed ke) ed)]
    [(string=? "left" ke) (cursor-left ed)]
    [(string=? "right" ke) (cursor-right ed)]
    [else ed]))

; Editor -> Editor
; moves the editor index back one value, if possible
(check-expect (cursor-left (make-editor "foo" 1))
              (make-editor "foo" 0))
(check-expect (cursor-left (make-editor "foo" 0))
              (make-editor "foo" 0))
(define (cursor-left ed)
  (cond
    [(= (editor-index ed) 0) ed]
    [else (make-editor
           (editor-text ed)
           (sub1 (editor-index ed)))]))

; Editor -> Editor
; moves the editor index forward one value, if possible
(check-expect (cursor-right (make-editor "foo" 1))
              (make-editor "foo" 2))
(check-expect (cursor-right (make-editor "foo" 3))
              (make-editor "foo" 3))
(define (cursor-right ed)
  (cond
    [(=
      (editor-index ed)
      (string-length (editor-text ed))) ed]
    [else (make-editor
           (editor-text ed)
           (add1 (editor-index ed)))]))

; Editor -> Editor
; executes one backspace action on the text at the cursor index
(check-expect (backspace (make-editor "bar" 1))
              (make-editor "ar" 0))
(check-expect (backspace (make-editor "bar" 2))
              (make-editor "br" 1))
(define (backspace ed)
  (cursor-left
   (make-editor
    (string-append
     (remove-last-char
      (pre-text (editor-text ed) (editor-index ed)))
     (post-text (editor-text ed) (editor-index ed)))
    (editor-index ed))))

; Editor 1String -> Editor
; adds character c to the editor at the cursor index
(check-expect (add-char
               (make-editor "oo" 0) "f")
              (make-editor "foo" 1))
(check-expect (add-char
               (make-editor "fo" 2) "o")
              (make-editor "foo" 3))
(define (add-char ed c)
  (cursor-right
   (make-editor
    (string-append
     (pre-text (editor-text ed) (editor-index ed))
     c
     (post-text (editor-text ed) (editor-index ed)))
    (editor-index ed))))

;; STRING HELPER FUNCTIONS

; String Number -> String
; returns the part of text in front of a given index
(check-expect (pre-text "foo" 0) "")
(check-expect (pre-text "foo" 2) "fo")
(define (pre-text s i)
  (cond
    [(= i 0) ""]
    [else (substring s 0 i)]))

; String Number -> String
; returns the part of text after of a given index
(check-expect (post-text "foo" 0) "foo")
(check-expect (post-text "foo" 2) "o")
(check-expect (post-text "foo" 3) "")
(define (post-text s i)
  (substring s i))

; String -> String
; removes the last character on a string, if any
(check-expect (remove-last-char "foo") "fo")
(check-expect (remove-last-char "") "")
(define (remove-last-char s)
  (cond
    [(= (string-length s) 0) ""]
    [else (substring s 0 (sub1 (string-length s)))]))

