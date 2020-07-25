;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-085) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define HEIGHT 20)
(define WIDTH 200)
(define CURSOR (rectangle 1 20 "solid" "red"))
(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define FONTSIZE 16)

(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor String String)
; interpretation (make-editor s t) describes an editor
; whose visible text is (string-append s t) with 
; the cursor displayed between s and t

; Editor -> Image
; renders an image of the editor
(check-expect (render (make-editor "hello" "world"))
  (overlay/align "left" "center"
    (beside
      (render-text "hello")
      CURSOR
      (render-text "world"))
    BACKGROUND))
(define (render ed)
  (overlay/align "left" "center"
   (beside
     (render-text (editor-pre ed))
     CURSOR
     (render-text (editor-post ed)))
   BACKGROUND))

; String -> Image
; renders an image block of text
(check-expect (render-text "hello")
              (text "hello" FONTSIZE "black"))
(define (render-text t)
  (text t FONTSIZE "black"))

; Editor KeyEvent -> Editor
(check-expect (edit (make-editor "hello" "world") "o")
              (make-editor "helloo" "world"))
(check-expect (edit (make-editor "hello" "world") "left")
              (make-editor "hell" "oworld"))
(check-expect (edit (make-editor "hello" "world") "right")
              (make-editor "hellow" "orld"))
(check-expect (edit (make-editor "" "world") "left")
              (make-editor "" "world"))
(check-expect (edit (make-editor "hello world" "") "right")
              (make-editor "hello world" ""))
(check-expect (edit (make-editor "hello" "world") "\b")
              (make-editor "hell" "world"))
(check-expect (edit (make-editor "" "world") "\b")
              (make-editor "" "world"))
(check-expect (edit (make-editor "" "world") "\t")
              (make-editor "" "world"))
(check-expect (edit (make-editor "" "world") "\r")
              (make-editor "" "world"))

(define (edit ed ke)
  (cond
    [(string=? "\b" ke) (backspace ed)]
    [(string=? "\t" ke) ed]
    [(string=? "\r" ke) ed]
    [(= (string-length ke) 1)
     (add-to-pre ed ke)]
    [(string=? "left" ke) (move-left ed)]
    [(string=? "right" ke) (move-right ed)]
    [else ed]))

; Editor -> Editor
; executes one backspace action on the editor
(check-expect (backspace (make-editor "hello" "world"))
              (make-editor "hell" "world"))
(check-expect (backspace (make-editor "" ""))
              (make-editor "" ""))
(define (backspace ed)
  (make-editor
   (remove-last (editor-pre ed))
   (editor-post ed)))

; Editor -> Editor
; moves the cursor one position left
(check-expect (move-left (make-editor "hello" "world"))
              (make-editor "hell" "oworld"))
(check-expect (move-left (make-editor "" "world"))
              (make-editor "" "world"))
(define (move-left ed)
  (make-editor
   (remove-last (editor-pre ed))
   (string-append
    (last-char (editor-pre ed))
    (editor-post ed))))

; Editor -> Editor
; moves the curson one position right
(check-expect (move-right (make-editor "hello" "world"))
              (make-editor "hellow" "orld"))
(check-expect (move-right (make-editor "hello" ""))
              (make-editor "hello" ""))
(define (move-right ed)
  (make-editor
   (string-append
    (editor-pre ed)
    (first-char (editor-post ed)))
   (remove-first (editor-post ed))))

; Editor 1String -> Editor
; adds a character to the pre part of an Editor
(check-expect (add-to-pre (make-editor "hello" "world") "o")
  (make-editor "helloo" "world"))
(define (add-to-pre ed s)
  (make-editor
   (string-append (editor-pre ed) s)
   (editor-post ed)))

; String -> String
; removes the last character from a string
(check-expect (remove-last "ball") "bal")
(check-expect (remove-last "") "")
(define (remove-last s)
  (cond
    [(= (string-length s) 0) s]
    [else
     (substring s 0 (sub1 (string-length s)))]))

; String -> 1String
; gives the last character of a string
(check-expect (last-char "bar") "r")
(check-expect (last-char "") "")
(define (last-char s)
  (cond
    [(= (string-length s) 0) ""]
    [else
      (substring s (sub1 (string-length s)))]))

; String -> 1String
; gives the first character of a string
(check-expect (first-char "foo") "f")
(check-expect (first-char "") "")
(define (first-char s)
  (cond
    [(= (string-length s) 0) ""]
    [else (substring s 0 1)]))

; String -> 1String
; removes the first letter of a string
(check-expect (remove-first "foo") "oo")
(check-expect (remove-first "") "")
(define (remove-first s)
    (cond
    [(= (string-length s) 0) ""]
    [else (substring s 1)]))

(define (run ed)
  (big-bang ed
    [to-draw render]
    [on-key edit]))
