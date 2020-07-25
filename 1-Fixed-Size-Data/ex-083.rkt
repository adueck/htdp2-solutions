;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-083) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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