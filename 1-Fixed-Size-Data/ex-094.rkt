;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-094) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define ASPECT-RATIO (/ 3 4))
(define WIDTH 400)
(define HEIGHT (* WIDTH ASPECT-RATIO))
(define BACKGROUND
  (above (empty-scene WIDTH (* HEIGHT 0.8) "blue")
         (empty-scene WIDTH (* HEIGHT 0.2) "green")))

(define TANK (rectangle
              (* WIDTH 0.18)
              (* WIDTH 0.08)
              "solid" "brown"))

(define UFO (ellipse
             (* WIDTH 0.18)
             (* WIDTH 0.08)
             "solid" "grey"))

(define DEMO-SCENE
  (place-images/align
   (list TANK UFO)
   (list (make-posn (* WIDTH 0.3) HEIGHT)
         (make-posn (* WIDTH 0.6) (* HEIGHT 0.2)))
   "center" "bottom"
   BACKGROUND))
