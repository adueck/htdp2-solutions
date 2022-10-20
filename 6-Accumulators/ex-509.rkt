;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-509) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
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

; Help from https://github.com/S8A/htdp-exercises/blob/master/ex508.rkt

; Lo1S Number -> Editor
; returns an editor from Lo1S ed
; with cursor at the point of the mouse click x
(check-expect (split-structural (list "f" "o" "o") 18)
              (make-editor (reverse (list "f" "o")) (list "o")))
(check-expect (split-structural (list "a" "b" "c") 18)
              (make-editor (reverse (list "a" "b")) (list "c")))
(check-expect (split-structural (list "a" "b" "c" "d" "e" "f") 38)
              (make-editor (reverse (list "a" "b" "c" "d")) (list "e" "f")))
(define (split-structural ed x)
  (local
    ((define (remove-prefix pre)
       (explode (substring (implode ed) (length pre))))
     (define (get-prefix l)
       (cond
         [(empty? l) '()]
         [else
          (local ((define p l)
                  (define s (remove-prefix p))
                  (define p-text (editor-text p)))
            (if (<= (image-width p-text) x)
                l
                (get-prefix (rest l))))]))
     (define prefix (reverse (get-prefix (reverse ed)))))
  (make-editor (reverse prefix)
               (remove-prefix prefix))))

; Lo1S Number -> Editor
; returns an editor from Lo1S ed
; with cursor at the point of the mouse click x
(check-expect (split (list "f" "o" "o") 18)
              (make-editor (reverse (list "f" "o")) (list "o")))
(check-expect (split (list "a" "b" "c") 18)
              (make-editor (reverse (list "a" "b")) (list "c")))
(check-expect (split (list "a" "b" "c" "d" "e" "f") 38)
              (make-editor (reverse (list "a" "b" "c" "d")) (list "e" "f")))
(define (split ed x)
  (local
    (; Lo1S Lo1S -> Editor
     ; accumulator pre is the list of letters to the left of post
     ; that have already fit before the the x coordinate
     (define (split/a post pre)
       (cond
         [(empty? post) (make-editor (reverse pre) '())]
         [else
          (local
            ((define pre+1-width (image-width (editor-text (append
                                                            pre
                                                            (list (first post)))))))
            (if (<= pre+1-width x)
                (split/a (rest post) (append pre
                                             (list (first post))))
                (make-editor (reverse pre) post)))])))
  (split/a ed '())))

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
(check-expect (editor-render (create-editor "pre" "post"))
              (place-image/align
  (beside (text "pre" FONT-SIZE FONT-COLOR)
          CURSOR
          (text "post" FONT-SIZE FONT-COLOR))
  1 1
  "left" "top"
  MT))
(check-expect (editor-render (create-editor "" "post"))
              (place-image/align
  (beside CURSOR
          (text "post" FONT-SIZE FONT-COLOR))
  1 1
  "left" "top"
  MT))
(check-expect (editor-render (create-editor "pre" ""))
              (place-image/align
  (beside (text "pre" FONT-SIZE FONT-COLOR)
          CURSOR)
  1 1
  "left" "top"
  MT))
(check-expect (editor-render (create-editor "" ""))
              (place-image/align
  CURSOR
  1 1
  "left" "top"
  MT))
(define (editor-render e)
  (place-image/align
   (beside (editor-text (reverse (editor-pre e)))
           CURSOR
           (editor-text (editor-post e)))
   1 1
   "left" "top"
   MT))

; Lo1s -> Image
; renders a line of text based on an array of 1Strings
(check-expect
  (editor-text
   (cons "p" (cons "o" (cons "s" (cons "t" '())))))
  (text "post" FONT-SIZE FONT-COLOR))
(define (editor-text s)
  (text (my-implode s) FONT-SIZE FONT-COLOR))

; Lo1s -> String
; Transforms a list of 1Strings into a String
(check-expect (my-implode '()) "")
(check-expect (my-implode (cons "a" (cons "b" (cons "c" '()))))
              "abc")
(define (my-implode s)
  (cond
    [(empty? s) ""]
    [(cons? s) (string-append
                (first s) (my-implode (rest s)))]))

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
    [(key=? k "left") (editor-lft ed)]
    [(key=? k "right") (editor-rgt ed)]
    [(key=? k "\b") (editor-del ed)]
    [(key=? k "\t") ed]
    [(key=? k "\r") ed]
    [(= (string-length k) 1) (editor-ins ed k)]
    [else ed]))

; Editor -> Editor
; Moves the cursor one position left, if possible
(check-expect (editor-lft (create-editor "" ""))
              (create-editor "" ""))
(check-expect
  (editor-lft (create-editor "cd" "fgh"))
  (create-editor "c" "dfgh"))
(check-expect
  (editor-lft (create-editor "cdef" ""))
  (create-editor "cde" "f"))
(define (editor-lft ed)
  (make-editor
   (remove-first (editor-pre ed))
   (grab-first (editor-pre ed) (editor-post ed))))

; Lo1s -> Lo1s
; removes the first 1String from a list of 1Strings, if possible
(check-expect (remove-first '()) '())
(check-expect (remove-first (cons "a" (cons "b" (cons "c" '()))))
              (cons "b" (cons "c" '())))
(define (remove-first l)
  (cond
    [(empty? l) '()]
    [(cons? l) (rest l)]))

; Lo1s Lols -> Lols
; grabs the first string from la and moves it to the beginning of lb
(check-expect (grab-first '() '()) '())
(check-expect (grab-first '() (cons "c" (cons "d" '())))
              (cons "c" (cons "d" '())))
(check-expect (grab-first (cons "a" (cons "b" '())) '())
              (cons "a" '()))
(check-expect (grab-first (cons "a" (cons "b" '()))
                          (cons "c" (cons "d" '())))
              (cons "a" (cons "c" (cons "d" '()))))
(define (grab-first la lb)
  (cond
    [(and (empty? la) (empty? lb)) '()]
    [(empty? la) lb]
    [else (cons (first la) lb)]))

; Editor -> Editor
; Moves the cursor one position right, if possible
(check-expect (editor-rgt (create-editor "" ""))
              (create-editor "" ""))
(check-expect
  (editor-rgt (create-editor "cd" "fgh"))
  (create-editor "cdf" "gh"))
(check-expect
  (editor-rgt (create-editor "cdef" ""))
  (create-editor "cdef" ""))
(define (editor-rgt ed)
  (make-editor
   (grab-first (editor-post ed) (editor-pre ed))
   (remove-first (editor-post ed))))

; Editor -> Editor
; Deletes one character behind the cursor, if possible
(check-expect (editor-del (create-editor "" ""))
              (create-editor "" ""))
(check-expect
  (editor-del (create-editor "cd" "fgh"))
  (create-editor "c" "fgh"))
(check-expect
  (editor-del (create-editor "cdef" ""))
  (create-editor "cde" ""))
(define (editor-del ed)
  (make-editor
   (remove-first (editor-pre ed))
   (editor-post ed)))

; Editor 1String -> Editor
; insert the 1String k between pre and post
(check-expect
  (editor-ins (make-editor '() '()) "e")
  (make-editor (cons "e" '()) '()))
(check-expect
  (editor-ins
    (make-editor (cons "d" '())
                 (cons "f" (cons "g" '())))
    "e")
  (make-editor (cons "e" (cons "d" '()))
               (cons "f" (cons "g" '()))))
(define (editor-ins ed k)
  (make-editor (cons k (editor-pre ed))
               (editor-post ed)))



; main : String -> Editor
; launches the editor given some initial string 
(define (main s)
   (big-bang (create-editor s "")
     [on-mouse
      (lambda (w x y me)
        (if (mouse=? me "button-down")
            (split (append
                               (reverse (editor-pre w))
                               (editor-post w))
                              x)
            w))]
     [on-key editor-kh]
     [to-draw editor-render]))
