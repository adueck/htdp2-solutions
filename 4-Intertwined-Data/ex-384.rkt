;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-384) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/batch-io)
(require 2htdp/image)
(require 2htdp/universe)

(define PREFIX "Https://www.google.com/finance?q=")
(define SIZE 22) ; font size 
 
(define-struct data [price delta])
; A StockWorld is a structure: (make-data String String)
; Interpretation the price is the price of the stock
; and the delta is the change in the stock price
 
; String -> StockWorld
; retrieves the stock price of co and its change every 15s
(define (stock-alert co)
  (local ((define url (string-append PREFIX co))
          ; [StockWorld -> StockWorld]
          ; fetches and retrieves the data for the stock
          (define (retrieve-stock-data __w)
            (local ((define x (read-xexpr/web url)))
              (make-data (get x "price")
                         (get x "priceChange"))))
          ; StockWorld -> Image
          ; renders the data in human readable format
          (define (render-stock-data w)
            (local (; [StockWorld String -> String] -> Image
                    ; creates a word of a certain text and colour
                    (define (word sel col)
                      (text (sel w) SIZE col)))
              (overlay (beside (word data-price 'black)
                               (text "  " SIZE 'white)
                               (word data-delta 'red))
                       (rectangle 300 35 'solid 'white)))))
    (big-bang (retrieve-stock-data 'no-use)
      [on-tick retrieve-stock-data 15]
      [to-draw render-stock-data])))

; Xexpr.v3 String -> String
; retrieves the value of the "content" attribute 
; from a 'meta element that has attribute "itemprop"
; with value s
(check-expect
  (get '(meta ((content "+1") (itemprop "F"))) "F")
  "+1")
 
(define (get x s)
  (local ((define result (get-xexpr x s)))
    (if (string? result)
        result
        (error "not found"))))

(define (get-xexpr x s) #false)

; Xexpr.v3 Symbol -> [Maybe String]
; searches an arbitrary Xexpr.v3 for the desired attribute
(check-expect (get-nr-xexpr '(word ((style "bold"))) 'style) "bold")
(check-expect (get-nr-xexpr '(word ((style "bold"))) 'text) #false)
(check-expect (get-nr-xexpr '(parent ((hair "black")) (child ((eyes "blue")))) 'eyes) "blue")
(check-expect (get-nr-xexpr '(parent (child1 ((eyes "blue")))
                                  (child2 ((eyes "green")))
                                  (child3 (grandchild ((hair "blond"))))) 'hair) "blond")
(define (get-nr-xexpr x s)
  (local
    ((define attr (xexpr-attr x))
     (define body (xexpr-content x))
     (define found (find-attr attr s)))
    (if (string? found)
        found
        (local
          ((define (search-body lox s)
             (cond
               [(empty? lox) #false]
               [else (local
                       ((define fres (get-nr-xexpr (first lox) s)))
                       (if (string? fres)
                           fres
                           (search-body (rest lox) s)))])))
          (search-body body s)))))
    




; Xexpr -> [List-of Attribute]
; retrieves the list of attributes of xe
(define (xexpr-attr xe)
  (local ((define x-info (rest xe)))
    (cond
      [(empty? x-info) '()]
      [else (if
             (list-of-attributes? (first x-info))
             (first x-info)
             '())])))

; LOA-or-X -> boolean
; determines whether x is an element of [List-of Attribute]
; #false otherwise
(define (list-of-attributes? x)
  (cond
    [(empty? x) #true]
    [else (local
            ((define neloa-or-x (first x)))
            (cons? neloa-or-x))]))

; Xexpr -> Symbol
; returns the name of an Xexpr
(define (xexpr-name x)
  (first x))

; Xexpr -> [List-of Xexpr]
; returns the content of an Xexpr
(define (xexpr-content x)
  (local
    ((define x-info (rest x)))
    (cond
      [(empty? x-info) '()]
      [else
       (if (list-of-attributes? (first x-info))
           (rest x-info)
           x-info)])))

; [List-of Attributes] Symbol -> String or #false
; If the attributes list associates the symbol with a string,
; the function retrieves this string; otherwise it returns #false
(define (find-attr loa s)
  (local
    ((define res (assq s loa)))
    (cond
      [(false? res) #false]
      [else (second res)])))

