;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-076) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct movie [title producer year])
; A Movie is a structure:
;  (make-movie String String Number)
; interpretation the title, producer, and year of a movie

(define struct person [name hair eyes phone])
; A Person is a structure:
;  (make-person String String String Number)
; interpretation the name, hair color, eye color,
; and phone number of a person

(define struct pet [name number])
; A Pet is a structure:
;  (make-pet String Number)
; interpretation the name and ID number of a pet

(define-struct CD [artist title price])
; A CD is a structure:
;  (make-CD String String Number)
; interpretation the artise, title, and price of a CD

(define-struct sweater [material size producer])
; A Sweater is a structure:
;  (make-sweater String Size Producer)
; interpretation the material, size, and producer of a sweater

; A Size is on of three Strings:
; - "S"
; - "M"
; - "L"
; interpretation the three possible sizes that a Sweater
; can be produced in
