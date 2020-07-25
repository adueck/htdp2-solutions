;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-030) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define COST-PER-ATTENDEE 1.50)
(define SNAPSHOT-ATTENDANCE 120)
(define SNAPSHOT-PRICE 5.0)
(define TICKET-INCREASE 0.1)
(define CHANGE-BY-TICKET-INCREASE 15)
(define PRICE-SENSITIVITY (/ CHANGE-BY-TICKET-INCREASE TICKET-INCREASE))

(define (attendees ticket-price)
  (-
   SNAPSHOT-ATTENDANCE
   (*
    (- ticket-price SNAPSHOT-PRICE)
    PRICE-SENSITIVITY)))

(define (revenue ticket-price)
  (* ticket-price (attendees ticket-price)))

(define (cost ticket-price)
  (* COST-PER-ATTENDEE (attendees ticket-price)))

(define (profit ticket-price)
  (- (revenue ticket-price)
     (cost ticket-price)))





