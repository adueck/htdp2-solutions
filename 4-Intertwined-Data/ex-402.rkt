;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-402) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; Exercise 354 is an example of a function that consumes two
; complex inputs. Because the task is to iteratively apply all
; the values in the association list, we can see that parameter
; as playing the dominant role. It then seems most logical to follow
; strategy #1 as explained at the beginning of IV.23.5:
;
; When designing functions that consume two complex inputs:
;
; ┌---┐
; | 1.| One of the parameters plays a dominant role:
; └---┘   - Iterate on the element that plays the dominant role
;           and consider the other one an atomic piece of data.
;
;    2. Both paramaters must be have the same size/length:
;         - Traverse both together, in a synchronized manner.
;
;    3. There's no obvious connection between the two paramaters:
;         - Analyze all possible cases with examples, and
;           make a template with all those cases and work out
;           from that.
