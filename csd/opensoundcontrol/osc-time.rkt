#lang racket

;; Copyright 2012 John Clements (clements@racket-lang.org)
;; this code is released under a BSD license

(require racket/date
         "osc-defns.rkt")

(provide (contract-out 
          [date->osc-date (-> date? inexact-real? osc-date?)]
          [osc-date->date-and-frac
                        (-> osc-date? (list/c date? 
                                              inexact-real?))]
                       
                       [osc-date->bytes
                        (-> osc-date? 8bytes?)]
                       
                       [bytes->osc-date
                        (-> 8bytes? osc-date?)]))

;; OSC dates consist of a fixed-point number with 
;; 32 bits for the second, and 32 bits for the sub-second.

;; seconds are measured since January 1, 1900, meaning
;; that they'll run out in 2036 or so. Oh well.

;; there's actually a reasonable bijection between
;; the seconds representation and racket's notion of a 
;; date, for the dates in the range 1900-2036, anyway.

;; I'm representing an OSC-date as a list of two
;; integers in the range 0 - #xffffffff,
;; or the symbol 'now for the special "now" value.


(define (8bytes? b)
  (and (bytes? b) (= (bytes-length b) 8)))

;; got to be careful here.... we'll see what servers actually
;; use.
(define leap-years-added 
  (- (floor (/ (- 1970 1900) 4)) 1))

(define seconds-per-day (* 60 60 24))

(define seconds-offset
  (* seconds-per-day
     (+ (* 365 (- 1970 1900)) leap-years-added)))

(define (date->osc-date d frac)
  (list (+ (date->seconds d)
           seconds-offset)
        (inexact->exact (floor (* frac #x100000000)))))

(define (osc-date->date-and-frac osc-date)
  (match osc-date
    ['now (raise-type-error 'osc-date->date-and-frac
                            "non-'now' osc value"
                            0 osc-date)]
    [(list seconds frac)
     (list (seconds->date (- seconds seconds-offset))
           (/ (exact->inexact frac) #x100000000))]))


(define (osc-date->bytes osc-date)
  (match osc-date
    ['now (bytes 0 0 0 0 0 0 0 1)]
    [(list seconds frac)
     (bytes-append (integer->integer-bytes seconds 4 #f #t)
                   (integer->integer-bytes frac 4 #f #t))]))

(define (bytes->osc-date bytes)
  (match bytes
    [#"\0\0\0\0\0\0\0\1" 'now]
    [else (list
           (integer-bytes->integer (subbytes bytes 0 4) #f #t)
           (integer-bytes->integer (subbytes bytes 4 8) #f #t))]))

