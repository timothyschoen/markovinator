#lang racket

;; Copyright 2012 John Clements (clements@racket-lang.org)
;; released under a BSD license

(require "osc-common.rkt"
         "osc-defns.rkt"
         "osc-time.rkt")

(provide bytes->osc-element)

;; this file contains a function that takes a bytes-string
;; and parses it into an OSC element. See "osc-defns.rkt"
;; for a definition of what makes an OSC element.

(define (bytes->osc-element bytes)
  ;; could be a contract:
  (when (or (not (bytes? bytes))
            (= (bytes-length bytes) 0)
            (not (= (modulo (bytes-length bytes) 4) 0)))
    (raise-type-error 'bytes->osc-element 
                      "non-empty byte string of length divisible by 4"
                       0 bytes))
  (bytes->osc-element/offset bytes 0 (bytes-length bytes)))

(define (bytes->osc-element/offset bytes offset stop-len)
  (match (bytes-ref bytes offset)
    [35 ;; the # char
     (parse-bundle bytes offset stop-len)]
    [47 ;; the / char
     (parse-message (subbytes bytes offset stop-len))]
    [other
     (raise-type-error 
      'bytes->osc-element
      "byte string beginning with # or /"
      0 bytes)]))


(define (parse-bundle bytes offset stop-len)
  (unless (bytes=? (subbytes/b bytes 
                               offset
                               (+ offset 8)
                               stop-len)
                   #"#bundle\0")
    (error 'bytes->osc-element
           "bundle didn't begin with #bundle at offset ~s in input: ~e"
           offset bytes))
  (define timestamp (bytes->osc-date
                     (subbytes/b bytes (+ offset 8) (+ offset 16) 
                                 stop-len)))
  (define elements
    (let loop ([offset (+ offset 16)])
      (cond
        [(<= stop-len offset)
         empty]
        [else
         (define-values (message offset2)
           (parse-length-and-element bytes offset stop-len))
         (cons message (loop offset2))])))
  (osc-bundle timestamp elements))

(define (parse-length-and-element bytes offset stop-len)
  (define length (integer-bytes->integer (subbytes/b bytes
                                                     offset
                                                     (+ offset 4)
                                                     stop-len)
                                         #f #t))
  (when (< stop-len (+ offset 4 length))
    (error 'parse-length-and-element
           "bundle element length ~s too large for containing bundle"
           length))
  (define element (bytes->osc-element/offset bytes (+ offset 4)
                                          (+ offset 4 length)))
  (values element (+ offset 4 length)))

(define (parse-message bytes)
  (parse-message/offset bytes 0))
 
(define (parse-message/offset bytes offset)
  (define-values (addr-bytes offset2)
    (parse-string-from bytes offset))
  (define-values (type-str-bytes offset3)
    (parse-string-from bytes offset2))
  (unless (eq? (bytes-ref type-str-bytes 0) 44)
    (error 'bytes->osc-element
           "expected comma to begin type string at offset ~v in ~e"
           offset2 bytes))
  (define type-chars (rest (bytes->list type-str-bytes)))
  (define-values (revargs offset4)
    (for/fold ([args empty] [offset offset3])
      ([t type-chars])
      (define-values (arg new-offset)
        (read-arg-of-type t offset bytes))
      (values (cons arg args) new-offset)))
  (define args (reverse revargs))
  (osc-message addr-bytes args))


(define (read-arg-of-type char offset bytes)
  (match char
    [115 ;; s
     (parse-string-from bytes offset)]
    [105 ;; i
     (values (integer-bytes->integer bytes #t #t
                                     offset (+ offset 4))
             (+ offset 4))]
    [102 ;; f
     (values (floating-point-bytes->real
              bytes #t offset (+ offset 4))
             (+ offset 4))]
    [100 ;; d
     (values (list 
              'd
              (floating-point-bytes->real
               bytes #t offset (+ offset 8)))
             (+ offset 8))]
    [98 ;; b
     (define len (integer-bytes->integer bytes #f #t
                                         offset (+ offset 4)))
     (define bstr (subbytes bytes (+ offset 4)
                            (+ offset 4 len)))
     (values (list 'blob bstr) (round-up-to-4
                                (+ offset 4 len)))]
    
    [other
     (error 'read-arg-of-type
            "unimplemented type char: ~v" other)]))

;; just like subbytes, but don't allow reading past a given limit
(define (subbytes/b bytes start end limit)
  (when (< limit end)
    (raise-type-error 'subbbytes/b 
                      (format "end index smaller than limit ~s" limit)
                      1 start end limit))
  (subbytes bytes start end))


;;#"sifb"'(115 105 102 98)

(define (parse-string-from bytes offset)
  (match (regexp-match #px#"^([^\0]*)\0" bytes offset)
    [(list dc match) (values match (round-up-to-4
                                    (+ offset
                                       (bytes-length match)
                                       1)))]
    [#f (error 'parse-string-from 
               "no nul char found while parsing for string at offset ~v in ~e"
               offset
               bytes)]))

