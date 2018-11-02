#lang racket

(require    "osc-to-bytes.rkt"
            "osc-defns.rkt"
            "bytes-to-osc.rkt")

(provide    wait-for-osc-message
            start-receiving-osc)

; We need this socket for communication
(define the-socket (udp-open-socket))

; This buffer will be used to temporarily store incoming data
(define receive-buffer #f)
(define (start-receiving-osc [port 12345] [ip #f])
    (set! receive-buffer (make-bytes 10000 0))
    (udp-bind! the-socket ip port))

(define (wait-for-osc-message [socket the-socket] [buffer receive-buffer])
    (define-values (len hostname src-port) (udp-receive! socket buffer))
    (define received (subbytes buffer 0 len))
    (define decoded-message (bytes->osc-element received))
    (list (bytes->string/utf-8 (osc-message-address decoded-message)) (osc-message-args decoded-message)))
