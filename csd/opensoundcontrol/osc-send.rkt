#lang racket

(require    "osc-to-bytes.rkt"
            "osc-defns.rkt"
            "bytes-to-osc.rkt")

(provide    send-osc-message
            start-sending-osc)

; We need this socket for communication
(define the-socket (udp-open-socket))

(define the-port #f)
(define the-ip #f)

(define (start-sending-osc [ip "127.0.0.1"] [port 12345])
    (set! the-ip ip)
    (set! the-port port))


(define (send-osc-message address arguments)
    (udp-send-to the-socket the-ip the-port
        (osc-element->bytes
            (osc-message (string->bytes/utf-8 address) arguments))))

