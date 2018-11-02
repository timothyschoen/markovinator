         
(define receive-port 12345) ; port number we will listen to
(start-receiving-osc receive-port)

;============ receiving OSC messages ==============

; osc-receive: wait for 1 OSC message and deliver it
; format: '(address (param1 param2 ..etc))
(define (osc-receive)
  (wait-for-osc-message))

(displayln "Waiting for incoming messages...")

; wait for a message using (osc-receive), display their contents and
; call osc-receive again for the next message
(define (osc-loop)
  (define message (osc-receive))
  (display "Got message: ")(displayln message)
  (osc-loop))

; start listening
(osc-loop)
