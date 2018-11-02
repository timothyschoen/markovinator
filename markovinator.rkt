
(require csd/music_transforms)
(require csd/lilypond)
(require csd/graph_utils)
(require csd/markov_utils)
(require csd/opensoundcontrol/osc-send)
(require csd/opensoundcontrol/osc-receive)


(define (connect {port-in 12121} {port-out 21212})
  (define host-ip "127.0.0.1")
  (define host-port port-out) ; port number on the receiver
  (start-sending-osc host-ip host-port)
  (define receive-port port-in) ; port number we will listen to
  (start-receiving-osc receive-port host-ip))


  (define (markovinator markov_order)
  ;(connect port-in port-out)
  (define startlist (build-list (+ markov_order 5) (lambda (x) '(0 0 0))))
  (define noteslist (compose-list markov_order))
  (define player (thread (lambda () (playthread noteslist markov_order startlist))))

      (define (main noteslist allmessages)
        (define main_thread (current-thread))
        (define message (wait-for-osc-message))
        (define n_lastmessage (append (list (modulo (first (first (rest message))) 12)) (rest (first (rest message)))))
        (define newlist (mod-list noteslist (map first allmessages) markov_order n_lastmessage))
        (thread-send player newlist)
        (main newlist (append (list n_lastmessage) allmessages)))

    (main noteslist startlist))

    (define (receiver lst state)
    (define received (thread-try-receive))
    (if (false? received) (begin (if (eq? state 1) (displayln "Chain up-to-date") null) lst)
    (begin (displayln "Updating Chain...") (receiver received 1))))

    (define (playthread noteslist markov_order allnotes {previoustime (current-inexact-milliseconds)})
        (define newlist (receiver noteslist 0))
        (define nextlist (select-list newlist markov_order (map first allnotes)))

        (define fixedlist (if (empty? (rest nextlist)) allnotes (rest nextlist)))
        (define newnote (first (shuffle fixedlist)))

        (define finalnote (if
        (and (>= 20 (last newnote)) (>= 20 (last (first allnotes))) (>= 20 (last (second allnotes))) (>= 20 (last (third allnotes))))
        (list-set newnote (- (length newnote) 1) (if (empty? (filter (lambda (x) (>= x 40)) (map last allnotes))) 200 (first (shuffle (filter (lambda (x) (>= x 40)) (map last allnotes)))))) newnote))

        (define n_allnotes (append (list finalnote) allnotes))
        (define sleeptime (-  (last finalnote) (- (current-inexact-milliseconds) previoustime)))

        (sleep (/ (if (negative? sleeptime) 0 sleeptime) 1000))
        ;(sleep 0.25)
        (send-osc-message "/note" finalnote)
        (playthread newlist markov_order n_allnotes))
