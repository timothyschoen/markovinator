#lang racket

(provide    mod-list
            select-list
            compose-list
            backshift)


; This support file contains essential functions for working with higher-order markov chains
; The markov principle states that we can predict the next state of any system by looking at its previous state(s)
; If a state was proceded by numbers 2 and 4, we will look in the sublist 4 of list 2 to find the possible next states
; This is an example of a second order markov chain, because it analyses the previous two states of our system

; Keep in mind that in these functions, 0 means first order markov!

; Compose-list creates a base list that we can later use to write and read values to/from
; Because this tool was written for music purposes, we'll create 12 values by default
; You can change this to any value you like by changing the range
;
; Example of 2nd order, 12 value, markov baselist: (compose-list 2)


            (define (compose-list markov_order {range 12} {previous_lst (build-list range list)})
              (if (<= markov_order 0)
              previous_lst
              (compose-list (- markov_order 1) range (map (lambda (x) (append x (list previous_lst))) (build-list range list)))))


; Select-list reads the possible next values from a list, using a list of the preceding values
; Example: (select-list (compose-list 2) 2 '(1 3)) will give us all possible options after a 1 followed by a 3
; (because we just made a new list with compose-list, there will be no options available)

        (define (select-list lst markov_order previous_lst)
              (if (<= markov_order 0) (list-ref lst (first previous_lst)) (select-list (first (rest (list-ref lst (first previous_lst))))  (- markov_order 1) (rest previous_lst))))

; Mod-list can add new values in the correct part of a base list when you provide it with the preceding values
; Example: (define newlist (mod-list (compose-list 2) '(1 3 4) 2 '("newitem")))
;          (rest (select-list newlist 2 '(1 3 4)))
;  This example will correctly return "newitem"

          (define (mod-list lst previous_lst markov_order new_item)
              (if (<= markov_order 0)
              (list-set lst (first previous_lst) (append (list-ref lst (first previous_lst)) (list new_item)))
              (list-set lst (first previous_lst) (append (list (first (list-ref lst (first previous_lst)))) (list (mod-list (first (rest (list-ref lst (first previous_lst)))) (rest previous_lst) (- markov_order 1) new_item))))))

; Backshift extracts lower-order markov options from a higher order markov list
; This is useful when there are no possible options using the current markov order

        (define (backshift lst previous_lst markov_order backshift_lvl {n 0} {range 12})
        (define default_list (build-list range values))
        (if (= backshift_lvl 0)
        (filter-not (lambda (x) (= 1 (length x))) (map (lambda (x) (select-list lst markov_order (list-set previous_lst (- markov_order n) x))) default_list))
        (map (lambda (x) (filter-not empty? x)) (map (lambda (x) (backshift lst (list-set previous_lst (- markov_order n) x) markov_order (- backshift_lvl 1) (+ n 1))) default_list))))
