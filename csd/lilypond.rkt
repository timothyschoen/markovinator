;********************************************************************
;       (c) Copyright 2015, Hogeschool voor de Kunsten Utrecht
;                       Hilversum, the Netherlands
;********************************************************************
;
; File name	: lilypond.rkt
; System name	: SCEME: Scheme Music Composition Environment
;
; Description   : Racket Scheme class for creating Lilypond files
;
; Authors       : Marc Groenewegen, Daan van Hasselt
; E-mail        : marc.groenewegen@hku.nl, contact@daanvanhasselt.com
;
;*********************************************************************
;
; Class and convenience functions for writing Lilypond files
;
; Examples:
;  at end of file
;
;*********************************************************************

#lang racket

(provide lilypond-open)
(provide lilypond-title)
(provide lilypond-composer)
(provide lilypond-tempo)
(provide lilypond-key)
(provide lilypond-clef)
(provide lilypond-time-signature)
(provide lilypond-instrument)
(provide lilypond-part)
(provide lilypond-write)
(provide lilypond-close)


; create a lilygenerator so the user doesn't need to specify it with
;  every function call
(define lilygenerator #f)


(define LilyGenerator%
 (class object%

  ; define some default values
  (define lilyversion "2.18.2")
  (define fileport 0)
  (define filetype 'part)
  (define parts '())
  (define title "Something good")
  (define composer "Composer")
  ;
  ; part-specific
  ;
  (define version-written #f)
  (define header-written #f)
  (define first-part #t)
  (define partname "piano")
  (define tempo 120)
  (define keybase "c")
  (define keynumber 0)
  (define keytype "major")
  (define clef "treble")
  (define time-signature "4/4")
  (define instrument-specified #f)
  (define instrument-name "violin")
  (define instrument "violin") ; literal string describing the instrument


  (define major-scales (list
  '(c cis d dis e f fis g gis a ais b)
  '(bis cis d dis e eis fis g gis a ais b)
  '(c des d es e f ges g as a bes b)
  '(c cis d dis e f fis g gis a ais b)
  '(bis cis d dis e eis fis g gis a ais b)
  '(c des d es e f ges g as a bes b)
  '(c cis d dis e f fis g gis a ais b)
  '(c des d es e f ges g as a bes b)
  '(c cis d dis e eis fis g gis a ais b)
  '(c des d es e f ges g as a bes b)
  '(c cis d dis e f fis g gis a ais b)
  '(bis cis d dis e eis fis g gis a ais b)
  '(c des d es e f ges g as a bes b)
  '(c cis d dis e f fis g gis a ais b)
  '(bis cis d dis e eis fis g gis a ais b)
  '(c des d es e f ges g as a bes b)
  '(c cis d dis e f fis g gis a ais b)))

  (define minor-scales (list
  '(c des d es e f ges g as a bes b)
  '(bis cis d dis e f fis g gis a ais b)
  '(c des d es fes f ges g as beses bes ces)
  '(c des d es e f ges g as a bes b)
  '(c cis d dis e eis fis g gis a ais b)
  '(c des d es e f ges g as a bes ces)
  '(c cis d dis e f fis g gis a ais b)
  '(c des d es e f ges g as a bes b)
  '(c cis d dis e f fis g gis a ais b)
  '(c des eses es fes f ges g as beses bes ces)
  '(c des d es e f ges g as a bes b)
  '(c cis d dis e f fis g gis a ais b)
  '(c des d es fes f ges g as a bes ces)
  '(c cis d dis e f fis g gis a ais b)
  '(bis cis d dis e eis fis g gis a ais b)
  '(c des d es e f ges g as a bes b)
  '(c cis d dis e f fis g gis a ais b)))


   (define base-notes (vector "c" "cis" "des" "d" "dis" "es" "e" "f" "fis"
                           "ges" "g" "gis" "as" "a" "ais" "bes" "b"))

  ; calculate scaleDurations by inverting time signature
  ;
  (define (scale-durations ts)
    (string-join (reverse (string-split ts "/")) "/" ))

  (super-new)

  (define/public (openFile filename)
    ; part name is basename of filename
   (set! partname (car (string-split filename ".")))
   (set! fileport (open-output-file filename #:exists 'replace)))

  ; Supported types:
  ; 'definition : only the note material
  ; 'part : a complete part, including the score
  ; 'score : multi-part score
  ;
  (define/public (set-type newtype)
   (set! filetype newtype))

  (define/public (set-title newtitle)
   (set! title newtitle))

  (define/public (set-composer new-composer)
   (set! composer new-composer))

  (define/public (set-tempo new-tempo)
   (set! tempo new-tempo))

  (define/public (set-keybase new-keybase)
   (set! keybase (string-downcase new-keybase))
   (set! keynumber (vector-member keybase base-notes)))

  (define/public (set-keytype new-keytype)
   (set! keytype new-keytype))

  (define/public (set-clef new-clef)
   (set! clef new-clef))

  (define/public (set-time-signature new-time-signature)
   (set! time-signature new-time-signature))

  (define/public (set-instrument new-instrument-name new-instrument)
   (set! instrument-name new-instrument-name)
   (set! instrument new-instrument)
   (set! instrument-specified #t))

  (define/public (write-version)
    (when (not version-written)
      (begin
        (fprintf fileport "\\version ~s\n\n" lilyversion)
        (set! version-written #t))))

  (define/public (write-header)
    (when (not header-written)
      (begin
        (fprintf fileport
"\\header {
  title = ~s
  composer = ~s
}\n\n" title composer)
        (set! header-written #t))))


; 'definition
; - does not write a header
; - writes notes for one part
; - part name is basename of the file
;
; 'part
; - includes one definition file
; - part name is basename of the file
; - writes a score block for one part with one Staff
;
;
; 'score
; - includes multiple definition files
; - writes a score block with a StaffGroup and a Staff for each definition
; - part names are the basenames of the files
;
;

  (define/public (write-part notes)
     (cond
	((eq? filetype 'definition)
          (write-version)
	  (write-definition notes))
        ((eq? filetype 'part)
          (write-version)
          (write-header)
          (fprintf fileport "\\score\n{\n") ; open score
          (write-next-part partname notes #t))
        ((eq? filetype 'score)
          (write-version)
          (write-header)
          (if first-part ; before first part, open score and StaffGroup
            (begin
              (fprintf fileport "\\score\n{\n  \\new StaffGroup\n  {\n    <<\n")
              (write-next-part partname notes #t)
              (set! first-part #f))
            (write-next-part partname notes #f)))))


  (define/public (write-definition notes)
    (fprintf fileport "~s = " partname)
    (parse notes))


  (define/public (add-part name) ; start new part with its own name
    (set! partname name))


  (define/public (write-next-part name notes with-tempo)
   (begin
    (fprintf fileport "  \\new Staff\n  {\n")

    (when with-tempo (fprintf fileport "    \\tempo 4=~a\n" tempo))
    (fprintf fileport "    \\key ~a \\~a\n    \\clef ~a\n" keybase keytype clef)
    (when instrument-specified
      (fprintf fileport
	(format "    \\set Staff.instrumentName = \"~a\"\n" instrument-name))
      (fprintf fileport
	(format "    \\set Staff.midiInstrument = #\"~a\"\n" instrument)))
      ; for other time signatures than 4/4 write ts and duration
      (when (not (string=? time-signature "4/4"))
        (begin
          (fprintf fileport
            (format "    \\set Staff.timeSignatureFraction = ~a\n" time-signature))
          (fprintf fileport
            (format "    \\scaleDurations ~a\n" (scale-durations time-signature)))))
      (parse notes)
      (fprintf fileport "  } % Staff\n\n"))) ; close staff


  ; convert a midi note number into a note name (primitive version)
  ;
  ; should be able to:
  ; * handle symbolic note names as well as numbers
  ; * use modal correction
  (define/private (number-to-note number)
     (if (string=? keytype "major")
       (list-ref (list-ref major-scales keynumber) (modulo number 12))
       (list-ref (list-ref minor-scales keynumber) (modulo number 12))))


  ;; relative to lilypond's middle C !!
  (define/private (number-to-octave number)
    (- (floor (/ number 12)) 4))

  ; If length is an exact integer, leave it as is it. If not, assume the
  ;  note is dotted, which means it is 3/2 times as long as the integer.
  ; Since this has already been discounted we need to nullify it and add a
  ;  dot to accord with Lilypond's notation
  (define/private (length-encoding number)
    (if (symbol? number) number ; probably a tie, i.e. 4~ or something
      (if (exact-integer? number) number
        (format "~a." (* 3/2 number)))))


  ;; Lilypond uses single quote and comma to raise or lower a note's pitch by
  ;;  one octave
  ;; (number-to-quotes) takes a number and returns a string
  ;;  number==0 --> empty string
  ;;  number>0 --> string containing <number> quotes
  ;;  number<0 --> string containing <number> commas
  (define (number-to-quotes number)
    (cond ((= number 0) "")
          ((> number 0) (string-append "'" (number-to-quotes (- number 1))))
          ((< number 0) (string-append "," (number-to-quotes (+ number 1))))))

  ; display a single note
  (define/private (display-note note)
    (define new-length (length-encoding (caddr note)))
    (fprintf fileport "~a~a"
      (number-to-note (cadr note)) ; note name
      (number-to-quotes (number-to-octave (cadr note)))) ; quotes for octave
    ; ff checken of new-length wel een number is (!)
      (fprintf fileport "~a " new-length)) ; length changes to new value

  ; display a rest (a.k.a. nap)
  (define/private (display-nap nap)
    (fprintf fileport "r~a " (length-encoding (cadr nap))))

  ; parse a composition
  (define/private (parse item)
    ; the first element of the item we're parsing is always a keyword
    (define keyword (car item))
    (cond
      ((equal? keyword 'serial) ; the start of a serial block
       (fprintf fileport "{ "); the start of a serial block
       (for ((i (cdr item))); parse the rest of the items
	     (parse i))
       (fprintf fileport "} \\\\ \n")); the end of a serial block
       ; double backslash creates a new voice for every block

      ((equal? keyword 'parallel)
       (fprintf fileport "<< "); the start of a parallel block
       (for ((i (cdr item))); parse the rest of the items
	     (parse i))
       (fprintf fileport ">> ")); the end of parallel

      ; handling of tuplets:
      ((equal? keyword 'tuplet)
       ; tuplet start with fraction consisting of the number of
       ;  notes divided by the target number of notes as given
       (fprintf fileport "\\tuplet ~a/~a {"
         (- (length item) 2)
         (cadr item))
       (for ((i (cddr item))); parse the rest of the items
	     (parse i))
       (fprintf fileport "} ")); tuplet end

      ; notes and naps should just be displayed
      ((equal? keyword 'note)
	 (display-note item))
      ((equal? keyword 'nap)
	 (display-nap item)))) ; end parse

  (define/public (closeFile)
    (when (eq? filetype 'score) ; close StaffGroup
      (fprintf fileport "\n    >>\n  } % StaffGroup\n\n"))
    ; write layout, midi and close score
    (fprintf fileport "\n  \\layout {
    \\context { \\RemoveEmptyStaffContext }
  }
  \\midi { }
} % score" )
    (close-output-port fileport))

)) ; class LilyGenerator%


; show warning if functions are called on non-existent object
(define (warning-no-lily-object)
  (display "First start a new Lilypond file with (lilypond-open filename)\n"))

(define (lilypond-open filename (filetype 'part))
  (set! lilygenerator (new LilyGenerator%))
  (send lilygenerator openFile filename)
  (send lilygenerator set-type filetype))

(define (lilypond-title title)
  (if (equal? lilygenerator #f)
    (warning-no-lily-object)
    (send lilygenerator set-title title)))

(define (lilypond-composer composer)
  (if (equal? lilygenerator #f)
    (warning-no-lily-object)
    (send lilygenerator set-composer composer)))

(define (lilypond-tempo tempo)
  (if (equal? lilygenerator #f)
    (warning-no-lily-object)
    (send lilygenerator set-tempo tempo)))

(define (lilypond-part name)
  (if (equal? lilygenerator #f)
    (warning-no-lily-object)
    (send lilygenerator add-part name)))

(define (lilypond-key keybase keytype)
  (if (equal? lilygenerator #f)
    (warning-no-lily-object)
    (begin
      (send lilygenerator set-keybase keybase)
      (send lilygenerator set-keytype keytype))))

(define (lilypond-clef clef)
  (if (equal? lilygenerator #f)
    (warning-no-lily-object)
    (send lilygenerator set-clef clef)))

(define (lilypond-time-signature time-signature)
  (if (equal? lilygenerator #f)
    (warning-no-lily-object)
    (send lilygenerator set-time-signature time-signature)))

(define (lilypond-instrument instrument-name instrument)
  (if (equal? lilygenerator #f)
    (warning-no-lily-object)
    (send lilygenerator set-instrument instrument-name instrument)))

(define (lilypond-write notes)
  (if (equal? lilygenerator #f)
    (warning-no-lily-object)
    (send lilygenerator write-part notes)))

(define (lilypond-close)
      (send lilygenerator closeFile)
      (set! lilygenerator #f)) ; instruct garbage collector to destroy the object


; Examples
;
; (define notes '(serial
;  (note 61 8) (note 60 8) (note 67 8) (note 69 8) (note 70 8)
;  (note 70 8) (note 69 8) (nap 70 8) (note 67 8) (note 63 8)
;  (note 65 8) (note 67 8) (note 63 8) (nap 62 8) (note 62 8)
;  (note 60 8))
;
; with a tuplet
; '(serial
;   (note 60 4)
;   (note 60 4)
;   (note 60 4)
;   (tuplet 2 (note 60 8) (note 60 8) (note 60 8)))
;
; The elaborate way: open a file, specify some props and write it
; (lilypond-open "happiness.ly")
; (lilypond-title "Een en al vrolijkheid")
; (lilypond-composer "Marc")
; (lilypond-tempo 78)
; (lilypond-key "g" "minor")
; (lilypond-clef "treble")
; (lilypond-time-signature "3/4")
; (lilypond-instrument "guitar" "acoustic guitar (nylon)")
; (lilypond-write notes)
; (lilypond-close)
;

