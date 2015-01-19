;; NWC to Denemo
;; A Lexer / Parser to import NWC files into Denemo
;; By Nils Gey, July / August 2010
;; Usage of SILex and LALR-scm 
;;;;;;;;;;;;;;;;;;;;;;;;

;(hashq-set! nwc:Nwc2LyTable '0 "b'")


;;TOC
;; Libs
;; init input port
;; load and init lexer
;; lalr-parser definition
;; execute the parser
;; cleaning up (input port...)
;;;;;

;; Libs
(load "/home/nils/git-denemo/actions/mxml2ly2denemo/lalr.scm")
(load "/home/nils/git-denemo/actions/mxml2ly2denemo/silex.scm")

;; Input Port
(set-current-input-port (open-input-file "/home/nils/sampleonestaff.nwctxt"))

;; Import Functions
(define nwc:cur_pos_offset 0)
 
(define (nwc:ChangeClef clef) ; clef is a string
    
    (define (PutDenemoClef clef) 
        (if (d-MoveCursorLeft)
        (begin
            (d-MoveCursorRight)
            (d-InsertClef clef))
        (d-InitialClef clef)))

    (cond
        ((string-ci=? clef "Treble") (begin (set! nwc:cur_pos_offset 0) (PutDenemoClef "Treble")))
        ((string-ci=? clef "TrebleDown") (begin (set! nwc:cur_pos_offset -7) (PutDenemoClef "Treble"))) 
        ((string-ci=? clef "TrebleUp") (begin (set! nwc:cur_pos_offset 7) (PutDenemoClef "Treble Octava bassa")))
        
        ((string-ci=? clef "Bass") (begin (set! nwc:cur_pos_offset -12) (PutDenemoClef "Bass")))
        ((string-ci=? clef "BassDown") (begin (set! nwc:cur_pos_offset -19) (PutDenemoClef "Bass")))
        ((string-ci=? clef "BassUp") (begin (set! nwc:cur_pos_offset -5) (PutDenemoClef "Bass Octava bassa")))
        
        ((string-ci=? clef "Percussion") (begin (set! nwc:cur_pos_offset -12) (PutDenemoClef "Bass")))
        ((string-ci=? clef "PercussionDown") (begin (set! nwc:cur_pos_offset -19) (PutDenemoClef "Bass Octava bassa")))
        ((string-ci=? clef "PercussionUp") (begin (set! nwc:cur_pos_offset -5) (PutDenemoClef "Bass")))
        
        ((string-ci=? clef "Tenor") (begin (set! nwc:cur_pos_offset -8) (PutDenemoClef "Tenor")))
        ((string-ci=? clef "TenorDown") (begin (set! nwc:cur_pos_offset -15) (PutDenemoClef "Tenor")))
        ((string-ci=? clef "TenorUp") (begin (set! nwc:cur_pos_offset -1) (PutDenemoClef "Tenor")))
        
        ((string-ci=? clef "Alto") (begin (set! nwc:cur_pos_offset -6) (PutDenemoClef "Alto")))
        ((string-ci=? clef "AltoDown") (begin (set! nwc:cur_pos_offset -13) (PutDenemoClef "Alto")))
        ((string-ci=? clef "AltoUp") (begin (set! nwc:cur_pos_offset 1) (PutDenemoClef "Alto")))        
        
        (else (display "NWC Import Error: Clef unknown"))
    )
)   
    
(define (nwc:PutNote listy) ;position is a string
    (define duration (list-ref listy 0))
    (define dots (list-ref listy 1))
    (define position (list-ref listy 2))
    (cond
        ((string=? duration "Whole") (d-Insert0))
        ((string=? duration "Half") (d-Insert1))
        ((string=? duration "4th") (d-Insert2))
        ((string=? duration "8th") (d-Insert3))
        ((string=? duration "16th") (d-Insert4))
        ((string=? duration "32nd") (d-Insert5))
        ((string=? duration "64nd") (d-Insert6))
        ((string=? duration "128nd") (d-Insert7))
        ((string=? duration "256nd") (d-Insert8))
        (else (display "NWC Import Error: Duration unknown"))
    )
    
    (if (> dots 0) (d-AddDot))
    (if (= dots 2) (d-AddDot))

    
    ;(+ nwc:cur_pos_offset (string->number position))
)


;; Lexer
(define (mtoken symbol value) 
    (make-lexical-token symbol (make-source-location (current-input-port) (lexer-get-line) (lexer-get-column) (lexer-get-offset) -1) value)
)

(lex "/home/nils/git-denemo/actions/mxml2ly2denemo/nwctext.l" "/home/nils/git-denemo/actions/mxml2ly2denemo/nwctext.l.scm" 'counters 'all) ; Oh no!! The generated scm file has comments in the language of the devil!
(load "/home/nils/git-denemo/actions/mxml2ly2denemo/nwctext.l.scm")
(lexer-init 'port (current-input-port)) 




;; Parser Definition

;Helper to print out a value with a custom description, for console output

(define (display-combo string value)
    (display string)
    (display ": ")
    (display value)
    (newline)
) 

(define nwctext-parser

  (lalr-parser
   ;; --- token definitions
   (ALT DBLQUOTE INTEGER STRING LETTER NEXTISNOTE DURATION POSITION WHITESPACE TITLE NUMBER DURATIONVALUE ERROR DOUBLEDOTTED DOTTED BAR-DOUBLE CLEFVALUE CLEF CLEF8UP CLEF8DOWN STAFF STAFFNAME STAFFLABEL)
    
    (commands (commands command) : #t
              (command)          : #t)
    (command 
            (INTEGER)       : (display-combo "Int" $1)
            (LETTER)        : (display-combo "letter" $1)
            (note)          : (nwc:PutNote $1)
            (TITLE)         : (display-combo "title" $1)
            (ERROR)         : (display-combo "errorr" $1)
            (BAR-DOUBLE)    : (display-combo "dblbar" $1)
            (clef)          : (nwc:ChangeClef $1)
            (staff)         : (display-combo "staff" $1)
            (WHITESPACE)    : #f            
    )
    
    (note
        (NEXTISNOTE duration position)  : (list $2 0 $3)
        (NEXTISNOTE duration dots position) : (list $2 $3 $4)
    )
    
        (position
            (POSITION NUMBER)   : $2
            (POSITION ALT NUMBER) : (string-append $2 $3)
        )
        
    
        (duration
            (DURATION DURATIONVALUE) : $2
        )
        
        (dots
            (DOTTED) : 1
            (DOUBLEDOTTED) : 2
        )

    (clef
        (CLEF CLEFVALUE)            : $2
        (CLEF CLEFVALUE CLEF8UP)    : (string-append $2 "Up")
        (CLEF CLEFVALUE CLEF8DOWN)  : (string-append $2 "Down")
    )
    
    (staff
        (STAFF STAFFNAME STRING ) : $3
        (STAFF STAFFNAME STRING STAFFLABEL STRING)  : (string-append $3 " " $5)
    )
    
  )
)

; Just to get this out of my way... I don't wanted to make errors anyway! (real function later)
(define (displayerror arg1 arg2)
        (display arg1)
        (display arg2)(newline)
)

(system "clear")
(d-New)
(nwctext-parser lexer displayerror)


;; Close input port
(close (current-input-port)) 
