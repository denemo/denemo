;;;; TransposeScorePrint
(define-once Transpose::Interval "c ees")

(let ((text #f) (global-tag "GlobalTranspose")(tag "TransposeOnPrint"))
(define (get-step note)
    (let ((step #f))
        (case (string-ref note 0)
            ((#\c) (set! step 0))
            ((#\d) (set! step 1))
            ((#\e) (set! step 2))
            ((#\f) (set! step 3))
            ((#\g) (set! step 4))
            ((#\a) (set! step 5))
            ((#\b) (set! step 6)))
       step))
(define (get-accidental note)
    (define NATURAL 0)
    (define FLAT (/ -1 2))
    (define SHARP (/ 1 2))
    (define DOUBLE-FLAT -1)
    (define DOUBLE-SHARP 1)
    (if (string-contains note "isis")
        DOUBLE-SHARP
        (if (string-contains note "eses")
            DOUBLE-FLAT
            (if (string-contains note "is")
                SHARP
                (if (string-contains note "es")
                    DOUBLE-FLAT
                    NATURAL)))))
(define (step-diff base note)
    (define step1 (get-step base))
    (define step2 (get-step note))
    (set! step1 (- step2 step1))
    (while (< step1 0)
        (set! step1 (+ step1 7)))
    step1)
    
(define (acc-diff base note)
    (define acc1 (get-accidental base))
    (define acc2 (get-accidental note))
    (/ (- acc2 acc1) 2))
    
(define (parser-location)
	(if (equal? (d-GetLilyVersion) "2.20")
		""
		"parser location"))
		
(if (and TransposeScorePrint::params (not (equal?  TransposeScorePrint::params "edit")))
    (set! Transpose::Interval TransposeScorePrint::params)
    (set! Transpose::Interval (d-GetLilyPondSyntaxFromUser  (_ "Set Transpose Interval") (_ "Give Interval to transpose by as two note names, 
     for example \"c g\" means transpose 5th up.
    Note names are in Dutch!!! a,b,c ... are the same but
    \"es\" = flat, so e.g. bes means b-flat
    \"is\" = sharp so e.g fis means f-sharp
    Use commas for octave(s) down, 
    single-quotes for octave(s) up
    e.g. c c' means octave up.
    You do not have to start with c
    e.g. d e means a tone higher.
    ")  (string-append  "\\score {<< {\\clef treble \\key c \\major c' d' e' f' g' a' b'} >>} _\\markup\\huge \"" (_ "Will be transposed to:")  "\"   \\score{<<\\transpose ")     " {\\clef treble \\key c \\major c'^\\markup\\huge{C} d'^\\markup\\huge{D} e'^\\markup\\huge{E} f'^\\markup\\huge{F} g'^\\markup\\huge{G} a'^\\markup\\huge{A} b'^\\markup\\huge{B}} >>}"     
     Transpose::Interval )))
(if Transpose::Interval
  (let ((base #f)(note (string-tokenize Transpose::Interval)) (choice (RadioBoxMenu
                     (cons "Global (includes quoted music)\n(and any layout)"   'global)   
                        (cons "Main Score Only\n(overrides global for layout)" 'score))))
              (d-DirectiveDelete-score "TransposeScorePrint") ;;;get rid of old style transpose directive      
              (set! base (car note))
              (set! note (cadr note))
              (case choice
                     ((global)      
                         (d-DirectivePut-score-override global-tag  DENEMO_OVERRIDE_AFFIX)
                         (d-DirectivePrioritizeTag-score global-tag)
                         (d-DirectivePut-score-prefix global-tag   (string-append 
                                "#(define DenemoTransposeStep " (number->string (step-diff base note)) ")\n"
                                "#(define DenemoTransposeAccidental " (number->string (acc-diff base note)) ")\n"
                                "\nDenemoGlobalTranspose = #(define-music-function ("
                                (parser-location) 
                                " arg)(ly:music?) #{\\transpose "
                     Transpose::Interval "#arg #})\n"))
                            (d-DirectivePut-score-postfix tag  "\\DenemoGlobalTranspose "))
                (else
                         (d-DirectivePut-score-postfix tag (string-append  "\\transpose " Transpose::Interval " "))))
      (set! text (string-append  "Print transposed:  " Transpose::Interval " ")) 
      (d-DirectivePut-score-display tag text)
      (d-DirectivePut-score-override tag DENEMO_OVERRIDE_GRAPHIC)
      (d-SetSaved #f))))
