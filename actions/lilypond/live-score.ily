%\version "2.19.25"

#(define (override-color-for-all-grobs color)
  (lambda (context)
   (let loop ((x all-grob-descriptions))
    (if (not (null? x))
     (let ((grob-name (caar x)))
      (ly:context-pushpop-property context grob-name 'color color)
      (loop (cdr x)))))))

#(define (note-id grob) 
   (let* ((origin (ly:input-file-line-char-column
                   (ly:event-property (ly:grob-property grob 'cause) 'origin))))
  (string-concatenate 
    (list 
      "Note-" 
      (ly:format "~a-~a"
        (cadr origin)
        (caddr origin))))))

#(define (rest-id grob) 
   (let* ((origin (ly:input-file-line-char-column
                   (ly:event-property (ly:grob-property grob 'cause) 'origin))))
  (string-concatenate 
    (list 
      "Rest-" 
      (ly:format "~a-~a"
        (cadr origin)
        (caddr origin))))))

liveScoreOn = {
  \override NoteHead.id = #note-id
  \override Rest.id = #rest-id
}

% This part is based on the original event-listener.ly file which is
% part of LilyPond.

%\version "2.16.0"

%%%% Helper functions

#(define (filename-from-staffname context)
   "Constructs a filename in the form
@file{@var{original_filename}-@var{staff_instrument_name}.notes} if the
staff has an instrument name.  If the staff has no instrument
name, it uses "unnamed-staff" for that part of the filename."
   (let* ((inst-name (ly:context-property context 'instrumentName)))
     (string-concatenate (list
                          (substring (object->string (command-line))
                           ;; filename without .ly part
                           (+ (string-rindex (object->string (command-line)) #\sp) 2)
                           (- (string-length (object->string (command-line))) 5))
                          "-"
                          (if (string? inst-name)
                              inst-name
                            "unnamed-staff")
                          ".notes"))))

#(define (format-moment moment)
   (inexact->exact (round (* 1000 
    (/ (ly:moment-main-numerator moment)
       (ly:moment-main-denominator moment))))))
       

#(define (moment-grace->string moment)
   "Prints a moment without grace note(s) as a float such as
0.25000.  Grace notes are written with the grace duration as a
separate \"dashed\" number, i.e. 0.25000-0.12500.  This allows any
program using the output of this function to interpret grace notes
however they want (half duration, quarter duration?  before beat,
after beat?  etc.)."
   (if
       (zero? (ly:moment-grace-numerator moment))
       (ly:format "~a" (format-moment moment))
       ;; grace notes have a negative numerator, so no "-" necessary
       (ly:format
         "~a"
         (format-moment moment))))
         

#(define (make-output-string-line context values)
   "Constructs a tab-separated string beginning with the
score time (derived from the context) and then adding all the
values.  The string ends with a newline."
   (let* ((moment (ly:context-current-moment context)))
    (string-append
     (string-join
       (append
         (list (moment-grace->string moment))
         (map
             (lambda (x) (ly:format "~a" x))
             values))
       "\t")
     "\n")))


#(define (print-line context . values)
   "Prints the list of values (plus the score time) to a file, and
optionally outputs to the console as well.  context may be specified
as an engraver for convenience."
   (if (ly:translator? context)
       (set! context (ly:translator-context context)))
   (let* ((p (open-file "events.txt" "a")))
     ;; for regtest comparison
    (if (defined? 'EVENT_LISTENER_CONSOLE_OUTPUT)
     (ly:progress
      (make-output-string-line context values)))
    (display
     (make-output-string-line context values)
     p)
    (close p)))


%%% main functions

#(define (format-rest engraver event)
   (let* ((origin (ly:input-file-line-char-column
                   (ly:event-property event 'origin))))
     (print-line engraver
                 "rest"                
                 (ly:duration->string
                  (ly:event-property event 'duration))
                 (format-moment (ly:duration-length
                                 (ly:event-property event 'duration)))
                 ;; point and click info
                 (ly:format "point-and-click ~a ~a"
                            (caddr origin)
                            (cadr origin)))))

#(define (format-note engraver event)
   (let* ((origin (ly:input-file-line-char-column
                   (ly:event-property event 'origin))))
     (print-line engraver
                 "note"
                 
                 (ly:duration->string
                  (ly:event-property event 'duration))
                 (format-moment (ly:duration-length
                                 (ly:event-property event 'duration)))
                 ;; point and click info
                 (ly:format "point-and-click ~a ~a"
                            (caddr origin)
                            (cadr origin))
                 ;; get a MIDI pitch value.
                 (+ 60 (ly:pitch-semitones
                        (ly:event-property event 'pitch))))))

#(define (format-tempo engraver event)
    (if  (ly:event-property event 'tempo-unit)
        (print-line engraver
               "tempo"
               ; get length of quarter notes, in seconds
               (* (ly:event-property event 'metronome-count)
                   (format-moment (ly:duration-length (ly:event-property
                                                       event
                                                       'tempo-unit)))))))

#(define (format-breathe engraver event)
   (print-line engraver
               "breathe"))

#(define (format-glissando engraver event)
   (print-line engraver
               "gliss"))

#(define (format-tie engraver event)
   (print-line engraver
               "tie"))

#(define (format-articulation engraver event)
   (print-line engraver
               "script"
               (ly:event-property event 'articulation-type)))

#(define (format-text engraver event)
   (print-line engraver
               "text"
               (ly:event-property event 'text)))

#(define (format-slur engraver event)
   (print-line engraver
               "slur"
               (ly:event-property event 'span-direction)))

#(define (format-dynamic engraver event)
   (print-line engraver
               "dynamic"
               (ly:event-property event 'text)))

#(define (format-cresc engraver event)
   (print-line engraver
               "cresc"
               (ly:event-property event 'span-direction)))

#(define (format-decresc engraver event)
   (print-line engraver
               "decresc"
               (ly:event-property event 'span-direction)))

#(define (format-textspan engraver event)
   (let* ((context (ly:translator-context engraver))
          (moment (ly:context-current-moment context))
          (spanner-props (ly:context-grob-definition context 'TextSpanner))
          (details (assoc-get 'bound-details spanner-props))
          (left-props (assoc-get 'left details '()))
          (left-text (assoc-get 'text left-props '())))
     (print-line engraver
                 "set_string"
                 (ly:event-property event 'span-direction)
                 left-text)))


%%%% The actual engraver definition: We just install some listeners so we
%%%% are notified about all notes and rests. We don't create any grobs or
%%%% change any settings.

#(define-markup-command (with-url layout props url arg)
    (string? markup?)
    ;;;(display url)
    (interpret-markup layout props arg))
    


\layout {
 \override Score.NoteHead.id = #note-id
  \override Score.Rest.id = #rest-id
  \context {
  \Voice
  \consists #(make-engraver
              (listeners
               (tempo-change-event . format-tempo)
               (rest-event . format-rest)
               (note-event . format-note)
               ;(articulation-event . format-articulation)
               ;(text-script-event . format-text)
               ;(slur-event . format-slur)
               ;(breathing-event . format-breathe)
               ;(dynamic-event . format-dynamic)
               ;(crescendo-event . format-cresc)
               ;(decrescendo-event . format-decresc)
               ;(text-span-event . format-textspan)
               ;(glissando-event . format-glissando)
               ;(tie-event . format-tie)
               ))
  }
}
#(open-file  "events.txt" "w")
pageBreak = {}
