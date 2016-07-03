;;;Vibrato
(let ((tag "Vibrato")(amplitudes "")(wavelength "1")(thickness "0.2") (input #f)(message (_ "Cancelled"))
    (choice (list (cons (_ "Start a vibrato") 'start) (cons (_ "Stop a vibrato") 'stop))))
    (d-LilyPondInclude "vibrato.ily")
    (if (d-Directive-chord? tag)
        (set! choice (cons* (cons (_ "Delete") 'delete)  choice)))
    (set! choice (RadioBoxMenuList choice)) 
    (case choice
        ((delete)
                (set! message (_ "Deleted"))
                (d-DirectiveDelete-chord tag))
        ((stop)
            (set! message #f)
            (d-DirectivePut-chord-display tag (_"Vibrato End"))
            (d-DirectivePut-chord-postfix tag "\\stopTrillSpan ")
            (d-DirectivePut-chord-override tag DENEMO_OVERRIDE_AFFIX)
            (d-SetSaved #f))
        ((start)
            (let loop ()
                (set! input (d-GetUserInput (_ "Vibrato") (_ "Give amplitude or 0 to finish giving amplitudes") "0.5"))
                (if (and input (string->number input))
                    (begin
                        (if (positive? (string->number input))
                            (begin
                                (set! amplitudes (string-append amplitudes " " input))
                                (loop))))))
            (if (not (null? amplitudes))
                (begin
                    (set! wavelength (d-GetUserInput (_ "Vibrato") (_ "Give wavelength for wavy line ") "1.0"))
                    (if (and wavelength (string->number wavelength))
                        (begin
                            (set! thickness  (d-GetUserInput (_ "Vibrato") (_ "Give thickness for wavy line ") "0.2"))
                            (if (and thickness (string->number thickness))
                                (begin
                                    (set! message #f)
                                    (d-DirectivePut-chord-display tag "Vibrato")
                                    (d-DirectivePut-chord-prefix tag (string-append " \\vibrato #'(" amplitudes ") #" wavelength " #" thickness " "))
                                    (d-DirectivePut-chord-postfix tag "\\startTrillSpan ")
                                    (d-DirectivePut-chord-override tag DENEMO_OVERRIDE_AFFIX)
                                    (d-SetSaved #f)))))))))
    (if message
        (d-WarningDialog message)))