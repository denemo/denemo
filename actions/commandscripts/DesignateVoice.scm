;;;DesignateVoice
(let ((choice #f)(label #f)
        (OneVoice (_ "OneVoice:  Default-For a single voice on a staff")) (VoiceOne
(_ "VoiceOne:  Designate as upper voice"))
        (VoiceTwo (_ "VoiceTwo:  Designate as lower voice"))(VoiceThree (_ "VoiceThree: 
Horizontally offset upper voice"))
        (VoiceFour (_ "VoiceFour:  Horizontally offset lower voice")) 
        )
(set! choice (d-GetOption (string-append OneVoice stop VoiceOne stop VoiceTwo
stop VoiceThree stop VoiceFour stop) ) )
(cond
        ( (equal? choice VoiceOne ) (begin (set! choice "voiceOne") (set! label
(_ "Voice1")) ))
        ( (equal? choice VoiceTwo ) (begin (set! choice "voiceTwo") (set! label
(_ "Voice2")) ))
        ( (equal? choice VoiceThree ) (begin (set! choice "voiceThree") (set! label
(_ "Voice3")) ))
        ( (equal? choice VoiceFour ) (begin (set! choice "voiceFour") (set! label
(_ "Voice4")) ))
        ( (equal? choice OneVoice ) (begin (set! choice "oneVoice") (set! label
(_ "1Voice")) ) )
)
(if  choice
        (begin
                (d-DirectivePut-standalone "Voice" )
                (d-DirectivePut-standalone-display "Voice" label )
                (d-DirectivePut-standalone-ty "Voice" 60 )
                (d-DirectivePut-standalone-postfix "Voice" (string-append "\" choice ) )
                (d-DirectivePut-standalone-minpixels "Voice" 10 )
                (d-MoveCursorRight) 
        )
)
)