;;;CompactChordSymbol
(let ((tag "CompactChordSymbol")
    (params CompactChordSymbol::params)(symbol #f))
    (if (Note?)
        (begin
            (set! symbol (d-GetUserInput (_ "Chord Symbol") (_ "Give text of chord symbol") "9add13"))
            (d-DirectivePut-chord-postfix tag (string-append "_\\markup\\scale #'(.4 . .6) 
\\sans \\fontsize #10 { \\hspace #8 \\bold " symbol  "}"))
            (d-DirectivePut-chord-display tag symbol)
            (d-DirectivePut-chord-minpixels tag 30)
            (d-DirectivePut-chord-data tag symbol)
            (d-SetSaved #f)
            (d-RefreshDisplay))
        (d-WarningDialog (_ "Insert Root note of chord for symbol to attach to"))));
        
