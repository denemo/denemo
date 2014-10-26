;;;;;;ColorChordSymbol
(let ((tag "ColorChordSymbol")(params ColorChordSymbol::params))
    (if (equal? params "edit")
        (set! params #f))
    
    (let ((color (d-SelectColor (_ "Color for Next Chord"))))
        (if color
            (begin
                (d-Directive-standalone tag)
                (d-DirectivePut-standalone-postfix tag (format #f "\\once \\override ChordName.color = #(rgb-color ~s ~s ~s) " (list-ref color 0)  (list-ref color 1) (list-ref color 2)))
                (d-DirectivePut-standalone-data tag (format #f "'~s" color))
                (d-DirectivePut-standalone-display tag (_ "Color"))
                (d-DirectivePut-standalone-minpixels tag 50)
                (d-SetSaved #f)
                (d-RefreshDisplay)))))