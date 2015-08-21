
;;;SustainOff
(let ((tag "SustainOff"))
 (if (d-Directive-standalone? tag)
    (d-DirectiveDelete-standalone tag)
    (begin
        (if (d-MoveCursorLeft)
            (if (d-Directive-standalone? tag)
              (d-DirectiveDelete-standalone tag)
                (d-MoveCursorRight)))
    (StandAloneDirectiveProto (cons "SustainOff" "\\sustainOff") #f "\n!P\nDenemo\n24")
    (d-DirectivePut-standalone-midibytes tag "0xB$ 0x40 0x00")
    (d-DirectivePut-standalone-gy tag 44)
    (d-DirectivePut-standalone-grob tag tag)
    (d-MoveCursorRight)))
(d-RefreshDisplay)
(d-SetSaved #f))
        
