;ShortInstrumentName
(let ((tag "ShortInstrumentName") (current "") (thematch ""))
  (if (d-Directive-staff? "DynamicsStaff")
    (begin
        (d-WarningDialog (_ "Instrument Name should not be set on a Dynamics Line"))
        (d-DirectiveDelete-staff tag))
    (begin
      (set! current (d-DirectiveGet-staff-postfix "ShortInstrumentName" ))
      (if (boolean? current)
          (set! current "Vln")
          (begin
        (set! thematch (string-match "\\\\set Staff.shortInstrumentName = \"([^\"]*)\"" current))
        (if (regexp-match? thematch)
            (set! current (match:substring thematch 1))
            (set! current "Vln"))))
      (set! current (d-GetUserInput (_ "Short Instrument Name") (_ "Give Short Instrument Name:") current))
      (if current
        (let ((indent "0")( size (/ (string->number (d-ScoreProperties "query=fontsize")) 10.0))) 
            (d-DirectivePut-staff-display tag current)
            (d-DirectivePut-staff-override tag   (logior DENEMO_ALT_OVERRIDE  DENEMO_OVERRIDE_AFFIX  DENEMO_OVERRIDE_GRAPHIC))
            (d-DirectivePut-staff-prefix tag  (string-append " shortInstrumentName = \"" current "\""))
            (d-DirectivePut-staff-ignore tag (d-GetIdForName (d-StaffProperties "query=lily_name"))) ;(SetDirectiveConditional "staff" tag)
            (set! indent (max (string->number indent) (* size (string-length current))))
            (d-ScoreShortIndent indent)
            (d-SetSaved #f))))))
