;LigatureEnd
(let ((tag "LigatureEnd"))
    (StandAloneDirectiveProto (cons tag "\\]  ") #f)
    (d-DirectivePut-standalone-graphic tag "\n]\nDenemo\n40")
    (d-DirectivePut-standalone-gy tag 10)
    (d-DirectivePut-standalone-override tag DENEMO_OVERRIDE_GRAPHIC)
    (d-DirectivePut-standalone-display tag (_ "Ligature End"))
    (d-RefreshDisplay)
    (d-SetSaved #f))
