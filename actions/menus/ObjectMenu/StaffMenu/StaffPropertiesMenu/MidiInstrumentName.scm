;;;MidiInstrumentName
(let ((params MidiInstrumentName::params) (tag  "MidiInstrumentName") (current ""))
(if (equal? params "edit")
    (set! params #f))
  (if (string? params)
    (begin
        (set! current params)
        (set! params #f))
    (begin
            (set! current "violin")
            (set! current (d-GetUserInput (_ "MIDI Instrument Name") (_ "Give name of MIDI instrument for current staff:") current))))
  (if (string? current)
     (begin
        (d-DirectivePut-staff-display tag current)
        (d-DirectivePut-staff-override tag  DENEMO_OVERRIDE_GRAPHIC)
        (d-DirectivePut-staff-postfix tag  (string-append "\n\\set Staff.midiInstrument = #\"" current "\" ")))))
