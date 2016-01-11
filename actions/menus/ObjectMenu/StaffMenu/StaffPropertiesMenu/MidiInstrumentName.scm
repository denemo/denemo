;;;MidiInstrumentName
(let ((tag  "MidiInstrumentName") (current "") (thematch "") (indent "0.0") (size 16.0) (nextmovement #f) (staff (number->string (d-GetStaff))))
(if (equal? MidiInstrumentName::params "edit")
    (set! MidiInstrumentName::params #f))
  (if (string? MidiInstrumentName::params)
    (begin
        (set! current InstrumentName::params)
        (set! InstrumentName::params #f))
    (begin
            (set! current "Violin")
            (set! current (d-GetUserInput (_ "InstrumentName") (_ "Give name of MIDI instrument for current staff:") current))))

  (if (string? current)
     (begin
        (d-DirectivePut-staff-display tag current)
        (d-DirectivePut-staff-override tag  DENEMO_OVERRIDE_GRAPHIC)
        (d-DirectivePut-staff-postfix tag  (string-append "\\set Staff.midiInstrument = #\"" current "\" ")))))