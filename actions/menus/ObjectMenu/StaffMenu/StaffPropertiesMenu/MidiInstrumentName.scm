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
        (d-DirectivePut-staff-override tag  (logior DENEMO_ALT_OVERRIDE  DENEMO_OVERRIDE_AFFIX  DENEMO_OVERRIDE_GRAPHIC))
        (d-DirectivePut-staff-prefix tag  (string-append " midiInstrument = #\"" current "\" ")))))
