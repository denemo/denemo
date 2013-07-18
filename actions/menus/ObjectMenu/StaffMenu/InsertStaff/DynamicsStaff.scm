;;DynamicsStaff
(let ((tag "DynamicsStaff"))
(d-AddAfter)
(d-DirectivePut-clef-graphic tag "
D
Denemo
48")
(d-DirectivePut-clef-gy tag 36)
(d-DirectivePut-clef-override tag (logior DENEMO_OVERRIDE_GRAPHIC DENEMO_OVERRIDE_LILYPOND ))
(d-DirectivePut-keysig-override tag  (logior DENEMO_OVERRIDE_GRAPHIC DENEMO_OVERRIDE_LILYPOND ) )
(d-DirectivePut-timesig-override tag DENEMO_OVERRIDE_LILYPOND )
(d-DirectivePut-staff-prefix tag " \\new Dynamics <<\n" )
(d-DirectivePut-staff-graphic tag "Dynamics Staff" )
(d-DirectivePut-staff-override tag  DENEMO_OVERRIDE_LILYPOND )
(d-DirectivePut-voice-override tag  DENEMO_OVERRIDE_LILYPOND )
(d-InfoDialog (_ "This staff is purely for holding cresc. dim hairpins and dynamic markings so that they can be positioned (using dummy notes or spacers) and will align with each other. Create this between the staffs for a piano work.")))
