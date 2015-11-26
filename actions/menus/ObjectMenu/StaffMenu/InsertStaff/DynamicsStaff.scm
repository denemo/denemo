;;DynamicsStaff
(let ((tag "DynamicsStaff")(name (d-StaffProperties "query=denemo_name")))
    (if (not (d-Directive-staff? tag))
        (begin
            (d-SetSaved #f)
            (d-PushPosition)
            (d-NewStructuredStaff)
            (d-InitialClef "Alto")
            (d-SetStaffRangeHi 0)
            (d-SetStaffRangeLo 0)
            (d-StaffProperties (string-append "denemo_name=" name))
            (d-SetColorOfStaff #x20A0E000)
            (d-SetLinesInStaff 1)
            (d-ShortenStaffHeight 40)
            (d-DirectivePut-clef-graphic tag "\nD\nDenemo\n48")
            (d-DirectivePut-clef-gy tag 36)
            (d-DirectivePut-clef-override tag (logior DENEMO_OVERRIDE_GRAPHIC DENEMO_OVERRIDE_LILYPOND ))
            (d-DirectivePut-keysig-override tag  (logior DENEMO_OVERRIDE_GRAPHIC DENEMO_OVERRIDE_LILYPOND))
            (d-DirectivePut-timesig-override tag (logior DENEMO_OVERRIDE_GRAPHIC DENEMO_OVERRIDE_LILYPOND))
            (d-DirectivePut-staff-prefix tag " \\new Dynamics <<\n" )
            (d-DirectivePut-staff-graphic tag "Dynamics Staff" )
            (d-DirectivePut-staff-override tag  (logior DENEMO_OVERRIDE_GRAPHIC  DENEMO_OVERRIDE_LILYPOND ))
            (d-DirectivePut-voice-override tag   (logior DENEMO_OVERRIDE_GRAPHIC DENEMO_OVERRIDE_LILYPOND ))
            (d-MuteStaff #t)
            (d-PopPosition)
            (d-MoveToStaffDown)
            (d-CursorToNote "c'")))
    (d-InfoDialog (_ "This line (\"staff\") is for holding cresc. dim hairpins and dynamic markings so that they can be positioned (using dummy notes, colored blue) and will align with each other. Create this between the staffs for a piano work or on any staff where alignment of hairpins and dynamic marks is needed.\nWARNING: Do not place clef changes or other non-duration items in this staff - it may trigger the creation of a separate staff on typesetting!\nThis Dynamics \"staff\" must have the same part name as the staff the dynamics should appear on.")))
