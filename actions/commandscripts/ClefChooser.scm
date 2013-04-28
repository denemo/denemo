;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;;;ClefChooser
(let ((choice "Bass"))
(set! choice (d-GetOption (string-append "Treble" stop "Bass" stop "Alto" stop "Tenor" stop "Treble Octava bassa" stop "Bass Octava bassa"stop "Soprano" stop "Drum" stop)))
(cond
((boolean? choice) () )
((equal? choice "Drum")
(begin
;(d-MoveCursorRight)
(d-InitialClef "Bass")
(d-DirectivePut-clef-override "DrumClef" (logior DENEMO_OVERRIDE_GRAPHIC DENEMO_OVERRIDE_LILYPOND))
(d-DirectivePut-clef-postfix "DrumClef" "\\clef percussion\n ")
(d-DirectivePut-clef-graphic "DrumClef" "DrumClef")
(d-StaffProperties "midi_channel=9")
(d-PushPosition)
(d-MoveToBeginning)
(d-DirectivePut-standalone-postfix "MiddleCPosition" "\\set Staff.middleCPosition = #6")
(d-PopPosition)
;; (d-DirectivePut-voice-postfix "DrumClef" "\\drummode ")
;; (d-DirectivePut-voice-override "DrumClef" DENEMO_OVERRIDE_LILYPOND)
;; (d-DirectivePut-staff-postfix "DrumClef" "<< { \\new DrumStaff\n")
;; (d-DirectivePut-staff-override "DrumClef" DENEMO_OVERRIDE_LILYPOND)
))
(#t
(if (d-MoveCursorLeft)
(begin
(d-MoveCursorRight)
(d-InsertClef choice))
(begin
(if (equal? (d-DirectiveGetTag-clef ) "DrumClef" ) (d-DirectiveDelete-clef "DrumClef") )
(d-InitialClef choice)
)))))
(d-RefreshDisplay)