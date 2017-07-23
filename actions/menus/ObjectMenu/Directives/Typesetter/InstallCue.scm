;;;;InstallCue
(let ((tag "InstallCue")(params InstallCue::params)(cuename #f)(clef (d-GetPrevailingClef)))
    (define (get-cuenames theclef)
        (define cuenames '())
        (define this-movement (number->string (d-GetMovement)))
        (define this-staff (d-GetStaff))
        (define transpose #f)
        (define (unique-staff-name)
            (string-append (d-StaffProperties "query=denemo_name") (_ " on Staff ")  (number->string (d-GetStaff))))
        (if (d-Directive-score? "GlobalTranspose")
                (set! transpose  (d-GetUserInput (_ "Transpose Cue") (_ "Give note (in LilyPond notation) that middle C should transpose to\nin this cue:") "c'")))

        (d-PushPosition)
        (while (d-MoveToStaffUp))
        (let loop ((count 0))
            (define clef (d-GetPrevailingClef))
            (if (not (= this-staff (d-GetStaff)))
            (set! cuenames (cons 
                (cons (unique-staff-name)           
                        (cons (if (d-Directive-score? "GlobalTranspose")
                            (begin
                                (if transpose
                                  (if (equal? clef theclef)
                                     (string-append "\\transposedCueDuring #\"" (unique-staff-name) " Mvmnt " this-movement "\" #1 " transpose " {")
                                     (string-append "\\transposedCueDuringWithClef #\"" (unique-staff-name) " Mvmnt " this-movement "\"#1 " transpose " #\"" (string-downcase clef) "\" {"))))
                            (begin
                               (if (equal? clef theclef)    
                                 (string-append "\\cueDuring #\"" (unique-staff-name) " Mvmnt " this-movement "\"#1 {")
                                 (string-append "\\cueDuringWithClef #\"" (unique-staff-name) " Mvmnt " this-movement "\"#1 #\"" (string-downcase clef) "\" {"))))
                            (string-append "\\addQuote \"" (unique-staff-name) " Mvmnt " this-movement "\" \\"(d-GetVoiceIdentifier) "\n")))
                                 cuenames)))        
            (if (d-MoveToStaffDown)
                (loop (1+ count))))
        (d-PopPosition)
        cuenames)
        
     (if (equal? params "edit")
        (begin
            (if (equal? "}" (d-DirectiveGet-standalone-postfix tag))
            (d-InfoDialog (_ "This marks the end of a cue - where notes from another part are printed.\nThis marker can be cut and pasted to another position to alter the extent of the cue."))
            (d-InfoDialog (_ "This marks the start of a cue - if you make an alteration to the number of staffs you will need to delete this cue and remake it.
Watch out that you do not end up with two End Cue directives
as this will not typeset."))))
        (let ((deftag "CuesUsed"))
            (d-DirectivePut-score-prefix deftag (string-append "\ntransposedCueDuringWithClef  =
#(define-music-function
   (parser location what dir pitch clef main-music)
   (string? ly:dir? ly:pitch? string? ly:music?)
   (make-music 'QuoteMusic
         'element main-music
         'quoted-context-type 'CueVoice
         'quoted-context-id \"cue\"
         'quoted-music-name what
         'quoted-music-clef clef
         'quoted-voice-direction dir
         'quoted-transposition pitch))\n"))             
        (d-DirectivePut-score-display deftag deftag)
        (d-DirectivePut-score-override deftag DENEMO_OVERRIDE_AFFIX)
        (set! cuename (get-cuenames clef))
        (if (null? cuename)
            (begin
                (d-WarningDialog (_ "There are no other staffs for this one to take a cue from.")))
            (begin
                (set! cuename (RadioBoxMenuList cuename))
                (if cuename
                (begin
                    (d-DirectivePut-score-prefix (cdr cuename) (cdr cuename))
                    (d-Directive-standalone tag)            
                    (d-DirectivePut-standalone-minpixels tag 30)
                    (d-DirectivePut-standalone-postfix tag (car cuename))
                    (d-DirectivePut-standalone-display tag (_ "Start Cue"))
                      (d-DirectivePut-standalone-graphic "InstallCue" "
[
Denemo 48")
                    (d-PushPosition)
                    (GoToMeasureEnd)
                    (if (d-Directive-standalone? tag)
                        (if (d-MoveToMeasureRight)
                            (GoToMeasureEnd)
                            (d-WarningDialog (_ "Nowhere to place the End Cue marker"))))
                    (if (d-Directive-standalone? tag)
                        (d-DirectiveDelete-standalone tag)
                        (begin
                            (d-Directive-standalone tag)
                            (d-DirectivePut-standalone-minpixels tag 30)
                            (d-DirectivePut-standalone-display tag (_ "End Cue"))
                            (d-DirectivePut-standalone-graphic "InstallCue" "
]
Denemo 48")
                            (d-DirectivePut-standalone-postfix tag "}")))
                     (d-SetSaved #f)
                     (d-RefreshDisplay)
                    (d-PopPosition))))))))

