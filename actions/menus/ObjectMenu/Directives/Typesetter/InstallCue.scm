;;;;InstallCue
(let ((tag "InstallCue")(params InstallCue::params)(cuename #f)(clef (d-GetPrevailingClef)))
    (define (get-cuenames theclef)
        (define cuenames '())
        
        (d-PushPosition)
        (while (d-MoveToStaffUp))
        (let loop ((count 0))
            (define clef (d-GetPrevailingClef))
            (set! cuenames (cons 
                (cons (d-StaffProperties "query=denemo_name") 
                    (if (equal? clef theclef)
                    
                      (string-append "\\cueDuring #\"" (d-StaffProperties "query=denemo_name") " Mvmnt " (number->string (d-GetMovement)) "\"#1 {")
                      (string-append "\\cueDuringWithClef #\"" (d-StaffProperties "query=denemo_name") " Mvmnt " (number->string (d-GetMovement)) "\"#1 #\"" (string-downcase clef 0 1) "\" {"))) cuenames))
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
        (begin
            (set! cuename (RadioBoxMenuList (get-cuenames clef)))
            (if cuename
                (begin
                    (d-Directive-standalone tag)            
                    (d-DirectivePut-standalone-minpixels tag 30)
                    (d-DirectivePut-standalone-postfix tag cuename)
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
                    (d-PopPosition))))))

