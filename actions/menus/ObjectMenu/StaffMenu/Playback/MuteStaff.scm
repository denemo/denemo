;;;MuteStaff
(let ((tag "MuteStaff"))
    (if (equal? MuteStaff::params "delete")
        (begin
            (d-LockDirective #f)
            (set! MuteStaff::params #f)
            (d-DirectiveDelete-standalone tag)
            (d-StaffMasterVolume #t))       
        (begin
            
            (d-PushPosition)
            (d-MoveToBeginning)
            (if (not (d-Directive-standalone? tag))
                (begin
                    (d-Directive-standalone tag)
                    (d-LockDirective #t)))
            (d-DirectivePut-standalone-minpixels tag 50)
            (d-DirectivePut-standalone-gx tag 20)
            
            (if (equal? MuteStaff::params "unmute")
                (d-StaffMasterVolume #t)) ;;pass #t to force un-mute
            
            (if (zero? (d-StaffMasterVolume))
                (begin
                    (d-DirectivePut-standalone-graphic tag "Speaker_Icon_Mute")
                    (d-StaffMasterVolume #f))
                (begin
                    (d-StaffMasterVolume #t)
                     (if (< (d-StaffMasterVolume) 0.2)
                        (d-WarningDialog (_ "The volume set on this staff is very low, you may not hear it.\nSee Staff->Staff Properties->Built-in Staff Properties, MIDI tab.")))
                    (d-DirectivePut-standalone-graphic tag "Speaker_Icon")))
            (d-SetSaved #f)
            (d-PopPosition)))
    (if (d-Directive-standalone? tag)
        (d-MoveCursorRight))
   (d-RefreshDisplay))
