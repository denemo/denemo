; SwitchMensuralBarlines // "Mensurstriche" layout for one movement
; Consist of two independent, Denemo Directives.
(if  (d-DirectiveGet-layout-postfix "MensuralBarlines") 
    (begin ;If there is a directive, delete all
        (d-DirectiveDelete-layout "MensuralBarlines")
        (d-DirectiveDelete-score "MensuralBarlines"))
    (begin   ;If there is no directive present, create one
        ; Make barlines transparent only within the staff-lines range
        (d-DirectivePut-layout-postfix "MensuralBarlines" " 
        \\context {
            \\Score
            \\override BarLine #'transparent = ##t
          }
        ")

        ; Override the automatically placed \EndMovementBarline with a explicitly drawn end-barline.
        ; This will enable the final Mensural Barline, but will not disturb other movements.
        (d-DirectivePut-score-prefix "MensuralBarlines" "
        EndMovementBarline = { \\once \\override Score.BarLine #'transparent = ##f \\bar \"|.\" }
        ")
        (d-DirectivePut-score-override "MensuralBarlines" DENEMO_OVERRIDE_AFFIX)
        (d-PushPosition)
        ; Surround all Staffs with a StaffGroup, which draws the barlines between the staffs. 
        (while (d-StaffUp))
        (d-GroupStaffStart)
        (while (d-StaffDown))
        (d-BraceEnd)
        (d-PopPosition)))
 (d-SetSaved #f)
