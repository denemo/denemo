;;;PolymetricStaffs
 (let ((tag "PolymetricStaffs"))
     (if (d-Directive-layout? tag)
        (begin
            (d-DirectiveDelete-layout tag)
            (d-WarningDialog (_ "Polymetric Staffs will no longer typeset")))
        (begin
         (d-DirectivePut-layout-postfix tag
           "\\context {
              \\Score
              \\remove \"Timing_translator\"
              \\remove \"Default_bar_line_engraver\"
              \\remove \"Repeat_acknowledge_engraver\"
              \\remove \"Volta_engraver\"
            }

            \\context{
              \\Staff
              \\consists \"Timing_translator\"
              \\consists \"Default_bar_line_engraver\"
              \\consists \"Repeat_acknowledge_engraver\"
              \\consists \"Volta_engraver\"
            }")
        (d-WarningDialog (_ "Staffs can have differing time signatures")))))
                                   