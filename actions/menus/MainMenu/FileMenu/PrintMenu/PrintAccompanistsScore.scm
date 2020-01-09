;;PrintAccompanistsScore new version
(let ((tag "PrintAccompanistsScore")(current (_ "Accompanist's Score"))(saved (d-GetSaved)) (count (d-GetUserInput (_ "Print Accompanist's Score") (_ "How many staffs to make small?") "1")))
  (if (and count (string->number count))
      (let ((name (string-append (_ "Accompanist with ") count (_ " cue part") )))
        (set! current (d-DirectiveGet-scoreheader-display tag))
        (if (not current)
            (set! current (d-GetUserInput (_ "Print Accompanist's Score") (_ "Give Instrumentation ") (_ "Accompanist's Score"))))
        (if (not current)
            (set! current " "))
        (d-DirectivePut-scoreheader-postfix tag (string-append "instrumentation = \\markup {" current "}\n"))
        (d-DirectivePut-scoreheader-data tag current)
        (d-DirectivePut-scoreheader-override tag DENEMO_OVERRIDE_TAGEDIT) ;BookInstrumentation
        (d-ScoreheaderDirectivePutAllow tag (d-GetIdForName name))
        (set! count (string->number count))
        (let movement ((mnum 1))
            (let loop ((number 1))
                  (if (d-GoToPosition mnum number 1 1)
                    (begin
                        (d-DirectivePut-staff-prefix tag " fontSize = #-3\n \\override StaffSymbol #'staff-space = #(magstep -3)\n")
                        (d-DirectivePut-staff-override tag  (logior DENEMO_OVERRIDE_GRAPHIC DENEMO_OVERRIDE_AFFIX DENEMO_ALT_OVERRIDE))
                        (d-StaffDirectivePutAllow tag (d-GetIdForName name))
                        (if (< number count)
                            (loop (1+ number)))
                        (movement (1+ mnum))))))
                          (disp "layout is1 " (d-GetLayoutName) "\n")
                (d-DeleteLayout name) 
                   (d-SelectDefaultLayout) ; this is needed so that the Accompanist's score is a customized version of the Default Score Layout
                (d-CreateLayout name))))
                 
