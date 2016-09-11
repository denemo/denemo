;;;PageNumbersWithInstrument
(let ((tag "PageNumbersWithInstrument"))
(if (d-Directive-paper? tag)
    (begin
        (d-DirectiveDelete-paper tag)
        (d-InfoDialog (_ "Default Page Numbering Restored")))
(d-DirectivePut-paper-postfix tag
"
   oddHeaderMarkup = \\markup
   \\fill-line {
     \"\"
     \\on-the-fly #not-part-first-page \\fromproperty #'header:instrumentation
     \\on-the-fly #print-page-number-check-first \\number \\fromproperty 
#'page:page-number-string
   }

   %% evenHeaderMarkup would inherit the value of
   %% oddHeaderMarkup if it were not defined here
   evenHeaderMarkup = \\markup
   \\fill-line {
     \\on-the-fly #print-page-number-check-first \\number \\fromproperty 
#'page:page-number-string
     \\on-the-fly #not-part-first-page \\fromproperty #'header:instrumentation
     \"\"
   }
"))
(d-SetSaved #f))
