;;;MeasuresPerLine
(let ((tag "MeasuresPerLine")(params  MeasuresPerLine::params)(value #f))
    (if (and params (not (equal? params "edit")))
        (set! value params)
        (set! value (d-GetUserInput (_ "Measures per line") (_ "Give required measures per line: ") "4")))
    (if (and value (string->number value))
        (begin
        
            (d-DirectivePut-score-display tag  (_ "Measures per line"))
                        (d-DirectivePut-score-override tag (logior DENEMO_OVERRIDE_AFFIX))
                        (d-DirectivePut-score-prefix tag "
#(define ((bars-per-line-engraver bar-list) context)
  (let* ((working-copy bar-list)
         (total (1+ (car working-copy))))
    `((acknowledgers
       (paper-column-interface
        . ,(lambda (engraver grob source-engraver)
             (let ((internal-bar (ly:context-property context 'internalBarNumber)))
               (if (and (pair? working-copy)
                        (= (remainder internal-bar total) 0)
                        (eq? #t (ly:grob-property grob 'non-musical)))
                   (begin
                     (set! (ly:grob-property grob 'line-break-permission) 'force)
                     (if (null? (cdr working-copy))
                         (set! working-copy bar-list)
                         (begin
                           (set! working-copy (cdr working-copy))))
                           (set! total (+ total (car working-copy))))))))))))")     
        
            (d-DirectivePut-layout-display tag (_ "Measures per line"))
            (d-DirectivePut-layout-postfix tag  (string-append "
\\context {
      \\Score
      %use the line below to insist on your layout
      %\\override NonMusicalPaperColumn.line-break-permission = ##f
      \\consists #(bars-per-line-engraver '( " value "))}\n"))
      
            (d-SetSaved #f))))
