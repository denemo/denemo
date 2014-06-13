;;RefreshDynamicDirectives
(let loop ((number 0))
  (define tag (d-DirectiveGetNthTag-score number))
  (if tag
    (let ((command (string-append "d-" tag)))
     ; (disp "looking at " tag "and " command "\n")
        (if (positive? (logand (d-DirectiveGet-score-override tag) DENEMO_OVERRIDE_DYNAMIC))
            (let ((sym (string->symbol command)))
                (if (defined? sym)
                    (eval-string (string-append "(" command " 'refresh)")))))
        (loop (+ 1 number)))))
(let loop ((number 0))
  (define tag (d-DirectiveGetNthTag-movementcontrol number))
  (if tag
    (let ((command (string-append "d-" tag)))
     ; (disp "looking at " tag "and " command "\n")
        (if (positive? (logand (d-DirectiveGet-movementcontrol-override tag) DENEMO_OVERRIDE_DYNAMIC))
            (let ((sym (string->symbol command)))
                (if (defined? sym)
                    (eval-string (string-append "(" command " 'refresh)")))))
        (loop (+ 1 number)))))        
;;; FIXME there are 13 other types of directive to refresh ,,,
