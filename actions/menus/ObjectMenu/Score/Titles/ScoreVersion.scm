;;;ScoreVersion
(let* ((tag "ScoreVersion")(current (d-DirectiveGet-score-data tag )))
  (if (not current)
      (set! current (_ "v 1.0")))
  (set! current (d-GetUserInput (_ "Version") (_ "Give a version for this edition:") current))
  (if current
    (begin 
        (d-DirectivePut-score-display tag current)
        (d-DirectivePut-score-data tag current)
        (case (RadioBoxMenu 
            (cons (_ "Show on first page") 'first)
            (cons (_ "Show on every page") 'all)
            (cons (_ "Delete") 'delete))
         ((first)
            (d-DirectiveDelete-paper tag)
            (d-DirectivePut-score-override tag  DENEMO_OVERRIDE_AFFIX)
            (d-DirectivePut-score-prefix tag  (string-append "\\markup \\teeny \"" current "\"")))
        ((all)
            (d-DirectivePut-score-prefix tag "")
            (d-DirectivePut-paper-postfix tag (string-append "\nevenHeaderMarkup = \\markup \\teeny {" current "}\n"   
            "oddHeaderMarkup = \\markup \\teeny {" current "}\n")))
        ((delete)
            (d-DirectiveDelete-paper tag)
            (d-DirectiveDelete-score tag)))         
        (d-SetSaved #f))
    (d-InfoDialog (_ "Cancelled"))))