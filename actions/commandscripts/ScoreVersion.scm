;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;;;ScoreVersion
(let ((tag "ScoreVersion")(current #f) (thematch #f))
  (set! current (d-DirectiveGet-score-prefix tag ))
  (if (boolean? current)
      (set! current (_ "v 1.0"))
      (begin
	(set! thematch (string-match "\\\\markup \\\\teeny \"([^\"]*)\"" current))
	(if (regexp-match? thematch)
	    (set! current (match:substring thematch 1))
	    (set! current (_ "v 1.0")))))
  (set! current (d-GetUserInput (_ "Version") (_ "Give a version for this edition:") current))
  (if current
  	(begin 
  		(d-DirectivePut-score-display tag current)
  		;;(d-DirectivePut-score-override tag  DENEMO_OVERRIDE_GRAPHIC)
  		(d-DirectivePut-score-prefix tag  (string-append "\\markup \\teeny \"" current "\""))
  		(d-SetSaved #f))
  	(d-DirectiveDelete-score tag)))

