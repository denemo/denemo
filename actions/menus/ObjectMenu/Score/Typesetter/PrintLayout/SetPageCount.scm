;;;SetPageCount
(let* ((tag "SetPageCount")(count (d-GetUserInput (_ "Total Page Count") (_ "Give Pages Required\n(0 for optimal): ") "4"))
		(layout (d-GetLayoutName))(id (d-GetLayoutId))
		(restrict (RadioBoxMenu  (cons (string-append (_ "Restrict to layout: " ) layout) layout) (cons (_ "For any Layout") 'all))))
		
	(if restrict
		(begin
			(if (eq? restrict 'all)
				(set! restrict #f))
			(if restrict
				(begin
					(if (not (d-Directive-score? tag))
						(d-DirectivePut-score-display tag (_ "Optimal"))) ;;ensure the default comes before any conditional
					(set! tag (string-append tag "\n" restrict))))
		
			(if (and count (string->number count) (> (string->number count) 0))
				(begin
					(d-DirectivePut-score-prefix tag (string-append "\\paper { page-count=" count "}"))
					(d-DirectivePut-score-display tag (string-append (if restrict layout "") (_ ": Page Count") count))
					(if restrict
						(d-DirectivePut-score-allow tag id))
					(d-SetSaved #f))
				(begin
					(if restrict
						(d-DirectiveDelete-score tag)
						(begin
							(d-DirectivePut-score-prefix tag "")
							(d-DirectivePut-score-display tag (_ "Optimal"))))
					(d-WarningDialog (_ "Optimal page count restored")))))
		(d-WarningDialog (_ "Cancelled"))))
