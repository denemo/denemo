;NoteSpacing
(let* ((tag "NoteSpacing") (layout (d-GetLayoutName))(id (d-GetLayoutId))
		(restrict (RadioBoxMenu  (cons (string-append (_ "Restrict to layout: " ) layout) layout) (cons (_ "For any Layout") 'all))))
	(if restrict
		(begin
			(if (eq? restrict 'all)
				(set! restrict #f))
			(if restrict
				(begin
					(if (not (d-Directive-layout? tag))
						(d-DirectivePut-layout-display tag (_ "Default"))) ;;ensure the default comes before any conditional
					(set! tag (string-append tag "\n" restrict))))
			(if (d-DirectiveGet-layout-postfix tag)
				(begin
					(if restrict
						(d-DirectiveDelete-layout tag)
						(begin
							(d-DirectivePut-layout-display tag (_ "Default"))
							(d-DirectivePut-layout-postfix tag "")))
					(d-WarningDialog (string-append (if restrict layout "")
							(if restrict (_ ": This movement will have default horizontal note spacing for this layout")
										 (_ " This movement will have default horizontal note spacing")))))
				(begin
					(if restrict 
						(d-DirectivePut-layout-display tag (string-append "For Layout: " restrict))
						(d-DirectivePut-layout-display tag (_ "Custom")))
					(d-DirectivePut-layout-postfix tag (string-append " \\override Score.SpacingSpanner.common-shortest-duration = #(ly:make-moment "
						(DenemoGetDuration (_ "Choose basis for horizontal spacing ( 𝅝    is tightest, 𝅘𝅥𝅲    is loosest)"))
						") "))
					(if restrict
						(d-DirectivePut-layout-allow tag id)))))
		(d-WarningDialog (_ "Cancelled"))))
