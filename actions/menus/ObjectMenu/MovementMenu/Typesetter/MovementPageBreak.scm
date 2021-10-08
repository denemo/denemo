;;; MovementPageBreak
(let ((tag "MovementPageBreak") 
    (params  MovementPageBreak::params)
    (Removed    (_ "Page Break Removed before Movement Title"))
    (Inserted  (_ "Page Break Inserted before Movement Title"))
    (TitledPiece "TitledPiece"))
    (define (put-break)
		(if (not (d-IsInteractive))
			(d-DirectiveDelete-movementcontrol tag)) ;;to remove any conditionality
        (d-DirectivePut-movementcontrol-override tag (logior DENEMO_OVERRIDE_GRAPHIC DENEMO_OVERRIDE_TAGEDIT))
        (d-DirectivePut-movementcontrol-display tag "Page Break before Movement")
        (d-DirectivePut-movementcontrol-prefix  tag "\n\\pageBreak\n")
        (if (d-IsInteractive)
			(begin
				(SetDirectiveConditional "movementcontrol"  tag)
				(TimedNotice Inserted)))        
        (d-DirectivePrioritizeTag-movementcontrol tag))
        
  (if (and (d-IsInteractive) (d-Directive-movementcontrol? tag))
            (let ((choice (RadioBoxMenu (cons (_ "Make Conditional") 'conditional) (cons (_ "Delete Page Break") 'delete))))
                (if (eq? choice 'delete)
                    (begin
                        (TimedNotice Removed)
                        (d-DirectiveDelete-movementcontrol tag))
                    (SetDirectiveConditional "movementcontrol"  tag)))     
            (put-break))
   (d-SetSaved #f))
