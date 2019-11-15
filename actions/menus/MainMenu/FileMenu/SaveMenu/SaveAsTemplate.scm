;;SaveAsTemplate
(let ((warning #f))
    (d-GoToPosition 1 1 1 1)
    (if (not (EmptyMeasure?))
        (if (d-GetSaved)
            (let ((choice (RadioBoxMenu (cons (_ "Remove All Music?") 'remove) (cons (_ "Keep Music?") 'keep))))
                (case choice
                    ((remove)
                        (begin
                            (if (d-Directive-standalone? "DenemoLink")
                                (begin
                                    (d-SetMark)
                                    (d-Copy))
                                (d-ClearClipboard))
                            (d-DirectiveDelete-scoreheader "ScoreIncipit")
                            (d-DeleteFromCursorToEnd 'all)
                            (let loop () 
                                (if (d-NextMovement)
                                    (begin 
                                    	(d-DeleteMovement)
                                    	(d-GoToPosition 1 1 1 1) 
                                    	 (loop))))
                            (d-Paste)))))
            (set! warning (string-append (_ "Cancelled: ") (_ "Score is not saved")))))
    (if warning
        (d-WarningDialog warning)
        (d-SaveTemplate)))
