;;SaveAsTemplate
(let ((warning #f))
    (d-GoToPosition 1 1 1 1)
    (if (not (EmptyMeasure?))
        (if (d-SetSaved)
            (let ((choice (RadioBoxMenu (cons (_ "Remove All Music?") 'remove) (cons (_ "Keep Music?") 'keep))))
                (case choice
                    ((remove)
                        (begin
                            (if (d-Directive-standalone? "DenemoLink")
                                (begin
                                    (d-SetMark)
                                    (d-Copy)))
                            (let loop () 
                                (d-DeleteFromCursorToEnd 'all)
                                (if (and (d-NextMovement) (d-GoToPosition #f 1 1 1))
                                    (loop)))
                            (d-GoToPosition 1 1 1 1)
                            (d-Paste)))))
            (set! warning (string-append (_ "Cancelled: ") (_ "Score is not saved")))))
    (if warning
        (d-WarningDialog warning)
        (d-SaveTemplate)))
