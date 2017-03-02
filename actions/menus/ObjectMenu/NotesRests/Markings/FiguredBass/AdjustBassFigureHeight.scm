;;;AdjustBassFigureHeight
(let ((tag "FBHeight1") (offset #f)(params AdjustBassFigureHeight::params))
    (if (and params (not (equal? params "edit")))
        (begin
            (set! tag (car AdjustBassFigureHeight::params))
            (set! offset (cdr AdjustBassFigureHeight::params))))
    (if (not offset)
        (begin
            (set! offset (d-DirectiveGet-note-display tag))
            (if (not offset)
                (set! offset 7.0))
            (set! offset (d-GetUserInput (_ "Figured Bass Height") (_ "Give height adjustment (unit = staff space): ") offset))))
    (if (and (string? offset) (string->number offset))
        (begin
            (d-DirectivePut-note-prefix tag (string-append "\\once \\override Staff.BassFigureAlignmentPositioning #'Y-offset = #'" offset " "))
            (d-DirectivePut-note-override tag DENEMO_ALT_OVERRIDE)
            (d-DirectivePut-note-display tag offset)
            (d-DirectivePut-note-ty tag -10))   
        (d-DirectiveDelete-note tag))   
    (d-RefreshDisplay)
    (d-SetSaved #f))
