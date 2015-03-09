;;;CheckDirectivePairs
(define-once CheckScore::error-position #f)
(define CheckDirectivePairs::return #f)
(d-PushPosition)
(d-MoveToBeginning)
(let ((last-start #f)
        (InCue #f))
    (let loop () 
        (let ((tag (d-DirectiveGetForTag-standalone)))
            (if (equal? tag "InstallCue")
                (begin
                    (set! last-start (GetPosition))
                    (if (equal? (d-DirectiveGet-standalone-postfix tag) "}")
                        (if InCue
                            (set! InCue #f)
                            (begin
                                (set! CheckDirectivePairs::return (_ "Bad End Cue Marker"))
                                (set! CheckScore::error-position (GetPosition))))
                        (if InCue
                            (begin
                                (set! CheckDirectivePairs::return (_ "Bad Start Cue Marker"))
                                (set! CheckScore::error-position (GetPosition)))
                            (set! InCue #t)))))
                ;;; other paired tags here
                
                
            (if (and (not CheckDirectivePairs::return) (d-NextStandaloneDirective))
                    (loop))))
     (if InCue
        (begin
            (set! CheckScore::error-position last-start)
            (set! CheckDirectivePairs::return (_ "Start Cue without End Cue Marker")))))
                
(d-PopPosition)                
(if (and (not CheckDirectivePairs::params) CheckDirectivePairs::return)
    (begin
        (apply d-GoToPosition CheckScore::error-position)
        (d-WarningDialog CheckDirectivePairs::return)))