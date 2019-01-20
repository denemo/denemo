;;;Swap Passages
(let ((continue #t))
    (define (merge-if-needed)
        (define underfull #f)
        (if (d-MoveToMeasureRight)
            (begin
                (set! underfull (UnderfullMeasure?))
                (d-MoveToMeasureLeft)))
         (if (or underfull (UnderfullMeasure?))
            (d-MergeWithNextMeasure)))
    (define (swap-passage)
        (define start-tick (d-GetStartTick))
        (define end-tick #f)
        (define end-measure #f)
        (define startpos (GetPosition))
        (d-MoveCursorRight)
        (d-SetMark)
        (while (and (not (end-passage?)) (d-CursorRight)))
        (if (end-passage?)
            (begin
                (d-CursorLeft)
                (set! end-measure (d-GetMeasure))
                (set! end-tick (d-GetEndTick))
                (d-Cut)
                (d-PushClipboard)
                (apply d-GoToPosition startpos)
                
                (if (d-MoveToStaffDown)
                    (let ((position #f))
                        (while (and (not (eq? start-tick (d-GetStartTick))) (d-MoveCursorRight)))
                        (d-UnsetMark)
                        (if (eq? start-tick (d-GetStartTick))
                            (let ()
                                (define (at-end?)
                                    (and (eq? end-measure (d-GetMeasure)) (eq? end-tick (d-GetEndTick))))
                                (set! position (GetPosition)) 
                                (d-SetMark)
                                (while (and (not (at-end?)) (d-CursorRight)))
                                (if (at-end?)
                                    (begin
                                        (d-Cut)
                                        (d-PushClipboard)
                                        (apply d-GoToPosition position)
                                        (d-PopClipboard 1)
                                        (d-Paste)
                                        (merge-if-needed)
                                        (apply d-GoToPosition startpos)
                                        (d-MoveCursorRight)
                                        (d-PopClipboard)
                                        (d-Paste)
                                        (merge-if-needed))
                                    (begin
                                        (set! continue #f)
                                        (d-Undo)
                                        (d-WarningDialog (_ "Unable to find corresponding passage end.")))))
                         (begin
                            (set! continue #f)
                            (d-Undo)
                            (d-WarningDialog (_ "Unable to find corresponding passage start.")))))
                   (d-WarningDialog (_ "No staff below to swap passages with."))))
            (begin
                (set! continue #f)
                (d-WarningDialog (_ "Start Passage with no End Passage.")))))

   (define (start-passage?)
        (d-Directive-standalone? "StartPassage"))
   (define (end-passage?)
        (d-Directive-standalone? "EndPassage"))
    ;;; procedure starts here
    (d-PushPosition)
    (d-MoveToBeginning)
    (let loop ()
        (if (start-passage?)
            (swap-passage)
            (if continue
                (while (d-MoveCursorRight)
                    (loop))))
        (if (and continue (d-MoveCursorRight))
            (loop)))
     (d-PopPosition))
    
