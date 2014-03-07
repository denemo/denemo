;;DeleteSlur
(let ()
    (define (delete-from-end)
        (while (d-IsSlurEnd) (d-ReduceSlur))
        (TimedNotice (_ "Slur Deleted")))
    (define (find-slur-end)
        (let loop ()
            (if (d-IsSlurStart)
                (begin
                    (TimedNotice (_ "Not on a slur"))
                    #f)
                (if (d-IsSlurEnd)
                    #t
                    (if (d-MoveCursorRight)
                        (loop)
                        (begin
                            (TimedNotice (_ "Not on a slur"))
                            #f))))))
    (if (and (d-IsSlurStart) (d-MoveCursorRight) (find-slur-end))
        (delete-from-end)
        (if (find-slur-end)
            (delete-from-end))))
        