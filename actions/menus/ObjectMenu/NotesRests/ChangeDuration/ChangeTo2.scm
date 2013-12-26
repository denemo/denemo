;;;ChangeTo2
(let ((shift #f))
    (if (Appending?)
        (begin
            (set! shift #t)
            (d-MoveCursorLeft)))
    (if (Music?)
        (d-Change2))
    (if shift
        (d-MoveCursorRight)))
