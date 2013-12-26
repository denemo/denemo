;;;ChangeTo3
(let ((shift #f))
    (if (Appending?)
        (begin
            (set! shift #t)
            (d-MoveCursorLeft)))
    (if (Music?)
        (d-Change3))
    (if shift
        (d-MoveCursorRight)))
