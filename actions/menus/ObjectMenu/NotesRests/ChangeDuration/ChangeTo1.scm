;;;ChangeTo1
(let ((shift #f))
    (if (Appending?)
        (begin
            (set! shift #t)
            (d-MoveCursorLeft)))
    (if (Music?)
        (d-Change1))
    (if shift
        (d-MoveCursorRight)))
