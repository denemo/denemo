;;;ChangeTo4
(let ((shift #f))
    (if (Appending?)
        (begin
            (set! shift #t)
            (d-MoveCursorLeft)))
    (if (Music?)
        (d-Change4))
    (if shift
        (d-MoveCursorRight)))
