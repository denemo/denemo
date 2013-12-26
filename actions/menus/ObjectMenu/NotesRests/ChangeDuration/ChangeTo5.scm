;;;ChangeTo5
(let ((shift #f))
    (if (Appending?)
        (begin
            (set! shift #t)
            (d-MoveCursorLeft)))
    (if (Music?)
        (d-Change5))
    (if shift
        (d-MoveCursorRight)))
