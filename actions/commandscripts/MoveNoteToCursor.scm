(define cursor (string-upcase (d-GetCursorNote)))
(define command (string-append "(d-ChangeTo" cursor ")"))
(eval-string command)