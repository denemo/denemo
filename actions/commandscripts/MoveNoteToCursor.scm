;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
(define cursor (string-upcase (d-GetCursorNote)))
(define command (string-append "(d-ChangeTo" cursor ")"))
(eval-string command)