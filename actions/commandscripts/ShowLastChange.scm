;;;;ShowLastChange
(let ((last (d-GetLastChange)))
  (if last
    (d-InfoDialog (string-append "The last change was:\n" last))
    (d-InfoDialog "No change recorded in Undo stack")))
