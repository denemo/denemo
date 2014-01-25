;;;CheckAndPrint
(d-CheckScore 0)
(if CheckScore::return
    (d-InfoDialog (string-append (_ "You may need to fix this warning:\n\"") CheckScore::return (_ "\"\nbefore printing.")))
    (begin
        ;;;d-Typeset then PrintView ???
        (d-Print)))
