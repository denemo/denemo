;;QuickLilyPondExport
(d-ExportMUDELA (let ((f (d-Open "query=filename"))) 
    (if f (substring f 0 (- (string-length f) 7))
        (d-WarningDialog  (_ "The score does not have a file name, so no file name for the output LilyPond file can be constructed. Save the score first.")))))
