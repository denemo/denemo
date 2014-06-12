;;TocTitle
(d-LilyPondInclude "book-titling.ily")
(d-PushPosition)
(while (d-PreviousMovement))
(let ((tag "TableOfContents") (title "Contents") (pb 3) (mc  " \\markuplist \\table-of-contents\n"))
(if (d-Directive-paper? tag)
    (set! title (d-DirectiveGet-paper-display tag)))
(set! title (d-GetUserInput (_ "Table of Contents Title") (_ "Give title for the table of contents\nBlank to delete") title))
;(set! pb (d-GetUserInput (_ "Table of Contents Page Break Control") (_ "Give 0 - No page breaks\n1 - Page Break before Table of Contents\n2 - Page Break after\n3 - Both") pb))
(set! pb (RadioBoxMenu  
        (cons (_ "Page Break before Table of Contents") 'before)
        (cons (_ "Page Break after Table of Contents") 'after)
        (cons (_ "Page Break before and after") 'both)
        (cons (_ "No page breaks") #f)))
    (case pb
        ((before)
            (set! mc (string-append "\n\\pageBreak\n" mc)))
          ((after)
                (set! mc (string-append mc "\n\\pageBreak\n")))
           ((both)
                (set! mc (string-append  "\n\\pageBreak\n" mc "\\pageBreak\n"))))

(if (string-null? title)
    (begin
        (d-WarningDialog (_ "Deleted"))
        (d-DirectiveDelete-paper tag)
        (d-DirectiveDelete-movementcontrol tag))
    (begin
        (d-DirectivePut-paper-postfix tag (string-append "\ntocTitle = \"" title "\"\n"))
        (d-DirectivePut-paper-display tag title)
        (d-DirectivePut-movementcontrol-prefix tag mc)))
(d-SetSaved #f))
(d-PopPosition)
