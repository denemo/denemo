;;;PrintFirstPageNumber
(let ((tag "PrintFirstPageNumber"))
    (if (and (d-DirectiveGet-paper-data tag) (eval-string (d-DirectiveGet-paper-data tag)))
        (begin
            (d-DirectivePut-paper-data tag "#f")
             (d-DirectivePut-paper-postfix tag "\nprint-first-page-number = ##f")
             (d-InfoDialog (_ "No number will be printed on the first page")))
          (begin
             (d-DirectivePut-paper-data tag "#t")
             (d-DirectivePut-paper-postfix tag "\nprint-first-page-number = ##t")
             (d-InfoDialog (_ "Page number will be printed on the first page"))))
(d-SetSaved #f))
