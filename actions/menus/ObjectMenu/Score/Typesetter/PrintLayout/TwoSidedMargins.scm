;;;TwoSidedMargins
(let ((tag "TwoSidedMargins"))
    (if (d-Directive-paper? tag)
        (begin
            (d-DirectiveDelete-paper tag)
            (d-InfoDialog (_ "Single-sided margins will be used")))
             (d-DirectivePut-paper-postfix tag "
    two-sided = ##t
          "))
(d-SetSaved #f))
