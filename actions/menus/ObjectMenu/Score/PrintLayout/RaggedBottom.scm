;;;RaggedBottom
(let ((tag "RaggedBottom"))
  (if (d-Directive-paper? tag)
    (d-DirectiveDelete-paper tag)
    (begin
      (d-DirectivePut-paper-postfix tag "\nragged-bottom = ##t")
      (d-DirectivePut-paper-override tag DENEMO_OVERRIDE_GRAPHIC)
    )
  )
  (d-SetSaved #f)
)