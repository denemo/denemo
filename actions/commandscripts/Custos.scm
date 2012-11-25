;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;;;Custos
(let ((tag "Custos"))
 	(if (d-Directive-layout? tag)
 		(begin
 			(d-DirectiveDelete-layout tag)
 			(d-InfoDialog "Custos now turned off"))
 		(begin
 		(d-DirectivePut-layout-postfix tag 
" \\context {
      \\Staff
      \\consists \"Custos_engraver\"
      \\override Custos #'style = #'mensural
    }"))))