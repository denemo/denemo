;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
(let ()
(define lilytype (GetTypeAsLilypond))  
(define lilycontext  (GetContextAsLilypond))
(if lilytype
  (StandAloneDirectiveProto (cons "HideNext" (string-append  "\\once \\override " lilycontext "." lilytype " #'stencil = ##f"  )))
  #f
)
)