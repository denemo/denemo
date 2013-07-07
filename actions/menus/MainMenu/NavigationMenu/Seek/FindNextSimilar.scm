;;FindNextSimilar
(let  ()
  (define target (d-DirectiveGetTag-standalone))
  (if target
    (begin  
      (FindNextObjectAllColumns (lambda () (d-Directive-standalone? target))))
    (begin
    	#f)))