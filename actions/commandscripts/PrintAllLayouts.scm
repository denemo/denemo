;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;;;;;;;PrintAllLayouts
(if (not (defined? 'PrintAllLayouts::Finished))
  (define PrintAllLayouts::Finished #f))
  
(if PrintAllLayouts::Finished
  (begin
    (if (d-SelectNextCustomLayout)
      (begin
	(set! FinalizeTypesetting  (lambda ()  (set! FinalizeTypesetting DefaultFinalizeTypesetting)(d-PrintTypesetPDF)))	
	(d-TypesetForScript "(d-PrintView)"))
      (begin
	(set!  PrintAllLayouts::Finished #f)
	(d-SelectDefaultLayout)
	(set! FinalizePrint DefaultFinalizePrint))))
  (begin
    (if (d-SelectFirstCustomLayout)
      (begin
	(set! FinalizePrint (lambda ()  (set! PrintAllLayouts::Finished #t) (d-PrintAllLayouts)))
	(set! FinalizeTypesetting  (lambda ()  (set! FinalizeTypesetting DefaultFinalizeTypesetting)(d-PrintTypesetPDF)))	
	(d-TypesetForScript "(d-PrintView)")
	)
      (begin
	(d-WarningDialog (_ "No Layouts Available?"))))))
