;;BookInstrumentation
(let ((edit (d-Directive-scoreheader? "BookInstrumentation")))
    (if BookInstrumentation::params
    (BookTitles::Do "Instrumentation" "instrumentation" BookInstrumentation::params #f edit)
        (BookTitles::Do "Instrumentation" "instrumentation" "Full Score"  (_ "Give instrumentation for the default layout or blank out to delete: ") edit)))
