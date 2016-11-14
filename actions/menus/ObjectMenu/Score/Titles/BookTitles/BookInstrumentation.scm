;;BookInstrumentation
(if BookInstrumentation::params
    (BookTitles::Do "Instrumentation" "instrumentation" BookInstrumentation::params #f #f)
        (BookTitles::Do "Instrumentation" "instrumentation" "Full Score"  (_ "Give instrumentation for the default layout or blank out to delete: ") #f))
