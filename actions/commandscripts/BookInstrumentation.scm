;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;;BookInstrumentation
(if BookInstrumentation::params
 	(BookTitles::Do "Instrumentation" "instrumentation" BookInstrumentation::params #f)
        (BookTitles::Do "Instrumentation" "instrumentation" "Full Score"  (_ "Give instrumentation for the default layout or blank out to delete: ")))
