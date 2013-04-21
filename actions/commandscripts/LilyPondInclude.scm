;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;;;LilyPondInclude
(let ((include LilyPondInclude::params))
  (define (setinclude include)
    (define tag (string-append "LilyPondInclude:" include))
    (d-SetSaved #f)
    (d-DirectivePut-score-display tag include)
    (d-DirectivePut-score-override tag (logior DENEMO_OVERRIDE_AFFIX DENEMO_OVERRIDE_DYNAMIC))
    (d-DirectivePut-score-prefix tag (string-append "\\include \"" DENEMO_LILYPOND_DIR "/" include "\"\n")))
 ;;;;;start of procedure   
 
  (if include
      (setinclude include)
    (begin   
      (set! include (d-GetUserInput (_ "Include LilyPond File") (_ "Give LilyPond file name:") "book-titling.ily"));;;FIXME get list of possible files
      (if include
	(setinclude include)
	(d-WarningDialog (_ "LilyPondInclude unchanged"))))))