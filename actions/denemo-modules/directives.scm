; Prototype to insert Lilypond Standalone Directives. Wants a pair with car Tag and cdr lilypond: (cons "BreathMark" "\\breathe")
(define* (StandAloneDirectiveProto pair #:optional (step? #t) (graphic #f) (displaytext #f) (minpixels #f) (data #f) (overrides #f))
    (d-Directive-standalone (car pair))
    (d-DirectivePut-standalone-postfix (car pair) (cdr pair))
    (d-DirectivePut-standalone-minpixels (car pair) 30)
    (if data
        (d-DirectivePut-standalone-data (car pair) data))
    (if graphic ;If the user specified a graphic use this, else greate a display text
        (begin (d-DirectivePut-standalone-graphic (car pair) graphic)
               (d-DirectivePut-standalone-override (car pair) DENEMO_OVERRIDE_GRAPHIC)))
    (if displaytext
          (d-DirectivePut-standalone-display (car pair) displaytext)
          (d-DirectivePut-standalone-display (car pair) (cdr pair)))
    (if minpixels
      (d-DirectivePut-standalone-minpixels (car pair) minpixels))
    (if overrides
      (d-DirectivePut-standalone-override (car pair) overrides))
    (if step?
        (d-MoveCursorRight))
    (d-RefreshDisplay))

(define (EditStandaloneDirective tag display)
 (define (EditLilyPond)
    (define current (d-DirectiveGet-standalone-postfix tag))
    (if tag
        (let ((text (d-GetUserInput (_ "Modifying the LilyPond Text") (_ "Give LilyPond syntax to be emitted by this Denemo Directive") current)))
            (if text
                (begin (d-DirectivePut-standalone-postfix tag text) 
                        (d-SetSaved #f))
                (d-InfoDialog (_ "Cancelled"))))
        (d-InfoDialog (_ "This directive currently creates no LilyPond via its postfix field, use the Advanced Edit instead"))))
 (define choice (RadioBoxMenu
          (cons (_ "Help ")   'help)   
          (cons (_ "Delete")   'delete)   
          (cons (_ "Edit LilyPond") 'lilypond)
          (cons (_ "Advanced") 'advanced)))
 (case choice
            ((delete) (d-DirectiveDelete-standalone tag))
            ((help) (d-InfoDialog (_ "This is a Directive object.\nDouble click on it to get information about it.")))
            ((lilypond) (EditLilyPond))
            ((advanced) (d-DirectiveTextEdit-standalone  tag))
            ((#f)  (d-InfoDialog (_ "Cancelled")))))
            
; Procedure to insert Self-Editing Lilypond Standalone Directives. Takes a pair with car Tag and cdr lilypond: (cons "BreathMark" "\\breathe") with optional boolean to step right after insertion and graphic
(define* (StandAloneSelfEditDirective pair #:optional (step? #t) (graphic #f) (displaytext #f) (minpixels #f) (overrides #f))
    (if (d-Directive-standalone? (car pair))
      (EditStandaloneDirective (car pair) displaytext)
      (StandAloneDirectiveProto pair step? graphic displaytext minpixels overrides)))

;Wrapper to attach any lilypond directive anywhere.
;;Wants four strings and an arbitrary number of tags (numbers) for overrides.
;;the tag parameter can be a single string or a pair. A single string is both the tag and display, a pair is (cons "tag" "display") 
(define (AttachDirective type field tag content . overrides)
    (define proc-put (string-append "d-DirectivePut-" type "-" field))
    ;(define proc-get (string-append "d-DirectiveGet-" type "-" field))
    ;(define proc-del (string-append "d-DirectiveDelete-" type))
    (define proc-dis (string-append "d-DirectivePut-" type "-display"))
    (define proc-ovr (string-append "d-DirectivePut-" type "-override"))
    (define dis #f)
    (if (pair? tag)
        (begin (set! dis (cdr tag)) (set! tag (car tag)))
        (set! dis tag))
    ((eval-string proc-put) tag content)
    (if (member DENEMO_OVERRIDE_GRAPHIC overrides) ; If DENEMO_OVERRIDE_GRAPHIC is there just go on
        ((eval-string proc-ovr) tag (apply logior overrides))
        (if (equal? type "staff") ; if not test if its a staff directive: we must enforce graphic to make sure staff-icons work.
            ((eval-string proc-ovr) tag (apply logior (append (list DENEMO_OVERRIDE_GRAPHIC) overrides)))
            ((eval-string proc-ovr) tag (apply logior overrides)))) ; not a staff, everythings ok without DENEMO_OVERRIDE_GRAPHIC
    ((eval-string proc-dis) tag dis)
    (d-SetSaved #f)
    #t)

(define (EditStaffDirective tag)        
    (let ((choice #f))
      (begin
        (set! choice (d-GetOption (string-append cue-Delete stop cue-Advanced stop)))
        (cond
         ((boolean? choice)
          (d-WarningDialog (_ "Operation cancelled")))
         ((equal? choice  cue-Advanced)
          (d-DirectiveTextEdit-staff tag))
         ((equal? choice  cue-Delete)
          (d-DirectiveDelete-staff tag))))))
        
    
    
; ToggleDirective is a script to help you by creating and deleting Denemo-Directives with the same command.
;; return value is #t if directive was created or #f if it was deleted. This can be used as hook for further scripting.
;; example (ToggleDirective "staff" "prefix" "Ambitus" "\\with { \\consists \"Ambitus_engraver\" }")
(define (ToggleDirective type field tag content . overrides) ; four strings and an arbitrary number of flags (numbers) for override field.
    (define proc-get (string-append "d-DirectiveGet-" type "-" field))
    (define proc-del (string-append "d-DirectiveDelete-" type))
    (define dis #f)
    (if (pair? tag)
        (begin (set! dis (cdr tag)) (set! tag (car tag)))
        (set! dis tag))     
    (if ((eval-string proc-get) tag)
        (begin  ((eval-string proc-del) tag)
                (d-SetSaved #f)
                #f)
        (apply AttachDirective type field (cons tag dis) content overrides)))

;;;;;;provides the editing options for a standalone directive that can only be toggled off/on.      
(define (EditForStandaloneToggle tag)
    (let ((choice (RadioBoxMenu 
         (cons (_ "Help") 'help)
         (cons (_ "Edit Similar") 'similar) 
         (cons (_ "Object Inspector") 'inspect) 
         (cons (_ "Delete") 'delete))))
            (case choice
                ((similar) (d-EditSimilar))
                ((help) (let ((thehelp (d-GetHelp tag)))
                                (if (string? thehelp)
                                    (d-InfoDialog thehelp #f)
                                    (d-InfoDialog (_ "No help!")))))
                ((inspect)
                   (d-DisplayCurrentObject))
                  ((delete)
                    (d-DirectiveDelete-standalone tag)))))
                      
; d-DirectivePut-standalone a convenience function for standalone directives
(define (d-DirectivePut-standalone tag)
  (d-DirectivePut-standalone-minpixels tag 0)
  (d-MoveCursorLeft))

(define (d-Directive-standalone tag)
  (if (not (d-Directive-standalone? tag))
      (d-DirectivePut-standalone tag)))

(define* (d-Directive-standalone?  #:optional (tag #f))
  (if (equal? tag #f)
      (string? (d-DirectiveGetForTag-standalone ""))
      (equal? tag (d-DirectiveGetForTag-standalone tag))))

(define (d-DirectiveGetTag-standalone)
  (d-DirectiveGetForTag-standalone ""))


; d-DirectivePut-score a convenience function for score directives
(define (d-DirectivePut-score tag)
  (d-DirectivePut-score-minpixels tag 0))

(define (d-Directive-score tag)
      (d-DirectivePut-score tag))


;;procedure for creating/updating a directive attached to the the score expects the parameters a test for the directive already present a procedure for putting the directive one for deleting it a tag then two fields for getting a new value a default value a proc for testing the validity of the value and information to give the user when the directive is deleted.
(define (ManageSystemDirective  params present? put-proc get-proc del-proc tag title prompt  value test  deletion-info)
    (if (number? params)
        (begin
            (put-proc (number->string params)))
        (begin
            (if (present? tag)
                (let ((choice (RadioBoxMenu (cons (_ "Help") 'help) (cons (_ "Edit") 'edit) (cons (_ "Delete") 'delete))))
                    (case choice
                        ((help)
                            (let ((thehelp (d-GetHelp tag)))
                                (set! params 'cancel)
                                (if (string? thehelp)
                                    (d-InfoDialog thehelp #f)
                                    (d-InfoDialog (_ "No help!")))))
                        ((delete)
                            (d-SetSaved #f)
                            (del-proc tag)
                            (set! params 'cancel)
                            (d-InfoDialog deletion-info #f)) ;#f => modal
                        ((edit)
                            (set! params (get-proc))
                            (if (test params)
                                (begin
                                    (set! value params)
                                    (set! params #f))
                                (set! params 'cancel))))))
                     (if (not (eq? params 'cancel))
                (let ((value (d-GetUserInput title prompt value)))
                    (if (test value)
                        (put-proc value)))))))

  

(define* (d-Directive-chord?  #:optional (tag #f))
  (if (equal? tag #f)
      (string? (d-DirectiveGetForTag-chord ""))
      (equal? tag (d-DirectiveGetForTag-chord tag))))
(define (d-DirectiveGetTag-chord)
  (d-DirectiveGetForTag-chord ""))

(define* (d-Directive-note?  #:optional (tag #f))
  (if (equal? tag #f)
      (string? (d-DirectiveGetForTag-note ""))
      (equal? tag (d-DirectiveGetForTag-note tag))))
(define (d-DirectiveGetTag-note)
  (d-DirectiveGetForTag-note ""))

(define* (d-Directive-score?  #:optional (tag #f))
  (if tag   
    (d-DirectiveGetForTag-score tag)
        (d-DirectiveGetForTag-score "")))

(define (d-DirectiveGetTag-score)
  (d-DirectiveGetForTag-score ""))

(define* (d-Directive-scoreheader?  #:optional (tag #f))
  (if (equal? tag #f)
      (string? (d-DirectiveGetForTag-scoreheader ""))
      (equal? tag (d-DirectiveGetForTag-scoreheader tag))))
(define (d-DirectiveGetTag-scoreheader)
  (d-DirectiveGetForTag-scoreheader ""))


(define* (d-Directive-movementcontrol?  #:optional (tag #f))
  (if (equal? tag #f)
      (string? (d-DirectiveGetForTag-movementcontrol ""))
      (equal? tag (d-DirectiveGetForTag-movementcontrol tag))))
(define (d-DirectiveGetTag-movementcontrol)
  (d-DirectiveGetForTag-movementcontrol ""))

(define* (d-Directive-header?  #:optional (tag #f))
  (if (equal? tag #f)
      (string? (d-DirectiveGetForTag-header ""))
      (equal? tag (d-DirectiveGetForTag-header tag))))
(define (d-DirectiveGetTag-header)
  (d-DirectiveGetForTag-header ""))


(define* (d-Directive-paper?  #:optional (tag #f))
  (if (equal? tag #f)
      (string? (d-DirectiveGetForTag-paper ""))
      (equal? tag (d-DirectiveGetForTag-paper tag))))
(define (d-DirectiveGetTag-paper)
  (d-DirectiveGetForTag-paper ""))


(define* (d-Directive-layout?  #:optional (tag #f))
  (if (equal? tag #f)
      (string? (d-DirectiveGetForTag-layout ""))
      (equal? tag (d-DirectiveGetForTag-layout tag))))
(define (d-DirectiveGetTag-layout)
  (d-DirectiveGetForTag-layout ""))


(define* (d-Directive-staff?  #:optional (tag #f))
  (if (equal? tag #f)
      (string? (d-DirectiveGetForTag-staff ""))
      (equal? tag (d-DirectiveGetForTag-staff tag))))
(define (d-DirectiveGetTag-staff)
  (d-DirectiveGetForTag-staff ""))


(define* (d-Directive-voice?  #:optional (tag #f))
  (if (equal? tag #f)
      (string? (d-DirectiveGetForTag-voice ""))
      (equal? tag (d-DirectiveGetForTag-voice tag))))
(define (d-DirectiveGetTag-voice)
  (d-DirectiveGetForTag-voice ""))


(define* (d-Directive-clef?  #:optional (tag #f))
  (if (equal? tag #f)
      (string? (d-DirectiveGetForTag-clef ""))
      (equal? tag (d-DirectiveGetForTag-clef tag))))
(define (d-DirectiveGetTag-clef)
  (d-DirectiveGetForTag-clef ""))


(define* (d-Directive-keysig?  #:optional (tag #f))
  (if (equal? tag #f)
      (string? (d-DirectiveGetForTag-keysig ""))
      (equal? tag (d-DirectiveGetForTag-keysig tag))))
(define (d-DirectiveGetTag-keysig)
  (d-DirectiveGetForTag-keysig ""))


(define* (d-Directive-timesig?  #:optional (tag #f))
  (if (equal? tag #f)
      (string? (d-DirectiveGetForTag-timesig ""))
      (equal? tag (d-DirectiveGetForTag-timesig tag))))
(define (d-DirectiveGetTag-timesig)
  (d-DirectiveGetForTag-timesig ""))
  
(define* (d-Directive-stemdirective?  #:optional (tag #f))
  (if (equal? tag #f)
      (string? (d-DirectiveGetForTag-stemdirective ""))
      (equal? tag (d-DirectiveGetForTag-stemdirective tag))))
(define (d-DirectiveGetTag-stemdirective)
  (d-DirectiveGetForTag-stemdirective ""))
  
   
(define (d-DirectiveDelete-standalone Tag)
    (if (equal? (d-DirectiveGetTag-standalone) Tag)
    (begin (d-DeleteObject) #t)
    #f))

(define* (ToggleChordDirective tag fontname lilypond #:optional (override #f) (display #f))
    (if (d-Directive-chord? tag)
          (d-DirectiveDelete-chord tag)
          (begin
           
            (if fontname
                (begin
                    (d-DirectivePut-chord-gx tag 7)
                    (d-DirectivePut-chord-graphic tag fontname)))
            (if display
                (d-DirectivePut-chord-display tag display))
            (d-DirectivePut-chord-postfix tag lilypond)
            (if override
          (d-DirectivePut-chord-override tag override))
           (d-SetSaved #f))))      
                                   
(define* (SetDirectiveConditional #:optional (directive #f) (type/tag #f))
    (define choice (if (equal? (d-StaffProperties "query=lily_name") (d-GetLayoutName))
                        (RadioBoxMenu
                            (cons (string-append (_ "Allow for Layout \"") (d-GetLayoutName) "\"") 'default)
                            (cons (string-append (_ "Ignore for Layout \"") (d-GetLayoutName) "\"") 'ignore-default)
                            (cons (_ "For all Layouts") 'all))
                        (RadioBoxMenu
                            (cons (string-append (_ "Allow for the Layout for Part \"") (d-StaffProperties "query=lily_name") "\"")   'only)   
                            (cons (string-append (_ "Ignore for the Layout for Part \"") (d-StaffProperties "query=lily_name") "\"")   'ignore-only)   
                            (cons (string-append (_ "Allow for Layout \"") (d-GetLayoutName) "\"") 'default)
                            (cons (string-append (_ "Ignore for Layout \"") (d-GetLayoutName) "\"") 'ignore-default)
                            (cons (_ "For all Layouts") 'all))))
                            
     (if type/tag 
            (case choice
                    ((ignore-only) ((eval-string (string-append "d-DirectivePut-" (car type/tag) "-ignore")) (cdr type/tag)  
                                    (d-GetCurrentStaffLayoutId))  (disp "Issued " (string-append "d-DirectivePut-" (car type/tag) "-ignore")   " with tag " (cdr type/tag) " and id " (d-GetCurrentStaffLayoutId)))
                    ((ignore-default) ((eval-string (string-append "d-DirectivePut-" (car type/tag) "-ignore")) (cdr type/tag)  
                                    (d-GetLayoutId))   (disp "Issued " (string-append "d-DirectivePut-" (car type/tag) "-ignore")   " with tag " (cdr type/tag) " and id " (d-GetLayoutId)) )
                    ((all) ((eval-string (string-append "d-DirectivePut-" (car type/tag) "-ignore")) (cdr type/tag)   0)
                            ((eval-string (string-append "d-DirectivePut-" (car type/tag) "-allow")) (cdr type/tag)   0))
                    ((default) ((eval-string (string-append "d-DirectivePut-" (car type/tag) "-allow")) (cdr type/tag) 
                            (d-GetLayoutId)))
                    ((only) ((eval-string (string-append "d-DirectivePut-" (car type/tag) "-allow")) (cdr type/tag) 
                            (d-GetCurrentStaffLayoutId))))
            (if (pair? directive)
                (case choice
                    ((ignore-only) (d-DirectiveNotForLayout (cons (cons (d-StaffProperties "query=lily_name") (d-GetCurrentStaffLayoutId)) directive))) 
                    ((ignore-default)  (d-DirectiveNotForLayout (cons (cons (d-GetLayoutName) (d-GetLayoutId))directive)))
                    ((all) (d-DirectiveForAllLayouts directive))
                    ((default) (d-DirectiveOnlyForLayout (cons (cons (d-GetLayoutName) (d-GetLayoutId)) directive)))
                    ((only) (d-DirectiveOnlyForLayout  (cons (cons (d-StaffProperties "query=lily_name") (d-GetCurrentStaffLayoutId)) directive))))
                (case choice
                    ((ignore-only) (d-NotForLayout (cons (d-StaffProperties "query=lily_name") (d-GetCurrentStaffLayoutId))))
                    ((ignore-default) (d-NotForLayout (cons (d-GetLayoutName) (d-GetLayoutId))))
                    ((all) (d-ForAllLayouts))
                    ((default) (d-OnlyForLayout (cons (d-GetLayoutName) (d-GetLayoutId))))
                    ((only) (d-OnlyForLayout (cons (d-StaffProperties "query=lily_name") (d-GetCurrentStaffLayoutId))))))))

;;; Toggle the DENEMO_OVERRIDE_HIDDEN override of the directive at the cursor
(define (ToggleHidden type tag) ;;; eg (ToggleHidden "note" "Fingering")
    (if (eval-string (string-append "(d-Directive-" type "? \"" tag "\")"))

        (let ((override (eval-string (string-append "(d-DirectiveGet-" type "-override " "\"" tag "\")"))))
            (eval-string (string-append "(d-DirectivePut-" type "-override " "\"" tag "\" " "(logxor DENEMO_OVERRIDE_HIDDEN " (number->string override) "))")))
        #f))

    
(define d-DirectiveGet-standalone-graphic d-DirectiveGet-standalone-graphic_name)            
(define d-DirectiveGet-chord-graphic d-DirectiveGet-chord-graphic_name)            
(define d-DirectiveGet-note-graphic d-DirectiveGet-note-graphic_name)            
(define d-DirectiveGet-clef-graphic d-DirectiveGet-clef-graphic_name)            
(define d-DirectiveGet-keysig-graphic d-DirectiveGet-keysig-graphic_name)            
(define d-DirectiveGet-timesig-graphic d-DirectiveGet-timesig-graphic_name)            
(define d-DirectiveGet-tuplet-graphic d-DirectiveGet-tuplet-graphic_name)            



