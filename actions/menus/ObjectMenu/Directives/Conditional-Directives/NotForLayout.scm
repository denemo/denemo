;;;;;;; NotForLayout
 (let ((tag (d-DirectiveGetTag-standalone)) (params NotForLayout::params) (layout (d-GetLayoutName))(id (d-GetLayoutId)))
    (define (d-InfoDialog string)
        (Help::TimedNotice (string-append string "\n") 5000))
    (define (put-cond)
        (d-DirectivePut-standalone-ignore tag id)
        ;(d-DirectivePut-standalone-display tag  (string-append (_ "Not for ") layout))
        (d-DirectivePut-standalone-ty tag 60)
        (d-DirectivePut-standalone-tx tag -30))
  (if tag
    (let ((interactive #f))
        (if (pair? params)
            (begin
            (set! interactive (not (eq? (car params) 'noninteractive)))
                (if (not interactive)
                        (set! params (cdr params)))
             (set! layout (car params))
             (set! id (cdr params))))
        (put-cond)
        (if interactive
            (d-InfoDialog (string-append (_ "This Directive ") "\"" tag "\"" (_ " will not be typeset for the layout ") "\"" layout"\"")))
        (d-SetSaved #f)
        (d-RefreshDisplay))
    (d-MakeDirectiveConditional)))
