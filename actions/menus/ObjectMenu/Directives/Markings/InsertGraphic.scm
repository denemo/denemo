;;;InsertGraphic
(let ((tag "InsertGraphic")(params InsertGraphic::params)(file "")(data #f)(x-offset "0")(y-offset "0")(scale "1"))
    (if (equal? params "edit")
        (set! params #f))
    (set! data (d-DirectiveGet-standalone-data tag))
    (if data
        (begin
            (set! data (eval-string data))
            (set! file (assq-ref data 'file))
            (set! scale (assq-ref data 'scale))
            (set! x-offset (assq-ref data 'x-offset))
            (set! y-offset (assq-ref data 'y-offset))))
            
    (if  params
        (begin
            (set! file (assq-ref params 'file))
            (set! scale (assq-ref params 'scale))
            (set! x-offset (assq-ref params 'x-offset))
            (set! y-offset (assq-ref params 'y-offset))))
    (if (not params)
        (set! file (d-GetUserInput "Graphic File" "Give File Name (eps)" file)))
    (if (not scale)
        (set! scale "1"))
    (if file
        (begin
            (set! data (assq-set! data 'file file))
            (set! data (assq-set! data 'scale scale))
            (if x-offset
                (set! data (assq-set! data 'x-offset x-offset)))
            (if y-offset
                (set! data (assq-set! data 'y-offset y-offset)))
            (d-Directive-standalone tag)
            (d-DirectivePut-standalone-data tag (format #f "'~s" data))
            (if (and x-offset y-offset)
                (d-DirectivePut-standalone-prefix tag (string-append "<>-\\tweak #'extra-offset #'(" x-offset " . " y-offset ")")))
            (d-DirectivePut-standalone-postfix tag (string-append  "^\\markup\\epsfile #X #" scale " #\"" file "\" "))
            (d-DirectivePut-standalone-graphic tag "\nG\nDenemo\n30")
            (d-DirectivePut-standalone-minpixels tag 30)
            (d-SetSaved #f)
            (d-RefreshDisplay))))