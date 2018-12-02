;;FilterByUserCondition
(let ((str "")(condition #f)(tag "IndexEntry")(list-of-entries '()) (thefile #f) (transpose #f) (title #f) (composer #f) (comment #f) (incipit #f) (instruments '())(startdir ""))

    (define (indexTest condn) 
                
            (if (string-contains condn "title")
                (set! condn (regexp-substitute/global #f "title" condn
                   'pre (string-append "\"" title "\"") 'post))) 
            (if (string-contains condn "composer")     
                (set! condn (regexp-substitute/global #f "composer" condn
                   'pre (string-append "\"" composer "\"") 'post)))
            (if (string-contains condn "comment")     
                (set! condn (regexp-substitute/global #f "comment" condn
                   'pre (string-append "\"" comment "\"") 'post)))
            (if (string-contains condn "instruments")     
                (set! condn (regexp-substitute/global #f "instruments" condn
                   'pre (format #f "'~s" instruments) 'post)))
            ;(if (string-contains condn "incipit")     
                ;(set! condn (regexp-substitute/global #f (string-match "incipit" condn)
                   ;'pre (string-append "\"" (scheme-escape incipit) "\"") 'post)))
            (if (string-contains condn "filename")     
                (set! condn (regexp-substitute/global #f "filename" condn
                   'pre (string-append "\"" thefile "\"") 'post)))
            ;(if (string-contains condn "transpose")     
                ;(set! condn (regexp-substitute/global #f (string-match "transpose" condn)
                   ;'pre (string-append "\"" transpose "\"") 'post)))

            ;(disp "Have condn " condn "\n\n")    
          (eval-string condn))
                   

    (define (create-lilypond data)
        (if data
            (begin
                (set! thefile (assq-ref data 'thefile))
                (set! transpose (assq-ref data 'transpose))
                (set! title (assq-ref data 'title))
                (set! composer (assq-ref data 'composer))
                (set! comment (assq-ref data 'comment))
                (set! incipit (assq-ref data 'incipit))
                (set! instruments (assq-ref data 'instruments))
                (if (indexTest condition)
                    (begin
                        (set! instruments (string-join instruments ", "))
                        (set! str (string-append str
                            "\\noPageBreak\\markup \"" composer ": " title "\"\n"
                            "\\noPageBreak\\markup {instrumentation:"  instruments "}\n"
                            (if (string-null? comment) "" (string-append  "\\noPageBreak\\markup\\bold\\italic {\"Comment:" comment "\"}\n"))                            
                            transpose "\n"
                            incipit "\n\\noPageBreak\\incipit\n"
                             "\\noPageBreak\\markup {\\with-url #'\"scheme:(d-OpenNewWindow \\\"" thefile "\\\")\" \"Filename: ." (substring thefile (string-prefix-length startdir thefile)) "\"}\n"
                            "\\markup {\\column {\\draw-hline}}")))
                    (delq! data DenemoIndexEntries)))))

;;;;actual procedure        
   (let ((data (d-DirectiveGet-movementcontrol-data tag)))
        (if data
           (begin
              (set! DenemoIndexEntries (cons #f (eval-string data))) 
              (set! condition (d-GetUserInput (_ "Index Filter")
               (_ "Give condition to filter on in Scheme syntax\nvariables are the strings\nfilename composer title instruments") "(and (string-contains title \"Sonata\")(= (length instruments) 2))"))
              (if condition
                  (begin
                    (set! startdir (d-DirectiveGet-movementcontrol-data (string-append tag "StartDir")))
                    (if (not startdir)
                        (set! startdir ""))
                    (d-SetSaved #f)
                    (map  create-lilypond (cdr DenemoIndexEntries))
                    (set! DenemoIndexEntries (cdr DenemoIndexEntries)) 
                    (d-DirectivePut-movementcontrol-postfix tag (string-append "\\markup \\bold\\center-column{\\line{Filtered by "
                            condition 
                            "}}\\markup {\\column {\\draw-hline}} \\markup {\\center-column {\\vspace #2 }}\n\n"
                            str
                            "\n\\noPageBreak\\markup {\\column {\\draw-hline}}\\noPageBreak\\markup {\\center-column {\\vspace #2 }}\\noPageBreak\\markup\\huge{"
                             (_ "End of Index. Number of entries ") (number->string (length DenemoIndexEntries)) ".}"))
                    (d-DirectivePut-movementcontrol-data tag (format #f "'~s" DenemoIndexEntries))
                    (d-SetSaved #f))
                    (d-WarningDialog (_ "Cancelled"))))
            (d-WarningDialog (_ "Create index first")))))
