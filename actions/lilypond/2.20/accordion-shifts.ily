
;;;Accordion 16v
(d-DirectivePut-score-prefix "Diskant und Punkt" "Diskant=\\markup {
  \\musicglyph #\"accordion.accDiscant\"
}
punkt = \\markup {
  \\musicglyph #\"accordion.accDot\"
}
accBasson = ^\\markup {
  \\combine
  \\Diskant
  \\raise #0.5 \\punkt
}
accPiccolo = ^\\markup {
    \\combine
       \\Diskant
       \\raise #2.5 \\punkt
}
accBandon = ^\\markup {
  \\combine
    \\Diskant
    \\combine
      \\raise #0.5 \\punkt
      \\raise #1.5 \\punkt
}
accVCello = ^\\markup {
  \\combine
    \\Diskant
    \\combine
      \\raise #0.5 \\punkt
      \\combine
        \\translate #'(-0.6 . 0) \\raise #1.5 \\punkt
        \\translate #'(0.6 . 0) \\raise #1.5 \\punkt
}
accHarmon = ^\\markup {
  \\combine
    \\Diskant
    \\combine
      \\raise #0.5 \\punkt
      \\combine
        \\raise #1.5 \\punkt
        \\raise #2.5 \\punkt
}
accOrgel = ^\\markup {
  \\combine
   \\Diskant
    \\combine
      \\raise #0.5 \\punkt
      \\raise #2.5 \\punkt
}
accHaupt = ^\\markup {
  \\combine
    \\Diskant
    \\combine
      \\raise #0.5 \\punkt
      \\combine
          \\translate #'(0.6 . 0) \\raise #1.5 \\punkt
          \\combine
            \\translate #'(-0.6 . 0) \\raise #1.5 \\punkt
            \\raise #2.5 \\punkt
}
accCelesta = ^\\markup {
  \\combine
    \\Diskant
    \\combine
       \\translate #'(0.6 . 0)  \\raise #1.5 \\punkt
      \\translate #'(-0.6 . 0) \\raise #1.5 \\punkt
}
accOboe = ^\\markup {
  \\combine
    \\Diskant
    \\combine
      \\raise #1.5 \\punkt
      \\raise #2.5 \\punkt
}
accClarin = ^\\markup {
  \\combine
    \\Diskant
    \\raise #1.5 \\punkt
}

accGeige = ^\\markup {
  \\combine
    \\Diskant
    \\combine
     \\translate #'(-0.6 . 0)  \\raise #1.5 \\punkt
      \\combine
        \\translate #'(0.6 . 0) \\raise #1.5 \\punkt
        \\raise #2.5 \\punkt
}
")
(d-DirectivePut-chord-postfix "Accordion16v" "\\accBasson")
(d-DirectivePut-chord-display "Accordion16v" "Accordion 16v")
(d-RefreshDisplay)
;;;End of scheme script


;;;Accordion Tremolo
(d-DirectivePut-score-prefix "Diskant und Punkt" "Diskant=\\markup {
  \\musicglyph #\"accordion.accDiscant\"
}
punkt = \\markup {
  \\musicglyph #\"accordion.accDot\"
}
accBasson = ^\\markup {
  \\combine
  \\Diskant
  \\raise #0.5 \\punkt
}
accPiccolo = ^\\markup {
    \\combine
       \\Diskant
       \\raise #2.5 \\punkt
}
accBandon = ^\\markup {
  \\combine
    \\Diskant
    \\combine
      \\raise #0.5 \\punkt
      \\raise #1.5 \\punkt
}
accVCello = ^\\markup {
  \\combine
    \\Diskant
    \\combine
      \\raise #0.5 \\punkt
      \\combine
        \\translate #'(-0.6 . 0) \\raise #1.5 \\punkt
        \\translate #'(0.6 . 0) \\raise #1.5 \\punkt
}
accHarmon = ^\\markup {
  \\combine
    \\Diskant
    \\combine
      \\raise #0.5 \\punkt
      \\combine
        \\raise #1.5 \\punkt
        \\raise #2.5 \\punkt
}
accOrgel = ^\\markup {
  \\combine
   \\Diskant
    \\combine
      \\raise #0.5 \\punkt
      \\raise #2.5 \\punkt
}
accHaupt = ^\\markup {
  \\combine
    \\Diskant
    \\combine
      \\raise #0.5 \\punkt
      \\combine
          \\translate #'(0.6 . 0) \\raise #1.5 \\punkt
          \\combine
            \\translate #'(-0.6 . 0) \\raise #1.5 \\punkt
            \\raise #2.5 \\punkt
}
accCelesta = ^\\markup {
  \\combine
    \\Diskant
    \\combine
       \\translate #'(0.6 . 0)  \\raise #1.5 \\punkt
      \\translate #'(-0.6 . 0) \\raise #1.5 \\punkt
}
accOboe = ^\\markup {
  \\combine
    \\Diskant
    \\combine
      \\raise #1.5 \\punkt
      \\raise #2.5 \\punkt
}
accClarin = ^\\markup {
  \\combine
    \\Diskant
    \\raise #1.5 \\punkt
}

accGeige = ^\\markup {
  \\combine
    \\Diskant
    \\combine
     \\translate #'(-0.6 . 0)  \\raise #1.5 \\punkt
      \\combine
        \\translate #'(0.6 . 0) \\raise #1.5 \\punkt
        \\raise #2.5 \\punkt
}
")
(d-DirectivePut-chord-postfix "AccordionTremolo" "\\accCelesta")
(d-DirectivePut-chord-display "AccordionTremolo" "Accordion Tremolo")
(d-RefreshDisplay)
;;;End of scheme script


;;;Accordion 4v
(d-DirectivePut-score-prefix "Diskant und Punkt" "Diskant=\\markup {
  \\musicglyph #\"accordion.accDiscant\"
}
punkt = \\markup {
  \\musicglyph #\"accordion.accDot\"
}
accBasson = ^\\markup {
  \\combine
  \\Diskant
  \\raise #0.5 \\punkt
}
accPiccolo = ^\\markup {
    \\combine
       \\Diskant
       \\raise #2.5 \\punkt
}
accBandon = ^\\markup {
  \\combine
    \\Diskant
    \\combine
      \\raise #0.5 \\punkt
      \\raise #1.5 \\punkt
}
accVCello = ^\\markup {
  \\combine
    \\Diskant
    \\combine
      \\raise #0.5 \\punkt
      \\combine
        \\translate #'(-0.6 . 0) \\raise #1.5 \\punkt
        \\translate #'(0.6 . 0) \\raise #1.5 \\punkt
}
accHarmon = ^\\markup {
  \\combine
    \\Diskant
    \\combine
      \\raise #0.5 \\punkt
      \\combine
        \\raise #1.5 \\punkt
        \\raise #2.5 \\punkt
}
accOrgel = ^\\markup {
  \\combine
   \\Diskant
    \\combine
      \\raise #0.5 \\punkt
      \\raise #2.5 \\punkt
}
accHaupt = ^\\markup {
  \\combine
    \\Diskant
    \\combine
      \\raise #0.5 \\punkt
      \\combine
          \\translate #'(0.6 . 0) \\raise #1.5 \\punkt
          \\combine
            \\translate #'(-0.6 . 0) \\raise #1.5 \\punkt
            \\raise #2.5 \\punkt
}
accCelesta = ^\\markup {
  \\combine
    \\Diskant
    \\combine
       \\translate #'(0.6 . 0)  \\raise #1.5 \\punkt
      \\translate #'(-0.6 . 0) \\raise #1.5 \\punkt
}
accOboe = ^\\markup {
  \\combine
    \\Diskant
    \\combine
      \\raise #1.5 \\punkt
      \\raise #2.5 \\punkt
}
accClarin = ^\\markup {
  \\combine
    \\Diskant
    \\raise #1.5 \\punkt
}

accGeige = ^\\markup {
  \\combine
    \\Diskant
    \\combine
     \\translate #'(-0.6 . 0)  \\raise #1.5 \\punkt
      \\combine
        \\translate #'(0.6 . 0) \\raise #1.5 \\punkt
        \\raise #2.5 \\punkt
}
")
(d-DirectivePut-chord-postfix "Accordion4v" "\\accPiccolo")
(d-DirectivePut-chord-display "Accordion4v" "Accordion 4v")
(d-RefreshDisplay)
;;;End of scheme script


;;;Accordion 4v8v
(d-DirectivePut-score-prefix "Diskant und Punkt" "Diskant=\\markup {
  \\musicglyph #\"accordion.accDiscant\"
}
punkt = \\markup {
  \\musicglyph #\"accordion.accDot\"
}
accBasson = ^\\markup {
  \\combine
  \\Diskant
  \\raise #0.5 \\punkt
}
accPiccolo = ^\\markup {
    \\combine
       \\Diskant
       \\raise #2.5 \\punkt
}
accBandon = ^\\markup {
  \\combine
    \\Diskant
    \\combine
      \\raise #0.5 \\punkt
      \\raise #1.5 \\punkt
}
accVCello = ^\\markup {
  \\combine
    \\Diskant
    \\combine
      \\raise #0.5 \\punkt
      \\combine
        \\translate #'(-0.6 . 0) \\raise #1.5 \\punkt
        \\translate #'(0.6 . 0) \\raise #1.5 \\punkt
}
accHarmon = ^\\markup {
  \\combine
    \\Diskant
    \\combine
      \\raise #0.5 \\punkt
      \\combine
        \\raise #1.5 \\punkt
        \\raise #2.5 \\punkt
}
accOrgel = ^\\markup {
  \\combine
   \\Diskant
    \\combine
      \\raise #0.5 \\punkt
      \\raise #2.5 \\punkt
}
accHaupt = ^\\markup {
  \\combine
    \\Diskant
    \\combine
      \\raise #0.5 \\punkt
      \\combine
          \\translate #'(0.6 . 0) \\raise #1.5 \\punkt
          \\combine
            \\translate #'(-0.6 . 0) \\raise #1.5 \\punkt
            \\raise #2.5 \\punkt
}
accCelesta = ^\\markup {
  \\combine
    \\Diskant
    \\combine
       \\translate #'(0.6 . 0)  \\raise #1.5 \\punkt
      \\translate #'(-0.6 . 0) \\raise #1.5 \\punkt
}
accOboe = ^\\markup {
  \\combine
    \\Diskant
    \\combine
      \\raise #1.5 \\punkt
      \\raise #2.5 \\punkt
}
accClarin = ^\\markup {
  \\combine
    \\Diskant
    \\raise #1.5 \\punkt
}

accGeige = ^\\markup {
  \\combine
    \\Diskant
    \\combine
     \\translate #'(-0.6 . 0)  \\raise #1.5 \\punkt
      \\combine
        \\translate #'(0.6 . 0) \\raise #1.5 \\punkt
        \\raise #2.5 \\punkt
}
")
(d-DirectivePut-chord-postfix "Accordion4v8v" "\\accOboe")
(d-DirectivePut-chord-display "Accordion4v8v" "Accordion 4v8v")
(d-RefreshDisplay)
;;;End of scheme script


;;;Accordion 4v8vTremolo
(d-DirectivePut-score-prefix "Diskant und Punkt" "Diskant=\\markup {
  \\musicglyph #\"accordion.accDiscant\"
}
punkt = \\markup {
  \\musicglyph #\"accordion.accDot\"
}
accBasson = ^\\markup {
  \\combine
  \\Diskant
  \\raise #0.5 \\punkt
}
accPiccolo = ^\\markup {
    \\combine
       \\Diskant
       \\raise #2.5 \\punkt
}
accBandon = ^\\markup {
  \\combine
    \\Diskant
    \\combine
      \\raise #0.5 \\punkt
      \\raise #1.5 \\punkt
}
accVCello = ^\\markup {
  \\combine
    \\Diskant
    \\combine
      \\raise #0.5 \\punkt
      \\combine
        \\translate #'(-0.6 . 0) \\raise #1.5 \\punkt
        \\translate #'(0.6 . 0) \\raise #1.5 \\punkt
}
accHarmon = ^\\markup {
  \\combine
    \\Diskant
    \\combine
      \\raise #0.5 \\punkt
      \\combine
        \\raise #1.5 \\punkt
        \\raise #2.5 \\punkt
}
accOrgel = ^\\markup {
  \\combine
   \\Diskant
    \\combine
      \\raise #0.5 \\punkt
      \\raise #2.5 \\punkt
}
accHaupt = ^\\markup {
  \\combine
    \\Diskant
    \\combine
      \\raise #0.5 \\punkt
      \\combine
          \\translate #'(0.6 . 0) \\raise #1.5 \\punkt
          \\combine
            \\translate #'(-0.6 . 0) \\raise #1.5 \\punkt
            \\raise #2.5 \\punkt
}
accCelesta = ^\\markup {
  \\combine
    \\Diskant
    \\combine
       \\translate #'(0.6 . 0)  \\raise #1.5 \\punkt
      \\translate #'(-0.6 . 0) \\raise #1.5 \\punkt
}
accOboe = ^\\markup {
  \\combine
    \\Diskant
    \\combine
      \\raise #1.5 \\punkt
      \\raise #2.5 \\punkt
}
accClarin = ^\\markup {
  \\combine
    \\Diskant
    \\raise #1.5 \\punkt
}

accGeige = ^\\markup {
  \\combine
    \\Diskant
    \\combine
     \\translate #'(-0.6 . 0)  \\raise #1.5 \\punkt
      \\combine
        \\translate #'(0.6 . 0) \\raise #1.5 \\punkt
        \\raise #2.5 \\punkt
}
")
(d-DirectivePut-chord-postfix "Accordion4v8vTremolo" "\\accGeige")
(d-DirectivePut-chord-display "Accordion4v8vTremolo" "Accordion 4v8vTremolo")
(d-RefreshDisplay)
;;;End of scheme script


;;;Accordion 4v8v16v
(d-DirectivePut-score-prefix "Diskant und Punkt" "Diskant=\\markup {
  \\musicglyph #\"accordion.accDiscant\"
}
punkt = \\markup {
  \\musicglyph #\"accordion.accDot\"
}
accBasson = ^\\markup {
  \\combine
  \\Diskant
  \\raise #0.5 \\punkt
}
accPiccolo = ^\\markup {
    \\combine
       \\Diskant
       \\raise #2.5 \\punkt
}
accBandon = ^\\markup {
  \\combine
    \\Diskant
    \\combine
      \\raise #0.5 \\punkt
      \\raise #1.5 \\punkt
}
accVCello = ^\\markup {
  \\combine
    \\Diskant
    \\combine
      \\raise #0.5 \\punkt
      \\combine
        \\translate #'(-0.6 . 0) \\raise #1.5 \\punkt
        \\translate #'(0.6 . 0) \\raise #1.5 \\punkt
}
accHarmon = ^\\markup {
  \\combine
    \\Diskant
    \\combine
      \\raise #0.5 \\punkt
      \\combine
        \\raise #1.5 \\punkt
        \\raise #2.5 \\punkt
}
accOrgel = ^\\markup {
  \\combine
   \\Diskant
    \\combine
      \\raise #0.5 \\punkt
      \\raise #2.5 \\punkt
}
accHaupt = ^\\markup {
  \\combine
    \\Diskant
    \\combine
      \\raise #0.5 \\punkt
      \\combine
          \\translate #'(0.6 . 0) \\raise #1.5 \\punkt
          \\combine
            \\translate #'(-0.6 . 0) \\raise #1.5 \\punkt
            \\raise #2.5 \\punkt
}
accCelesta = ^\\markup {
  \\combine
    \\Diskant
    \\combine
       \\translate #'(0.6 . 0)  \\raise #1.5 \\punkt
      \\translate #'(-0.6 . 0) \\raise #1.5 \\punkt
}
accOboe = ^\\markup {
  \\combine
    \\Diskant
    \\combine
      \\raise #1.5 \\punkt
      \\raise #2.5 \\punkt
}
accClarin = ^\\markup {
  \\combine
    \\Diskant
    \\raise #1.5 \\punkt
}

accGeige = ^\\markup {
  \\combine
    \\Diskant
    \\combine
     \\translate #'(-0.6 . 0)  \\raise #1.5 \\punkt
      \\combine
        \\translate #'(0.6 . 0) \\raise #1.5 \\punkt
        \\raise #2.5 \\punkt
}
")
(d-DirectivePut-chord-postfix "Accordion4v8v16v" "\\accHarmon")
(d-DirectivePut-chord-display "Accordion4v8v16v" "Accordion 4v8v16v")
(d-RefreshDisplay)
;;;End of scheme script


;;;Accordion 4v8v16vTremolo
(d-DirectivePut-score-prefix "Diskant und Punkt" "Diskant=\\markup {
  \\musicglyph #\"accordion.accDiscant\"
}
punkt = \\markup {
  \\musicglyph #\"accordion.accDot\"
}
accBasson = ^\\markup {
  \\combine
  \\Diskant
  \\raise #0.5 \\punkt
}
accPiccolo = ^\\markup {
    \\combine
       \\Diskant
       \\raise #2.5 \\punkt
}
accBandon = ^\\markup {
  \\combine
    \\Diskant
    \\combine
      \\raise #0.5 \\punkt
      \\raise #1.5 \\punkt
}
accVCello = ^\\markup {
  \\combine
    \\Diskant
    \\combine
      \\raise #0.5 \\punkt
      \\combine
        \\translate #'(-0.6 . 0) \\raise #1.5 \\punkt
        \\translate #'(0.6 . 0) \\raise #1.5 \\punkt
}
accHarmon = ^\\markup {
  \\combine
    \\Diskant
    \\combine
      \\raise #0.5 \\punkt
      \\combine
        \\raise #1.5 \\punkt
        \\raise #2.5 \\punkt
}
accOrgel = ^\\markup {
  \\combine
   \\Diskant
    \\combine
      \\raise #0.5 \\punkt
      \\raise #2.5 \\punkt
}
accHaupt = ^\\markup {
  \\combine
    \\Diskant
    \\combine
      \\raise #0.5 \\punkt
      \\combine
          \\translate #'(0.6 . 0) \\raise #1.5 \\punkt
          \\combine
            \\translate #'(-0.6 . 0) \\raise #1.5 \\punkt
            \\raise #2.5 \\punkt
}
accCelesta = ^\\markup {
  \\combine
    \\Diskant
    \\combine
       \\translate #'(0.6 . 0)  \\raise #1.5 \\punkt
      \\translate #'(-0.6 . 0) \\raise #1.5 \\punkt
}
accOboe = ^\\markup {
  \\combine
    \\Diskant
    \\combine
      \\raise #1.5 \\punkt
      \\raise #2.5 \\punkt
}
accClarin = ^\\markup {
  \\combine
    \\Diskant
    \\raise #1.5 \\punkt
}

accGeige = ^\\markup {
  \\combine
    \\Diskant
    \\combine
     \\translate #'(-0.6 . 0)  \\raise #1.5 \\punkt
      \\combine
        \\translate #'(0.6 . 0) \\raise #1.5 \\punkt
        \\raise #2.5 \\punkt
}
")
(d-DirectivePut-chord-postfix "Accordion4v8v16vTremolo" "\\accHaupt")
(d-DirectivePut-chord-display "Accordion4v8v16vTremolo" "Accordion 4v8v16vTremolo")
(d-RefreshDisplay)
;;;End of scheme script


;;;Accordion 8v16vTremolo
(d-DirectivePut-score-prefix "Diskant und Punkt" "Diskant=\\markup {
  \\musicglyph #\"accordion.accDiscant\"
}
punkt = \\markup {
  \\musicglyph #\"accordion.accDot\"
}
accBasson = ^\\markup {
  \\combine
  \\Diskant
  \\raise #0.5 \\punkt
}
accPiccolo = ^\\markup {
    \\combine
       \\Diskant
       \\raise #2.5 \\punkt
}
accBandon = ^\\markup {
  \\combine
    \\Diskant
    \\combine
      \\raise #0.5 \\punkt
      \\raise #1.5 \\punkt
}
accVCello = ^\\markup {
  \\combine
    \\Diskant
    \\combine
      \\raise #0.5 \\punkt
      \\combine
        \\translate #'(-0.6 . 0) \\raise #1.5 \\punkt
        \\translate #'(0.6 . 0) \\raise #1.5 \\punkt
}
accHarmon = ^\\markup {
  \\combine
    \\Diskant
    \\combine
      \\raise #0.5 \\punkt
      \\combine
        \\raise #1.5 \\punkt
        \\raise #2.5 \\punkt
}
accOrgel = ^\\markup {
  \\combine
   \\Diskant
    \\combine
      \\raise #0.5 \\punkt
      \\raise #2.5 \\punkt
}
accHaupt = ^\\markup {
  \\combine
    \\Diskant
    \\combine
      \\raise #0.5 \\punkt
      \\combine
          \\translate #'(0.6 . 0) \\raise #1.5 \\punkt
          \\combine
            \\translate #'(-0.6 . 0) \\raise #1.5 \\punkt
            \\raise #2.5 \\punkt
}
accCelesta = ^\\markup {
  \\combine
    \\Diskant
    \\combine
       \\translate #'(0.6 . 0)  \\raise #1.5 \\punkt
      \\translate #'(-0.6 . 0) \\raise #1.5 \\punkt
}
accOboe = ^\\markup {
  \\combine
    \\Diskant
    \\combine
      \\raise #1.5 \\punkt
      \\raise #2.5 \\punkt
}
accClarin = ^\\markup {
  \\combine
    \\Diskant
    \\raise #1.5 \\punkt
}

accGeige = ^\\markup {
  \\combine
    \\Diskant
    \\combine
     \\translate #'(-0.6 . 0)  \\raise #1.5 \\punkt
      \\combine
        \\translate #'(0.6 . 0) \\raise #1.5 \\punkt
        \\raise #2.5 \\punkt
}
")
(d-DirectivePut-chord-postfix "Accordion8v16vTremolo" "\\accVCello")
(d-DirectivePut-chord-display "Accordion8v16vTremolo" "Accordion 8v16vTremolo")
(d-RefreshDisplay)
;;;End of scheme script


;;;Accordion 8v
(d-DirectivePut-score-prefix "Diskant und Punkt" "Diskant=\\markup {
  \\musicglyph #\"accordion.accDiscant\"
}
punkt = \\markup {
  \\musicglyph #\"accordion.accDot\"
}
accBasson = ^\\markup {
  \\combine
  \\Diskant
  \\raise #0.5 \\punkt
}
accPiccolo = ^\\markup {
    \\combine
       \\Diskant
       \\raise #2.5 \\punkt
}
accBandon = ^\\markup {
  \\combine
    \\Diskant
    \\combine
      \\raise #0.5 \\punkt
      \\raise #1.5 \\punkt
}
accVCello = ^\\markup {
  \\combine
    \\Diskant
    \\combine
      \\raise #0.5 \\punkt
      \\combine
        \\translate #'(-0.6 . 0) \\raise #1.5 \\punkt
        \\translate #'(0.6 . 0) \\raise #1.5 \\punkt
}
accHarmon = ^\\markup {
  \\combine
    \\Diskant
    \\combine
      \\raise #0.5 \\punkt
      \\combine
        \\raise #1.5 \\punkt
        \\raise #2.5 \\punkt
}
accOrgel = ^\\markup {
  \\combine
   \\Diskant
    \\combine
      \\raise #0.5 \\punkt
      \\raise #2.5 \\punkt
}
accHaupt = ^\\markup {
  \\combine
    \\Diskant
    \\combine
      \\raise #0.5 \\punkt
      \\combine
          \\translate #'(0.6 . 0) \\raise #1.5 \\punkt
          \\combine
            \\translate #'(-0.6 . 0) \\raise #1.5 \\punkt
            \\raise #2.5 \\punkt
}
accCelesta = ^\\markup {
  \\combine
    \\Diskant
    \\combine
       \\translate #'(0.6 . 0)  \\raise #1.5 \\punkt
      \\translate #'(-0.6 . 0) \\raise #1.5 \\punkt
}
accOboe = ^\\markup {
  \\combine
    \\Diskant
    \\combine
      \\raise #1.5 \\punkt
      \\raise #2.5 \\punkt
}
accClarin = ^\\markup {
  \\combine
    \\Diskant
    \\raise #1.5 \\punkt
}

accGeige = ^\\markup {
  \\combine
    \\Diskant
    \\combine
     \\translate #'(-0.6 . 0)  \\raise #1.5 \\punkt
      \\combine
        \\translate #'(0.6 . 0) \\raise #1.5 \\punkt
        \\raise #2.5 \\punkt
}
")
(d-DirectivePut-chord-postfix "Accordion8v" "\\accClarin")
(d-DirectivePut-chord-display "Accordion8v" "Accordion 8v")
(d-RefreshDisplay)
;;;End of scheme script


;;;Accordion 8v16v
(d-DirectivePut-score-prefix "Diskant und Punkt" "Diskant=\\markup {
  \\musicglyph #\"accordion.accDiscant\"
}
punkt = \\markup {
  \\musicglyph #\"accordion.accDot\"
}
accBasson = ^\\markup {
  \\combine
  \\Diskant
  \\raise #0.5 \\punkt
}
accPiccolo = ^\\markup {
    \\combine
       \\Diskant
       \\raise #2.5 \\punkt
}
accBandon = ^\\markup {
  \\combine
    \\Diskant
    \\combine
      \\raise #0.5 \\punkt
      \\raise #1.5 \\punkt
}
accVCello = ^\\markup {
  \\combine
    \\Diskant
    \\combine
      \\raise #0.5 \\punkt
      \\combine
        \\translate #'(-0.6 . 0) \\raise #1.5 \\punkt
        \\translate #'(0.6 . 0) \\raise #1.5 \\punkt
}
accHarmon = ^\\markup {
  \\combine
    \\Diskant
    \\combine
      \\raise #0.5 \\punkt
      \\combine
        \\raise #1.5 \\punkt
        \\raise #2.5 \\punkt
}
accOrgel = ^\\markup {
  \\combine
   \\Diskant
    \\combine
      \\raise #0.5 \\punkt
      \\raise #2.5 \\punkt
}
accHaupt = ^\\markup {
  \\combine
    \\Diskant
    \\combine
      \\raise #0.5 \\punkt
      \\combine
          \\translate #'(0.6 . 0) \\raise #1.5 \\punkt
          \\combine
            \\translate #'(-0.6 . 0) \\raise #1.5 \\punkt
            \\raise #2.5 \\punkt
}
accCelesta = ^\\markup {
  \\combine
    \\Diskant
    \\combine
       \\translate #'(0.6 . 0)  \\raise #1.5 \\punkt
      \\translate #'(-0.6 . 0) \\raise #1.5 \\punkt
}
accOboe = ^\\markup {
  \\combine
    \\Diskant
    \\combine
      \\raise #1.5 \\punkt
      \\raise #2.5 \\punkt
}
accClarin = ^\\markup {
  \\combine
    \\Diskant
    \\raise #1.5 \\punkt
}

accGeige = ^\\markup {
  \\combine
    \\Diskant
    \\combine
     \\translate #'(-0.6 . 0)  \\raise #1.5 \\punkt
      \\combine
        \\translate #'(0.6 . 0) \\raise #1.5 \\punkt
        \\raise #2.5 \\punkt
}
")
(d-DirectivePut-chord-postfix "Accordion8v16v" "\\accBandon")
(d-DirectivePut-chord-display "Accordion8v16v" "Accordion 8v16v")
(d-RefreshDisplay)
;;;End of scheme script


;;;Accordion 4v16v
(d-DirectivePut-score-prefix "Diskant und Punkt" "Diskant=\\markup {
  \\musicglyph #\"accordion.accDiscant\"
}
punkt = \\markup {
  \\musicglyph #\"accordion.accDot\"
}
accBasson = ^\\markup {
  \\combine
  \\Diskant
  \\raise #0.5 \\punkt
}
accPiccolo = ^\\markup {
    \\combine
       \\Diskant
       \\raise #2.5 \\punkt
}
accBandon = ^\\markup {
  \\combine
    \\Diskant
    \\combine
      \\raise #0.5 \\punkt
      \\raise #1.5 \\punkt
}
accVCello = ^\\markup {
  \\combine
    \\Diskant
    \\combine
      \\raise #0.5 \\punkt
      \\combine
        \\translate #'(-0.6 . 0) \\raise #1.5 \\punkt
        \\translate #'(0.6 . 0) \\raise #1.5 \\punkt
}
accHarmon = ^\\markup {
  \\combine
    \\Diskant
    \\combine
      \\raise #0.5 \\punkt
      \\combine
        \\raise #1.5 \\punkt
        \\raise #2.5 \\punkt
}
accOrgel = ^\\markup {
  \\combine
   \\Diskant
    \\combine
      \\raise #0.5 \\punkt
      \\raise #2.5 \\punkt
}
accHaupt = ^\\markup {
  \\combine
    \\Diskant
    \\combine
      \\raise #0.5 \\punkt
      \\combine
          \\translate #'(0.6 . 0) \\raise #1.5 \\punkt
          \\combine
            \\translate #'(-0.6 . 0) \\raise #1.5 \\punkt
            \\raise #2.5 \\punkt
}
accCelesta = ^\\markup {
  \\combine
    \\Diskant
    \\combine
       \\translate #'(0.6 . 0)  \\raise #1.5 \\punkt
      \\translate #'(-0.6 . 0) \\raise #1.5 \\punkt
}
accOboe = ^\\markup {
  \\combine
    \\Diskant
    \\combine
      \\raise #1.5 \\punkt
      \\raise #2.5 \\punkt
}
accClarin = ^\\markup {
  \\combine
    \\Diskant
    \\raise #1.5 \\punkt
}

accGeige = ^\\markup {
  \\combine
    \\Diskant
    \\combine
     \\translate #'(-0.6 . 0)  \\raise #1.5 \\punkt
      \\combine
        \\translate #'(0.6 . 0) \\raise #1.5 \\punkt
        \\raise #2.5 \\punkt
}
")
(d-DirectivePut-chord-postfix "Accordion4v16v" "\\accOrgel")
(d-DirectivePut-chord-display "Accordion4v16v" "Accordion 4v16v")
(d-RefreshDisplay)
;;;End of scheme script

