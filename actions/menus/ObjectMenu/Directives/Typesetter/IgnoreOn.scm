;;;IgnoreOn
  (StandAloneDirectiveProto (cons "IgnoreOn" "\n%{\n") #f #f (_ "Start Ignore"))
  (if (not IgnoreOn::params)
    (d-ChooseCondition))
