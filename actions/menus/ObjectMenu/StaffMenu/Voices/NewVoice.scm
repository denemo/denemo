;;NewVoice
(let ((name (d-StaffProperties "query=denemo_name")))
    (d-NewStructuredStaff 'voice)
    (d-StaffProperties (string-append "denemo_name=" name)))
