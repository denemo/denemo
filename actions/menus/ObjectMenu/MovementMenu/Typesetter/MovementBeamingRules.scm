;;;MovementBeamingRules
(let ((tag "MovementBeamingRules")(spec '()))
    (set! spec (assoc-set! spec 'delete d-DirectiveDelete-layout))
    (set! spec (assoc-set! spec 'get d-DirectiveGet-layout-data))
    (set! spec (assoc-set! spec 'put d-DirectivePut-layout-data))
    (set! spec (assoc-set! spec 'proc d-DirectivePut-layout-postfix))
    (set! spec (assoc-set! spec 'layout ""))
    (set! spec (assoc-set! spec 'tag tag))
    (d-SetBeamExceptions spec))

            
