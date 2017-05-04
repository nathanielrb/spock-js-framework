;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; App

(define-syntax-rule (render-click (var) body ...)
  (lambda (this)
    (node (var)
	  (set-click this
		     (callback (lambda (this)
				 (let ((vars (begin body ...)))
				   (send-vars vars))))))))

(define click-alert (render-click (x) (alert x)
				  '(status "Loaded" messages (1 2 3))))

(define click2 (render-click (y) (alert "going") '(x "go")))

(register-callback "click-alert" click-alert)

(register-callback "click2" click2)

(define single-render
  (render (status) (<div> (<text> status))))

(register-callback "single-render" single-render)

(define map-render
  (render (messages) (map (lambda (x) (<div> (<text> x))) messages)))

(register-callback "map-render" map-render)

(init)
