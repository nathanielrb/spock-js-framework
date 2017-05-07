;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; App

(define click1 (bind-click (status) '((status . "Loading..."))))

(register-callback "click1" click1)

(define map-render2
  (render (status messages)
	  (map (lambda (y) (<div> (<text> "status") (<text> y)))
	       messages)))

(define map-render
  (render (status)
    (<div> (<text> status))))

(register-callback "map-render" map-render)



;(node (messages)
 ;   (node (status)
  ;(print "b")
;  (send-var 'status-messages  (cons status messages))))

(init)

