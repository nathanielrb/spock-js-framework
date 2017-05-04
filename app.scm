;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; App

(define click0 (render-click (x) '(messages (1 2 3))))

(register-callback "click0" click0)

(define click0b (render-click (x) '(messages (2 3 4))))

(register-callback "click0b" click0b)

(define click1 (render-click (x) '(status "Loaded")))

(register-callback "click1" click1)

(define click2 (render-click (y) '(status "loading...")))

(register-callback "click2" click2)

(define single-render
  (render (status) (<div> (<text> status))))

(register-callback "single-render" single-render)

(define map-render
  (render (status messages) (map (lambda (y) (<div> (<text> status) (<text> y))) messages)))

(define map-render2
  (render (status-messages) (map (lambda (y) (<div> (<text> y))) status-messages)))

(register-callback "map-render" map-render)
(register-callback "map-render2" map-render2)

(node (status)
    (node (messages)
    (print "a")
	    (send-var 'status-messages (cons status messages))))

;(node (messages)
 ;   (node (status)
  ;(print "b")
;  (send-var 'status-messages  (cons status messages))))

(init)

