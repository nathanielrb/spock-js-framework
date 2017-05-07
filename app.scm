;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; App

(define click1 (bind-click () '((status . "Loading..."))))

(register-callback "click1" click1)

(define click2 (bind-click (count) (begin `((count . ,(+ count 1))))))

(register-callback "click2" click2)

(define (range start end)
  (if (equal? start end)
      '()
      (cons start (range (+ 1 start) end))))

(define click0 (bind-click (count) `((messages . ,(range 0 count)))))

(register-callback "click0" click0)

(define single-render
  (render (status)
	  (<div> (<text> status))))

(register-callback "single-render" single-render)

(define map-render
  (render (messages count)
	  (map (lambda (msg)
		 (<div> (<text> msg)
			(<text> count)
			(<button> #:click
				  (lambda ()
				    (print "yay " msg)
				    '((status . "reloading")))
				  (<text> "click me"))))
	       messages)))

(register-callback "map-render" map-render)

(init  '((status . "Not loaded yet.")
	 (messages . (8))
	 (count . 20)))

