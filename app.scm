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

(define map-render3
  (lambda (this)
    (compose-signals (messages count)
      (let ((ref (%inline .cloneNode this #f)))
	(for-each (lambda (msg)
		    (node-append-child
		     ref
		     (<div> (<div> (<b> (<text> msg)))
			    (<div> (<text> count))
			    (<div> (set-click
				    (<button> (<text> "click me"))
				    (callback (lambda ()
						(print "yay " msg)
						'((status . "reloading")))))))))
		  messages)
	(replace-node ref this)
	(set! this ref)))))


	;(replace-node ref this)
	;(set! this ref)))))

(define map-messages
  (map-render messages (count)
	      (lambda (msg)
		(<div> (<div> (<b> (<text> msg)))
		       (<div> (<text> " ducks" count))
		       (<div> (<text> count))
		       (<div> (set-click
			       (<button> (<text> "click me"))
			       (callback (lambda ()
					   (print "yay " msg)
					   '((status . "reloading"))))))))))

(register-callback "map-messages" map-messages)

(init  '((status . "Not loaded yet.")
	 (messages . (8))
	 (count . 200)))
