;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; App

(define click1 (bind-click () '((status . "Loading...") (class . "green"))))

(register-callback "click1" click1)

(define click2 (bind-click (count) (begin `((count . ,(+ count 1))))))

(register-callback "click2" click2)

(define (range start end)
  (if (equal? start end)
      '()
      (cons start (range (+ 1 start) end))))

(define click0 (bind-click (count) `((messages . ,(range 0 count))
				     (class . "green"))))

(register-callback "click0" click0)

(define changeme
  (bind-change ()
	       (lambda (this)				   
		 `((status . ,(%property-ref
			       .value (%property-ref .target this)))))))

(register-callback "changeme" changeme)

(define single-render
  (render (status)
	  (h "b" #f (vector status status))))

(register-callback "single-render" single-render)

(define map-messages
  (for msg messages (count class)
       (h "div" #f
	  (vector
	   (h "div" #f msg)
	   (h "div"  #f  (vector " ducks: " count))
	   (h "div" #f
	      (vector
	       (h "button" #f "click me")))))))
;; {{class `("click-me" ,class)}}
;;	      (set-callback
;;	       (<button> () "click me")
;;	       (lambda (event) (alert "Clicked"))))))
;;		      (set-click
;;		       (callback (lambda ()
;;				   (print "yay " msg)
;;				   '((status . "reloading")))))))))

(register-callback "map-messages" map-messages)

(init  `((status . "Not loaded yet.")
	 (messages . ,(range 0 8));;(8 9 10))
	 (count . 4)
	 (class . "blue")))
