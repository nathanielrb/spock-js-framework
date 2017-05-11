;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; App

(define click1 (bind-click () '((status . "loaded.") (class . "blue"))))

(register-callback "click1" click1)

(define click2 (bind-click (count) (begin `((class . "green")))))

(register-callback "click2" click2)

(define click3 (bind-click (count) (begin `((count . ,(+ count 1))))))

(register-callback "click3" click3)

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
	   (h "i" #f "Status: ")
	   (h "b" (% "attrs" (% "class" "green"))
	      (vector status status))))

(register-callback "single-render" single-render)

(define map-messages
  (for msg messages (count class status)
	   (<div> #f status)
	   (<div> #f msg)
	   ;;(h "div" (% "class" (% "green" (equal? class "green") "blue" (equal? class "blue")))
	   (<div> (% "attrs" (% "class" class))
		  (vector " ducks: " count))
	   (h "div" #f
	      (vector
	       (h "button" #f "click me")))))
;; {{class `("click-me" ,class)}}
;;	      (set-callback
;;	       (<button> () "click me")
;;	       (lambda (event) (alert "Clicked"))))))
;;		      (set-click
;;		       (callback (lambda ()
;;				   (print "yay " msg)
;;				   '((status . "reloading")))))))))

(register-callback "map-messages" map-messages)

(init  `((status . "initialized")
	 (messages . ,(range 0 800));;(8 9 10))
	 (count . 4)
	 (class . "blue")))
