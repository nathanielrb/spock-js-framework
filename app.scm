;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; App

(define click1 (bind-click () '((status . "loaded.") (class . "blue"))))

(register-component "click1" click1)

(define click2 (bind-click (count) (begin `((class . "green")))))

(register-component "click3"
  (bind .onclick (count)
	(log count) (send (selected count)
			  (values count (+ count 1)))))

(define (range start end)
  (if (equal? start end)
      '()
      (cons start (range (+ 1 start) end))))

(define click0 (bind-click (count) `((messages . ,(range 0 count))
				     (class . "green"))))

(register-component "click0" click0)

(define changeme
  (bind-change ()
	       (lambda (this)				   
		 `((status . ,(%property-ref
			       .value (%property-ref .target this)))))))

(register-component "changeme" changeme)

(define single-render
  (render (status)
	   (h "i" #f "Status: ")
	   (h "b" (% "attrs" (% "class" "green"))
	      (vector status status))))

(register-component "single-render" single-render)

(define map-messages
  (for msg messages (count class status selected)
       (<div> (% "on" (% "mouseover" (callback
				      (lambda (this)
					(send (selected) msg)))))
					;;;(log msg)
					;;(send-vars `((selected . ,msg)))))))
	      (vector
	       (<div> #f status)
	       (<div> #f msg)
	       ;;(h "div" (% "class" (% "green" (equal? class "green") "blue" (equal? class "blue")))
	       (<div> (% "attrs" (% "class" class))
		      (vector " ducks: " count))
	       (h "div" (% "class" (% "selected" (equal? msg selected)))
		  (vector
		   msg "/" (or selected "none")
		   (h "button"
		      (% "on" (% "click" (callback (lambda (this) (log msg)))))
		      "click me")))))) )

;; {{class `("click-me" ,class)}}
;;	      (set-callback
;;	       (<button> () "click me")
;;	       (lambda (event) (alert "Clicked"))))))
;;		      (set-click
;;		       (callback (lambda ()
;;				   (print "yay " msg)
;;				   '((status . "reloading")))))))))


(define map-table
  (render (selected)
    (<div> #f
	   (map (lambda (x)
		  (<div> #f
			 (map (lambda (y)
				(<div>
				 (% "class" (% "cell" #t "selected" (equal? (+ x y) selected))
				    "on" (% "mouseover" (callback
							 (lambda (this)
							   (send (selected-square) (+ x y))))))
				 (vector x ", " y)))
			      (range 0 20))))
		(range 0 20)))))

(register-component "map-messages" map-table)

;; (register-component "map-messages" map-messages)



(init  `((status . "initialized")
	 (messages . ,(range 0 800));;(8 9 10))
	 (count . 4)
	 (class . "blue")
	 (selected . #f))
       (catch-vars (selected-square) (send (selected) (+ selected-square 1)))
       (catch-vars (selected-square)
		   (ajax "GET"
			 "http://lookup.dbpedia.org/api/search.asmx/PrefixSearch?QueryClass=&MaxHits=5&QueryString=Brussels"
			 ;;(jstring (string-append "http://www.tenforce.com/" selected))
			 (lambda (x)
			   (log (.responseXML (.currentTarget x))); (.responseText x))
			   (send (selected) (+ selected-square 2))))))

;;	    (log "yup"))
	    ;(when selected-square
	     ; (send (selected) (+ 1 selected-square))))

