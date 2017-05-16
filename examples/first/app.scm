;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; App

(register-component "click"
		    (bind-this .onclick (count)
			       (callback
				(lambda (event)
				  (log count) (send (selected count)
						    (values count (+ count 1)))))))

(register-component "showarg"
  (lambda (this)	    
    (let ((arg  (.name (.dataset this))))
      (render this (count)
		  (<div> #f arg)))))
  
(define (range start end)
  (if (equal? start end)
      '()
      (cons start (range (+ 1 start) end))))

(define single-render
  (render-this (status)
    (h "i" #f "Status: ")
    (h "b" (% "attrs" (% "class" "green"))
       (vector status status))))

(register-component "single-render" single-render)

(define map-table
  (render-this (selected)
    (<div> #f
	   (map (lambda (x)
		  (<div> #f
			 (map (lambda (y)
				(<div>
				 (% "class" (% "cell" #t "selected" (equal? (+ x y) selected))
				    "on" (% "click" (cb (selected-square) (+ x y))))
				 (let* ((c #f)
					(n (call/cc (lambda (k) (set! c k) 0))))
				   (vector
				    (<div> (% "class" (% "green" #t)
					      "on" (% "mouseover" (callback
							      (lambda (event)
								(c (+ n 1))))))
					   (vector n))))))
			      (range 0 20))))
		(range 0 20)))))

(register-component "map-messages" map-table)


(init  `((status . "initialized")
	 (messages . ,(range 0 800));;(8 9 10))
	 (count . 4)
	 (class . "blue")
	 (selected . #f)) )

;; (register-async) or something similar so only fires when it receives...

(catch-vars (selected-square)
	    (send (selected) (+ selected-square 1)))

(catch-vars (selected-square)
;	    (when (selected-square)
	      (ajax "GET"
		    "http://lookup.dbpedia.org/api/search.asmx/PrefixSearch?QueryClass=&MaxHits=5&QueryString=Brussels"
		    (cb (selected)
			(lambda (x)
			  (log (.responseXML (.currentTarget x))); (.responseText x))
			  (+ selected-square 2)))))

(start)
