(define (qsa element query) (%inline .querySelectorAll element query))

(define (qs element query) (%inline .querySelector element query))

(define (dqsa element query) (%inline .querySelectorAll document.body query))

(define (dqs element query) (qs document.body query))

(define (log msg) (%inline console.log msg))

(define (alert msg) (%inline window.alert msg))

(define (get-id id) (%inline document.getElementById id))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Events etc.

(define (add-event-listener event element callback)
  (%inline .addEventListener element event callback))

(define (set-click elt cb)
  (set! (.onclick elt) cb))

(define (set-html elt content)
  (set! (.innerHTML elt) content))

(define (ajax method path)
  (let ((x (%inline "new XMLHttpRequest")))
    (log x)
    (%inline ".open" x method path #t)
    (set! (.onreadystatechange x)
	  (callback (lambda ()
		      (%inline "alert" 5)
		      (log (.responseText x)))))
    (%inline ".send" x)))

(define (register-callback name cb)
  (let ((symname (if (string? name) (string->symbol name) name)))
    (put! symname 'callback cb)))

(define (get-callback name)
  (let ((symname (if (string? name) (string->symbol name) name)))
    (get symname 'callback)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Signals

(define call/cc call-with-current-continuation)

(define queue '())

(define (yield)
  (if (null? queue)
      (print "done")
      (let ((next-cc (car queue)))
	(set! queue (cdr queue))
	((next-cc)))))

(define-syntax-rule
  (node (var) body ...)
  (let ((var (call/cc
	      (lambda (k)		    
		(put! (quote var) 'conts
		      (cons k (or (get (quote var) 'conts) '())))
		"..."))))
    body
    ...
    (yield)))

(define (send-var x val)
  (set! queue (map (lambda (k)
		     (lambda () (k val)))
		   (get x 'conts)))
  (yield))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; App

(define (new-element name children class)
  (let ((elt (%inline document.createElement name)))
    (%inline .classList.add elt class)
					;(for-each (lambda (child)
    (log children)
    (let ((child (car children)))
      (%inline .appendChild elt child))
    ;children
    elt))
    
(define (<text> text)
  (%inline document.createTextNode text))

(define-syntax-rule (<div> children ...)
  (new-element "div" (list children ...) "class"))

(define (click-alert this)
  ;;(set! (.onclick this)
  (set-click this
	     (callback (lambda (this)
			 (send-var 'status "Loaded")
			 (alert "clicked")))))

(register-callback "click-alert" click-alert)

(define (render this)
  (let ((original this)
	(parent (.parentNode this)))
    (node (status)
	  (let ((new (<div> (<text> status))))
	    (%inline .replaceChild parent new this)
	    (set! this new)))))
  
	 ;   (log parent)
	  ;;(log (.parentNode this)))))

	  ;;(set-html this status))))

(register-callback "render" render)

(define (map-render this)
  (let ((original this)
	(parent (.parentNode this))
	(these (list this)))
    (node (status)
	  (let ((new-nodes (map (lambda (x)
				  (<div> (<text> status)))
				'(1 2 3))))
	    (map (lambda (node)
		   (%inline .insertBefore (.parentNode this) node this))
		 new-nodes)

	    (map (lambda (node) (%inline .removeChild parent node)) these)
	    (set! this (car new-nodes))
	    (set! these new-nodes)))))
  
(register-callback "map-render" map-render)

(define (spock-elements)
  (%inline Array.prototype.slice.call
	   (%inline .querySelectorAll document.body "*[spock]")))

;(%inline Array.prototype.forEach.call
;	 (%inline .querySelectorAll document.body "*[spock]")
;	 (callback (lambda (e) (log e))))

;(%inline Array.prototype.forEach.call
;	 (%inline .querySelectorAll document.body "*[spock]") ;; (spock-elements)
;	 (callback (lambda (e)
;		     (log e)
;		     (%inline .parentNode.appendChild e (<div> (<text> "hi"))))))
;		     (let ((name (%inline .getAttribute e "spock")))
;		       ((get-callback name) e)))))
	 

(map (lambda (e)
     (let ((name (%inline .getAttribute e "spock")))
       ((get-callback name) e)))
     (spock-elements))


;(let ((app  (query-selector "#app")))
 ; (let ((button (query-selector "#click")))
  ;  (log (.innerHTML button))
   ; (add-event-listener "click" button
;			(callback (lambda (event)
;				    (ajax "GET" "http://www.google.com")
;				    (log "clicked"))))))
