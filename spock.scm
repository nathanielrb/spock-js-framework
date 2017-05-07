;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DOM

(define (qsa element query) (%inline .querySelectorAll element query))

(define (qs element query) (%inline .querySelector element query))

(define (dqsa element query) (%inline .querySelectorAll document.body query))

(define (dqs element query) (qs document.body query))

(define (log msg) (%inline console.log msg))

(define (alert msg) (%inline window.alert msg))

(define (get-id id) (%inline document.getElementById id))

(define (new-element name children class)
  (let ((elt (%inline document.createElement name)))
    (%inline .classList.add elt class)
    (map (lambda (child)
	   (%inline .appendChild elt child))
	 children)
    elt))
    
(define (<text> text)
  (%inline document.createTextNode text))

(define-syntax-rule (<div> children ...)
  (new-element "div" (list children ...) "class"))

(define (remove node)
  (%inline .removeChild (.parentNode node) node))

(define (insert-before node1 node2)
  (%inline .insertBefore (.parentNode node2) node1 node2))

(define (append-child parent child)
  (%inline .appendChild parent child))

(define (set-html elt content)
  (set! (.innerHTML elt) content))

(define dd (%inline "new diffDOM"))

(define (patch A B)
  (%inline .apply dd A (%inline .diff dd A B)))

(define (spock-elements)
  (%inline Array.prototype.slice.call
	   (%inline .querySelectorAll document.body "*[spock]")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Events 

(define (add-event-listener event element callback)
  (%inline .addEventListener element event callback))

(define (set-click elt cb)
  (set! (.onclick elt) cb))

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

(define *queue* '())

(define (*enqueue* x)
  (set! *queue* (append *queue* (list x))))

(define (*dequeue*)
  (let ((first (car *queue*)))
    (set! *queue* (cdr *queue*))
    first))

(define (yield)
  (if (null? *queue*)
      "loading..."
      (let ((next-cc (*dequeue*)))
	((next-cc)))))

(define (assoc-val key l)
  (cdr (assoc key l)))

(define-syntax with-bindings
  (syntax-rules ()
    ((with-bindings (vars ...) bindings body ...)
     (let ((vars (assoc-val (quote vars) bindings)) ...)
       body ...))))

(define merge-bindings append)

(define-syntax-rule (put-continuations vars ...)
  (call/cc
   (lambda (k)
     (map (lambda (var)
	    (put! (quote var) 'continuations
		  (cons k (or (get (quote var) 'continuations) '()))))
	  (list (quote vars) ...))
     '())))

(define-syntax-rule (compose-signals (vars ...) body ...)
  (let ((bindings '()))    
    (let ((signals (put-continuations vars ...)))
      (set! bindings (merge-bindings signals bindings))
      (with-bindings (vars ...)
		     bindings body ...
		     (yield)))))

(define (get-binding binding-name bindings)
  (let ((binding-pair (assoc binding-name bindings)))
    (and binding-pair (cdr binding-pair))))

(define (send-bindings var bindings)
  ((get var 'continuations) bindings))

(define (merge-continuation-lists clists)
  (let loop ((merged '())
	     (clists clists))
    (if (null? clists)
	merged
	(loop (merge-alist-sets merged (car clists))
	      (cdr clists)))))

(define (send-vars var-bindings)
  (map (lambda (varset)
	 (let ((var-names (car varset))
	       (continuations (cdr varset)))
	   (when (not (null? continuations))
	     (map (lambda (k)
		    (*enqueue* (lambda () 
				 (k (map (lambda (var)
					   (cons var (get-binding var var-bindings)))
					 var-names)))))
		  continuations))))
       (merge-continuation-lists
	(map (lambda (var-pair)
	       (let ((var (car var-pair)))
		 (cons var (get var 'continuations))))
	     var-bindings))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; API

(define-syntax-rule (render (vars ...) body)
  (lambda (this)
    (compose-signals (vars ...)
	  (let ((new-nodes body)
		(ref (%inline .cloneNode this #f)))
	    (if (list? body)
		(for-each (lambda (node)
			    (append-child ref node))
			  new-nodes)
		(append-child ref body))
	    (patch this ref)))))

(define-syntax-rule (bind-click (var) body ...)
  (lambda (this)
    (compose-signals (var)
	  (set-click this
		     (callback (lambda (this)
				 (let ((vars (begin body ...)))
				   (send-vars vars))))))))

(define (init)
  (map (lambda (e)
	 (let ((name (%inline .getAttribute e "spock")))
	   ((get-callback name) e)))
       (spock-elements)))
