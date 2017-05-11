;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DOM

(define (qsa element query) (%inline .querySelectorAll element query))

(define (qs element query) (%inline .querySelector element query))

(define (dqsa element query) (%inline .querySelectorAll document.body query))

(define (dqs element query) (qs document.body query))

(define (log msg) (%inline console.log msg))

(define (alert msg) (%inline window.alert msg))

(define (get-id id) (%inline document.getElementById id))

(define (remove-node node)
  (%inline .removeChild (.parentNode node) node))

(define (insert-node-before node1 node2)
  (%inline .insertBefore (.parentNode node2) node1 node2))

(define (replace-node node1 node2)
  (%inline .replaceChild (.parentNode node2) node1 node2))

(define (node-append-child parent child)
  (%inline .appendChild parent child))

(define (set-html elt content)
  (set! (.innerHTML elt) content))

(define (spock-elements)
  (%inline Array.prototype.slice.call
	   (%inline .querySelectorAll document.body "*[spock]")))

(define (add-event-listener event element callback)
  (%inline .addEventListener element event callback))

(define (set-event event elt cb)
  (set! (event elt) cb) elt)

(define-syntax-rule (define-event name event)
  (define (name elt cb)
    (set! (event elt) cb) elt))

(define-event set-click .onclick)

(define-event set-change .onchange)

(define-event set-input .oninput)

(define (old-set-click elt cb)
  (set! (.onclick elt) cb) elt)

(define (slice a)
  (%inline Array.prototype.slice.call a))

(define (children node)
  (slice (%property-ref .childNodes node)))

(define (car-safe p)
  (and (pair? p) (car p)))

(define (cdr-safe p)
  (and (pair? p) (cdr p)))

(define (empty? x)
  (not (and (void? x) (null? x) (not x))))

(define (nodename elt)
  (%property-ref .nodeName elt))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; snabbdom Virtual DOM

;; (define (alist->js-obj alist)
;;   ...

(define (h selector props children)
  (let ((kids (cond ((list? children) (list->vector children))
		    ((or (vector? children) (string? children)) children)
		    (else #f))))
    ;;(print "PROPS:") (log props)
    (%inline "snabbdom.h" selector props kids)))

(define-syntax define-element
  (syntax-rules ()
    ((define-element name elt)
     (define-syntax name
       (syntax-rules ()
	 ((name props children)
	  (h elt props children)))))))

(define-element <div> "div")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Events 

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

(define waiting (lambda () #f))

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
      (waiting)
      (let ((next-cc (*dequeue*)))
	(print "YIELDING")
	((next-cc)))))

(define (assoc-val key l)
  (cdr-safe (assoc key l)))

(define-syntax with-bindings
  (syntax-rules ()
    ((with-bindings (vars ...) bindings body ...)
     (let ((vars (assoc-val (quote vars) bindings)) ...)
       body ...))))

(define merge-bindings append)

;; thread *inits???

(define (put-cons! var property val)
  (put! var property
	(cons val (or (get var property) '()))))

(define-syntax-rule (catch-vars (vars ...) body ...)
  (letrec ((bindings (call/cc (lambda (k)
				(map (lambda (var)
				       (put-cons! var 'continuations
						  (lambda (vals) (k (append vals bindings)))))
				     (list (quote vars) ...))
				(list (assoc (quote vars) *inits*) ...)))))
      (with-bindings (vars ...) bindings
		     body ...)
      (yield)))

(define (get-binding binding-name bindings)
  (let ((binding-pair (assoc binding-name bindings)))
    (and binding-pair (cdr binding-pair))))

(define (send-bindings var bindings)
  ((get var 'continuations) bindings))

(define (three-way-split A B)
  (let loop ((As A)
	     (Bs B)
	     (accA '())
	     (accAB '())
	     (accB '()))
    (if (null? Bs)
	(list (append As accA) accAB accB)
	(let ((b (car Bs)))
	  (let subloop ((As As)
			(Aleft '()))
	    (if (null? As)
		(loop Aleft (cdr Bs) accA accAB (cons b accB))
		(let ((a (car As)))
		  (if (equal? b a)
		      (loop (append Aleft (cdr As)) (cdr Bs) accA (cons b accAB) accB)
		      (subloop (cdr As) (cons a Aleft))))))))))

(define (merge-alist-sets L new-var-set)
  (let ((newvar (car new-var-set)))
    (let loop ((Ls L)
	       (newset (cdr new-var-set))
	       (accum '()))
    (if (or (null? newset) (null? Ls))
	(cons (cons (list newvar) newset) accum)
	(let* ((vars (caar Ls))
	       (set (cdar Ls))
	       (split (three-way-split set newset))
	       (A (car split))
	       (AB (cadr split))
	       (B (caddr split)))
	  (loop (cdr Ls) B
		(append (list (cons vars A)
			      (cons (cons newvar vars)
				    AB))
			accum)))))))

(define (merge-continuation-lists clists)
  (let loop ((merged '())
	     (clists clists))
    (if (null? clists)
	merged
	(loop (merge-alist-sets merged (car clists))
	      (cdr clists)))))

(define (send-vars var-bindings)
  (map (lambda (varset)
	 (print "CONTINUATIONS: " varset)
	 (let ((var-names (car varset))
	       (continuations (cdr varset)))
	   (when (not (null? continuations))
	     (map (lambda (k) (*enqueue* (lambda ()  (k var-bindings))))
		  continuations))))
       (merge-continuation-lists
	(map (lambda (var-pair)
	       (let ((var (car var-pair)))
		 (cons var (get var 'continuations))))
	     var-bindings)))
  (print "QUEUE: " *queue*)
  (yield))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; API

;; do this without set! and the this/ref switch?
(define-syntax-rule (render (vars ...) body ...)
  (lambda (this)
    (let ((ref this))
      (catch-vars (vars ...)
        (let ((newnode (h (nodename this) #f (vector body ...))))
	  ;; (list (nodename this) '() (list body ...))))	
	  ;;(print "new ") (log newnode)
	  (log newnode)
	  (%inline "patch" ref newnode)
	  (set! ref newnode))))))
	;;(patch (.parentNode this) this newnode)))))

(define-syntax-rule (for xvar Xs (other-vars ...) body ...)
  (lambda (this)
    (let ((ref this)) ;;  (%inline .cloneNode this #f)))
      (catch-vars (Xs other-vars ...)
        (let ((newnode (h (nodename this) #f ;; (list (nodename this) '()
			  (apply append
				 (map
				  (lambda (xvar)
				    ((lambda (xvar) (list body ...)) xvar))
				  Xs)))))
	(%inline "patch" ref newnode)
	(set! ref newnode))))))
	
	
(define-syntax-rule (bind-click (vars ...) body ...)
  (lambda (this)
    (catch-vars (vars ...)
	  (set-click this
		     (callback (lambda (this)
				 (let ((var-bindings (begin body ...)))
				   (send-vars var-bindings))))))))

(define-syntax-rule (bind-change (vars ...) bodyf)
  (lambda (this)
    (catch-vars (vars ...)
	  (set-change this
		     (callback (lambda (this)
				 (let ((var-bindings (bodyf this)))
				   (send-vars var-bindings))))))))

(define-syntax-rule (bind-input (vars ...) bodyf)
  (lambda (this)
    (catch-vars (vars ...)
	  (set-input this
		     (callback (lambda (this)
				 (let ((var-bindings (bodyf this)))
				   (send-vars var-bindings))))))))

(define (init bindings)
  (set! *inits* bindings)
  (map (lambda (e)
	 (let ((name (%inline .getAttribute e "spock")))
	   ((get-callback name) e)))
       (spock-elements))
  (call/cc (lambda (k) (set! waiting k)))
  (begin (print "waiting...")))
