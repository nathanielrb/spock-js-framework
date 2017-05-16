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

(define-syntax-rule (bind-event event elt cb)
  (begin (set! (event elt) cb)
	 elt))

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

(define (h selector props children)
  (let ((kids (cond ((list? children) (list->vector children))
		    ((or (vector? children) (string? children)) children)
		    (else #f))))
    (%inline "snabbdom.h" selector props kids)))

(define (patch old new)
  (%inline "patch" old new))

(define-syntax define-element
  (syntax-rules ()
    ((define-element name elt)
     (define-syntax name
       (syntax-rules ()
	 ((name selector)
	  (lambda (props children)
	    (h (jstring (string-append elt selector))
	       props children)))
	 ((name props children)
	  (h elt props children)))))))

(define-element <div> "div")

(define-element <h1> "h1")

(define-element <h2> "h2")

(define-element <h3> "h3")

(define-element <h4> "h4")

(define-element <select> "select")

(define-element <option> "option")

(define-element <input> "input")

(define-element <a> "a")

(define-element <span> "span")

(define-element <b> "b")

(define-element <i> "i")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Events

;; TO DO
;; Handle different states (loading, done, error...)

(define (ajax-cb x)
  (log (.responseText x)))

(define (ajax method path cb)
  (let ((x (%inline "new XMLHttpRequest")))
    (%inline ".open" x method path #t)
    (set! (.onload x)
      (callback
       (lambda (response)
         (if (equal? (.status x) 200)
             (cb response)
             (begin (log "Ajax error")
                    (log event))))))
    (set! (.responseType x) "json")
    (%inline ".send" x)))

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
				(let ((K (lambda (vals) (k (append vals bindings)))))
				  (map (lambda (var)
					 (put-cons! var 'continuations K))
				       (list (quote vars) ...)))
				(list (assoc (quote vars) *inits*) ...)))))
    (with-bindings (vars ...) bindings body ...)
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
  (yield))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; API

(define (register-component name cb)
  (let ((symname (if (string? name) (string->symbol name) name)))
    (put! symname 'callback cb)))

;; (send (a b) (... (values 1 2)))
(define-syntax send
  (syntax-rules ()
    ((send (vars ...) body)
     (call-with-values
	 (lambda () body)
       (lambda (vars ...)
	 (send-vars
	  (list (cons (quote vars) vars) ...)))))))

;; possible without set! and the this/ref switch?

(define-syntax-rule (render ref (vars ...) body ...)
  (catch-vars (vars ...)
    (let ((newnode (h (nodename ref) #f (vector body ...))))
      (set! ref (patch ref newnode)))))

(define-syntax-rule (render-this (vars ...) body)
  (lambda (this)
    (let ((ref this))
      (catch-vars (vars ...)
        (let ((newnode body)) ;(h (nodename this) #f (vector body ...))))
	  (if newnode
	      (set! ref (patch ref newnode))
	      ref))))))

(define-syntax-rule (bind this event (vars ...) body ...)
  (catch-vars (vars ...)
    (bind-event event this
		(callback (lambda (this)
			    body ...)))))

(define-syntax-rule (bind-this event (vars ...) cb)
  (lambda (this)
    (catch-vars (vars ...)
      (bind-event event this cb))))

(define-syntax cb
  (syntax-rules ()
    ((cb (vars ...) body)
     (callback
      (lambda (event)
	(send (vars ...)
              (body event)))))))

(define-syntax-rule (init bindings body ...)
  (begin
    (set! *inits* bindings)
    (map (lambda (e)
	   (let ((name (%inline .getAttribute e "spock")))
	     ((get-callback name) e)))
	 (spock-elements)) ))
;    body ...
 ;   (call/cc (lambda (k) (set! waiting k)))
  ;  (begin (print "waiting..."))))

(define (start)
  (call/cc (lambda (k) (set! waiting k)))
  (begin (print "waiting...")))
