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

(define dd (%inline "new diffDOM"))

;(define (patch A B)
 ; (%inline .apply dd A (%inline .diff dd A B)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Virtual DOM

(define (text-element? elt)
  (or (string? elt) (number? elt)))

(define (reify-element element)
  ;;(print "NODE " element)
  (if (text-element? element)
      (make-text-element element)
      (let* ((node (%inline document.createElement (element-name element))))
	(class-add node element)
	(for-each (lambda (child)
		    (%inline .appendChild node (reify-element child)))
		  (element-children element))
	node)))

(define (class-add node elt)
  (let ((class (element-attribute-val elt 'class)))
    ;;(print "CLASS " class)
    (when class
	  (%inline .classList.add node (element-attribute-val elt 'class)))))

(define (make-text-element text)
  (%inline document.createTextNode text))

(define (element name attributes children)
  (list name attributes children))

(define (element-type elt)
  (if (string? elt) 3 1))      

(define (element-name elt)
  (car elt))

(define (element-attributes elt)
  (car-safe (cdr-safe elt)))

(define (element-attribute-val elt attr)
  (assoc-val attr (element-attributes elt)))

(define (element-children elt)
  (and (pair? elt)
       (caddr elt)))

(define-syntax-rule (<div> ((attr val) ...) children ...)
  (element "DIV" (list (cons (quote attr) val) ...) (list children ...)))

(define-syntax-rule (<span> ((attr val) ...) children ...)
  (element "span" #f (list children ...)))

(define-syntax-rule (<b> ((attr val) ...) children ...)
  (element "b" (list (cons (quote attr) val) ...) (list children ...)))

(define-syntax-rule (<i> ((attr val) ...) children ...)
  (element "i" #f (list children ...)))

(define-syntax-rule (<button> ((attr val) ...) children ...)
  (element "button"  #f (list children ...)))

(define (nodename elt)
  (%property-ref .nodeName elt))

(define (changed? real virtual)
  ;;(print "types: "  (%property-ref .nodeType real) " " (element-type virtual))
  ;;(print "names: " (%property-ref .nodeName real) " " (element-name virtual))
  ;;(print "content: " (%property-ref .nodeName real) " " virtual)
  ;;(print "text: " (%property-ref .textContent real))
  (or (not (equal? (%property-ref .nodeType real)
		   (element-type virtual)))
      (not (equal? (nodename real) (element-name virtual)))
      (and (= (%property-ref .nodeType real) 3)
	   (not (equal? (%property-ref .textContent real) virtual)))))

(define (children node)
  (%inline Array.prototype.slice.call
	   (%property-ref .childNodes node)))

(define (car-safe p)
  (and (pair? p) (car p)))

(define (cdr-safe p)
  (and (pair? p) (cdr p)))

(define (empty? x)
  (not (and (void? x) (null? x) (not x))))

(define (patch parent real virtual)
  (cond ((empty? real) (node-append-child parent (reify-element virtual)))
	((empty? virtual) (remove-node real))
	((changed? real virtual)
	 ;; (begin (print "replaiching in " real)
	 (replace-node (reify-element virtual) real)) 
	(else (begin
		;;(print "doing")
		;; events, props
		(do ((rcs (vector->list (children real)) (cdr-safe rcs))
		     (vcs (element-children virtual) (cdr-safe vcs)))
		    ((and (or (not rcs) (null? rcs)) (or (not vcs) (null? vcs))))
		(patch real (car-safe rcs) (car-safe vcs)))))))

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
(define-syntax-rule (put-continuations vars ...)
  (call/cc
   (lambda (k)
     (map (lambda (var)
	    (print "putting " var ": " k " into " (get var 'continuations))
	    (put! var 'continuations
		  (cons k (or (get var 'continuations) '()))))
	  (list (quote vars) ...))
     (list (assoc (quote vars) *inits*) ...))))

(define-syntax-rule (compose-signals (vars ...) body ...)
  (let ((bindings '()))    
    (let ((signals (put-continuations vars ...)))
      (set! bindings (merge-bindings signals bindings))
      (with-bindings (vars ...) bindings
		     body ...)
      (yield))))

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
    (compose-signals (vars ...)
      (let ((ref (%inline .cloneNode this #f))
	    (newnode (list (nodename this) '() (list body ...))))
	(print "new " newnode)
	(patch (.parentNode this) this newnode)))))

;;	(if (list? new-nodes)
;;	    (for-each (lambda (node)
;;			(node-append-child ref node))
;;		      new-nodes)
;;	    (node-append-child ref new-nodes))
;;	(replace-node ref this)
;;	(set! this ref)))))

(define (remove-all-children node)
  (for-each (lambda (child)
	      (%inline .removeChild node child))
	    (%inline Array.prototype.slice.call
		     (%property-ref .childNodes node))))

(define-syntax-rule (for xvar Xs (other-vars ...) body ...)
  (lambda (this)
    (compose-signals (Xs other-vars ...)
      (let ((ref (%inline .cloneNode this #f))
	    (newnode (list (nodename this) '()
			   (apply append
			   (map
			    (lambda (xvar)
			      ((lambda (xvar) (list body ...)) xvar))
			    Xs)))))
	(patch (.parentNode this) this newnode)))))

(define-syntax-rule (for2 xvar Xs (other-vars ...) body)
  (lambda (this)
    (compose-signals (Xs other-vars ...)
      (let ((ref (%inline .cloneNode this #f)))
	(remove-all-children this)
	(for-each
	 (lambda (x)
	   (node-append-child this ((lambda (xvar) body) x)))
	 Xs)))))
	
(define-syntax-rule (bind-click (vars ...) body ...)
  (lambda (this)
    (compose-signals (vars ...)
	  (set-click this
		     (callback (lambda (this)
				 (let ((var-bindings (begin body ...)))
				   (send-vars var-bindings))))))))

(define-syntax-rule (bind-change (vars ...) bodyf)
  (lambda (this)
    (compose-signals (vars ...)
	  (set-change this
		     (callback (lambda (this)
				 (let ((var-bindings (bodyf this)))
				   (send-vars var-bindings))))))))

(define-syntax-rule (bind-input (vars ...) bodyf)
  (lambda (this)
    (compose-signals (vars ...)
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
