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

(define *queue* '())

(define (yield)
  (if (null? *queue*)
      "loading..."
      (let ((next-cc (*dequeue*)))
	((next-cc)))))

;; (define-syntax save-continuation
;;   (syntax-rules ()
;;    ((save-continuation var body) (call/cc (lambda (k) (put! var 'c k) body)))))
;;
;; (let ((x (save-continuation 'x 5)) (y (save-continuation 'y 7))) (+ x y))
;;
;; ((get 'x 'c) 20)
;;
;; ((get 'y 'c) 20)

(define-syntax-rule (put-continuation var)
  (call/cc
   (lambda (k)
     (when (not (get (quote var) 'value))
       (put! (quote var) 'conts
	     (cons (lambda (val)
		     (k val))
		   (or (get (quote var) 'conts) '()))))
     ;(print "getting") (print (quote var))
     ;(print (get (quote var) 'value))
     (get (quote var) 'value))))
    
;	   "...")))))

(define-syntax-rule (node (var ...) body ...)
  (let ((var (put-continuation var)) ...)
    body ... (yield)))

(define (*enqueue* x)
  (set! *queue* (append *queue* (list x))))

(define (*dequeue*)
  (let ((first (car *queue*)))
    (set! *queue* (cdr *queue*))
    first))

(define (send-var var val)
  ;;(print (get 'status 'value))
  (put! 'status 'value val)
  (map (lambda (k)
	 (*enqueue* (lambda () (k val))))
       (get var 'conts))
  (yield))

(define (send-vars vars-vals)
  (if (null? vars-vals)
      (yield)
      (let ((var (car vars-vals))
	    (val (cadr vars-vals)))
	(map (lambda (k)
	       (*enqueue* (lambda () (k val))))
	     (get var 'conts))
	(send-vars (cddr vars-vals)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DOM

(define (new-element name children class)
  (let ((elt (%inline document.createElement name)))
    (%inline .classList.add elt class)
					;(for-each (lambda (child)
					;(let ((child (car children)))
    (map (lambda (child)
	   (%inline .appendChild elt child))
	 children)
    ;children
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

(define dd (%inline "new diffDOM"))

(define (patch A B)
  (%inline .apply dd A (%inline .diff dd A B)))

(define-syntax-rule (render (var ...) body)
  (lambda (this)
    (node (var ...)
	  (let ((new-nodes body)
		(ref (%inline .cloneNode this #f)))
	    (if (list? body)
		(for-each (lambda (node)
			    (append-child ref node))
			  new-nodes)
		(append-child ref body))
	    (patch this ref)))))

(define-syntax-rule (render-click (var) body ...)
  (lambda (this)
    (node (var)
	  (set-click this
		     (callback (lambda (this)
				 (let ((vars (begin body ...)))
				   (send-vars vars))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Init

(define (spock-elements)
  (%inline Array.prototype.slice.call
	   (%inline .querySelectorAll document.body "*[spock]")))

(define (init)
  (map (lambda (e)
	 (let ((name (%inline .getAttribute e "spock")))
	   ((get-callback name) e)))
       (spock-elements)))
