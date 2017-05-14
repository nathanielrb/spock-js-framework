(define queue '())

(define (current-continuation)
  (call/cc (lambda (cc) (cc cc))))

(define (get-var x)
;  (let ((cc (current-continuation)))
  (call/cc
     (lambda (k)
       (print "setting")
       (put! x 'conts (cons k (get x 'conts)))
       "...")))

(define-syntax get-var
  (syntax-rules ()
    ((get-var x)
;  (let ((cc (current-continuation)))
     (call/cc
      (lambda (k)
	(print "Setting")
	(put! x 'conts (cons k (get x 'conts)))
	"...")))))

(define (send-var x val)
  (set! queue (map (lambda (k)
		     (lambda () (k val)))
		   (get x 'conts)))
  (yield))

(define (yield)
  (if (null? queue)
      (print "done")
      (let ((next-cc (car queue))
	    (cc (current-continuation)))
	(set! next cc)
	(set! queue (cdr queue))
	((next-cc)))))

(define (next) (current-continuation))

(define (render body)
  (let ((now (current-continuation)))
    (print body)
    (yield)))

(define-syntax node
  (syntax-rules ()
    ((node (var) body)
     (let ((var (call/cc
		 (lambda (k)		    
		   (put! (quote var) 'conts
			 (cons k (or (get (quote var) 'conts) '())))
		   (print "Setting value")
		   "..."))))
       (print body)
       (yield)))))

;(define-syntax compose-vars
;  (syntax-rules ()
;    ((compose-vars vars ...)
;     ((let

(define (boot)
  ;;(render (lambda ()
  (put! 'str 'conts '())
  (render (string-append "george " (get-var 'str)))
  ;;(render (lambda ()
  (render (string-append "jim " (get-var 'str)))
  (render (string-append "joe " (get-var 'str))))

(define (update)
  (send-var 'str " is cool"))

;; (let ((x 10)) (fluid-let ((x (call/cc (lambda (k) (set! here k) 5)))) (+ 5 x)))
;; (here 20)
;; remember <div render="map" data-x="name" data-y="place"> ...

;(render (string-append "jim" (call/cc
;			      (lambda (k)
;				(print "setting")
;				(put! x 'conts (cons k (get x 'conts)))
;				"..."))))

(define (current-continuation)
  (call/cc (lambda (cc) (cc cc))))

(define (compose-signals1 var vars)
  (map (lambda (v)
	 (put! v 'partial-compositions (list var)))
       vars)
  (put! var 'composition (lambda (bindings) (compose-bindings var bindings))))
;  (compose-bindings var '()))

(define (compose-bindings var bindings)
  (let ((bindings '()))    
    (let ((signals (call/cc (lambda (k) (put! var 'composition k)
				    '()))))
      (set! bindings (merge signals bindings))
      (send-composed-bindings bindings))))





(define (compose-signals var vars)
  (let ((bindings '()))    
    (let ((signals (call/cc (lambda (k) (map (lambda (var)
					       (put! var 'partial-compositions (list k)))
					     vars)
				    '()))))
      (set! bindings (merge signals bindings))
      (send-composed-bindings bindings))))

(define (send-composed-bindings bindings)
  (print "BINDINGS")
  (print bindings))

(define merge append)

(define (get-binding binding-name bindings)
  (let ((binding-pair (assoc binding-name bindings)))
    (when binding-pair (cdr binding-pair))))

(define (send-bindings var bindings)
  ((get var 'composition) bindings))

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
				 (k (map (lambda (var) (cons var (get-binding var var-bindings)))
					 var-names)))))
		  continuations))))
       (merge-continuation-lists
	(map (lambda (var-pair)
	       (let ((var (car var-pair)))
		 (cons var (get var 'partial-compositions))))
	     var-bindings))))



(define *queue* '())

(define (*enqueue* x)
  (set! *queue* (append *queue* (list x))))

(define (*dequeue*)
  (let ((first (car *queue*)))
    (set! *queue* (cdr *queue*))
    first))

(define-syntax cons-save
  (syntax-rules ()
    ((cons-save top k kar kdr)
     (let ((rest kdr))
       (let ((first (call/cc (lambda (cc) (put! k 'cont cc) kar))))
	 (top (cons first rest)))))))
