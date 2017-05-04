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
