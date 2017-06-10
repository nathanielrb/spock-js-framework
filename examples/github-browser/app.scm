;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Github API

(define (github-api-call uri cb)
  (ajax "GET" (jstring uri)
	(lambda (response)
	  (log (.response (.currentTarget response)))
	  (cb (.response (.currentTarget response))))))

(define (github-api-repos user cb)
  (github-api-call
   (string-append "https://api.github.com/users/" user "/repos")
   cb))

(define (github-api-repo user repo cb)
  (github-api-call
   (string-append "https://api.github.com/repos/" user "/" repo "/contents")
   cb))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Model

(define (make-filetree file children)
  (cons file children))

(define (make-empty-filetree file)
  (make-filetree file '()))

(define (make-filetree-children files)
  (map make-empty-filetree files))

(define (filetree-file filetree)
  (car filetree))

(define (filetree-children filetree)
  (cdr filetree))

(define (filetree-path filetree)
  (.path (filetree-file filetree)))

(define (filetree-type filetree)
  (.type (filetree-file filetree)))

(define (filetree-name filetree)
  (.name (filetree-file filetree)))

(define (substring=? str1 str2)
  (equal? str1 (substring str2 0 (string-length str1))))

(define (filetree-replace-children file children)
  (make-filetree (filetree-file file) children))

(define (filetree-insert-children filetree children path)
  (filetree-replace-children
   filetree
   (map (lambda (node)
	  (print "PATH " path " = " (filetree-path node) "?")
	  (print (equal? path (filetree-path node)))
	  (print (substring=? (filetree-path node) path))

	  (cond ((equal? path (filetree-path node))
		 (filetree-replace-children
		  node (if (or (pair? children) (vector? children))
			   (make-filetree-children children)
			   children)))
		((substring=? (filetree-path node) path)
		 (filetree-insert-children node children path))
		(else node)))
	(filetree-children filetree))))

(define (filetree-remove-children filetree path)
  (map (lambda (file-pair)
	 (cond ((equal? path (filetree-path node))
		(filetree-replace-children file '()))
		 ((substring=? (filetree-path node) path)
		  (filetree-replace-children
		   file (filetree-remove-children node path)))
		 (else node)))
       (filetree-children filetree)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; App

(register-component "change-user"
  (bind-this .onchange ()
	     (cb (user)
	      (lambda (event)
		(.value (.target event))))))

;; why can't I cons on an empty <option>???
(register-component "choose-repository"
  (render-this (repos)
    (<select> (% "on" (% "change" (cb (repo)
				      (lambda (event)
					(.value (.target event))))))
	      (cons (<option> (% "props" (% "value" #f))  "----")
		    (map (lambda (repo)
			   (<option> #f (.name repo)))
			 (if (vector? repos)
			     (vector->list repos)
			     '()))))))

(define (open-close-switch files file children)
  (cond ((null? children)
	 (a-click #f "+"
		  (cb (file-to-open)
		      (lambda (event)
			(values (cons (.url file) (.path file)))))))
	((equal? children 'loading) "...")
	(else (a-click #f "-"
		       (cb (newfile file-to-close current-files)
			   (lambda (event)
			     (values #f (.path file) files)))))))

(define (file-explorer filetree)
  (lambda (node)
    (when node
      (if (equal? (filetree-type node) "dir")
	  (h "li.dir" #f
	     (vector
	      (open-close-switch filetree
				 (filetree-file node)
				 (filetree-children node))
	      (filetree-path node)
	      (h "ul.files" #f
		 (if (or (pair? (filetree-children node)) (vector? (filetree-children node)))
		     (map (file-explorer filetree) (filetree-children node))
		     '()))))
	  (<li> #f (filetree-path node))))) )

(register-component
 "explorer"
 (render-this
  (files)
  (<ul> #f
	(map (file-explorer files)
	     (filetree-children files)))))

(init '())

(catch-vars (user)
	    (when user
	      (github-api-repos
	       user
	       (lambda (response)
		 (send (repos) response)))))

(catch-vars (user repo)
	    (when (and user repo)
	      (github-api-repo
	       user repo
	       (lambda (response)
		 (let ((filetree (make-filetree (% "path" "")
				      (make-filetree-children response))))
		 (send (files)
		       filetree))))))

;; event bus

(catch-vars (files file-to-open file-to-close)
	    (when (or file-to-open file-to-close)
	      (if file-to-open
		  (begin
		    (github-api-call
		     (car file-to-open)
		     (lambda (response)
		       (send (file-to-open file-to-close files)
			     (values #f #f
				     (filetree-insert-children
				      files response
				      (cdr file-to-open))))))
		    (send (file-to-open file-to-close files)
			  (values #f #f
				  (filetree-insert-children
				   files 'loading
				   (cdr file-to-open)))))
		  #f
		  )))

;(catch-vars (current-files file-to-open)
;	    (when file-to-open
;	      (github-api-call
;	       (car file-to-open)
;	       (lambda (response)
;		 (send (files)
;		       (filetree-insert-children
;			current-files response (cdr file-to-open)))))))

;;  (map (lambda (f)
;;	 (cond ((equal? (.path f) "b")
;;		(cons f newfiles))
;;	       ((equal (substring (.path f) 0 (string-length .)))
;;		(cons f 5))
;;	       (else f)))
;;       current-files))

;;(catch-vars (current-files file-to-close)
  ;(when file-to-close
   ; (send (files)
;	
(start)
