;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Github API

(define (github-api-call uri proc)
  (ajax 'GET uri
	(lambda (response)
	  (proc (.response (.currentTarget response))))))

(define (github-api-repos user proc)
  (github-api-call
   (string-append "https://api.github.com/users/" user "/repos")
   proc))

(define (github-api-repo user repo proc)
  (github-api-call
   (string-append "https://api.github.com/repos/" user "/" repo "/contents")
   proc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Model

(define top-node  (% "path" ""))

(define (make-tree file children)
  (cons file children))

(define (empty-tree file)
  (make-tree file '()))

(define (tree-obj filetree)
  (car filetree))

(define (tree-path node)
  (.path (tree-obj node)))

(define (tree-children filetree)
  (cdr filetree))

(define (tree-replace-children node children)
  (make-tree (tree-obj node) children))

(define (substring=? str1 str2)
  (equal? str1 (substring str2 0 (string-length str1))))

(define (tree-insert-children filetree children path)
  (tree-replace-children
   filetree
   (map (lambda (node)
	  (cond ((equal? (tree-path node) path)
		 (tree-replace-children
		  node (if (or (pair? children) (vector? children))
			   (map empty-tree children)
			   children)))
		((substring=? (tree-path node) path)
		 (tree-insert-children node children path))
		(else node)))
	(tree-children filetree))))

(define (tree-remove-children filetree path)
  (tree-replace-children
   filetree
   (map (lambda (node)
	  (cond ((equal? path (tree-path node))
		 (tree-replace-children node '()))
		((substring=? (tree-path node) path)
		 (tree-remove-children node path))
		(else node)))
	(tree-children filetree))))

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
		 (let ((filetree (make-tree top-node
				      (map empty-tree response))))
		 (send (files)
		       filetree))))))

(catch-vars
 (files file-to-open)
 (when file-to-open
   (send (file-to-open files)
	 (github-api-call
	  (.url file-to-open)
	  (lambda (response)
	    (send (files)
		  (tree-insert-children
		   files response
		   (.path file-to-open)))))
		    
	 (values #f
		 (tree-insert-children
		  files 'loading
		  (.path file-to-open))))))

(catch-vars
 (files file-to-close)
 (when file-to-close
   (send (file-to-close files)
	 (values #f
		 (tree-remove-children
		  files (.path file-to-close))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; App

(register-component
 "change-user"
 (render-this
  (user)
  (<input> #f (% "props" (% "type" "text" "value" (or user ""))
                 "on" (% "change"
                         (callback
                          (lambda (event)
                            (set-hash! (.value (.target event)))
                            (send (user)
                                  (.value (.target event))))))))))

(register-component "choose-repository"
  (render-this (repos)
    (<select> #f (% "on" (% "change" (callback
				   (lambda (event)
				     (send (repo)
					   (.value (.target event)))))))
	      (cons (<option> #f (% "props" (% "value" #f))  "----")
		    (map (lambda (repo)
			   (<option> #f #f (.name repo)))
			 (if (vector? repos)
			     (vector->list repos)
			     (vector)))))))

(define (open-close-switch node)
  (cond ((null? (tree-children node))
	 (a-click #f "+"
		  (lambda (event)
		    (send (file-to-open) (tree-obj node)))))
	((equal? children 'loading) "...")
	(else (a-click #f "-"
		       (lambda (event)
			 (send (file-to-close) (tree-obj node)))))))

(define (file-explorer node)
  (when node
    (if (equal? (.type (tree-obj node)) "dir")
	(<li> ".dir"
	 #f
	 (vector
	  (open-close-switch node)
	  (tree-path node)
	  (<ul> ".files"
	   #f
	   (if (or (pair? (tree-children node)) (vector? (tree-children node)))
	       (map file-explorer (tree-children node))
	       '()))))
	(<li> #f #f
	      (vector
	       (a-click #f (tree-path node)
		       (lambda (event)
			 (github-api-call
			  (.url (tree-obj node))
			  (lambda (response)
			    (send (file)
				  (%inline atob (.content response))))))))))))

(register-component
 "explorer"
 (render-this
  (files)
  (<ul> #f #f
	(map file-explorer (tree-children files)))))

(register-component
 "editor"
 (render-this (file)
	      (<pre> #f #f file)))


;; more serious - hash/router/path...
;; i.e., match path = /user/:name/:repo#file/:id
;; with path => '((name . "name") (repo . "repo") (id . "id"))
;; and round-tripping

(log window.location.pathname)
(map log (get-path))

(init ((user (get-hash))))


