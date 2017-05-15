;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(register-component "explorer"
  (render-this (files)
    (<div> #f
      (map (lambda (file-pair)
	     (let ((file (car file-pair))
		   (subfiles (cdr file-pair)))
	     (if (equal? (.type file) "dir")
		 (<div> #f
			(vector
			 (<a> (% "props" (% "href" "#")
				 "on" (% "click" (cb (newfile current-files)
						     (lambda (event)
						       (values (.url file) files)))))
			      (.path file))))
		   (<div> #f (.path file)))))
	   files))))

(init '())

(catch-vars (user)
	    (when user
	      (let ((url (string-append
			  "https://api.github.com/users/" user "/repos")))
		(print "AJAX")
		(ajax "GET" (jstring url)
		      (cb (repos)
			  (lambda (response)
			    (log (.response (.currentTarget response)))
			    (.response (.currentTarget response))))))))


(catch-vars (user repo)
	    (when (and user repo)
	      (let ((url (string-append
			  "https://api.github.com/repos/" user "/" repo "/contents")))
		(print "AJAX")
		(ajax "GET" (jstring url)
		      (cb (raw-files)
			  (lambda (response)
			    (log (.response (.currentTarget response)))
			    (.response (.currentTarget response))))))))

(define (list-files files)
  (map (lambda (f)
	 (cons f '()))
       files))

(catch-vars (raw-files)
	    (when raw-files
	      (send (files)
		    (list-files raw-files))))

(define (insert-files current-files newfiles newfile)
  current-files)

;;  (map (lambda (f)
;;	 (cond ((equal? (.path f) "b")
;;		(cons f newfiles))
;;	       ((equal (substring (.path f) 0 (string-length .)))
;;		(cons f 5))
;;	       (else f)))
;;       current-files))


(catch-vars (current-files newfile)
  (when newfile
    (print "AJAX2")
    (ajax "GET" (jstring newfile)
	  (cb (bobby)
	      (lambda (response)
		(log response)
		(let ((newfiles (.response (.currentTarget response))))
		  (insert-files current-files newfiles newfile)))))))
(catch-vars (bob)
	    (print "BOB")
	    (log bob))
	
(start)
