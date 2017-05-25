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

(define (open-close-switch files file children)
  (cond ((null? children)
	 (<a> (% "props" (% "href" "#")
		 "on" (% "click" (cb (newfile file-to-close current-files)
				     (lambda (event)
				       (values (cons (.url file) (.path file)) #f files)))))
	      "+"))
	((equal? children 'loading) "...")
	(else (<a>  (% "props" (% "href" "#")
		       "on" (% "click" (cb (newfile file-to-close current-files)
					   (lambda (event)
					     (values #f (.path file) files)))))
		    "-"))))

(define (file-explorer files)
  (lambda (file-pair)
    (let ((file (car file-pair))
	  (children (cdr file-pair)))
      (if (equal? (.type file) "dir")
	  (h "li.dir" #f
	     (vector
	      (open-close-switch files file children)
	      (.path file)
	      (h "ul.files" #f (map (file-explorer files) children))))
	  (<li> #f (.path file))))))

(register-component "explorer"
  (render-this (files)
(begin (print "RENDERING") (log files)
	       (<ul> #f
      (map (file-explorer files) files)))) )

(init '())
 
(catch-vars (user)
	    (when user
	      (let ((url (string-append
			  "https://api.github.com/users/" user "/repos")))
		(ajax "GET" (jstring url)
                      (lambda (response)
                        (send (repos)
                              (begin
                                (log (.response (.currentTarget response)))
                                (.response (.currentTarget response)))))))))


(catch-vars (user repo)
	    (when (and user repo)
	      (let ((url (string-append
			  "https://api.github.com/repos/" user "/" repo "/contents")))
		(ajax "GET" (jstring url)
                      (lambda (response)
                        (send (raw-files)
                              (begin
                                (log (.response (.currentTarget response)))
                                (.response (.currentTarget response)))))))))

(define (list-files files)
  (map (lambda (f)
	 (cons f '()))
       files))

(catch-vars (raw-files)
	    (when raw-files
	      (send (files)
		    (list-files raw-files))))

(define (substring=? str1 str2)
  (equal? str1 (substring str2 0 (string-length str1))))

(define (insert-files current-files newfiles newfile-path)
  (map (lambda (file-pair)
	 (let ((file (car file-pair))
	       (children (cdr file-pair)))
	   ;; (print "PAIR") (log file-pair) (log (.path file)) (log children)
	   (cond ((equal? newfile-path (.path file))
		  (begin (print "Equal") (log file) (print newfile-path)
			 (cons file (list-files newfiles))))
		 ((substring=? (.path file) newfile-path)
		  (cons file (insert-files children newfiles newfile-path)))
	       (else file-pair))))
  current-files))

;;  (map (lambda (f)
;;	 (cond ((equal? (.path f) "b")
;;		(cons f newfiles))
;;	       ((equal (substring (.path f) 0 (string-length .)))
;;		(cons f 5))
;;	       (else f)))
;;       current-files))


(catch-vars (current-files newfile)
  (when newfile
    (ajax "GET" (jstring (car newfile))
          (lambda (response)
            (send (files)
                  (begin
                    (log response)
                    (let ((newfiles (.response (.currentTarget response))))
                      (insert-files current-files newfiles (cdr newfile)))))))))

(define (remove-files current-files file-to-close)
  (map (lambda (file-pair)
	 (let ((file (car file-pair))
	       (children (cdr file-pair)))
	   (cond ((equal? file-to-close (.path file))
		  (cons file '()))
		 ((substring=? (.path file) file-to-close)
		  (cons file (remove-files children file-to-close)))
	       (else file-pair))))
  current-files))


(catch-vars (current-files file-to-close)
  (when file-to-close
    (send (files)
	  (remove-files current-files file-to-close))))

(catch-vars (bob)
	    (print "BOB")
	    (log bob))
	
(start)
