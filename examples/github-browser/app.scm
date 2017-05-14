;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; App

(register-component "change-repository"
  (bind-this .onchange ()
	     (cb (repo)
	      (lambda (event)
		(.value (.target event))))))
	       ;(let ((repo (.value
					;;(send (repository) repo)


(register-component "explorer"
  (render-this (files)
    (<div> #f
      (map (lambda (file-pair)
	     (let ((file (car file-pair))
		   (subfiles (cdr file-pair)))
	     (if (equal? (.type file) "dir")
		   (<div> (% "on"
			     (% "click" (cb (newfile current-files)
					    (values (.url file) files))))
			  (.path file))
		   (<div> #f (.path file)))))
	   files))))

(init  `((repository . "cm-typographic-studio/Cetri"))

       )

(catch-vars (repo)
	    (when repo
	      (let ((url (string-append
			  "https://api.github.com/repos/" repo "/contents")))
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


(catch-vars (newfile current-files)
  (when newfile
    (ajax "GET" (jstring newfile)
	  (cb (bob)
	      (lambda (response)
		(let ((newfiles (.response (.currentTarget response))))
		  (insert-files current-files newfiles newfile)))))))
(catch-vars (bob)
	    (print "BOB")
	    (log bob))
	
(start)
