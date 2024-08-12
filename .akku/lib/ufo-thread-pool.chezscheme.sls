#!r6rs

(library (ufo-thread-pool)
	(export 
    	init-thread-pool
		thread-pool?
		thread-pool-blocked?-ref
		thread-pool-blocked?-change
   		thread-pool-thread-number-ref
   		thread-pool-job-number-ref 
   		thread-pool-size-ref
    	thread-pool-size-add
		thread-pool-add-job
   		thread-pool-stop!
   		with-thread-pool-increment)
	(import 
		(chezscheme) 
		(ufo-thread-pool util blocking-queue) 
		(ufo-thread-pool util try)
		(only (srfi :13 strings) string-drop-right string-suffix?))

(define-record-type thread-pool
  	(fields 
    	(immutable mutex)
    	(immutable condition)
    	(immutable job-queue)
    	(mutable size)
	  	;; 'thread-number' may be different to 'size' if 'size' has recently been changed by the user
    	(mutable thread-number)
    	(mutable job-number)
    	(mutable blocked?)
    	(mutable stopped?)))

(define init-thread-pool
    (case-lambda
    	[() 
			(if (linux?)
				(let-values ([(in out err pid) (open-process-ports "nproc" 'block (make-transcoder (utf-8-codec)))])
					(init-thread-pool (string->number (string-drop-right (get-string-all out) 1)) #t))
				(raise "init-thread-pool without pool-size only works for Linux null!"))]
    	[(size) (init-thread-pool size #t)]
      	[(size blocked)
      		(when (< size 1)
        		(raise 
          			(condition (make-violation) (make-who-condition "init-thread-pool") (make-message-condition "size argument for init-thread-pool is less than 1"))))
      		(let 
    			([pool (make-thread-pool (make-mutex) (make-condition) (init-blocking-queue) size 0 0 blocked #f)])
        		(with-exception-handler
	        		(lambda (c)
	        		;; if starting any new threads failed, kill them all before re-raising the exception
	            		(do ([kill-count 0 (+ kill-count 1)]) ((= kill-count (thread-pool-thread-number pool)))
	              			(blocking-queue-push (thread-pool-job-queue pool) (cons (lambda () (raise 'kill-thread)) #f)))
	            		(raise c))
	        		(lambda ()
	          			(do ([count 0 (+ count 1)]) ((= count size))
	     					(fork-thread (lambda () (thread-loop pool)))
	     					(thread-pool-thread-number-set! pool (+ (thread-pool-thread-number pool) 1)))))
       			pool)]))

(define (object->string obj)
	(call-with-string-output-port
     	(lambda (p) (put-datum p obj))))

(define (thread-loop pool)
  	(call/1cc
   		(lambda (return)
       		(let loop ([job (blocking-queue-pop (thread-pool-job-queue pool))])
	 			(try
					;; doing job, but some exception might be raised, so, try to catch them
	  				((car job))
	  				(except c
		  				[(eq? c 'kill-thread)
		   				;; we don't decrement job here, as adding a 'kill-thread callback does not increment the number of tasks
		   					(with-mutex (thread-pool-mutex pool) 
		     					(thread-pool-thread-number-set! pool (- (thread-pool-thread-number pool) 1))
		     					(when (and (thread-pool-stopped? pool) (thread-pool-blocked? pool))
		       						(condition-broadcast (thread-pool-condition pool)))
		     					(return #f))]
		  				[else
		   					(let ([fail-handler (cdr job)])
		     					(if fail-handler
			 						(fail-handler c)
			 						(error "thread-loop" (string-append "Exception raised by thread pool task with no fail-handler: " (object->string c)))))]))
		   		(with-mutex (thread-pool-mutex pool) 
	   				(thread-pool-job-number-set! pool (- (thread-pool-job-number pool) 1))
	   				;; cater for a case where the number of threads in the pool has been reduced by the user
	   				(when (> (thread-pool-thread-number pool) (thread-pool-size pool))
	     				(thread-pool-thread-number-set! pool (- (thread-pool-thread-number pool) 1))
	     				(when (and (thread-pool-stopped? pool) (thread-pool-blocked? pool))
	       					(condition-broadcast (thread-pool-condition pool)))
	     				(return #f)))
	 				;; we are outside the mutex here
	 			(loop (blocking-queue-pop (thread-pool-job-queue pool)))))))

(define (thread-pool-size-add pool value)
    ;; to minimize contention, we want to start any new threads outside the mutex, but do the book-keeping within the mutex
	(let ([start-threads
	   		(with-mutex (thread-pool-mutex pool) 
	     		(cond
	      			[(thread-pool-stopped? pool) #f]
	      ;; if size is 0 because of an exception when trying to
	      ;; start new threads (see below) and a negative delta is
	      ;; passed, just do nothing
	      			[(and (zero? (thread-pool-size pool)) (< value 0)) #f]
	      			[(< value 0)
	       				(let* ([current-size (thread-pool-size pool)]
		      					[new-size (max 1 (+ current-size value))]
		      					[job-number (thread-pool-job-number pool)]
		      					[push-jobs
									(let ([difference (- current-size new-size)])
				    					(if (>= job-number difference) 
											0 
											(- difference job-number)))])
		 					(thread-pool-size-set! pool new-size)
		 					(do ([count 0 (+ count 1)]) ((= count push-jobs))
		   						(blocking-queue-push (thread-pool-job-queue pool) (cons (lambda () #f) #f))
		   						(thread-pool-job-number-set! pool (+ job-number 1))))
	       				#f]
	      			[(> value 0)
	       ;; don't return 'value' directly as the 'start-threads' value - the pool size cannot be more than thread-number, but it can be less if the pool size has
	       ;; recently been reduced but insufficient tasks have yet finished to thread-number to the pool size. It is more efficient only to start the number of
	       ;; threads representing the ones actually still required.
	       				(let* ([new-size (+ (thread-pool-size pool) value)]
		      					[start-threads (- new-size (thread-pool-thread-number pool))])
		 					(thread-pool-size-set! pool new-size)
		 					(if (> start-threads 0)
		     					(begin
		 							(thread-pool-thread-number-set! pool new-size)
		       						start-threads)
		     					#f))]
	      			[else #f]))])
      	(when start-threads
			(do ([count 0 (+ count 1)]) ((= count start-threads))
	  			(with-exception-handler
	    			(lambda (c)
	      			;; roll back for any unstarted threads
	      				(with-mutex (thread-pool-mutex pool)
							(thread-pool-thread-number-set! pool (+ (- (thread-pool-thread-number pool) start-threads) count))
					;; We could be down to 0 threads if all of these unfortunate events have occurred together: (i) in
					;; the period between this calling thread releasing the mutex acquired on entry to this procedure and
					;; acquiring it again on handling this exception, another thread tried, concurrently with this
					;; attemptthread-pool-blocking?during that period a number of tasks equal to that original size have finished, and
					;; (iii) the attempt to launch new threads failed with an exception without launching even one of them.
					;; In such a case we should be able to launch a rescue thread within the mutex because no other threads
					;; could be running in the pool.  If we still cannot launch a thread the program and/or system must be
					;; totally borked and there is little we can do.
							(when (zero? (thread-pool-thread-number pool))
		  			;; if this fails, all is lost (that is, we may have queued tasks in the pool with no thread startable
		  			;; to run them)
		  						(try 
		   							(fork-thread (lambda () (thread-loop pool)))
		   							(thread-pool-thread-number-set! pool 1)
		   							(except c [else #f])))
					;; reset size to the actual number of threads now running after (thread-loop pool)
							(thread-pool-size-set! pool (thread-pool-thread-number pool))
							(when (and (thread-pool-stopped? pool) (thread-pool-blocked? pool))
		  						(condition-broadcast (thread-pool-condition pool))))
	      				(raise c))
	    			(lambda () (fork-thread (lambda () (thread-loop pool)))))))))

(define (thread-pool-blocked?-ref pool)
  	(with-mutex (thread-pool-mutex pool)
    	(thread-pool-blocked? pool)))

(define (thread-pool-thread-number-ref pool)
  	(with-mutex (thread-pool-mutex pool)
    	(thread-pool-thread-number pool)))

(define (thread-pool-job-number-ref pool)
  	(with-mutex (thread-pool-mutex pool)
    	(thread-pool-job-number pool)))

(define (thread-pool-size-ref pool)
  	(with-mutex (thread-pool-mutex pool)
    	(thread-pool-size pool)))

(define (thread-pool-blocked?-change pool value)
  	(let ([cndn
	 		(with-mutex (thread-pool-mutex pool)
	   			(if (thread-pool-stopped? pool)
	       			(condition 
						(make-violation)
			  			(make-who-condition "thread-pool-blocked?-change")
			  			(make-message-condition "thread-pool-blocked?-change applied to a thread pool which has been closed"))
	       			(begin
		 				(thread-pool-blocked?-set! pool value)
		 				#f)))])
    	(when cndn (raise cndn))))

(define (thread-pool-stop! pool)
  	(let ([cndn
	  		(with-mutex (thread-pool-mutex pool)
	    		(if (thread-pool-stopped? pool)
					(condition (make-violation)
			   			(make-who-condition "thread-pool-stop!")
			   			(make-message-condition "thread-pool-stop! applied to a thread pool which has been closed"))
					(begin
		  				(thread-pool-stopped?-set! pool #t)
		  				(let ([thread-count (thread-pool-thread-number pool)])
		    ;; we could be adding more 'kill-thread callbacks than necessary here, because as we are doing this a concurrent
		    ;; call to thread-pool-change-size! may have failed to start a thread and raised an exception.  However, that doesn't
		    ;; matter - we just get left with a redundant callback in 'aq' which never gets used and disappears when the pool is
		    ;; garbage collected
		    				(do ([kill-count 0 (+ kill-count 1)]) ((= kill-count thread-count))
		      					(blocking-queue-push (thread-pool-job-queue pool) (cons (lambda () (raise 'kill-thread)) #f)))
		    				(when (thread-pool-blocked? pool)
		      					(do () ((= (thread-pool-thread-number pool) 0))
									(condition-wait (thread-pool-condition pool) (thread-pool-mutex pool)))))
		  				#f)))])
    	(when cndn (raise cndn))))

(define thread-pool-add-job
  	(case-lambda
    	[(pool job) (thread-pool-add-job pool job #f)]
    	[(pool job fail-handler)
     		(let ([cndn
	    ;; check the pool has not been closed and increment the task count under a single mutex locking operation to
	    ;; ensure atomicity within the pool
	    			(with-mutex (thread-pool-mutex pool)
	      				(if (thread-pool-stopped? pool)
		  					(condition (make-violation)
			     				(make-who-condition "thread-pool-add!")
			     				(make-message-condition "thread-pool-add! applied to a thread pool which has been closed"))
		  					(begin
		    ;; We need to hold the mutex when adding the task so that
		    ;; the whole operation is atomic - otherwise if
		    ;; thread-pool-stop! is called concurrently with
		    ;; thread-pool-add!, we cannot guarantee that a task will
		    ;; either run or a violation exception will be raised.  We
		    ;; must give this guarantee for await-task-in-thread-pool!
		    ;; to work correctly.  That is not too much of an additional
		    ;; point of contention, because a-queue-push! is itself
		    ;; serialized.
		    					(blocking-queue-push (thread-pool-job-queue pool) (cons job fail-handler))
		    					(thread-pool-job-number-set! pool (+ (thread-pool-job-number pool) 1))
		    					#f)))])
       			(when cndn (raise cndn)))]))

(define-syntax with-thread-pool-increment
  	(syntax-rules ()
    	[(_ pool body0 body1 ...)
     		(let ([p pool])
		;; The (i) increment, (ii) execution of body clauses, and (iii) decrement, form the three branches of a dynamic-wind, so the
		;; decrement automatically occurs if control leaves body execution because of an exception or other jump.
       			(dynamic-wind
					;;in
	 				(lambda () (thread-pool-size-add p 1))
					;;body
	 				(lambda () body0 body1 ...)
					;;out
	 				(lambda () (thread-pool-size-add p -1))))]))

(define (linux?)
  (string-suffix? "le" (symbol->string (machine-type))))
)