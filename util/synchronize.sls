(library (scheme-langserver util synchronize)
    (export 
        make-reader-writer-lock
        with-lock-write
        with-lock-read)
    (import (chezscheme))

; https://www.cnblogs.com/fortunely/p/15778050.html#%E4%BD%BF%E7%94%A81%E4%B8%AAmutex--2%E4%B8%AA%E6%9D%A1%E4%BB%B6%E5%8F%98%E9%87%8F
(define-record-type reader-writer-lock
    (fields 
    ;0 not locked
    ;-1 locked by writer
    ;>0 locked by reader 
        (mutable reader-count)
        (mutable waiting-reader-count)
        (mutable waiting-writer-count)
        (immutable mutex)
        (immutable read-condition)
        (immutable write-condition))
    (protocol
        (lambda (new)
            (lambda ()
            (new 0 0 0 (make-mutex) (make-condition) (make-condition))))))

(define-syntax with-lock-read
    (syntax-rules ()
        [(_ lock e0 e1 ...) 
            (begin 
                (reader-lock lock)
                e0 e1 ...
                (release-lock lock))]))

(define-syntax with-lock-write
    (syntax-rules ()
        [(_ lock e0 e1 ...) 
            (begin 
                (writer-lock lock)
                e0 e1 ...
                (release-lock lock))]))

(define (reader-lock lock) 
    (with-mutex (reader-writer-lock-mutex lock)
        (let loop ()
            (if (and (< (reader-writer-lock-reader-count lock) 0)
                    (> (reader-writer-lock-waiting-writer-count lock) 0))
                (begin
                    (reader-writer-lock-waiting-reader-count-set! 
                        lock 
                        (+ (reader-writer-lock-waiting-reader-count lock) 1))

                    (condition-wait (reader-writer-lock-read-condition lock) (reader-writer-lock-mutex lock))

                    (reader-writer-lock-waiting-reader-count-set! 
                        lock 
                        (- (reader-writer-lock-waiting-reader-count lock) 1))
                    (loop))))
        (reader-writer-lock-reader-count-set! 
            lock 
            (+ (reader-writer-lock-reader-count lock) 1))))

; // 尝试获取读锁，失败立即返回
; bool RWLock::tryrdlock()
; {
; 	bool res = true;
; 	rw_mutex.lock();
; 	{
; 		if (rw_refcount < 0 || rw_nwaitwriters > 0) { // 写优先
; 			res = false; /* held by a writer or waiting writers */
; 		}
; 		else {
; 			rw_refcount++; /* increment count of reader locks */
; 		}
; 	}
; 	rw_mutex.unlock();
; 	return res;
; }
; // 尝试获取写锁，失败立即返回
; bool RWLock::trywrlock()
; {
; 	bool res = true;
; 	rw_mutex.lock();
; 	{
; 		if (rw_refcount != 0) /* the lock is busy */
; 			res = false;
; 		else
; 			rw_refcount = -1; /* acquire the wr lock */
; 	}
; 	rw_mutex.unlock();
; 	return res;
; }

(define (writer-lock lock) 
    (with-mutex (reader-writer-lock-mutex lock)
        (let loop ()
            (if (not (zero? (reader-writer-lock-reader-count lock)))
                (begin
                    (reader-writer-lock-waiting-writer-count-set! 
                        lock 
                        (+ (reader-writer-lock-waiting-writer-count lock) 1))

                    (condition-wait (reader-writer-lock-write-condition lock) (reader-writer-lock-mutex lock))

                    (reader-writer-lock-waiting-writer-count-set! 
                        lock 
                        (- (reader-writer-lock-waiting-writer-count lock) 1))
                    (loop))))
        (reader-writer-lock-reader-count-set! lock -1)))


(define (release-lock lock) 
    (with-mutex (reader-writer-lock-mutex lock)
		;; give preference to waiting writers over waiting readers 
        (cond
            [(> (reader-writer-lock-reader-count lock) 0) 
                (reader-writer-lock-reader-count-set! 
                    lock 
                    (- (reader-writer-lock-reader-count lock) 1))]
            [(= (reader-writer-lock-reader-count lock) -1) 
                (reader-writer-lock-reader-count-set! 
                    lock 
                    0)]
            [else (raise 'unknown-error)])

        (cond
            [(and (> (reader-writer-lock-waiting-writer-count lock) 0) 
                    (zero? (reader-writer-lock-reader-count lock)))
                (condition-signal (reader-writer-lock-write-condition lock))]
            [else (condition-broadcast (reader-writer-lock-read-condition lock))])))
)