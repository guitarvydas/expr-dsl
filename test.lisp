(in-package :expr-dsl)

(defparameter *test-string4* "
garbage more garbage
... {a} ... {a.x} ...
... {a.x.y.z} ... {a} ...
% a garbage comment
   % 3 spaces then a comment
123 % an integer then a comment
even 123 more garbage
")

(defparameter *test-string3* "!@#{a.x.y.z}!@#")

(defparameter *test-string2* "!@#{x.y}!@#")

(defparameter *test-string1* "!@#{x}!@#")

(defparameter *test-string0* "
garbage more garbage
... ...
% a garbage comment
   % 3 spaces then a comment
123 % an integer then a comment
even 123 more garbage
")

(defun test ()
  (let ((p (make-instance 'expr-parser)))
    (let ((r (pasm:transpile p *dsl* *test-string0* 'expr)))
      (format *standard-output* "~&      result0=~a~%" r))
    (let ((r (pasm:transpile p *dsl* *test-string1* 'expr)))
      (format *standard-output* "~&      result1=~a~%" r))
    (let ((r (pasm:transpile p *dsl* *test-string2* 'expr)))
      (format *standard-output* "~&      result2=~a~%" r))
    (let ((r (pasm:transpile p *dsl* *test-string3* 'expr)))
      (format *standard-output* "~&      result3=~a~%" r))
    (let ((r (pasm:transpile p *dsl* *test-string4* 'expr)))
      (format *standard-output* "~&      result4=~a~%" r))))


#+nil (defun aa ()
  ;; routine to help me debug the workflow...
  (let ((p (make-instance 'expr-parser)))
    
    ;; transpile dsl spec
    (let ((scanned-pasm (scanner:scanner *dsl*)))
      (initially p scanned-pasm)
      (pasm::<pasm> p)
      (let ((dsl-program-as-string (get-output-stream-string (pasm:output-string-stream p))))
	(with-open-file (f "/tmp/temp.lisp" :direction :output :if-exists :supersede :if-does-not-exist :create)
	  (format f "(in-package :expr-dsl)~%~%")
	  (format f "~a" dsl-program-as-string))
	#+nil(format *standard-output* "~&PROGRAM:~%~s~%" dsl-program-as-string)
	;; now, the program sits in /tmp/temp.lisp
	(load "/tmp/temp.lisp")

	;; transpile dsl using transpiled spec
	(let ((scanned-dsl (scanner:scanner *test-string*)))
	  (initially p scanned-dsl)
	  (expr p)
	  (let ((expr-program-as-string (get-output-stream-string (pasm:output-string-stream p))))
	    (format *standard-output* "EXPR: ~a~%" expr-program-as-string))
	)))))

(defmethod ptrace ((self parser))
  (format *error-output* "~&trace in ~a next=(~a ~s ~a ~a) accepted=(~a ~s ~a ~a)~%"
	  (pasm:current-rule self)
	  (scanner:token-kind (pasm:next-token self)) (scanner:token-text (pasm:next-token self)) (scanner:token-line (pasm:next-token self)) (scanner:token-position (pasm:next-token self))
	  (scanner:token-kind (pasm:accepted-token self)) (scanner:token-text (pasm:accepted-token self)) (scanner:token-line (pasm:accepted-token self)) (scanner:token-position (pasm:accepted-token self))))
