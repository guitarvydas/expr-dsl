(in-package :expr-dsl)

(defparameter *test-string* "
garbage more garbage
{x}
% a garbage comment
   % 3 spaces then a comment
123 % an integer then a comment
even 123 more garbage
")

(defun test ()
  (let ((p (make-instance 'expr-parser)))
    (let ((r (pasm:transpile p *dsl* *test-string* 'parsing-assembler::expr)))
      (format *standard-output* "~&      result=~a~%" r))))
