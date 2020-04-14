(in-package :expr-dsl)

(defparameter *dsl*
  "
= fieldRef
  [ ?'.' 
    '.'
    SYMBOL
    exprPushSymbol
    exprReplaceFieldRefQuoted
    @fieldRef
  | * 
  ]

= expression
  '{' 
  {[ ?'{' @expression
   | ?'}' >
   | ?SYMBOL 
     SYMBOL 
     exprPushSymbol
     @fieldRef
     exprEmit 
     exprPop
   | * >
  ]}  
  '}'


= expr
  {[ ?EOF EOF >
   | ?'{' @expression
   | * . emitAcceptedToken
  ]}


")

;; mechanisms
(defclass expr-parser (pasm:parser)
  ((exprStack :accessor exprStack)))

(defmethod initially ((self expr-parser) token-list)
  (setf (exprStack self) nil)
  (call-next-method))

(defmethod exprPushSymbol ((self parser)) (push (scanner:token-text (pasm:accepted-token self)) (exprStack self)))
(defmethod exprPop ((self parser)) (pop (exprStack self)))
(defmethod exprEmit ((self parser)) (pasm:emit-string self (first (exprStack self))))
(defmethod exprReplaceFieldRefQuoted ((self parser))
  (let ((r-op (pop (exprStack self))))
    (let ((l-op (pop (exprStack self))))
      (push (format nil " (slot-value '~a ~a) " r-op l-op) (exprStack self)))))
(defmethod emitAcceptedToken ((self parser)) 
  (pasm:emit-string self (scanner:token-text (pasm:accepted-token self))))
;; end mechanisms


