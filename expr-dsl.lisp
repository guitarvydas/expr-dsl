(in-package :expr-dsl)

(defparameter *dsl*
  "
= symbolexpr
  SYMBOL symbolPush
  [ ?'.' 
    '.'
    emitFieldRefOpen
    @symbolexpr
    emitSpace
    symbolEmit
    symbolPop
    emitFieldRefClose
  | * 
    symbolEmit
    symbolPop
  ]

= expression
  '{' 
  {[ ?'{' @expression
   | ?'}' >
   | ?SYMBOL @symbolexpr
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
  ((symbolStack :accessor symbolStack)))

(defmethod initially ((self expr-parser) token-list)
  (setf (symbolStack self) nil)
  (call-next-method))

(defmethod emitFieldRefOpen  ((self parser)) (pasm:emit-string self " (slot-value '"))
(defmethod emitFieldRefClose ((self parser)) (pasm:emit-string self ") "))
(defmethod emitSymbol ((self parser)) (pasm:emit-string self (scanner:token-text (pasm:accepted-token self))))
(defmethod emitAcceptedToken ((self parser)) 
  (pasm:emit-string self (scanner:token-text (pasm:accepted-token self))))
(defmethod symbolPush ((self parser)) (push (scanner:token-text (pasm:accepted-token self)) (symbolStack self)))
(defmethod symbolPop  ((self parser)) (pop (symbolStack self)))
(defmethod symbolEmit ((self parser)) (pasm:emit-string self (first (symbolStack self))))
(defmethod emitSpace ((self parser)) (pasm:emit-string self " "))
;; end mechanisms


