(in-package :expr-dsl)

(defparameter *dsl*
  "
= symbolexpr
  SYMBOL symbolSave
  [ ?'.' 
    '.'
    emitFieldRefOpen
    @symbolexpr
    emitFieldRefClose
  | * 
    symbolEmitSavedSymbol
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
  ((savedSymbol :accessor savedSymbol)))

(defmethod initially ((self expr-parser) token-list)
  (setf (savedSymbol self) nil)
  (call-next-method))

(defmethod emitFieldRefOpen  ((self parser)) (pasm:emit-string self "(slot-value '"))
(defmethod emitFieldRefClose ((self parser)) (pasm:emit-string self ")"))
(defmethod emitSymbol ((self parser)) (pasm:emit-string self (scanner:token-text (pasm:accepted-token self))))
(defmethod emitAcceptedToken ((self parser)) 
  (pasm:emit-string self (scanner:token-text (pasm:accepted-token self))))
(defmethod symbolSave ((self parser)) (setf (savedSymbol self) (scanner:token-text (pasm:accepted-token self))))
(defmethod symbolEmitSavedSymbol ((self parser)) (pasm:emit-string self (savedSymbol self)))
;; end mechanisms


