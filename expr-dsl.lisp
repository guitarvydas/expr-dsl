(in-package :expr-dsl)

(defparameter *dsl*
  "
= expr
  {[ ?EOF EOF >
   | ?'{' @expression '}'
   | * >
  ]}

= expression
  '{' 
  {[ ?'{' @expression
   | ?SYMBOL @symbolexpr
   | * >
  ]}  
  '}'


= symbolexpr
  SYMBOL
  [ ?'.' 
    '.'
    emitFieldRefOpen
    @symbolexpr
    emitFieldRefClose
  | * 
    emitSymbol
  ]

")

;; mechanisms
(defclass expr-parser (pasm:parser) ())

(defmethod emitFieldRefOpen  ((self parser)) (pasm:emit-string self "(slot-value "))
(defmethod emitFieldRefClose ((self parser)) (pasm:emit-string self ")"))
(defmethod emitSymbol ((self parser)) (pasm:emit-string self (scanner:token-text (pasm:accepted-token self))))
;; end mechanisms


