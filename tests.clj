(require '[clojure.test :refer [is deftest run-tests]])

(load-file "basic.clj")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                        TESTS UNITARIOS                         ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest test-palabra-reservada?
   ;; Son palabras reservadas
   (is (= true (palabra-reservada? 'REM)))
   (is (= true (palabra-reservada? 'LET)))
   (is (= true (palabra-reservada? 'LIST)))
   (is (= true (palabra-reservada? 'FOR)))
   (is (= true (palabra-reservada? 'NEXT)))
   (is (= true (palabra-reservada? 'IF)))
   (is (= true (palabra-reservada? 'NEXT)))
   (is (= true (palabra-reservada? 'ENV)))
   (is (= true (palabra-reservada? 'EXIT)))
   (is (= true (palabra-reservada? 'MID$)))
   (is (= true (palabra-reservada? 'STR$)))
   (is (= true (palabra-reservada? '+)))
   (is (= true (palabra-reservada? '-)))
   (is (= true (palabra-reservada? '=)))
   (is (= true (palabra-reservada? 'AND)))
   
   ;; No son palabras reservadas
   (is (= false (palabra-reservada? 'SPACE)))
)

(deftest test-operador?
   ;; Son operadores
   (is (= true (operador? (symbol "+"))))
   (is (= true (operador? '+)))
   (is (= true (operador? '-)))
   (is (= true (operador? '*)))
   (is (= true (operador? '/)))
   (is (= true (operador? (symbol "^"))))
   (is (= true (operador? '=)))
   (is (= true (operador? '<>)))
   (is (= true (operador? '<)))
   (is (= true (operador? '<=)))
   (is (= true (operador? '>)))
   (is (= true (operador? '>=)))
   (is (= true (operador? 'AND)))
   (is (= true (operador? 'OR)))
   
   ;; No son operadores
   (is (= false (operador? (symbol "%"))))
   (is (= false (operador? 'NOR)))
)

(deftest test-anular-invalidos
   (is (=
      (anular-invalidos '(IF X & * Y < 12 THEN LET ! X = 0))
      '(IF X nil * Y < 12 THEN LET nil X = 0)
   ))
)

;; (deftest test-cargar-linea
;;    (is (=
;;       (cargar-linea '(10 (PRINT X)) [() [:ejecucion-inmediata 0] [] [] [] 0 {}])
;;       '[((10 (PRINT X))) [:ejecucion-inmediata 0] [] [] [] 0 {}]
;;    ))
;;    (is (=
;;       (cargar-linea '(20 (X = 100)) ['((10 (PRINT X))) [:ejecucion-inmediata 0] [] [] [] 0 {}])
;;       '[((10 (PRINT X)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}]
;;    ))
;;    (is (=
;;       (cargar-linea '(15 (X = X + 1)) ['((10 (PRINT X)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}])
;;       '[((10 (PRINT X)) (15 (X = X + 1)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}]
;;    ))
;;    (is (=
;;       (cargar-linea '(15 (X = X - 1)) ['((10 (PRINT X)) (15 (X = X + 1)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}])
;;       '[((10 (PRINT X)) (15 (X = X - 1)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}]
;;    ))
;; )

(deftest test-variable-float?
   (is (= false (variable-float? 'X$)))
   (is (= true (variable-float? 'X)))
   (is (= false (variable-float? 'X%)))
)

(deftest test-variable-integer?
   (is (= false (variable-integer? 'X$)))
   (is (= false (variable-integer? 'X)))
   (is (= true (variable-integer? 'X%)))
)

(deftest test-variable-string?
   (is (= true (variable-string? 'X$)))
   (is (= false (variable-string? 'X)))
   (is (= false (variable-string? 'X%)))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                      TESTS DE INTEGRACIÓN                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(run-tests)
  