(require '[clojure.test :refer [is deftest run-tests]])

(load-file "basic.clj")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                        TESTS UNITARIOS                         ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest test-palabra-reservada?
   ;; Son palabras reservadas
   (is (= true (palabra-reservada? 'REM)))
   (is (= true (palabra-reservada? 'LET/=)))
   (is (= true (palabra-reservada? 'LIST)))
   (is (= true (palabra-reservada? 'FOR/TO/NEXT/STEP)))
   (is (= true (palabra-reservada? 'IF/GOTO)))
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

;; (deftest test-anular-invalidos
;;    (is (=
;;       (anular-invalidos '(IF X & * Y < 12 THEN LET ! X = 0))
;;       '(IF X nil * Y < 12 THEN LET nil X = 0)
;;    ))
;; )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                      TESTS DE INTEGRACIÓN                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(run-tests)
  