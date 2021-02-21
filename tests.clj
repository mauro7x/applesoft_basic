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

(deftest test-cargar-linea
   (is (=
      (cargar-linea '(10 (PRINT X)) [() [:ejecucion-inmediata 0] [] [] [] 0 {}])
      '[((10 (PRINT X))) [:ejecucion-inmediata 0] [] [] [] 0 {}]
   ))
   (is (=
      (cargar-linea '(20 (X = 100)) ['((10 (PRINT X))) [:ejecucion-inmediata 0] [] [] [] 0 {}])
      '[((10 (PRINT X)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}]
   ))
   (is (=
      (cargar-linea '(15 (X = X + 1)) ['((10 (PRINT X)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}])
      '[((10 (PRINT X)) (15 (X = X + 1)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}]
   ))
   (is (=
      (cargar-linea '(15 (X = X - 1)) ['((10 (PRINT X)) (15 (X = X + 1)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}])
      '[((10 (PRINT X)) (15 (X = X - 1)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}]
   ))
)

(deftest test-expandir-nexts
   (is (=
      (expandir-nexts (list '(PRINT 1) (list 'NEXT 'A (symbol ",") 'B)))
      '((PRINT 1) (NEXT A) (NEXT B))
   ))
   (is (=
      (expandir-nexts (list '(PRINT 1) '(PRINT 2) '(PRINT 3)))
      '((PRINT 1) (PRINT 2) (PRINT 3))
   ))
   (is (=
      (expandir-nexts (list '(PRINT 1) '(NEXT A , B , C) '(PRINT 3)))
      '((PRINT 1) (NEXT A) (NEXT B) (NEXT C) (PRINT 3))
   ))
   (is (=
      (expandir-nexts (list '(PRINT 1) '(NEXT A , B , C)))
      '((PRINT 1) (NEXT A) (NEXT B) (NEXT C))
   ))
   (is (=
      (expandir-nexts (list '(NEXT A , B , C) '(PRINT 1)))
      '((NEXT A) (NEXT B) (NEXT C) (PRINT 1))
   ))
   (is (=
      (expandir-nexts (list '(NEXT A , B , C)))
      '((NEXT A) (NEXT B) (NEXT C))
   ))
   (is (=
      (expandir-nexts (list '(NEXT A)))
      '((NEXT A))
   ))
)


; dar-error: recibe un error (codigo o mensaje) y el puntero de 
; programa, muestra el error correspondiente y retorna nil, por
; ejemplo:
; user=> (dar-error 16 [:ejecucion-inmediata 4])
;
; ?SYNTAX ERRORnil
; user=> (dar-error "?ERROR DISK FULL" [:ejecucion-inmediata 4])
;
; ?ERROR DISK FULLnil
; user=> (dar-error 16 [100 3])
;
; ?SYNTAX ERROR IN 100nil
; user=> (dar-error "?ERROR DISK FULL" [100 3])
;
; ?ERROR DISK FULL IN 100nil
(deftest test-dar-error
   ;; Cómo testeamos input?
)

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

(deftest test-contar-sentencias
   (is (= 2 (contar-sentencias 10 [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}])))
   (is (= 1 (contar-sentencias 15 [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}])))
   (is (= 2 (contar-sentencias 20 [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}])))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                      TESTS DE INTEGRACIÓN                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(run-tests)
  