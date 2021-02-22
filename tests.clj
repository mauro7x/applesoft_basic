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
      (expandir-nexts (list '(PRINT 1) (list 'NEXT 'A (symbol ",") 'B (symbol ",") 'C) '(PRINT 3)))
      '((PRINT 1) (NEXT A) (NEXT B) (NEXT C) (PRINT 3))
   ))
   (is (=
      (expandir-nexts (list '(PRINT 1) (list 'NEXT 'A (symbol ",") 'B (symbol ",") 'C)))
      '((PRINT 1) (NEXT A) (NEXT B) (NEXT C))
   ))
   (is (=
      (expandir-nexts (list (list 'NEXT 'A (symbol ",") 'B (symbol ",") 'C) '(PRINT 1)))
      '((NEXT A) (NEXT B) (NEXT C) (PRINT 1))
   ))
   (is (=
      (expandir-nexts (list (list 'NEXT 'A (symbol ",") 'B (symbol ",") 'C)))
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

(deftest test-buscar-lineas-restantes
   (is (=
      (buscar-lineas-restantes [() [:ejecucion-inmediata 0] [] [] [] 0 {}])
      nil
   ))
   (is (=
      (buscar-lineas-restantes ['((PRINT X) (PRINT Y)) [:ejecucion-inmediata 2] [] [] [] 0 {}])
      nil
   ))
   ;; NO SÉ SI ESTA ES LA IDEA
   ;; (is (=
   ;;    (buscar-lineas-restantes ['((PRINT X) (PRINT Y)) [:ejecucion-inmediata 1] [] [] [] 0 {}])
   ;;    ((PRINT Y))
   ;; ))
   (is (=
      (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 2] [] [] [] 0 {}])
      (list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J)))
   ))
   (is (=
      (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}])
      (list '(10 (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J)))
   ))
   (is (=
      (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 0] [] [] [] 0 {}])
      (list '(10) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J)))
   ))
   (is (=
      (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [15 1] [] [] [] 0 {}])
      (list '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J)))
   ))
   (is (=
      (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [15 0] [] [] [] 0 {}])
      (list '(15) (list 20 (list 'NEXT 'I (symbol ",") 'J)))
   ))
   (is (=
      (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [] [] [] 0 {}])
      '((20 (NEXT I) (NEXT J)))
   ))
   (is (=
      (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 2] [] [] [] 0 {}])
      '((20 (NEXT I) (NEXT J)))
   ))
   (is (=
      (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 1] [] [] [] 0 {}])
      '((20 (NEXT J)))
   ))
   (is (=
      (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 0] [] [] [] 0 {}])
      '((20))
   ))
   (is (=
      (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 -1] [] [] [] 0 {}])
      '((20))      
   ))
   (is (=
      (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [25 0] [] [] [] 0 {}])
      nil
   ))
)

(deftest test-continuar-linea
   ;; FALTA TESTEAR OUTPUT
   (is (=
      (continuar-linea [(list '(10 (PRINT X)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [] [] [] 0 {}])
      [nil [(list '(10 (PRINT X)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [] [] [] 0 {}]]
   ))
   (is (=
      (continuar-linea [(list '(10 (PRINT X)) '(15 (GOSUB 100) (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [[15 2]] [] [] 0 {}])
      [:omitir-restante [(list '(10 (PRINT X)) '(15 (GOSUB 100) (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [15 1] [] [] [] 0 {}]]
   ))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                       TESTS IN PROGRESS                        ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                        REMAINING TESTS                         ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(deftest test-extraer-data
   ; extraer-data: recibe la representación intermedia de un programa
   ; y retorna una lista con todos los valores embebidos en las
   ; sentencias DATA, por ejemplo:
   ; user=> (extraer-data '(()))
   ; ()
   ; user=> (extraer-data (list '(10 (PRINT X) (REM ESTE NO) (DATA 30)) '(20 (DATA HOLA)) (list 100 (list 'DATA 'MUNDO (symbol ",") 10 (symbol ",") 20))))
   ; ("HOLA" "MUNDO" 10 20)
)

(deftest test-ejecutar-asignacion
   ; ejecutar-asignacion: recibe una asignacion y un ambiente, y
   ; retorna el ambiente actualizado al efectuar la asignacion, por
   ; ejemplo:
   ; user=> (ejecutar-asignacion '(X = 5) ['((10 (PRINT X))) [10 1] [] [] [] 0 {}])
   ; [((10 (PRINT X))) [10 1] [] [] [] 0 {X 5}]
   ; user=> (ejecutar-asignacion '(X = 5) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 2}])
   ; [((10 (PRINT X))) [10 1] [] [] [] 0 {X 5}]
   ; user=> (ejecutar-asignacion '(X = X + 1) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 2}])
   ; [((10 (PRINT X))) [10 1] [] [] [] 0 {X 3}]
   ; user=> (ejecutar-asignacion '(X$ = X$ + " MUNDO") ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X$ "HOLA"}])
   ; [((10 (PRINT X))) [10 1] [] [] [] 0 {X$ "HOLA MUNDO"}]
)

(deftest test-preprocesar-expresion
   ; preprocesar-expresion: recibe una expresion y la retorna con
   ; las variables reemplazadas por sus valores y el punto por el
   ; cero, por ejemplo:
   ; user=> (preprocesar-expresion '(X$ + " MUNDO" + Z$) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X$ "HOLA"}])
   ; ("HOLA" + " MUNDO" + "")
   ; user=> (preprocesar-expresion '(X + . / Y% * Z) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 5 Y% 2}])
   ; (5 + 0 / 2 * 0)
)

(deftest test-desambiguar
   ; desambiguar: recibe un expresion y la retorna sin los + unarios,
   ; con los - unarios reemplazados por -u y los MID$ ternarios
   ; reemplazados por MID3$, por ejemplo: 
   ; user=> (desambiguar (list '- 2 '* (symbol "(") '- 3 '+ 5 '- (symbol "(") '+ 2 '/ 7 (symbol ")") (symbol ")")))
   ; (-u 2 * ( -u 3 + 5 - ( 2 / 7 ) ))
   ; user=> (desambiguar (list 'MID$ (symbol "(") 1 (symbol ",") 2 (symbol ")")))
   ; (MID$ ( 1 , 2 ))
   ; user=> (desambiguar (list 'MID$ (symbol "(") 1 (symbol ",") 2 (symbol ",") 3 (symbol ")")))
   ; (MID3$ ( 1 , 2 , 3 ))
   ; user=> (desambiguar (list 'MID$ (symbol "(") 1 (symbol ",") '- 2 '+ 'K (symbol ",") 3 (symbol ")")))
   ; (MID3$ ( 1 , -u 2 + K , 3 ))
)

(deftest test-precedencia
   ;; (is (= (precedencia 'OR) 1))
   ;; (is (= (precedencia 'AND) 2))
   ;; (is (= (precedencia '*) 6))
   ;; (is (= (precedencia '-u) 7))
   ;; (is (= (precedencia 'MID$) 9))
)

(deftest test-aridad
   ;; (is (= (aridad 'THEN) 0))
   ;; (is (= (aridad 'SIN) 1))
   ;; (is (= (aridad '*) 2))
   ;; (is (= (aridad 'MID$) 2))
   ;; (is (= (aridad 'MID3$) 3))
)

(deftest test-eliminar-cero-decimal
   (is (= (eliminar-cero-decimal 1.5) 1.5))
   (is (= (eliminar-cero-decimal 1.50) 1.5))
   (is (= (eliminar-cero-decimal 1.50000000000) 1.5))
   (is (= (eliminar-cero-decimal 1.0) 1))
   (is (= (eliminar-cero-decimal 'A) 'A))
)

(deftest test-eliminar-cero-entero
   (is (= (eliminar-cero-entero nil) nil))
   (is (= (eliminar-cero-entero 'A) "A"))
   (is (= (eliminar-cero-entero 0) "0"))
   (is (= (eliminar-cero-entero 1.5) "1.5"))
   (is (= (eliminar-cero-entero 1) "1"))
   (is (= (eliminar-cero-entero -1) "-1"))
   (is (= (eliminar-cero-entero -1.5) "-1.5"))
   (is (= (eliminar-cero-entero 0.5) ".5"))
   (is (= (eliminar-cero-entero -0.5) "-.5"))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(run-tests)
  