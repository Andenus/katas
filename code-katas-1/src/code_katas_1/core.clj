(ns code-katas-1.core)

(defn filter-odd
  "Escribir una funcion que retorne solamente los numeros impares de
   una secuencia"
  [s]
  (if (not (nil? (first s)))
    (if
     (not= (mod (first s) 2) 0)
     (cons (first s) (filter-odd (rest s)))
     (filter-odd (rest s)))
     ()
    )
  )

(defn nil-key
  "Escribir una funcion que dada una clave y un mapa, devuelva true, solamente si el mapa
   contiene una entrada con esa clave, y su valor es nil"
  [k m]
  (if (and (contains? m k) (nil? (get m k)))
    true
    false
   )
)

(defn range
  "Escribir una funcion que cree una lista de enteros en un rango dado.
   Restricciones: range"
  [start end]
  (if (not= start end)
    (cons start (range (+ start 1) end))
    ()
   )
)

(defn compress-sequence
  "Escribir una funcion que elimine los duplicados consecutivos
   de una secuencia"
  [s]
  (if (nil? (first s))
    (str)
    (if (char? (first s))
      (if (= (first s) (second s))
        (compress-sequence (rest s))
        (str (first s) (compress-sequence (rest s)))
       )
      (if (= (first s) (second s))
        (compress-sequence (rest s))
        (cons (first s) (compress-sequence (rest s)))
       )
      )
   )
  )

(defn max-value
  "Escribir una funcion que reciba un numero variable de parametros
   y retorne el que tenga el valor mayor
   Restricciones: max y max-key"
  [& args]
  (
    first (sort > args)
  )
)

(defn split-two
  "Escribir una funcion que parta una secuencia en dos partes
   Restricciones: split-at"
  [length s]
  (
   vector (vec (take length s)) (vec (drop length s))
   )
)

(defn inter-two
  "Escribir una funcion que reciba dos secuencias y retorne el primero de cada una,
   luego el segundo de cada una, luego el tercero, etc.
   Restricciones: interleave"
  [s1 s2]
  (
   if (or (nil? (first s1)) (nil? (first s2)))
    ()
    (cons (first s1) (cons (first s2) (inter-two (rest s1) (rest s2))))
  )
)

(defn retrieve-caps
  "Escribir una funcion que reciba un string y devuelva un nuevo string conteniendo
   solamente las mayusculas."
  [text]
  (
   apply str (re-seq #"[A-Z]" text)
   )
  )

(defn find-truth
  "Escribir una funcion que tome un numero variable de booleans, y devuelva true
   solamente si alguno de los parametros son true, pero no todos son true. En otro
   caso debera retornar false"
  [& xs]
  (
   if (and (not (every? true? xs)) (some true? xs))
    true
    false
   )
  )

(defn zip-map
  "Escribir una funcion que reciba un vector de claves y un vector de valores, y
   construya un mapa a partir de ellos.
   Restricciones: zipmap"
  [k v]
  (
   if (or (nil? (first k)) (nil? (first v)))
    {}
    (merge {(first k) (first v)} (zip-map (rest k) (rest v)))
   )
  )
