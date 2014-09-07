(ns code-katas-2.core)


(defn unpartial
  "Escribir una funcion que acepte una funcion parcial con cantidad de argumentos desconocida,
   retornar una funcion equivalente de n argumentos"
  [f]
  (fn [& args]
     (if (fn? (f (first args)))
       ((unpartial (f (first args))) (rest args))
       (f (first args))
         )
    )
  )


(defn search
  "Dado un numero cualquiera de secuencias, cada una ya ordenada de menor a mayor, encontrar el numero
   mas chico que aparezca en todas las secuencias, las secuencias pueden ser infinitas."
  [& seqs]
    (if (apply = (map #(first %) seqs))
      (ffirst seqs)
      (apply search (for [x seqs]
                      ((fn filtmins [myseq maxmins]
                         (drop-while #(< % (first maxmins)) myseq)) x (take 1 (sort > (map #(first %) seqs))))
                      )))
  )


(defn intercalar
  "Escriba una funcion que tome un predicado de 2 argumentos, un valor y una coleccion, y
   retorne una nueva coleccion donde el valor es insertado intercalado cada dos argumentos
   que cumplan el predicado"
  [predicado valor secuencia]
  (if (empty? secuencia)
    []
    (if (and (not (nil? (second secuencia))) (predicado (first secuencia) (second secuencia)))
      (lazy-seq (cons (first secuencia) (cons valor (intercalar predicado valor (rest secuencia)))))
      (lazy-seq (cons (first secuencia) (intercalar predicado valor (rest secuencia))))
      )
    )
  )


(defn tartamudeo
  "Escriba una funcion que retorne una secuencia lazy que comprima el tartamudeo de una secuencia de numeros.
   Comprimir el tartamudeo se refiere a que [1 1 1] se exprese como [3 1] y a su vez [3 1] se exprese como [1 3 1 1].

   La funcion debe aceptar una secuencia inicial de numeros, y devolver una secuencia infinita de compresiones, donde
   cada nuevo elemento es el elemento anterior comprimido."
  [secuencia]
  (drop 1  (iterate
              (fn tartaparcial [misecuencia] (let [conscount (inc (count ((fn consseq [aseq]
                                                                            (if (and (not (nil? (second aseq))) (= (first aseq) (second aseq)))
                                                                              (cons (first aseq) (consseq (rest aseq)))
                                                                              ()
                                                                              )) misecuencia)))]
                                                (if (nil? (first misecuencia))
                                                    ()
                                                    (if (nil? (second misecuencia))
                                                      (seq [conscount (first misecuencia)])
                                                      (cons conscount (cons (first misecuencia) (tartaparcial (drop conscount misecuencia))))))
                                               )) secuencia))

)
