(ns sicp.ch41.eval-start)

(def compound-proc?)
(def get-var)

(defn evall [exp env]
  (let [type-handler (get-var :_exp-type env)
        etype (type-handler exp env)
        handler (get-var etype env)]
    ; (prn exp etype (type etype) (type handler))
    (if (not (nil? handler))
      (handler exp env)
      (throw (Exception. (str "Undefined FN", etype))))))

(def primitive-proc-symbols #{'+ '- '* '/ '< '> '= 'prn})

(def primitives-env (into {} (map #(vector % (eval %)) primitive-proc-symbols)))

(defn exp-type [exp env]
  (cond
    (or (number? exp) (string? exp) (boolean? exp) (string? exp)
        (nil? exp)
        (and (seq? exp) (empty? exp))) :self-eval
    (symbol? exp) :symbol
    (not (nil? (primitives-env (first exp)))) :primitive-proc
    ; (compound-proc? exp env) :compound-proc
    (and (seq? exp) (not (empty? exp))) (first exp)
    :else (throw (Exception. (str "Unknow expression EVAL", exp)))))

(defn eval-seq [exps env]
  (cond
    (empty? exps) nil
    (empty? (rest exps)) (evall (first exps) env)
    :else
    (do
      (evall (first exps) env)
      (eval-seq (rest exps) env))))

(defn set-var! [name val env]
  (swap! env assoc name val))

(defn def-var [exp env]
  (set-var! (second exp) (evall (nth exp 2) env) env)
  ; (if-let [x (@env (second exp))]
  ;                       (throw (Exception. (str "VAR already defined ", (second exp))))
  ;                       (set-var! (second exp) (evall (nth exp 2) env) env))
  )

(defn get-var [name env]
  (if-not (nil? @env)
    (if-let [x (@env name)]
      x
      (get-var name (@env :_next-env)))))

(defn get-var [name env]
  ; (prn "get-var" name (@env name))
  (if (nil? env)
    (throw (Exception. (str "Undefined VAR name: ", name)))
    (if-let [x (@env name)]
      x
      (get-var name (@env :_next-env)))))

(defn compound-proc? [exp env] (and (seq? exp) (not (nil? (get-var (first exp) env)))))

(def fn-map
  {:_exp-type exp-type
   :self-eval (fn [exp env] exp)
   :symbol (fn [exp env] (let [x (get-var exp env)]
                           (if (nil? x)
                             (throw (Exception. (str "Undefined VAR", name)))
                             x)))

   'quote (fn [exp env] (second exp))
   'do (fn [exp env] (eval-seq (rest exp) env))
   'def (fn [exp env] (set-var! (second exp) (evall (nth exp 2) env) env))
   ; 'def (fn [exp env] (if-let [x (@env (second exp))]
   ;                      (throw (Exception. (str "VAR already defined ", (second exp))))
   ;                      (set-var! (second exp) (evall (nth exp 2) env) env)))
   :primitive-proc (fn [exp env] (apply (@env (first exp)) (map (fn [exp] (evall exp env)) (rest exp))))
   ; :compound-proc (fn [exp env] (apply (@env (first exp)) (map (fn [exp] (evall exp env)) (rest exp))))
   })

(def global-env (atom (merge primitives-env fn-map)))
