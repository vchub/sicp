(ns sicp.ch41.eval-start)

(def compound-proc?)
(def get-var)
(def eval-list)
(def apply-compound-proc)

(defn evall [exp env]
  (let [type-handler (get-var :_exp-type env)
        etype (type-handler exp env)
        handler (get-var etype env)]
    ; (prn exp etype (type etype) (type handler))
    (cond
      (nil? handler) (throw (Exception. (str "Undefined FN", etype)))
      (seq? handler) (apply-compound-proc handler (eval-list (rest exp) env) env)
      :else (handler exp env))
    ; (if (not (nil? handler))
    ;   (handler exp env)
    ;   (throw (Exception. (str "Undefined FN", etype))))
    ))

(def primitive-proc-symbols #{'+ '- '* '/ '< '> '= 'prn 'rest 'empty? nil?})

(def primitives-env (into {} (map #(vector % (eval %)) primitive-proc-symbols)))

(defn exp-type [exp env]
  (cond
    (or (number? exp) (string? exp) (boolean? exp) (nil? exp)
        (and (seq? exp) (empty? exp))) :self-eval
    (symbol? exp) :symbol
    (not (nil? (primitives-env (first exp)))) :primitive-proc
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

(defn get-var [name env]
  ; (prn "get-var" name (@env name))
  (if (nil? env)
    (throw (Exception. (str "Undefined VAR name: ", name)))
    (let [x (@env name)]
      (if (not (nil? x))
        x
        (get-var name (@env :_next-env))))))

(defn eval-list [exps env]
  (map (fn [exp] (evall exp env)) exps))

(defn extend-env [params args env]
  (let [e (atom (into {} (map (fn [p a] [p a])  params args)))]
    (swap! e assoc :_next-env env)
    e))

(defn apply-compound-proc [lambda args env]
  (let [params (second lambda)
        body (nth lambda 2)
        env (extend-env params args env)]
    ; (prn @env)
    ; (prn body)
    (cond
      ; (seq? body) (eval-seq body env)
      :else (evall body env))))

(do
  (def x 2)
  `(1 ~(dec x)))

(let [x [1 2 3]]
  (next x))

(def fn-map
  {:_exp-type exp-type
   :self-eval (fn [exp env] exp)
   :symbol get-var
   ; :symbol (fn [exp env] (let [x (get-var exp env)]
   ;                         (if (nil? x)
   ;                           (throw (Exception. (str "Undefined VAR", name)))
   ;                           x)))

   ; 'quote-l (fn [exp env] (second exp))
   'quote (fn [exp env] (second exp))
   'unquote-l (fn [exp env] (evall (second exp) env))
   'do (fn [exp env] (eval-seq (rest exp) env))
   'set! (fn [exp env] (set-var! (second exp) (evall (nth exp 2) env) env))
   ; 'def (fn [exp env] (set-var! (second exp) (evall (nth exp 2) env) env))
   'def (fn [exp env] (if (not (nil? (@env (second exp))))
                        (throw (Exception. (str "VAR already defined ", (second exp))))
                        (set-var! (second exp) (evall (nth exp 2) env) env)))
   :primitive-proc (fn [exp env] (apply (get-var (first exp) env) (eval-list (rest exp) env)))
   'fn (fn [exp env] exp)
   'if (fn [exp env] (if (evall (second exp) env)
                       (evall (nth exp 2) env)
                       (evall (nth exp 3) env)))
   'and (fn [exp env] (evall `(if ~(second exp)
                                ~(nth exp 2)
                                ~(second exp)) env))
   'or (fn [exp env] (evall `(if ~(second exp)
                               ~(second exp)
                               ~(nth exp 2)) env))
   ; as function in made language
   'my-cond '(fn [es] (do
                        (if (empty? es)
                          nil
                          (if (first es)
                            (first (rest es))
                            (my-cond (rest (rest es)))))))

   ; as half-macors
   'cond (fn [exp env] (if (empty? (rest exp))
                         nil
                         (if (evall (nth exp 1) env)
                           (evall (nth exp 2) env)
                           (evall ('cond (drop 3 exp)) env))))
   'first (fn [exp env] (evall (first (evall (second exp) env)) env))})

(def global-env (atom (merge primitives-env fn-map)))
