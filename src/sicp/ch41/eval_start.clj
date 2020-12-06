(ns sicp.ch41.eval-start)

(def _get-var)

(def primitive-proc-symbols #{'+ '- '* '/ '< '> '= 'prn 'rest 'empty? 'nil? 'cons 'list})

(def primitives-env (into {} (map #(vector % (eval %)) primitive-proc-symbols)))

(def global-env (atom primitives-env))

(defn install! [env fn-name tag proc] (swap! env assoc (list fn-name tag) proc))
(defn it! [tag proc] (install! global-env 'evall tag proc))

(defn dispatcher [env fn-name]
  (let [d (fn [tag args]
            (let [f (or (_get-var (list fn-name tag) env)
                        (_get-var (list fn-name :default) env))]
              ; (prn tag args)
              (apply f args)))]
    (fn [& args]
      (let [tag (first args)]
        (cond
          (or (number? tag) (string? tag) (boolean? tag) (nil? tag)) (d :self-eval args)
          (symbol? tag) (d :symbol args)
          :else (d (first tag) args))))))

(defn evall [exp env] ((dispatcher env 'evall) exp env))

(defn eval-seq [exps env]
  ; (reduce (fn [acc exp] (evall exp env)) exps)
  (loop [acc nil exps exps]
    (cond
      (empty? exps) acc
      :else (recur (evall (first exps) env) (rest exps)))))

(defn set-var! [name val env]
  (swap! env assoc name val)
  )

(defn _get-var [name env]
  (if (nil? env)
    nil
    (let [x (get @env name)]
      (if (not (nil? x))
        x
        (_get-var name (get @env :_next-env))))))

(defn get-var [name env]
  (let [x (_get-var name env)]
    (if (nil? x)
      (throw (Exception. (str "Undefined VAR name: ", name)))
      x)))

(defn eval-list [exps env]
  (map #(evall % env) exps))

(defn extend-env [params args env]
  ; (let [p-env (into @env (map vector params args))]
  ;   (atom p-env))
  (let [e (atom (into {} (map vector params args)))]
    (swap! e assoc :_next-env env)
    e))

(defn apply-proc [proc args env]
  (if (seq? proc)
    (let [params (second proc)
          body (nth proc 2)
          env (extend-env params args env)]
      (evall body env))
    (apply proc args)))

(def fn-map
  {'evall (fn [exp env] (evall (second exp) env))
   :self-eval (fn [exp env] exp)
   :symbol get-var
   :default (fn [exp env] (let [f (evall (first exp) env)]
                            ; (prn "f:" f)
                            (apply-proc f (eval-list (rest exp) env) env)))

   'quote (fn [exp env] (second exp))
   'unquote-l (fn [exp env] (evall (second exp) env))

   'do (fn [exp env] (eval-seq (rest exp) env))
   'set! (fn [exp env] (set-var! (second exp) (evall (nth exp 2) env) env))
   'def (fn [exp env] (if (not (nil? (@env (second exp))))
                        (throw (Exception. (str "VAR already defined ", (second exp))))
                        (set-var! (second exp) (evall (nth exp 2) env) env)))
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
   'first (fn [exp env] (evall (first (evall (second exp) env)) env))
   ; as function in made language
   'my-cond '(fn [es] (if (empty? es)
                        nil
                        (if (first es)
                          (first (rest es))
                          (my-cond (rest (rest es))))))
   ; as half-macors
   'cond (fn [exp env] (if (empty? (rest exp))
                         nil
                         (if (evall (nth exp 1) env)
                           (evall (nth exp 2) env)
                           (evall (cons 'cond (drop 3 exp)) env))))
   ; end of fn-map
   })

(doseq [[tag proc] fn-map] (it! tag proc))
; (prn global-env)


; (def global-env (atom (merge primitives-env fn-map)))
