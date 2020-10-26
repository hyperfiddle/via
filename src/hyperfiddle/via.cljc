(ns hyperfiddle.via
  (:require
    [meander.epsilon :as m :refer [match]]
    [meander.strategy.epsilon :as r]
    [minitest :refer [tests]]))


(defn tag-type
  "Extract the type from an action. Action is any keyword that starts with an uppercase letter.
  The type is encoded as the first segment.

  Action: :Eval.set-var
  Type: :Eval"
  [tag]
  (some->> (name tag) (re-matches #"^([A-Z][-a-z]*)(.*)$") second keyword))

(defn typed-tag? [tag]
  (not (nil? (tag-type tag))))

(defn typed-action? [val]
  (and #_(vector? val)
    (typed-tag? (first val))))

(defn action-tag [val]
  #_(assert (vector? val))
  (first val))

(defn action-type [val]
  (assert typed-action? val)
  (tag-type (action-tag val)))

; ----------------------------

(def ^:dynamic *scope nil)

(defmacro scope [desc & run]
  `(binding [*scope (conj (or *scope []) ~desc)] ~@run))

(defn push-scope [desc]
  (set! *scope (conj (or *scope []) desc)))

(defn with-scope [f]
  (let [s *scope]
    (fn [& args] (binding [*scope s] (apply f args)))))

; ----------------------------

(comment
  ; Goal here is to implement async/await sugar like this:
  (via* (->Maybe)
    (for [a ~(pure 1)
          b ~(+ a ~(pure 42))
          c 1]
      (pure (+ a b c))))
  => #:Maybe{:just 45}

  ; This has applications in using event-streams for UI programming in an ergonomic way
  ; (no r/atoms, UI is an expression)

  ; This approach is term rewriting
  (do> (~f ~a b ...)) => (fapply f a (pure b))              ; applicative await
  (do> (for [a ~(f ...) ...])) => (mlet [a (f ...)] ...)    ; monad await (flattened notation, later callbackified)
  (do> (x ~(y ~z)) => ((comp x y) ~z) => (fmap (comp x y) z)) ; optimize based on laws (todo)
  )

;(declare mlet pure fmap fapply bind)                       ; free symbols without ns or definition

(defn unquote-in-form? [form]
  (m/find form
    (m/scan (`unquote _))                                   ; (inc ~a b) but not (a (b ~c)) nor (inc a) nor ~a
    true))

(defn rewrite-binds [binds]
  (->> (partition 2 binds)
    (mapcat (fn [binding]
              (m/match binding
                (?k (`unquote ?form))                       ; [a ~(just 1)]
                [?k ?form]                                  ; {a (just 1)}

                (?k ?form) (m/subst [?k (pure ?form)]))))
    vec #_(apply ordered-map)))

(defn rewrite-aps [xs]
  (map (fn [x]
         (m/rewrite x
           (`unquote ?a) ?a
           ?a (pure ?a)))
    xs))

(defn rewrite-await
  "rewrite block to free Do-forms (mlet, pure, bind, fapply, fmap).
  A later pass should rewrite the free forms into something concrete."
  [form]
  (m/match form
    (`unquote ?v)                                           ; ~a (clojure.core/unquote a)
    `(unquote ~?v)                                          ; pass through, likely a type error e.g. (for [a ~[1]] ~a)

    (for [!binds ...] ?body)
    `(~'mlet ~(rewrite-binds !binds) ~?body)

    ; hack
    (mlet . _ ...)                                          ; (bind {} (clojure.core/unquote a))
    form                                                    ; leave alone

    ((m/pred symbol? ?f) (`unquote ?v))                     ; (f ~v) but not (f b) nor (f ~a ~b)
    `(~'fmap ~?f ~?v)

    (m/pred unquote-in-form?)                               ; (inc ~a b c) [a ~b] but not ~a
    `(~'fapply ~@(rewrite-aps form))

    _ form))                                                ; inc (f a b c)

(def rewrite-do (r/until = (r/top-down (r/attempt rewrite-await))))

(defmacro do> [body]
  (rewrite-do body))

(tests

  (rewrite-await '(a ~b)) => '(fmap a b)
  (rewrite-await '(a ~b ~c)) => '(fapply (pure a) b c)
  (rewrite-await '[a ~b]) => '(fapply (pure a) b c)
  (rewrite-await '[a ~b ~c]) => '(fapply (pure a) b c)

  (macroexpand-1 '(do> (+ a 1)))
  => '(+ a 1)

  (macroexpand-1 '(do> (inc ~a)))
  => '(fmap inc a)

  (macroexpand-1 '(do> (inc ~a ~b c)))
  => '(fapply (pure inc) a b (pure c))

  (macroexpand-1 '(do> (just 1)))
  => '(just 1)

  (macroexpand-1 '(do> (+ a ~(just 42))))
  => '(fapply (pure +) (pure a) (just 42))

  (macroexpand-1 '(do> (for [a 1] ...)))
  => '(mlet [a (pure 1)] ...)

  (macroexpand-1 '(do> (for [a ~(just 1)] ...)))
  => '(mlet [a (just 1)] ...)

  (macroexpand-1
    '(do> (for [a ~(just 1)
                b ~(+ a ~(just 42))
                c 1]
            ...)))
  => '(mlet [a (just 1),
             b (fapply (pure +) (pure a) (just 42)),
             c (pure 1)]
        ...)

  (macroexpand-1 '(do> ~a))
  => '(clojure.core/unquote a)

  (macroexpand-1 '(do> (for [] ~a)))                        ; likely type error
  => '(mlet [] (clojure.core/unquote a))                    ; leave it

  (macroexpand-1 '(do> (for [a ~(just 1)] ~a)))
  => '(mlet [a (just 1)] (clojure.core/unquote a))

  ;(macroexpand-1 '(do> (bind {} (clojure.core/unquote a))))
  ;=> '(mlet [] (clojure.core/unquote a))

  (macroexpand-1
    '(do> (for [a ~(just 1)
                b ~(+ a ~(just 42))
                c 1]
            ...)))
  => '(mlet [a (just 1) b (fapply (pure +) (pure a) (just 42)) c (pure 1)] ...)

  )

(defn mlet [binds body]                                     ; todo applicative-do
  ; This can't be a macro because it emits free forms which need to be rewritten
  ; before returning an ast to clojure eval
  (->> (reverse (partition 2 binds))
    (reduce (fn [acc [l r]]
              `(~'bind ~r (fn [~l] ~acc)))                  ; free bind
      body)))

;(defmacro fmap [& args] `(! :Do.fmap ~@args))
;(defmacro fapply [& args] `(! :Do.fapply ~@args))
;(defmacro pure [& args] `(! :Do.pure ~@args))
;(defmacro bind [& args] `(! :Do.bind ~@args))

(defn rewrite-free-sexp [form]
  (m/match form
    (fmap . !args ...)        `(! :Do.fmap ~@!args)
    (fapply . !args ...)      `(! :Do.fapply ~@!args)
    (bind . !args ...)        `(! :Do.bind ~@!args)
    (pure . !args ...)        `(! :Do.pure ~@!args)
    (mlet [!binds ...] ?body) (mlet !binds ?body)
    _ form))

(def rewrite-free (r/until = (r/top-down (r/attempt rewrite-free-sexp))))

(tests
  (rewrite-free '(mlet [a mv] ...))
  => '(hyperfiddle.via/! :Do.bind mv (clojure.core/fn [a] ...))

  (rewrite-free '(mlet [f (just +)] ...))
  => '(hyperfiddle.via/! :Do.bind (just +) (clojure.core/fn [f] ...))

  (rewrite-free '(mlet [a (just 1) b (fapply (pure +) (pure a) (just 42)) c (pure 1)] ...))
  ;=> '(bind (just 1) (clojure.core/fn [a]
  ;                     (bind (fapply (pure +) (pure a) (just 42)) (clojure.core/fn [b]
  ;                                                                  (bind (pure 1) (clojure.core/fn [c]
  ;                                                                                   (do ...)))))))
  => '(hyperfiddle.via/!
        :Do.bind
        (just 1)
        (clojure.core/fn
          [a]
          (hyperfiddle.via/!
            :Do.bind
            (hyperfiddle.via/! :Do.fapply (hyperfiddle.via/! :Do.pure +) (hyperfiddle.via/! :Do.pure a) (just 42))
            (clojure.core/fn [b] (hyperfiddle.via/! :Do.bind (hyperfiddle.via/! :Do.pure 1) (clojure.core/fn [c] ...))))))

  )

; ---

(defprotocol Do-via
  (resolver-for [H]))

(def ^:dynamic *stack [])                                   ; each via* layer has its own mutable state
(def ^:dynamic *resolve {})                                 ; methods available in this dynamic scope
(def ^:dynamic *this)                                       ; points to the active state record in a ! frame

(defmacro via [R & body]
  `(let [R# ~R                                              ; R is the user defined state instance, e.g. a defrecord
         fns# (resolver-for R#)]                            ; This is a protocol to allow for user defined state type

     (assert (every? typed-tag? (keys fns#)))               ; An action-type identifies a set of methods available on an object "of that action-type"

     (let [n# (count *stack)
           resolvers#                                       ; methods for an action-type (no inheritance, )
           (->> fns#
             (group-by (comp tag-type key))              ; Override resolver methods as a single unit (no inheritance, via* must provide complete impl). GT said this is an optimization?
             (reduce-kv (fn [m# action-type# methods#]
                          (assoc m# action-type# (into {::nth n#} methods#))) ; ?
               {}))]

       (binding [*stack (conj *stack R#)                    ; save the state pointer
                 *resolve (merge *resolve resolvers#)]      ; other action types are still available in dynamic scope

         ~@(map (comp rewrite-free rewrite-do) body)
         ))))

(defn ! "call methods on object from stack variable"
  [& [F & args :as action]]
  ;{:pre [(doto action println)]}
  (assert (typed-action? action))

  (let [R (get *resolve (action-type action))]
    (binding [*this (nth *stack (::nth R))]                 ; state monad
      (let [f (get R F)                                     ; resolve handler
            result (apply f args)]                          ; Re-entrant ! works
        (set! *stack (assoc *stack (::nth R) *this))        ; Carry state forward
        result))))

(defn get-state []
  (or *this
    (last *stack)))                                       ; ?

(tests

  ;(macroexpand-1 '(mlet [a ma b mb] ...))
  ;=> (bind ma (fn [a] (bind mb (fn [b] ...))))

  (try
    (macroexpand-1 '(via (reify)                           ; !
                      (for [f (pure +)
                            a (pure 1)
                            b ~(~f 10 ~a)]
                        (pure (inc b)))))
    ::worked
    (catch Exception e e))
  => ::worked
  )

(tests
  (do
    (defn just [v] {:Maybe/just v})                         ; none is nil

    ; https://twitter.com/dustingetz/status/1305190850784309249
    ; https://twitter.com/dustingetz/status/1312762707842564104
    (deftype Maybe []
      Do-via
      (resolver-for [R]
        {:Do.pure   (fn [v] {:Maybe/just v})
         :Do.fmap   (fn [f & fvs]
                      (let [vs (map :Maybe/just fvs)]
                        (if (every? identity vs)
                          (just (apply f vs)))))
         :Do.fapply (fn [& avs] (apply ! :Do.fmap #(apply % %&) avs))
         :Do.bind   (fn [{v :Maybe/just} cont] (if v (cont v)))})))
  => hyperfiddle.via.Maybe

  (via (->Maybe)
    (for [f (just +)
          a (just 1)
          b ~(~f 10 ~a)]
      (pure (inc b))))
  => #:Maybe{:just 12}

  (via (->Maybe)
    (for [a ~(just 1)
          b ~(+ a ~(just 42))
          c (for [i (range (+ a 2))] i)]                    ; vanilla for
      (pure (+ a b (reduce + c)))))
  => #:Maybe{:just 47}
  )
