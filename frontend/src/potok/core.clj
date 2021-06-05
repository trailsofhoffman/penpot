(ns potok.core
  (:refer-clojure :exclude [reify])
  (:require
   [cljs.core :as c]
   [clojure.string :as str]
   [cljs.analyzer :as ana]
   [cljs.compiler :as comp]))

(defmacro deftype2
  [t fields & impls]
  (#'c/validate-fields "deftype" t fields)
  (let [env &env
        r             (:name (ana/resolve-var (dissoc env :locals) t))
        [fpps pmasks] (#'c/prepare-protocol-masks env impls)
        protocols     (#'c/collect-protocols impls env)
        t             (vary-meta t assoc
                                 :protocols protocols
                                 :skip-protocol-flag fpps) ]
    `(deftype* ~t ~fields ~pmasks
       ~(if (seq impls)
          `(extend-type ~t ~@(#'c/dt->et t impls fields)))
       #_(set! (.-getBasis ~t) (fn [] '[~@fields]))
       #_(set! (.-cljs$lang$type ~t) true)
       #_(set! (.-cljs$lang$ctorStr ~t) ~(str r))
       #_(set! (.-cljs$lang$ctorPrWriter ~t) (fn [this# writer# opt#] (-write writer# ~(str r))))
       )))

(defmacro leaky-exists?
  [x]
  (if (symbol? x)
    (let [x (cond-> (:name (ana/resolve-var &env x))
              (= "js" (namespace x)) name)
          y (str (str/replace (str x) "/" "."))
          y (vary-meta (symbol "js" y) assoc :cljs.analyzer/no-resolve true)]
      (-> (list 'js* "(typeof ~{} !== 'undefined')" y)
          (vary-meta assoc :tag 'boolean)))
    `(some? ~x)))

(defmacro reify
  [type & impls]
  (let [t        (with-meta
                   (gensym
                    (str "t_"
                         (str/replace (str (comp/munge ana/*cljs-ns*)) "." "$")))
                   {:anonymous true
                    :cljs.analyzer/no-resolve true})
        this-sym (gensym "_")
        locals   (keys (:locals &env))
        ns       (-> &env :ns :name)
        ]
    `(do
       (when-not (leaky-exists? ~(symbol (str ns) (str t)))
         (deftype2 ~t [~@locals]
           ~'potok.core/Event
           (~'-type [_#] ~type)

           ~@impls))
       (new ~t ~@locals))))
