(ns app.util.potok-reify
  ;; (:refer-clojure :exclude [reify])
  ;; #?(:cljs (:require-macros [app.util.potok-reify]))

  (:require
   [clojure.string :as str]
   [cljs.analyzer :as ana]
   #?(:clj [cljs.compiler :as comp])
   #?(:clj [clojure.core :as c]
      :cljs [cljs.core :as c])
   #?(:cljs [potok.core])))

(defmacro reify2
  [type & impls]
  (let [t        (with-meta
                   (gensym
                    (c/str "t_"
                           (str/replace (str (comp/munge ana/*cljs-ns*)) "." "$")))
                   {:anonymous true})
        meta-sym (gensym "meta")
        this-sym (gensym "_")
        locals   (keys (:locals &env))
        ns       (-> &env :ns :name)
        ]
    `(do
       ;; (when-not (c/some? ~(symbol "potok.core" (c/str t)))
       ;; (when-not ~(list (symbol "js*") "(typeof ~{} !== 'undefined')" (symbol "js" (str t)))
       (when-not (c/exists? ~(symbol "js" (str t)))
         (deftype ~t [~@locals ~meta-sym]
           c/IWithMeta
           (~'-with-meta [~this-sym ~meta-sym] (new ~t ~@locals ~meta-sym))

           c/IMeta
           (~'-meta [~this-sym] ~meta-sym)

           ~'potok.core/Event
           (~'-type [_#] ~type)

           ~@impls)
         (set! ~(symbol "js" (str t)) ~t))
       (new ~t ~@locals ~(ana/elide-reader-meta (meta &form))))))
