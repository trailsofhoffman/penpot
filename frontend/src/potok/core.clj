(ns potok.core
  (:refer-clojure :exclude [reify])
  (:require
   [cljs.core :as c]
   [clojure.string :as str]
   [cljs.analyzer :as ana]
   [cljs.compiler :as comp]))

(defmacro reify
  [type & impls]
  (let [t        (with-meta
                   (gensym "ptk_event_")
                    ;; (c/str "ptk_event"
                    ;;        (str/replace (str (comp/munge ana/*cljs-ns*)) "." "$")))
                   {:anonymous true})
        meta-sym (gensym "meta")
        this-sym (gensym "_")
        locals   (keys (:locals &env))
        ns       (-> &env :ns :name)
        ]
    `(do
       (when-not (cljs.core/exists? ~(symbol "js" (str t)))
         (set! ~(symbol "js" (str t))
               (deftype ~t [~@locals ~meta-sym]
                 cljs.core/IWithMeta
                 (~'-with-meta [~this-sym ~meta-sym] (new ~t ~@locals ~meta-sym))

                 cljs.core/IMeta
                 (~'-meta [~this-sym] ~meta-sym)

                 ~'potok.core/Event
                 (~'-type [_#] ~type)

                 ~@impls)))
       (new ~(symbol "js" (str t)) ~@locals ~(ana/elide-reader-meta (meta &form))))))
