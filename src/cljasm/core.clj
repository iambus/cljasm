(ns cljasm.core
  (:use cljasm.asm
        cljasm.insn
        cljasm.loader)
  (:refer-clojure :exclude [pop]))

(defn random-name []
  (str (gensym 'ASM$)))

(defn random-qname []
  (str *ns* "." (random-name)))


(defn- to-type-signature [t]
  (let [t (or t 'Object)]
    (case t
      int "I"
      double "D"
      void "V"
      (str "L" (.replace (.getName ^Class (resolve t)) \. \/) ";"))))

(defn- to-method-signature [argument-types return-type]
  (format "(%s)%s" (apply str (map to-type-signature argument-types)) (to-type-signature return-type)))

(defn method-fn* [writer name args f]
  (let [return-type (:tag (meta name))
        argument-types (map (comp :tag meta) args)
        signature (to-method-signature argument-types (case name :init (or return-type 'void) return-type))
        method (visit-method writer :public (case name :init "<init>" (clojure.core/name name)) signature nil nil)]
    (visit-code method)
    (with-method-visitor method (f))
    (visit-maxs method)
    (visit-end method)))

(defmacro method-fn [writer name args & body]
  (let [bindings (interleave args (range 1 (inc (count args))))]
    `(method-fn* ~writer ~name ~args #(let [~@bindings] ~@body))))

(defn asm-fn* [f n]
  (let [signature (format "(%s)%s" (apply str (repeat n "Ljava/lang/Object;")) "Ljava/lang/Object;")
        classname (random-qname)
        cw (class-writer)]
    (visit cw :v1-5 [:public :super] classname nil clojure.lang.AFn nil)
    (method-fn* cw 'invoke (repeat n ^Object 'argument) f)
    (method-fn cw :init []
      (aload 0)
      (invokespecial clojure.lang.AFn "<init>" "()V")
      (return))
    (visit-end cw)
    (let [bytes (to-byte-array cw)
          c (define-class classname bytes)]
      (dump-to-file bytes "D:\\usr\\asm-4.0\\cljasm.class")
      (.newInstance c))))

(defmacro asm-fn [params & body]
  (let [bindings (interleave params (range 1 (inc (count params))))]
  `(asm-fn* #(let [~@bindings] ~@body) ~(count params))))

(defmacro def-asm-fn [name params & body]
  `(def ~name (asm-fn ~params ~@body)))


