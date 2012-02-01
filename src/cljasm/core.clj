(ns cljasm.core
  (:use cljasm.asm
        cljasm.insn
        cljasm.loader)
  (:refer-clojure :exclude [pop]))

(defn random-name []
  (str (gensym 'ASM$)))

(defn random-qname []
  (str *ns* "." (random-name)))

(defn asm-fn* [f n]
  (let [signature (format "(%s)%s" (apply str (repeat n "Ljava/lang/Object;")) "Ljava/lang/Object;")
        classname (random-qname)
        cw (class-writer)]
    (visit cw :v1-5 [:public :super] classname nil clojure.lang.AFn nil)
    (let [method (visit-method cw :public "invoke" signature nil nil)]
      (visit-code method)
      (with-method-visitor method (f))
      (visit-maxs method)
      (visit-end method))
    (let [method (visit-method cw :public "<init>" "()V" nil nil)]
      (visit-code method)
      (with-method-visitor method
        (aload 0)
        (invokespecial clojure.lang.AFn "<init>" "()V")
        (return))
      (visit-maxs method)
      (visit-end method))
    (visit-end cw)
    (let [c (define-class classname (to-byte-array cw))]
      (.newInstance c))))

(defmacro asm-fn [params & body]
  (let [bindings (interleave params (range 1 (inc (count params))))]
  `(asm-fn* #(let [~@bindings] ~@body) ~(count params))))

(defmacro def-asm-fn [name params & body]
  `(def ~name (asm-fn ~params ~@body)))


