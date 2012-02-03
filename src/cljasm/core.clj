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
      (let [c ^Class (resolve t)]
        (str "L" (.replace (.getName c) \. \/) ";")))))

(defn- to-method-signature [argument-types return-type]
  (format "(%s)%s" (apply str (map to-type-signature argument-types)) (to-type-signature return-type)))

(defn method-fn* [writer name args f]
  (let [return-type (:tag (meta name))
        return-type (case name <init> (or return-type 'void) return-type)
        argument-types (map (comp :tag meta) args)
        signature (to-method-signature argument-types return-type)
        method (visit-method writer :public (str name) signature nil nil)]
    (visit-code method)
    (with-method-visitor method (f))
    (visit-maxs method)
    (visit-end method)))

(defn asm-bind-args [args body]
  (let [bindings (interleave (map #(with-meta % nil) args) (range 1 (inc (count args))))]
    [args `(fn [] (let [~@bindings] ~@body))]))

(defmacro method-fn [writer name args & body]
  (let [bindings (interleave args (range 1 (inc (count args))))]
    `(method-fn* ~writer '~name ~args #(let [~@bindings] ~@body))))

(defn asm-class* [classname superclass interfaces & specs]
  (let [cw (class-writer)]
    (visit cw :v1-5 [:public :super] classname nil superclass interfaces)
    (doseq [spec specs]
      (apply method-fn* cw spec))
    (visit-end cw)
    (define-class classname (to-byte-array cw))))

(defmacro asm-class [classname superclass interfaces & specs]
  `(asm-class* ~classname ~superclass ~interfaces
    ~@(for [[name args & body] specs :let [[args body] (asm-bind-args args body)]]
        `['~name '~args ~body])))

(defmacro def-asm-class [classname superclass interfaces & specs]
  {:pre [(symbol? classname) (not (re-find #"\." (str classname)))]}
  (let [c classname
        qc (symbol (str *ns* "." classname))]
    `(let [class# (asm-class '~qc ~superclass ~interfaces ~@specs)]
       (import '~qc))))

(defn asm-fn* [f n]
  (.newInstance
    (asm-class* (random-qname) clojure.lang.AFn []
      ['<init> []
       (fn []
         (aload 0)
         (invokespecial clojure.lang.AFn "<init>" "()V")
         (return))]
      ['invoke (repeat n 'arg) f])))

(defmacro asm-fn [params & body]
  (let [bindings (interleave params (range 1 (inc (count params))))]
  `(asm-fn* #(let [~@bindings] ~@body) ~(count params))))

(defmacro def-asm-fn [name params & body]
  `(def ~name (asm-fn ~params ~@body)))


