(ns cljasm.insn
  (:use cljasm.asm
        cljasm.opcodes)
  (:require clojure.string)
  (:refer-clojure :exclude [pop]))

(def ^:dynamic *cljasm-method-visitor*)

(defmacro with-method-visitor [method & body]
  `(binding [*cljasm-method-visitor* {:method ~method :labels (atom {})}]
    ~@body))

(defn this-method []
  (:method *cljasm-method-visitor*))

(defn method-label [label]
  {:pre [(keyword? label)]}
  (get
    (swap! (:labels *cljasm-method-visitor*)
      (fn [labels]
        (if-let [label (get labels label)]
          labels
          (assoc labels label (new-label)))))
    label))


(defn definsn [f insn]
  (doseq [insn insn]
    (let [INSN (symbol (clojure.string/upper-case (str insn)))
          opcode (var-get (ns-resolve 'cljasm.opcodes INSN))]
      (intern *ns* insn
        (fn [& args]
          (apply f (this-method) opcode args))))))


(definsn visit-insn '[nop aconst_null iconst_m1 iconst_0 iconst_1 iconst_2 iconst_3 iconst_4 iconst_5 lconst_0 lconst_1 fconst_0 fconst_1 fconst_2 dconst_0 dconst_1 iaload laload faload daload aaload baload caload saload iastore lastore fastore dastore aastore bastore castore sastore #_pop pop2 dup dup_x1 dup_x2 dup2 dup2_x1 dup2_x2 swap iadd ladd fadd dadd isub lsub fsub dsub imul lmul fmul dmul idiv ldiv fdiv ddiv irem lrem frem drem ineg lneg fneg dneg ishl lshl ishr lshr iushr lushr iand land ior lor ixor lxor i2l i2f i2d l2i l2f l2d f2i f2l f2d d2i d2l d2f i2b i2c i2s lcmp fcmpl fcmpg dcmpl dcmpg ireturn lreturn freturn dreturn areturn return arraylength athrow monitorenter monitorexit])
(definsn visit-var-insn '[iload lload fload dload aload istore lstore fstore dstore astore ret])
(definsn visit-int-insn '[bipush sipush newarray])
(definsn visit-type-insn '[new anewarray checkcast instanceof])
(definsn visit-method-insn '[invokevirtual invokespecial invokestatic invokeinterface]) ; invokedynamic
(definsn visit-field-insn '[getstatic putstatic getfield putfield])

(defn iinc [var increment]
  (visit-iinc-insn (this-method) var increment))

(defn ldc [x]
  (visit-ldc-insn (this-method) x))

(defn visit-jump-keyword [visitor opcode label-keyword]
  (visit-jump-insn visitor opcode (method-label label-keyword)))

(definsn visit-jump-keyword '[ifeq ifne iflt ifge ifgt ifle if_icmpeq if_icmpne if_icmplt if_icmpge if_icmpgt if_icmple if_acmpeq if_acmpne goto jsr ifnull ifnonnull])

(defn asm-pop [type]
  (visit-insn (this-method) POP))

(defn asm-new [type]
  (visit-type-insn (this-method) NEW type))

(defn asm-label [label]
  {:pre [(keyword? label)]}
  (visit-label (this-method) (method-label label)))


