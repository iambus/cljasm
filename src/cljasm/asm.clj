(ns cljasm.asm
  (:use cljasm.opcodes)
  (:import [clojure.asm Opcodes ClassWriter MethodVisitor FieldVisitor Label])
  (:require clojure.string))

;(set! *warn-on-reflection* true)

(defn ^ClassWriter class-writer []
  (ClassWriter. ClassWriter/COMPUTE_MAXS))

(def access-codes
  {:public ACC_PUBLIC
   :private ACC_PRIVATE
   :protected ACC_PROTECTED
   :static ACC_STATIC
   :final ACC_FINAL
   :super ACC_SUPER
   :synchronized ACC_SYNCHRONIZED
   :volatile ACC_VOLATILE
   :bridge ACC_BRIDGE
   :varargs ACC_VARARGS
   :transient ACC_TRANSIENT
   :native ACC_NATIVE
   :interface ACC_INTERFACE
   :abstract ACC_ABSTRACT
   :strict ACC_STRICT
   :synthetic ACC_SYNTHETIC
   :annotation ACC_ANNOTATION
   :enum ACC_ENUM
   :deprecated ACC_DEPRECATED})

(defn ^int to-access-code [access]
  (let [access (if (sequential? access) access [access])]
    (reduce + (for [code access] (access-codes code code)))))

(defn ^String to-class-name [c]
  (.replace (if (class? c) (.getName ^Class c) (name c)) \. \/))

(defn ^String to-signature [c]
  (if (class? c)
    (str "L" (to-class-name c) ";")
    c))

(def version-codes
  {"1.1" V1_1
   "1.2" V1_2
   "1.3" V1_3
   "1.4" V1_4
   "1.5" V1_5
   "1.6" V1_6
   :v1-1 V1_1
   :v1-2 V1_2
   :v1-3 V1_3
   :v1-4 V1_4
   :v1-5 V1_5
   :v1-6 V1_6})

(defn ^int to-version [v]
  (version-codes v v))

(defn visit [^ClassWriter writer
             version
             access
             ^String name
             ^String signature
             superName
             interfaces]
  (.visit writer
    (to-version version)
    (to-access-code access)
    (to-class-name name)
    signature
    (to-class-name superName)
    (when interfaces (into-array String (map to-class-name interfaces)))))

(defn ^MethodVisitor visit-method [^ClassWriter writer
                                   access
                                   ^String name
                                   ^String desc
                                   ^String signature
                                   exceptions]
   (.visitMethod writer
     (to-access-code access)
     name
     desc
     signature
     (when exceptions (into-array ^String (map to-class-name exceptions)))))

(defn ^FieldVisitor visit-field [^ClassWriter writer
                        access
                        ^String name
                        ^String desc
                        ^String signature
                        value]
  (.visitField writer
    (to-access-code access)
    name
    desc
    signature
    value))

(defn visit-end [visitor]
  (.visitEnd visitor))

(defn to-byte-array [^ClassWriter writer]
  (.toByteArray writer))

(defn visit-code [^MethodVisitor visitor]
  (.visitCode visitor))

; NOP, ACONST_NULL, ICONST_M1, ICONST_0, ICONST_1, ICONST_2, ICONST_3, ICONST_4, ICONST_5, LCONST_0, LCONST_1, FCONST_0, FCONST_1, FCONST_2, DCONST_0, DCONST_1, IALOAD, LALOAD, FALOAD, DALOAD, AALOAD, BALOAD, CALOAD, SALOAD, IASTORE, LASTORE, FASTORE, DASTORE, AASTORE, BASTORE, CASTORE, SASTORE, POP, POP2, DUP, DUP_X1, DUP_X2, DUP2, DUP2_X1, DUP2_X2, SWAP, IADD, LADD, FADD, DADD, ISUB, LSUB, FSUB, DSUB, IMUL, LMUL, FMUL, DMUL, IDIV, LDIV, FDIV, DDIV, IREM, LREM, FREM, DREM, INEG, LNEG, FNEG, DNEG, ISHL, LSHL, ISHR, LSHR, IUSHR, LUSHR, IAND, LAND, IOR, LOR, IXOR, LXOR, I2L, I2F, I2D, L2I, L2F, L2D, F2I, F2L, F2D, D2I, D2L, D2F, I2B, I2C, I2S, LCMP, FCMPL, FCMPG, DCMPL, DCMPG, IRETURN, LRETURN, FRETURN, DRETURN, ARETURN, RETURN, ARRAYLENGTH, ATHROW, MONITORENTER, or MONITOREXIT
(defn visit-insn [^MethodVisitor visitor opcode]
  (.visitInsn visitor opcode))


; ILOAD, LLOAD, FLOAD, DLOAD, ALOAD, ISTORE, LSTORE, FSTORE, DSTORE, ASTORE or RET
(defn visit-var-insn [^MethodVisitor visitor opcode var]
  (.visitVarInsn visitor opcode var))

; BIPUSH, SIPUSH or NEWARRAY
(defn visit-int-insn [^MethodVisitor visitor opcode operand]
  (.visitIntInsn visitor opcode operand))

(defn visit-iinc-insn [^MethodVisitor visitor var increment]
  (.visitIincInsn visitor var increment))

; XXX: what does it mean?
(defn visit-ldc-insn [^MethodVisitor visitor cst]
  (.visitLdcInsn visitor cst))

; NEW, ANEWARRAY, CHECKCAST or INSTANCEOF
(defn visit-type-insn [^MethodVisitor visitor opcode type]
  (.visitTypeInsn visitor opcode (to-class-name type)))

(def invoke-codes
  {:virtual INVOKEVIRTUAL
   :special INVOKESPECIAL
   :static INVOKESTATIC
   :interface INVOKEINTERFACE})

; INVOKEVIRTUAL, INVOKESPECIAL, INVOKESTATIC, INVOKEINTERFACE or INVOKEDYNAMIC
(defn visit-method-insn [^MethodVisitor visitor opcode owner name ^String desc]
  (.visitMethodInsn visitor
    (invoke-codes opcode opcode)
    (to-class-name owner)
    (clojure.core/name name)
    desc))

(def field-codes
  {:get GETFIELD
   :put PUTFIELD
   :get-static GETSTATIC
   :put-static PUTSTATIC})

; GETSTATIC, PUTSTATIC, GETFIELD or PUTFIELD
(defn visit-field-insn [^MethodVisitor visitor opcode owner name desc]
  (.visitFieldInsn visitor
    (field-codes opcode opcode)
    (to-class-name owner)
    (clojure.core/name name)
    (to-signature desc)))

; IFEQ, IFNE, IFLT, IFGE, IFGT, IFLE, IF_ICMPEQ, IF_ICMPNE, IF_ICMPLT, IF_ICMPGE, IF_ICMPGT, IF_ICMPLE, IF_ACMPEQ, IF_ACMPNE, GOTO, JSR, IFNULL or IFNONNULL.
(defn visit-jump-insn [^MethodVisitor visitor opcode ^Label label]
  (.visitJumpInsn visitor opcode label))

; TODO: visitTableSwitchInsn, visitLookupSwitchInsn, visitMultiANewArrayInsn, visitTryCatchBlock
; TODO: visitLineNumber, visitLocalVariable

(defn visit-label [^MethodVisitor visitor ^Label label]
  (.visitLabel visitor label))

(defn visit-maxs
  ([^MethodVisitor visitor maxStack maxLocals]
    (.visitMaxs visitor maxStack maxLocals))
  ([^MethodVisitor visitor]
    (visit-maxs visitor 0 0)))


(defn new-label []
  (Label.))


