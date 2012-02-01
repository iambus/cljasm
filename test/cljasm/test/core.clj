(ns cljasm.test.core
  (:use cljasm.core
        cljasm.insn)
  (:use [clojure.test]))

(def-asm-fn hello-world [name]
  (getstatic System 'out java.io.PrintStream)
  (asm-new StringBuilder)
  (dup)
  (ldc "hi ")
  (invokespecial StringBuilder "<init>" "(Ljava/lang/String;)V")
  (aload 1)
  (invokevirtual StringBuilder 'append "(Ljava/lang/Object;)Ljava/lang/StringBuilder;")
  (ldc "!")
  (invokevirtual StringBuilder 'append "(Ljava/lang/String;)Ljava/lang/StringBuilder;")
  (invokevirtual StringBuilder 'toString "()Ljava/lang/String;")
  (dup_x1)
  (invokevirtual java.io.PrintStream 'println "(Ljava/lang/String;)V")
  (areturn))

(deftest test-hello-world
  (is (= (hello-world "u") "hi u!")))
