(ns cljasm.test.core
  (:use cljasm.core
        cljasm.insn)
  (:use clojure.test))

;; write a fn in asm:

(def-asm-fn hello-world [name]
  (getstatic System 'out java.io.PrintStream)
  (asm-new StringBuilder)
  (dup)
  (ldc "hi ")
  (invokespecial StringBuilder "<init>" "(Ljava/lang/String;)V")
  (aload name)
  (invokevirtual StringBuilder 'append "(Ljava/lang/Object;)Ljava/lang/StringBuilder;")
  (ldc "!")
  (invokevirtual StringBuilder 'append "(Ljava/lang/String;)Ljava/lang/StringBuilder;")
  (invokevirtual StringBuilder 'toString "()Ljava/lang/String;")
  (dup_x1)
  (invokevirtual java.io.PrintStream 'println "(Ljava/lang/String;)V")
  (areturn))

(deftest test-hello-world
  (is (= (hello-world "u") "hi u!")))


(def-asm-fn asm-boolean [x]
  (aload x)
  (instanceof Boolean)
  (ifeq :object)
  (aload x)
  (areturn)
  (asm-label :object)
  (aload x)
  (ifnull :false)
  (iconst_1)
  (goto :end)
  (asm-label :false)
  (iconst_0)
  (asm-label :end)
  (invokestatic Boolean "valueOf" "(Z)Ljava/lang/Boolean;")
  (areturn))

(deftest test-asm-boolean
  (is (= (asm-boolean true) true))
  (is (= (asm-boolean false) false))
  (is (= (asm-boolean 1) true))
  (is (= (asm-boolean 0) true))
  (is (= (asm-boolean "x") true))
  (is (= (asm-boolean "false") true))
  (is (= (asm-boolean "") true))
  (is (= (asm-boolean nil) false)))

;; extend exception in asm:

(def-asm-class SomeException Exception []
  (<init> [^String message]
    (aload 0)
    (aload message)
    (invokespecial Exception "<init>" "(Ljava/lang/String;)V")
    (return)))

(deftest test-exception
  (is (thrown? SomeException (throw (SomeException. "hi"))))
  (is (thrown-with-msg? SomeException #"^hi$" (throw (SomeException. "hi")))))

;; write a bean class with instance fields:

(def-asm-class BeanObject Object []
  [^String name]
  (<init> []
    (aload 0)
    (invokespecial Object "<init>" "()V")
    (return))
  (^void setName [^String name]
    (aload 0)
    (aload name)
    (putfield this-class-name "name" String)
    (return))
  (^String getName []
    (aload 0)
    (getfield this-class-name "name" String)
    (areturn)))

(deftest test-exception
  (let [x (BeanObject.)]
    (is (= (.getName x) nil))
    (.setName x "me")
    (is (= (.getName x) "me"))))


