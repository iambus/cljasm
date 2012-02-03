# cljasm

ASM API for clojure.

## Usage

	(ns cljasm.test
	  (:use cljasm.core
	        cljasm.insn))

	;; write a fn in asm:

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

	(hello-world "you")

	;; extend exception in asm:

	(def-asm-class MyException Exception []
	  (<init> [^String message]
	    (aload 0)
	    (aload message)
	    (invokespecial Exception "<init>" "(Ljava/lang/String;)V")
	    (return)))

	(throw (MyException. "something right!"))


## License

Copyright (C) 2012

Distributed under the Eclipse Public License, the same as Clojure.
