(ns cljasm.loader)

(def classloader (clojure.lang.DynamicClassLoader.))

(defn define-class [classname bytes]
  (.defineClass classloader classname bytes nil))

(defn dump-to-file [bytes path]
  (with-open [output (java.io.FileOutputStream. path)]
    (.write output bytes)))

