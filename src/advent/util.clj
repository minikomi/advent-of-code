(ns advent.util
  (:import [java.awt Toolkit]
           [java.awt.datatransfer StringSelection]))

(defmacro let-stop [bindings & body]
  (if (some #{'*stop*} bindings)
    (let [stopped-bindings
          (->> bindings
               (take-while #(not= '*stop* %))
               vec)
          caught-symbols
          (->> stopped-bindings
               (partition 2)
               (map #(vector (str (first %)) (first %)))
               (into {}))]
      `(let ~stopped-bindings
         (throw (ex-info (str "Let was stopped at stage "
                              (/ (count ~stopped-bindings) 2))
                         {:current-symbols ~caught-symbols}))))
    `(let* ~(destructure bindings) ~@body)))

(defmacro with-clipboard [& body]
  `(binding [*out* (java.io.StringWriter.)]
     (let [result# (do ~@body)]
       (.. Toolkit
           (getDefaultToolkit)
           (getSystemClipboard)
           (setContents (StringSelection. (str *out*)) nil))
       result#)))

(with-clipboard (print "aaa"))
