(defn read-lines [filename]
  (def ret @[])
  (with [f (file/open filename)]
    (while true
      (var l (file/read f :line))
      (if (nil? l)
        (break)
        (array/push ret (string/trim l)))))
  ret)

(defn read-file [filename]
  (with [f (file/open filename)]
    (var ret (file/read f :all))
    (file/close f)
    ret))

(defmacro loopv [head & body]
  (with-syms [$x]
    ~(do
       (var ,$x nil)
       (loop ,head (set ,$x (do ,;body)))
       ,$x)))
