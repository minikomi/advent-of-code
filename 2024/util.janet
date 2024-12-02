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
    (var ret (string/trim (file/read f :all)))
    (file/close f)
    ret))

(defmacro loopv [head & body]
  (with-syms [$x]
    ~(do
       (var ,$x nil)
       (loop ,head (set ,$x (do ,;body)))
       ,$x)))

(defn index-gen []
  (do (var i -1)
    (generate [i :iterate (++ i)]
      i)))

(defn cycle [s]
  (generate [i :in (index-gen)]
    (get s (% i (length s)))))

(defn lcm [& vs]
  (reduce
    (fn [lcm-acc n]
      (/ (* lcm-acc n) (math/gcd lcm-acc n)))
    1
    vs))

(defn transpose [arrs]
    (apply map array arrs))
