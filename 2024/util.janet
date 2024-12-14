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

(defn fnil [f & defaults]
  (fn [& args]
    (def args-with-defaults (map (fn [i d] (if (nil? i) d i)) args defaults))
    (f ;args-with-defaults ;(drop (length defaults) args))))

(def mtx-grammar
  (peg/compile ~{:pos (/ (* (line) (column)) ,tuple)
                 :char (* :pos (<- 1))
                 :main (/ (some (+ :s :char)) ,table)}))

(defn- drain-fibers
  "Canceling a group of fibers and wait for them all to complete."
  [super fibers reason]
  (each f fibers (ev/cancel f reason))
  (def n (length fibers))
  (table/clear fibers)
  (repeat n (ev/take super)))

(defn- join
  "Special case of supervise for implementing some parallel functions."
  [supervisor fibers]
  (var err-fiber nil)
  (defer (drain-fibers supervisor fibers "parent canceled")
    (while (next fibers)
      (def [sig fiber] (ev/take supervisor))
      (if (= sig :ok)
        (put fibers fiber nil)
        (do
          (drain-fibers supervisor fibers "sibling canceled")
          (propagate (fiber/last-value fiber) fiber))))))

(defn pcall
  "Call a function n times (in parallel) for side effects.
  Each function is called with an integer argument indicating a fiber index. Returns nil."
  [f n]
  (assert (> n 0))
  (def chan (ev/chan))
  (def new-f (if (function? f) f (fn [x] (f x))))
  (join chan
        (tabseq [i :range [0 n]
                 :let [fib (ev/go (fiber/new new-f :tp) i chan)]]
                fib fib)))

(defn pmap-full
  "Function form of `ev/gather`. If any of the
  sibling fibers error, all other siblings will be canceled.  Returns the gathered
  results in an array. `data` can be any indexed data structure."
  [f data]
  (def chan (ev/chan))
  (def res (if (dictionary? data) @{} @[]))
  (join chan
        (tabseq [[i x] :pairs data
                 :let [fib (ev/go (fiber/new (fn [] (put res i (f x))) :tp) nil chan)]]
          fib fib))
  res)

(defn pmap-limited
  "Similar to pmap-full, but only runs work n-ways parallel."
  [f data n-workers]
  (assert (> n-workers 0))
  (def res (if (dictionary? data) @{} @[]))
  (var cursor (next data nil))
  (defn worker [&]
    (while (not= nil cursor)
      (def value (get data cursor))
      (def key cursor)
      (set cursor (next data cursor))
      (put res key (f value))))
  (pcall worker n-workers)
  res)

(defn pmap
  "Map `f` over data in parallel, optionally limiting parallelism to
  `n` workers."
  [f data &opt n-workers]
  (if (= nil n-workers)
    (pmap-full f data)
    (pmap-limited f data n-workers)))

(defn add-v [v1 v2]
  [(+ (v1 0) (v2 0)) (+ (v1 1) (v2 1))])

(defn sub-v [v1 v2]
  [(- (v1 0) (v2 0)) (- (v1 1) (v2 1))])
