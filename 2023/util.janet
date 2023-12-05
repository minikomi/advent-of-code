(defn read-lines [filename]
  (def ret @[])
  (with [f (file/open filename)]
    (while true
      (var l (file/read f :line))
      (if (nil? l)
        (break)
        (array/push ret (string/trim l)))))
  ret)
