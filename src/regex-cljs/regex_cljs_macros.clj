(ns regex-cljs.regex-cljs-macros)

(defmacro letmap [& bindings]
  `(let [~@bindings]
     ~(let [syms (take-nth 2 bindings)]
        (zipmap (map keyword syms) syms))))

