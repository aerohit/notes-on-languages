;### Namespaces

; To access the current namespace
(prn *ns*)
(ns-name *ns*)

(def great-books ["East of Eden"])

; To get a list of all the interned vars in a namespace
(ns-interns *ns*)
(get (ns-interns *ns*) 'great-books)

; To get a map of namespaces clojure uses
(ns-map *ns*)
(get (ns-map *ns*) 'great-books)

; We can deref vars to get the objects they point to
(deref #'user/great-books)

;### Creating and switching namespaces

; To create a namespace
(create-ns 'cheese.taxonomy)
(ns-name (create-ns 'cheese.taxonomy))

; To switch to a namespace
(in-ns 'cheese.analysis)
