(load-file "./flow-eldoc.el")

(defun flow-eldoc-tests/check-highlight (str-in str-out)
  (should (equal (flow-eldoc/highlight-str str-in) str-out)))

(ert-deftest flow-eldoc-tests/check-basic-type ()
  (let* ((str "number")
         (result (type-name-color str)))
    (flow-eldoc-tests/check-highlight str result)))

(ert-deftest flow-eldoc-tests/check-optional-type ()
  (let* ((str "?number")
         (result (concat "?" (type-name-color "number"))))
    (flow-eldoc-tests/check-highlight str result)))

(ert-deftest flow-eldoc-tests/check-generic-type ()
  (let* ((str "Type<A, B>")
         (result (concat (generic-name-color "Type")
                         "<"
                         (type-name-color "A")
                         ", "
                         (type-name-color "B")
                         ">")))
    (flow-eldoc-tests/check-highlight str result)))

(ert-deftest flow-eldoc-tests/array-type ()
  (let* ((str "number[]")
         (result (concat (type-name-color "number") "[]")))
    (flow-eldoc-tests/check-highlight str result)))

(ert-deftest flow-eldoc-tests/func-type-no-args ()
  (let* ((str "() => void")
         (result (concat "() => " (type-name-color "void"))))
    (flow-eldoc-tests/check-highlight str result)))

(ert-deftest flow-eldoc-tests/generic-func-type-no-args ()
  (let* ((str "<A, B>() => void")
         (result (concat "<"
                         (type-name-color "A")
                         ", "
                         (type-name-color "B")
                         ">"
                         "() => "
                         (type-name-color "void"))))
    (flow-eldoc-tests/check-highlight str result)))

(ert-deftest flow-eldoc-tests/func-type-single-unamed-arg ()
  (let* ((str "(bool) => void")
         (result (concat "(" (type-name-color "bool") ") => " (type-name-color "void"))))
    (flow-eldoc-tests/check-highlight str result)))

(ert-deftest flow-eldoc-tests/func-type-multiple-unamed-args ()
  (let* ((str "(bool, number) => void")
         (result (concat "("
                         (type-name-color "bool")
                         ", "
                         (type-name-color "number")
                         ") => "
                         (type-name-color "void"))))
    (flow-eldoc-tests/check-highlight str result)))

(ert-deftest flow-eldoc-tests/func-type-single-named-arg ()
  (let* ((str "(foo: bool) => void")
         (result (concat
                  "("
                  (key-name-color "foo")
                  ": "
                  (type-name-color "bool")
                  ") => "
                  (type-name-color "void"))))
    (flow-eldoc-tests/check-highlight str result)))

(ert-deftest flow-eldoc-tests/func-type-multiple-named-arg ()
  (let* ((str "(foo: bool, bar: number) => void")
         (result (concat
                  "("
                  (key-name-color "foo")
                  ": "
                  (type-name-color "bool")
                  ", "
                  (key-name-color "bar")
                  ": "
                  (type-name-color "number")
                  ") => "
                  (type-name-color "void"))))
    (flow-eldoc-tests/check-highlight str result)))
