(load-file "./parser.el")

(defun parser-tests/check-parser (str ast)
  (should (equal (parser/parse str) (message "%s" (pp ast)))))

(ert-deftest parser-tests/single-key-dict ()
  (let ((str "{ foo: 1.122 }")
        (ast '((dict (entries (((key . "foo")
                                (value ((type . "1.122"))))))))))
    (parser-tests/check-parser str ast)))

(ert-deftest parser-tests/multiple-keys-dict ()
  (let ((str "{ foo: 1.122, bar: string }")
        (ast '((dict (entries (((key . "foo")
                                (value ((type . "1.122"))))
                               ((key . "bar")
                                (value ((type . "string"))))))))))
    (parser-tests/check-parser str ast)))

(ert-deftest parser-tests/nested-dict ()
  (let ((str "{ foo: 1.122, bar: { a: Type1, b: Type2 } }")
        (ast '((dict (entries (((key . "foo")
                                (value ((type . "1.122"))))
                               ((key . "bar")
                                (value (dict (entries (((key . "a")
                                                        (value ((type . "Type1"))))
                                                       ((key . "b")
                                                        (value ((type . "Type2")))))))))))))))
    (parser-tests/check-parser str ast)))

(ert-deftest parser-tests/single-arg-generic-type ()
  (let ((str "Type<string>")
        (ast '(((type . "Type")
                (generic (entries (((type . "string")))))))))
    (parser-tests/check-parser str ast)))


(ert-deftest parser-tests/multiple-args-generic-type ()
  (let ((str "Type1<string, Type2<number, string>, number>")
        (ast '(((type . "Type1")
                (generic (entries (((type . "string"))
                                   ((type . "Type2")
                                    (generic (entries (((type . "number"))
                                                       ((type . "string"))))))
                                   ((type . "number")))))))))
    (parser-tests/check-parser str ast)))

(ert-deftest parser-tests/big-test ()
  (let ((str "{foo: 1.122, bar: {a: hi<FooType<{i: am, thomas: binetruy<string>}, string>, boolean>, b: what}, hello: what}")
        (ast '((dict (entries (((key . "foo")
                                (value ((type . "1.122"))))
                               ((key . "bar")
                                (value (dict (entries (((key . "a")
                                                        (value ((type . "hi")
                                                                (generic (entries (((type . "FooType")
                                                                                    (generic (entries ((dict (entries (((key . "i")
                                                                                                                        (value ((type . "am"))))
                                                                                                                       ((key . "thomas")
                                                                                                                        (value ((type . "binetruy")
                                                                                                                                (generic (entries (((type . "string")))))))))))
                                                                                                       ((type . "string"))))))
                                                                                    ((type . "boolean"))))))))
                                                       ((key . "b")
                                                        (value ((type . "what")))))))))
                               ((key . "hello")
                                (value ((type . "what"))))))))))
    (parser-tests/check-parser str ast)))

(ert-deftest lexer-tests/is-digit ()
  (should (equal (lexer/is-digit (string-to-char "2")) 0))
  (should (equal (lexer/is-digit (string-to-char "*")) nil)) ; special char not allowed
  (should (equal (lexer/is-digit (string-to-char "b")) nil))) ; letters not allowed

(ert-deftest lexer-tests/is-type ()
  (should (equal (lexer/is-type (string-to-char "2")) nil)) ; cannot start by number
  (should (equal (lexer/is-type (string-to-char "$")) 0)) ; allowed special char
  (should (equal (lexer/is-type (string-to-char "_")) 0)) ; allowed special char
  (should (equal (lexer/is-type (string-to-char "&")) nil)) ; not an allowed special char
  (should (equal (lexer/is-type (string-to-char "s")) 0))) ; can start with letter
