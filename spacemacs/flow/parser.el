(defun lexer/is-digit (c)
  (string-match "[0-9]" (format "%c" c)))

(defun lexer/read-digit ()
  (let ((current-point (point))
        (match-end-pos (re-search-forward "\\([0-9]*\\.\\)?[0-9]+"))
        (read-value ""))
    (if match-end-pos
        (progn
          (setq read-value (format "%s"
                                   (buffer-substring current-point match-end-pos)))
          `(((type . "number")
             (value . ,read-value))))
      (nil))))

(defun lexer/is-type (c)
  (string-match "[a-zA-Z$_]+" (format "%c" c)))

(defun lexer/is-keyword (w)
  (message "reading kw")
  (toggle-case-fold-search)
  (let ((return-value (string-match "\\(type\\|class\\|interface\\|opaque\\)" (format "%s" w))))
    (toggle-case-fold-search)
    return-value))

(defun lexer/read-type ()
  (let ((current-point (point))
        (match-end-pos (re-search-forward "[a-zA-z$_][a-zA-Z0-9$_]*"))
        (read-value ""))
    (if match-end-pos
        (progn
          (setq read-value (format "%s"
                                   (buffer-substring current-point match-end-pos)))
          `(((type . ,(if (lexer/is-keyword read-value)
                          "keyword"
                        "type"))
             (value . ,read-value))))
      (nil))))

(defun lexer/is-special-char (c)
  (string-match "[]()[{}\\:=<>,|&?+]" (format "%c" c)))

(defun lexer/read-special-char ()
  (let ((current-point (point))
        (read-value ""))
    (setq read-value (format "%c"
                             (char-after)))
    `(((type . "special-char")
       (value . ,read-value)))))

(defun lexer/lex (str)
  (let ((output nil)
        (input nil)
        (current-character nil)
        (counter nil)
        (keywords '("type")))
    (defun lexer/test-character (predicate success-callback backward-on-success)
      (if (and (funcall predicate current-character)
               (not counter))
          (progn
            (setq counter 1)
            (setq output (append output
                                 (funcall success-callback)))
            (backward-char backward-on-success))
        nil))
    (defun lexer/top-level-parsing ()
      (lexer/test-character 'lexer/is-digit 'lexer/read-digit
                            1)
      (lexer/test-character 'lexer/is-special-char
                            'lexer/read-special-char 0)
      (lexer/test-character 'lexer/is-type 'lexer/read-type
                            1)
      (if counter
          (setq counter nil)
        nil))
    (with-temp-buffer
      "tmp"
      (insert str)
      (goto-char (point-min))
      (while (not (eobp))
        (setq current-character (char-after))
        (lexer/top-level-parsing)
        (forward-char 1))
      (setq input (buffer-string)))
    output))


(defun parser/parse-lexer-output (ast str &optional lexer-output)
  (let ((lexer-output (or lexer-output (lexer/lex str)))
        (i 0)
        (j nil))

    (defun parser/parse-dict-entry (dict-ast)
      (let ((type (cdr (assoc 'type (nth i lexer-output))))
            (value (cdr (assoc 'value (nth i lexer-output))))
            (dict-entry nil)
            (is-immutable nil)
            (next-value nil))
        (if (string= "+" value)
            (progn
              (setq is-immutable t)
              (setq i (1+ i))
              (setq value (cdr (assoc 'value (nth i lexer-output))))))
        (if (string= "[" value)
            (progn
              (setq i (1+ i)
                    value (alist-get 'value (nth i lexer-output))
                    next-value (alist-get 'value (nth (1+ 1) lexer-output)))
              (setq dict-entry (append dict-entry
                                       `((key . ,(parser/parse-function-args))
                                         (is-indexer-prop . t))))
              (setq i i
                    value (alist-get 'value (nth i lexer-output))))
          (setq dict-entry (append dict-entry
                                   `((key . ,value)))))
        (setq i (+ i 1))
        (setq value (cdr (assoc 'value (nth i lexer-output))))
        (if (and (not (equal ":" value))
                 (equal counter 0))
            (message "missing ':' ; got %s" value))
        (setq i (+ i 1))
        (setq dict-entry (append dict-entry
                               `((value . ,(parser/parse-type)))))
        (if is-immutable
            (setq dict-entry (append dict-entry `((is-immutable . ,t)))))
        (append dict-ast (list dict-entry))))

    (defun parser/loop-delimeter (close separator callback pass-arg)
      (let ((return-value nil)
            (counter 0)            ; main loop
            (close-counter 0)
            (close-chars (split-string close))     ; for handling multiple closing chars
            (fail nil))
        (while (equal counter 0)
          (setq i (1+ i))
          (if pass-arg
              (setq return-value (funcall callback return-value))
            (setq return-value (append return-value (funcall callback))))
          (let ((value (alist-get 'value (nth i lexer-output))))
            (if (equal (nth close-counter close-chars) value)
                (progn
                  (while (and (< close-counter (length close-chars))
                              (equal fail nil))
                    (progn
                      (let* ((j (+ i close-counter))
                             (current-value-bis (alist-get 'value (nth j lexer-output)))
                             (current-closing-char (nth close-counter close-chars)))
                        (if (string= current-value-bis current-closing-char)
                            (setq j (1+ j)
                                  close-counter (1+ close-counter))
                          (setq fail t)))))
                  (if (equal fail nil)
                      (setq counter 1
                            i (+ i (- (length close-chars) 1)))
                    nil)))
            (if (and (not (equal separator value))
                     (equal counter 0))
                (message "missing '%s' ; got %s" separator value))
            (if (> i (length lexer-output))
                (progn
                  (message "object not formatted properly (missing '%s')" close)
                  (setq counter 2)))))
        (list return-value)))

    (defun parser/parse-dictionary ()
      (let ((dict-ast nil)
            (counter 0)
            (is-exact nil)
            (return-value nil))
        (if (string= "|" (alist-get 'value (nth (1+ i) lexer-output)))
            (progn
              (message "yo")
              (setq is-exact t
                    i (1+ i)))
          nil)
        (setq return-value `((type . "dict")
                             (entries . ,(parser/loop-delimeter (if is-exact "| }" "}")
                                                                ","
                                                                'parser/parse-dict-entry
                                                                t))))
        (if is-exact
            (setq return-value (append return-value '((is-exact . t)))))
        return-value))

    (defun parser/parse-generic ()
      (let ((dict-ast nil)
            (counter 0))
        `(generic . ((entries . ,(parser/loop-delimeter ">" "," 'parser/parse-type nil))))))

    (defun parser/parse-function-args ()
      (let ((arg-ast nil)
            (is-named-arg nil)
            (current-value (cdr (assoc 'value (nth i lexer-output))))
            (next-value (cdr (assoc 'value (nth (1+ i) lexer-output))))
            (return-value '((key nil)
                            (value nil))))
        ;; check if next value is colon
        (if (string= ":" next-value)
            (setq return-value (parser/parse-dict-entry '()))
          (setq return-value `(((key . nil)
                                (value . ,(parser/parse-type))))))
        return-value))

    (defun parser/parse-function ()
      (let ((func-ast nil)
            (current-value (cdr (assoc 'value (nth i lexer-output))))
            (return-value `((type . "function")
                            (arguments . ,(parser/loop-delimeter ")" "," 'parser/parse-function-args nil)))))
        (setq i (1+ i))
        (setq current-value (cdr (assoc 'value (nth i lexer-output))))
        (if (string= "=" current-value)
          (progn
            (setq i (1+ i))
            (setq current-value (cdr (assoc 'value (nth i lexer-output))))
            (if (string= ">" current-value)
                (progn
                  (setq i (1+ i))
                  (setq current-value (cdr (assoc 'value (nth i lexer-output))))
                  (setq return-value (append return-value `((return-value . ,(parser/parse-type))))))
              (message "Function arrow miss formatted at %s" i)))
          (message "Missing function arrow at %s" i))
        return-value))

    (defun parser/get-matching-closing-bracket (j)
      (let* ((current-value (cdr (assoc 'value (nth j lexer-output)))))
        (if (not (string= "(" current-value))
            (message "Not on an opening bracket character, got %s" current-value)
          (while (not (string= ")" current-value))
            (progn
              (setq j (1+ j))
              (setq current-value (cdr (assoc 'value (nth j lexer-output))))
              (if (string= "(" current-value)
                  (setq j (parser/get-matching-closing-bracket j)))))
          j)))

    (defun parser/is-function ()
      (let* ((j (parser/get-matching-closing-bracket i))
            (current-value (alist-get 'value (nth j lexer-output))))
        (setq j (1+ j)
              current-value (alist-get 'value (nth j lexer-output)))
        (if (string= "=" current-value)
            (progn
              (setq j (1+ j)
                    current-value (alist-get 'value (nth j lexer-output)))
              (if (string= ">" current-value)
                  t
                nil))
          nil)))

    (defun parser/parse-group ()
      (let* ((closing-pos (parser/get-matching-closing-bracket i))
             ;; excludes brackets
             (group-lex (subseq lexer-output (+ i 1) (- closing-pos 1)))
             (return-value (parser/parse-lexer-output nil nil group-lex)))
        ;; skip closing bracket
        (setq i (1+ closing-pos))
        `((type . "group")
          (value . ,return-value))))

    (defun parser/parse-equality (return-value)
      (message "foobar ofoo yoo %s" return-value)
      (let ((counter 0)
            (current-value (alist-get 'value (nth i lexer-output))))
        (if (equal current-value "type")
            (setq return-value (append '((type . "alias")) return-value)))
        (if (equal current-value "class")
            (setq return-value (append '((type . "class")) return-value)))
        (if (equal current-value "interface")
            (setq return-value (append '((type . "interface")) return-value))
          (message "invalid keyword"))
        (setq i (1+ i)
              current-value (alist-get 'value (nth i lexer-output))
              return-value (append `((name . ,current-value)) return-value)
              i (1+ i)
              current-value (alist-get 'value (nth i lexer-output)))
        (message "=== %s" current-value)
        (if (equal "=" current-value)
            (setq i (1+ i)
                  return-value (append return-value `((value . ,(parser/parse-type)))))
          (message "missing equal sign in keyword equality"))
        return-value))

    (defun parser/parse-keyword (current-value)
      (let ((return-value nil))
        (if (string= current-value "opaque")
            (setq i (1+ i)
                  return-value '((is-opaque . t))))
        (setq return-value (parser/parse-equality return-value))
        return-value))

    (defun parser/parse-type (&optional closing-pos)
      (let* ((current-entry (nth i lexer-output))
             (current-type (cdr (assoc 'type current-entry)))
             (current-value (cdr (assoc 'value current-entry)))
             (return-value nil)
             (counter nil)
             (is-optional-type nil)
             (closing-pos (or closing-pos
                              (length lexer-output))))

        ; Maybe type
        (if (equal current-value "?")
            (progn
              (setq i (1+ i))
              (setq current-value (cdr (assoc 'value (nth i lexer-output))))
              (setq is-optional-type t))
          nil)


        ;;;; Operators on current type

        ;; keywords
        (if (equal current-type "keyword")
            (progn
              (setq return-value (parser/parse-keyword
                                  current-value)
                    counter 1)))

        (message "r3turning %s" return-value)

        ; Dict
        (if (and (equal current-value "{")
                 (not counter))
            (progn
              (setq counter 1)
              (setq return-value (parser/parse-dictionary))
              (setq i (1+ i))
              (setq current-value (cdr (assoc 'value (nth i lexer-output))))))

        ; Tuple
        (if (and (equal current-value "[")
                 (not counter))
            (progn
              (setq counter 1)
              (setq return-value `((type . "tuple")
                                  (value . ,(parser/loop-delimeter "]" "," 'parser/parse-type nil))))
              (setq i (1+ i))
              (setq current-value (cdr (assoc 'value (nth i lexer-output))))))

        (if (and (equal current-value "(")
                 (not counter))
            (let ((is-function (parser/is-function)))
              (progn
               (setq counter 1)
               (if is-function
                   (setq return-value (parser/parse-function))
                 (progn
                   (setq return-value (parser/parse-group))
                   (setq current-value (alist-get 'value (nth i lexer-output))))))))

        ; Default
        (if (not counter)
            (progn (setq return-value `((type . "name")
                                        (value . ,current-value)))
                   (setq i (1+ i))
                   (setq current-value (cdr (assoc 'value (nth i lexer-output))))))

        ;;;; Operators on previous type

        ; Generics
        (if (string= "<" current-value)
            (progn
              (setq return-value (append return-value (list (parser/parse-generic))))
              (setq i (+ 1 i))
              (setq current-value (cdr (assoc 'value (nth i lexer-output))))))

        ; Union
        (if (string= "&" current-value)
            (progn
              (setq i (1+ i))
              (setq return-value (append return-value `((intersection . ,(parser/parse-type)))))))

        ; Array
        (if (string= "[" current-value)
            (progn
              (setq return-value (append return-value `((is-array . ,t))))
              (setq i (1+ i))
              (setq current-value (cdr (assoc 'value (nth i lexer-output))))
              (if (not (string= "]" current-value))
                  (message "not array")
                (progn
                  (setq i (1+ i))
                  (setq current-value (cdr (assoc 'value (nth i lexer-output))))))))

        ; Intersection
        (if (and
             (string= "|" current-value)
             (not (string= "}" (alist-get 'value (nth (1+ i) lexer-output)))))
            (progn
              (setq i (1+ i))
              (setq return-value (append return-value `((union . ,(parser/parse-type)))))))

        ; Maybe
        (if is-optional-type
            (setq return-value (append return-value '((is-optional . t)))))

        (list return-value)))

    (while (< i (length lexer-output))
      (setq ast (append ast (parser/parse-type))))
    ast))

(defun parser/parse (str)
  (let ((ast nil))
    (setq ast (parser/parse-lexer-output ast str))
    ast))

(defun flow-eldoc/parse-type (str)
  (let* ((ast (car (parser/parse str)))
         (output "")
         (type (cdr (assoc 'type ast))))
    (if (equal type "dict")
        ()
      nil)
    (pp ast)))

(flow-eldoc/parse-type "{foo: 1.122, bar: yo}")

(provide 'parser)
