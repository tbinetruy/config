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

(defun lexer/read-type ()
  (let ((current-point (point))
        (match-end-pos (re-search-forward "[a-zA-z$_][a-zA-Z0-9$_]*"))
        (read-value ""))
    (if match-end-pos
        (progn
          (setq read-value (format "%s"
                                   (buffer-substring current-point match-end-pos)))
          `(((type . "type")
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


(defun parser/parse-lexer-output (ast str)
  (let ((lexer-output (lexer/lex str))
        (i 0)
        (j nil))

    (defun parser/parse-dict-entry (dict-ast)
      (let ((type (cdr (assoc 'type (nth i lexer-output))))
            (value (cdr (assoc 'value (nth i lexer-output))))
            (dict-entry nil)
            (is-immutable nil))
        (if (string= "+" value)
            (progn
              (setq is-immutable t)
              (setq i (1+ i))
              (setq value (cdr (assoc 'value (nth i lexer-output))))))
        (setq dict-entry (append dict-entry
                               `((key . ,value))))
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
            (counter 0))
        (while (equal counter 0)
          (setq i (1+ i))
          (if pass-arg
              (setq return-value (funcall callback return-value))
            (setq return-value (append return-value (funcall callback))))
          (let ((value (cdr (assoc 'value (nth i lexer-output)))))
            (if (equal close value)
                (progn
                  (setq counter 1)
                  (message "closing object")))
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
            (counter 0))
        `((type . "dict")
          (entries . ,(parser/loop-delimeter "}" "," 'parser/parse-dict-entry t)))))

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
              (message "%s" current-value)
              (if (string= "(" current-value)
                  (progn
                    (message "yoooo")
                    (setq j (parser/get-matching-closing-bracket j))
                nil))))
          j)))

    (defun parser/parse-type ()
      (let* ((current-entry (nth i lexer-output))
             (current-type (cdr (assoc 'type current-entry)))
             (current-value (cdr (assoc 'value current-entry)))
             (return-value nil)
             (counter nil)
             (is-optional-type nil))

        ;;;; Operators on upcoming type

        ; Maybe type
        (if (and (equal current-value "?"))
            (progn
              (setq i (1+ i))
              (setq current-value (cdr (assoc 'value (nth i lexer-output))))
              (setq is-optional-type t))
          nil)

        ;;;; Operators on current type

        ; Dict
        (if (and (equal current-value "{")
                 (not counter))
            (progn
              (setq counter 1)
              (setq return-value (parser/parse-dictionary))
              (setq i (1+ i))
              (setq current-value (cdr (assoc 'value (nth i lexer-output))))))

        (if (and (equal current-value "(")
                 (not counter))
            (progn
              (setq counter 1)
              (setq return-value (parser/parse-function))))

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
                  (message "array not closed")
                (progn
                  (setq i (1+ i))
                  (setq current-value (cdr (assoc 'value (nth i lexer-output))))))))

        ; Intersection
        (if (string= "|" current-value)
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
