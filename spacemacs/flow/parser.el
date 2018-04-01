
(defun lexer/is-digit (c)
  (string-match "[0-9]" (format "%c" c)))

(defun lexer/read-digit ()
  (let ((current-point (point))
        (match-end-pos (re-search-forward "\\([0-9]*\.\\)?[0-9]+"))
        (read-value ""))
    (if match-end-pos
        (progn
          (setq read-value (format "%s"
                                   (buffer-substring current-point match-end-pos)))
          `(((type . "number")
             (value . ,read-value))))
      (nil))))

(defun lexer/is-type (c)
  (string-match "[a-zA-Z0-9\$]+" (format "%c" c)))

(defun lexer/read-type ()
  (let ((current-point (point))
        (match-end-pos (re-search-forward "[a-zA-Z0-9]+"))
        (read-value ""))
    (if match-end-pos
        (progn
          (setq read-value (format "%s"
                                   (buffer-substring current-point match-end-pos)))
          `(((type . "type")
             (value . ,read-value))))
      (nil))))

(defun lexer/is-special-char (c)
  (string-match "[{}\\:=<>,]" (format "%c" c)))

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
    (list output)))


(defun parser/parse-lexer-output (ast)
  (let ((lexer-output (car (lexer/lex "{foo: 1.122, bar: {a: hi, b: what}}")))
        (i 0)
        (j nil))

    (defun parser/parse-dict-entry (dict-ast)
      (let ((type (cdr (assoc 'type (nth i lexer-output))))
            (value (cdr (assoc 'value (nth i lexer-output)))))
        (setq dict-ast (append dict-ast
                               `((key . ,value))))
        (setq i (+ i 2))
        (setq type (cdr (assoc 'type (nth i lexer-output))))
        (setq value (cdr (assoc 'value (nth i lexer-output))))
        (setq dict-ast (append dict-ast
                               `((value . ,(parser/parse-type)))))
        `(,dict-ast)))

    (defun parser/parse-dictionary ()
      (let ((dict-ast nil)
            (counter 0))
        (while (equal counter 0)
          (setq i (1+ i))
          (setq dict-ast (parser/parse-dict-entry dict-ast))
          (let ((value (cdr (assoc 'value (nth i lexer-output)))))
            (if (equal "}" value)
                (progn
                  (setq counter 1)
                  (message "closing object")))
            (if (and (not (equal "," value))
                     (equal counter 0))
                (message "missing comma, %s" value))
            (if (> i (length lexer-output))
                (progn
                  (message "object not formatted properly (missing '}')")
                  (setq counter 2)))))
        (setq i (+ i 1))
        `(dict . ((entries . ,dict-ast)))))

    (defun parser/parse-type ()
      (let* ((current-entry (nth i lexer-output))
             (current-type (cdr (assoc 'type current-entry)))
             (current-value (cdr (assoc 'value current-entry)))
             (return-value nil)
             (counter nil))
        (if (and (equal current-value "{")
                 (not counter))
            (progn
              (setq counter 1)
              (setq return-value (parser/parse-dictionary))))
        (if (not counter)
            (progn (setq return-value current-value)
                   (setq i (1+ i))))
        (list return-value)))

    (while (< i (length lexer-output))
      (setq ast (append ast (parser/parse-type))))
    ast))

(defun parser/parse ()
  (let ((ast nil))
    (setq ast (parser/parse-lexer-output ast))
    (message "%s" (pp ast))))

(parser/parse)
