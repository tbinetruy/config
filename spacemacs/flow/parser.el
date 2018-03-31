
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
    (format "%s" output)))

(message (lexer/lex"{foo: 1.122, bar: foobar}"))
