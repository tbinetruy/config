(defun parse/is-digit (c)
  (string-match "[0-9]" (format "%c" c)))

(defun parse/read-digit ()
  (let ((current-point (point))
        (match-end-pos (re-search-forward "\\([0-9]*\.\\)?[0-9]+"))
        (read-value ""))
    (if match-end-pos
        (progn
          (setq read-value (format "%s"
                                   (buffer-substring current-point match-end-pos)))
          `((type . "number")
            (value . ,read-value)))
      (nil))))

(defun parse/is-type (c)
  (string-match "[a-zA-Z0-9\$]+" (format "%c" c)))

(defun parse/read-type ()
  (let ((current-point (point))
        (match-end-pos (re-search-forward "[a-zA-Z0-9]+"))
        (read-value ""))
    (if match-end-pos
        (progn
          (setq read-value (format "%s"
                                   (buffer-substring current-point match-end-pos)))
          `((type . "type")
            (value . ,read-value)))
      (nil))))

(defun parse/is-special-char (c)
  (string-match "[{}\\:=<>,]" (format "%c" c)))

(defun parse/read-special-char ()
  (let ((current-point (point))
        (read-value ""))
    (setq read-value (format "%c"
                             (char-after)))
    `((type . "special-char")
      (value . ,read-value))))

(defun parse (str)
  (let ((output "")
        (current-character nil)
        (counter nil)
        (keywords '("type")))
    (defun parse/top-level-parsing ()
      (if (and (parse/is-digit current-character)
               (not counter))
          (progn
            (setq counter 1)
            (message "%s" (parse/read-digit))
            (backward-char 1))
        nil)
      (if (and (parse/is-special-char current-character)
               (not counter))
          (progn
            (setq counter 1)
            (message "%s" (parse/read-special-char)))
        nil)
      (if (and (parse/is-type current-character)
               (not counter))
          (progn
            (setq counter 1)
            (message "%s"
                     (parse/read-type))
            (backward-char 1))
        nil)
      (if counter
          (setq counter nil)
        nil))

    (with-temp-buffer
      "tmp"
      (insert str)
      (goto-char (point-min))
      (while (not (eobp))
        (setq current-character (char-after))
        (parse/top-level-parsing)
        (forward-char 1))
      (setq output (buffer-string)))
    (format "%s" output)))

(provide 'parse)

(message (parse "{foo: 1.122, bar: foobar}"))
