(defun parse-missing-type-annotation (err)
  )

(defun is-missing-type-annotation (err)
  (let ((message (alist-get 'message err)))
    (if message
       t
     nil)))

(defun print-h1 (h1)
  (insert (format "* %s \n\n\n" h1)))

(defun print-h2 (h2)
  (insert (format "** %s \n\n" h2)))

(defun print-h3 (h3)
  (insert (format "*** %s \n" h3)))

(defun color-error-message (msg)
  "Converts `foo` into foo in bold without backticks"
  (let ((colored-msg nil))
    (with-temp-buffer "testing"
                      (insert msg)
                      (goto-char (point-min))
                      (while (re-search-forward "`\\([A-Za-z\\.]+\\)`" nil t)
                        (replace-match (propertize (match-string-no-properties 1)
                                                   'face 'bold
                                                   'font-lock-face '(:foreground "spring green"))))
                      (goto-char (point-min))
                      (while (re-search-forward "\\[\\([0-9]+\\)\\]" nil t)
                        (replace-match (concat "["
                                               (format "%s" (propertize (match-string-no-properties 1)
                                                            'face 'bold
                                                            'font-lock-face '(:foreground "orange")))
                                               "]")))
                      (setq colored-msg (buffer-string)))
    colored-msg))

(defun get-error-message (err)
  (mapcar
   (lambda (y)
     (if (equal (format "%s" (cdr (assoc 'type y))) "Blame")
         (let ((colored-msg (color-error-message (alist-get 'descr y))))
           (insert colored-msg))
       (insert
        (propertize
         (format "%s " (cdr (assoc 'descr y)))
         'font-lock-face
         '(:foreground "orange")))))
   (cdr (assoc 'message err))))

(defun get-error-start (err)
  (mapcar
   (lambda (y)
     (if (equal (format "%s" (cdr (assoc 'type y))) "Blame")
         (insert (propertize (format "        error start: %s (%s)\n" (cdr (assoc 'context y)) (cdr (assoc 'descr y))) 'font-lock-face '(:foreground "white")))))
   (cdr (assoc 'message err))))

(defun get-err-extras (err)
  (mapcar (lambda (o) (aref (alist-get 'message o) 0))
          (alist-get 'extra err)))

(defun get-err-message (err)
  (aref (alist-get 'message err) 0))

(defun get-err-file-name (err)
  (let* ((messages  (get-err-extras err))
         (last-msg (nth (- (length messages) 1) messages))
         (path (alist-get 'path last-msg)))
    (format "%s" path)))


(defun get-file-lines-as-list (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" nil)))
;; thanks to “Pascal J Bourguignon” and “TheFlyingDutchman 〔zzbba…@aol.com〕”. 2010-09-02

(defun color-chars-in-str (str start end color)
  (concat (substring str 0 start)
          (substring (propertize str
                                 'font-lock-face
                                 `(:foreground ,color))
                     start end)
          (substring str end (length str))))

(defun color-line (str current-line start-col end-col start-line end-line color)
  (if (not (= start-line end-line)) ; multi line code
      (cond ((and (< current-line end-line)
                  (> current-line start-line)) ; sandwiched line
             (color-chars-in-str str 0 (length str) color))
            ((= current-line start-line) ; start line
             (color-chars-in-str str start-col (length str) color))
            ((= current-line end-line) ; end line
             (color-chars-in-str str 0 end-col color))
            (t str))
    (if (= line start-line) ; single line code
        (color-chars-in-str str start-col end-col color)
      str)))

(defun show-code (err)
  (let* ((extra-messages (cdr (get-err-extras err)))
         (message (get-err-message err))
         (msg-path (alist-get 'path message))
         (msg-line (alist-get 'line message))
         (msg-endline (alist-get 'endline message))
         (msg-start (1- (alist-get 'start message)))
         (msg-end (alist-get 'end message)))
    (mapcar (lambda (m)
              (let* ((path (alist-get 'path m))
                     (interval 3)
                     (result nil)
                     (line0 (- (alist-get 'line m) interval))
                     (endline (+ (alist-get 'endline m) interval))
                     (descr (alist-get 'descr m))
                     (start (- (alist-get 'start m) 1))
                     (end (alist-get 'end m))
                     (code (nth line0 (get-file-lines-as-list path))))
                (insert (format "   %s\n" path))
                (dotimes (i (- endline line0))
                  (progn
                    (setq line (+ line0 i)
                          code (nth (1- line) (get-file-lines-as-list path)))
                    (if (and code
                             (> line 0))
                        (progn
                          (setq result (format "%s %s |  %s \n"
                                               (if (equal (alist-get 'line m) line)
                                                   (color-error-message descr)
                                                 "   ")
                                               line
                                               (color-line
                                                (color-line code line start end
                                                            (+ line0 interval)
                                                            (- endline interval)
                                                            "orange")
                                                 line msg-start msg-end msg-line msg-endline "red")))
                          (insert result)))))
                (insert "\n")))
            extra-messages)))


(defun print-flow-status-error (e)
  (mapcar
   (lambda (err)
     (print-h3 (format "%s" (get-err-file-name err)))
     (insert "\n")
     (get-error-message err)
     (insert "\n\n")
     (show-code err)
     (insert "\n"))
   e))


;; create new window and show flow status in it
(defun create-flow-status-window (json)
  (interactive)
  ;;(split-window-right)
  (let ((file (buffer-file-name))
        (region (string-of-region))
        (buffer (current-buffer)))
    (switch-to-buffer-other-window "*Flowing status*")
    (erase-buffer)
    (print-h1 "flow-status-window")
    (print-h2 "Errors")
    (print-flow-status-error (cdr (assoc 'errors
                                         (json-read-from-string
                                          json)))))
  (outline-mode)
  (local-set-key (kbd "TAB") 'evil-toggle-fold))

(defun flow-status ()
  "Initialize flow"
  (interactive)
  (create-flow-status-window
   (shell-command-to-string
    (format "npm run -s flow -- status --json")))
  )
