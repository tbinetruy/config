(defcustom spark-shell//username nil "login used when connection over ssh")
(defcustom spark-shell//hostname nil "hostname used when connecting over ssh")
(defcustom spark-shell//bin-location nil "hostname used when connecting over ssh")
(defcustom spark-shell//buffer-name "**spark-shell**" "name of spark-shell buffer")

(defun spark-shell//spawn ()
  "Instantiate spark-shell over ssh in new window"
  (interactive)
  (pop-to-buffer
   (get-buffer-create
    (generate-new-buffer-name spark-shell//buffer-name)))
  (shell (current-buffer))
  (process-send-string nil (concat
                            "ssh "
                            spark-shell//username
                            "@"
                            spark-shell//hostname
                            "\n"))
  (process-send-string nil (concat
                            spark-shell//bin-location
                            "\n")))

(defun spark-shell//send-buffer ()
  (interactive)
  (with-current-buffer spark-shell//buffer-name (insert ":paste\n"))
  (append-to-buffer spark-shell//buffer-name (point-min) (point-max))
  (with-current-buffer spark-shell//buffer-name (comint-send-eof)))

