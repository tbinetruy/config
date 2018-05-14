(load-file "./parser.el")

(defun type-name-color (str)
  (propertize str 'face 'font-lock-type-face))

(defun generic-name-color (str)
  (propertize str 'face 'font-lock-comment-face))

(defun key-name-color (str)
  (propertize str 'face 'font-lock-variable-name-face))

(defun parse-generics (generics-ast)
  (let ((args (mapcar (lambda (e) (flow-eldoc/highlight-ast (list e))) generics-ast))
        (i 0)
        (str "<"))
    (while (< i (- (length args) 1))
      (setq str (concat str (nth i args) ", ")
            i (1+ i)))
    (setq str (concat str (nth (- (length args) 1) args) ">"))
    str))

(defun flow-eldoc/highlight-function (ast)
  (defun parse-return-value (return-value-ast)
    (concat " => " (flow-eldoc/highlight-ast return-value-ast)))

  (defun parse-argument (argument-ast)
    (let* ((key (alist-get 'key argument-ast))
           (value (flow-eldoc/highlight-ast (alist-get 'value argument-ast)))
           (str "")
           (is-optional (alist-get 'is-optional (car (alist-get 'value argument-ast)))))
      ;(if is-optional
      ;    (setq str "?"))
      (if key
          (setq str (concat str (key-name-color key) ": " value))
        (concat str value))))

  (defun parse-arguments (arguments-ast)
    (let ((args (mapcar #'parse-argument arguments-ast))
          (i 0)
          (str "("))
      (while (< i (- (length args) 1))
        (setq str (concat str (nth i args) ", ")
              i (1+ i)))
      (setq str (concat str (nth (- (length args) 1) args) ")"))
      str))


  (let ((arguments (parse-arguments (car (alist-get 'arguments ast))))
        (return-value (parse-return-value (alist-get 'return-value ast)))
        (generics (car (alist-get 'entries (alist-get 'generic ast)))))
    (concat (if generics (parse-generics generics) "") arguments return-value)))

(defun flow-eldoc/highlight-ast (_ast)
  (let* ((ast (car _ast))
         (counter t)
         (result nil)
         (value (alist-get 'value ast))
         (type (alist-get 'type ast))
         (generics (car (alist-get 'entries (alist-get 'generic ast))))
         (is-array (alist-get 'is-array ast))
         (is-optional (alist-get 'is-optional ast)))
    (if (and (equal type "name")
             counter)
        (progn
          (if is-optional
              (setq result "?"))
          (setq counter nil
                result (concat result (type-name-color value)))
          (if generics
              (setq result (generic-name-color result)
                    result (concat result (parse-generics generics))))
          (if is-array
              (setq result (concat result "[]")))))
    (if (and (equal type "function")
             counter)
        (progn
          (setq counter nil
                result (flow-eldoc/highlight-function ast))))

    result))

(defun flow-eldoc/highlight-str (str)
  (let* ((ast (parser/parse str)))
    (flow-eldoc/highlight-ast ast)))
