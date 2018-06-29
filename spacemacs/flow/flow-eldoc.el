(load-file "~/config/spacemacs/flow/parser.el")

(defun type-name-color (str)
  (propertize str 'face 'font-lock-type-face))

(defun kw-color (str)
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

(defun parse-arguments (arguments-ast open close)
  (let ((args (mapcar #'parse-argument arguments-ast))
        (i 0)
        (str open))
    (while (< i (- (length args) 1))
      (setq str (concat str (nth i args) ", ")
            i (1+ i)))
    (setq str (concat str (nth (- (length args) 1) args) close))
    str))

(defun flow-eldoc/highlight-function (ast)
  (defun parse-return-value (return-value-ast)
    (concat " => " (flow-eldoc/highlight-ast return-value-ast)))

  (let ((arguments (parse-arguments (car (alist-get 'arguments ast)) "(" ")"))
        (return-value (parse-return-value (alist-get 'return-value ast)))
        (generics (car (alist-get 'entries (alist-get 'generic ast)))))
    (concat (if generics (parse-generics generics) "") arguments return-value)))

(defun flow-eldoc/highlight-class (ast)
  (let ((name (alist-get 'name ast))
        (value (alist-get 'value ast)))
    (concat (kw-color "class") name " = " (flow-eldoc/highlight-ast value))))

(defun flow-eldoc/highlight-equality (ast type)
  (let* ((name (alist-get 'name ast))
        (value (alist-get 'value ast))
        (is-opaque (alist-get 'is-opaque ast))
        (generics (car (alist-get 'entries (alist-get 'generic ast))))
        (result (concat (kw-color type) " " name)))
    (if is-opaque
        (setq result (concat (kw-color "opaque ") result)))
    (if generics
        (setq result (concat result (parse-generics generics))))
    (if value
        (concat result " = " (flow-eldoc/highlight-ast value))
      result )))

(defun flow-eldoc/highlight-alias (ast)
  (flow-eldoc/highlight-equality ast "type"))

(defun flow-eldoc/highlight-class (ast)
  (flow-eldoc/highlight-equality ast "class"))

(defun flow-eldoc/highlight-interface (ast)
  (flow-eldoc/highlight-equality ast "interface"))

(defun parse-tuple (ast)
  (let* ((value (car (alist-get 'value ast)))
         (str "[")
         (i 0)
         (imax (length value)))
    (while (< i imax)
      (setq str (concat str
                        (flow-eldoc/highlight-ast
                         (list (nth i value)))
                        (if (< i (1- imax)) ", "))
            i (1+ i)))
    (concat str "]")))

(defun flow-eldoc/highlight-ast (_ast)
  (let* ((ast (car _ast))
         (counter t)
         (result nil)
         (value (alist-get 'value ast))
         (type (alist-get 'type ast))
         (union (alist-get 'union ast))
         (intersection (alist-get 'intersection ast))
         (generics (car (alist-get 'entries (alist-get 'generic ast))))
         (is-array (alist-get 'is-array ast))
         (is-optional (alist-get 'is-optional ast)))
    (if (and (equal type "name")
             counter)
        (progn
          (if is-optional
              (setq result (concat result "?")))
          (setq counter nil
                result (concat result (type-name-color value)))
          (if generics
              (setq result (generic-name-color result)
                    result (concat result (parse-generics generics))))
          (if is-array
              (setq result (concat result "[]")))))
    (if (and (equal type "tuple")
             counter)
        (progn
          (setq counter nil
                result (parse-tuple ast))))
    (if (and (equal type "alias")
             counter)
        (progn
          (setq counter nil
                result (flow-eldoc/highlight-alias ast))))
    (if (and (equal type "class")
             counter)
        (progn
          (setq counter nil
                result (flow-eldoc/highlight-class ast))))
    (if (and (equal type "interface")
             counter)
        (progn
          (setq counter nil
                result (flow-eldoc/highlight-interface ast))))
    (if (and (equal type "dict")
             counter)
        (progn
          (let ((is-exact (alist-get 'is-exact ast))
                (open "{")
                (close "}"))
            (if is-exact
                (setq open "{|"
                      close "|}"))
            (setq counter nil
                  result (parse-arguments (car (alist-get 'entries ast)) open close)))))
    (if (and (equal type "group")
             counter)
        (progn
          (setq counter nil
                result (concat "(" (flow-eldoc/highlight-ast value) ")"))))
    (if (and (equal type "function")
             counter)
        (progn
          (setq counter nil
                result (flow-eldoc/highlight-function ast))))
    (if union
        (setq result (concat result " | " (flow-eldoc/highlight-ast union))))
    (if intersection
        (setq result (concat result " & " (flow-eldoc/highlight-ast intersection))))
    result))

(defun flow-eldoc/highlight-str (str)
  (let* ((ast (parser/parse str))
         (return-val (flow-eldoc/highlight-ast ast)))
    (message return-val)
    return-val))
