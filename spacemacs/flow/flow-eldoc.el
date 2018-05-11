(load-file "./parser.el")

(defun flow-eldoc/highlight-function (ast)
  (defun parse-return-value (return-value-ast)
    (concat " => " (flow-eldoc/highlight-ast return-value-ast)))

  (defun parse-argument (argument-ast)
    (let ((key (alist-get 'key argument-ast))
          (value (flow-eldoc/highlight-ast (alist-get 'value argument-ast)))
          (str ""))
      (if key
          (setq str (concat (propertize key 'face 'font-lock-variable-name-face) ": " value))
        value)))

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
        (return-value (parse-return-value (alist-get 'return-value ast))))
    (concat arguments return-value)))

(defun flow-eldoc/highlight-ast (_ast)
  (let* ((ast (car _ast))
         (counter t)
         (result nil)
         (value (alist-get 'value ast))
         (type (alist-get 'type ast)))
    (if (and (equal type "name")
             counter)
        (progn
          (setq counter nil
                result (propertize value 'face 'font-lock-type-face))))
    (if (and (equal type "function")
             counter)
        (progn
          (setq counter nil
                result (flow-eldoc/highlight-function ast))))
    result))

(defun flow-eldoc/highlight-str (str)
  (let* ((ast (parser/parse str)))
    (flow-eldoc/highlight-ast ast)))
