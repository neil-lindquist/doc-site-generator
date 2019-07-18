(defpackage :doc-gen
  (:use :cl)
  (:export #:print-documentation
           #:print-package-documentation))
(in-package :doc-gen)

(defun sanitize-string (s)
  (cl-ppcre:regex-replace-all "\\*" (string s) "\\*"))

(defun doc-print (stream type name args doc)
  (format stream "<a name=\"~A-~@[~A:~]~A\"></a>**~A** - ~A ~@[~A~]~
                  ~@[  ~%~A~]~%~%"
          (string-downcase (sanitize-string type))
          (when (symbolp name)
            (string-downcase (sanitize-string (package-name (symbol-package name)))))
          (string-downcase (sanitize-string name))

          (sanitize-string type)
          (sanitize-string name)
          (when args (sanitize-string (format nil "~A" args)))

          doc))

(defgeneric print-documentation (type name stream)
  (:documentation "Outputs the documentation for the named system"))

(defmethod print-documentation ((_ (eql 'function)) name stream)
  (doc-print stream
          (cond
            ((macro-function name) "MACRO")
            ((typep (symbol-function name) 'standard-generic-function) "GENERIC")
            (t "FUNCTION"))
          name
          (sb-introspect:function-lambda-list name)
          (documentation name 'function)))

(defmethod print-documentation ((_ (eql 'variable)) name stream)
  (doc-print stream 'variable name nil (documentation name 'variable)))

(defmethod print-documentation ((_ (eql 'class)) name stream)
  (let ((class (find-class name)))
    (doc-print stream (cond
                        ((subtypep class 'condition) "CONDITION")
                        ((typep class 'standard-class) "CLASS")
                        ((typep class 'structure-class) "STRUCT")
                        (t (error "Unknown class type ~A for ~A" (type-of class) class)))
           name nil (documentation class t))))

(defmethod print-documentation ((_ (eql 'package)) name stream)
  (format stream "<br>~%### ")
  (doc-print stream 'package (string name) nil (documentation (find-package name) t)))



(defmethod symbol-bound-p ((_ (eql 'function)) symbol)
  (fboundp symbol))
(defmethod symbol-bound-p ((_ (eql 'class)) symbol)
  (find-class symbol nil))
(defmethod symbol-bound-p ((_ (eql 'variable)) symbol)
  (boundp symbol))


(defun print-package-documentation (package-name stream)
  "Prints the documentation for the package"
  (let ((package (find-package package-name)))
    (print-documentation 'package (package-name package) stream)
    (do-external-symbols (symbol package)
      (dolist (type '(function class variable))
        (when (and (eq (symbol-package symbol) package) ;don't document reexported symbols
                   (symbol-bound-p type symbol))
          (print-documentation type symbol stream))))))
