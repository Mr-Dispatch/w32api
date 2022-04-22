(defpackage w32api
  (:use :cl :rutils :st-json)
;  (:local-nicknames (:ax :alexandria))
  )

(in-package :w32api)

(named-readtables:in-readtable rutils-readtable)

(defun api-file-path (namespace)
  (asdf:system-relative-pathname :w32api (format nil "api/~a.json" namespace)))

(defun read-api-spec (namespace)
  (st-json:read-json
   (uiop:read-file-string (api-file-path namespace))))

(defgeneric namespace-type (kind name obj))

(defmethod namespace-type (kind name obj)
  (error "Unsupported type kind: ~a [in type ~a]" kind name))

(defun process-type-def (def)
  (switch ((getjso "Kind" def) :test 'equal)
    ("Native"
     (ensure-keyword (getjso "Name" def)))
    ("PointerTo"
     `(:pointer ,(process-type-def (getjso "Child" def))))
    (otherwise
     (error "Unsupported type def: ~a" def))))

(defmethod namespace-type ((kind (eql :nativetypedef)) name obj)
  `(defctype ,name ,(process-type-def (getjso "Def" obj))))

(defmethod namespace-type ((kind (eql :struct)) name obj)
  (format nil ";; TODO: struct not supported for now [~a]" name))

(defmethod namespace-type ((kind (eql :union)) name obj)
  (format nil ";; TODO: union not supported for now [~a]" name))

(defmethod namespace-type ((kind (eql :functionpointer)) name obj)
  `(defctype ,name :pointer))  ;; TODO: Does CFFI support "fully specced" function pointer types ?

(defmethod namespace-type ((kind (eql :enum)) name obj)
  `(defcenum (,name ,(ensure-keyword (getjso "IntegerBase" obj)))
     ,@(loop for v in (getjso "Values" obj)
             collect (list (ensure-keyword (getjso "Name" v))
                           (getjso "Value" v)))))

(defun namespace-types (ns-json)
  (loop for type in (getjso "Types" ns-json)
        collect (namespace-type (ensure-keyword (make-symbol (getjso "Kind" type)))
                                (make-symbol (getjso "Name" type))
                                type)))

(defun namespace-constants (ns-json)
  (loop for const in (getjso "Constants" ns-json)
        for name = (make-symbol (getjso "Name"const))
        for val = (getjso "Value" const)
        collect `(defconstant ,name ,val)))

(defun namespace-functions (ns-json))

(defun output-namespace-code (namespace &optional (stream t))
  (let ((ns-json (read-api-spec namespace)))
    (format stream ";;; Constant definitions~%")
    (loop for c in (namespace-constants ns-json)
          do (pprint c stream))
    (format stream "~%;;; Type definitions~%")
    (loop for ty in (namespace-types ns-json)
          if (consp ty)
            do (pprint ty stream)
          else
            do (format stream "~%~a" ty))))

#+nil (progn
  (ql:quickload :w32api)
  (in-package :w32api)
  (with-out-file (f "./out.lisp")
    (output-namespace-code "Foundation" f)))
