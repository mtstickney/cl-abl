(in-package :cl-user)

(defpackage :com.bitworks.abl
  (:use :cl))

(in-package :com.bitworks.abl)

(defvar *INDENT-LEVEL* 0)
(defvar *INDENT-SIZE* 8)
(defvar *DESTINATION* nil)

;; Lazy evaluation, the cheap way
(defmacro delay (expr) `(lambda () ,expr))
(defun force (thunk) (funcall thunk))

(defun ast-element (node)
  (car node))
(defun indentation-string ()
  (make-sequence 'string (* *INDENT-LEVEL* *INDENT-SIZE*) :initial-element #\Space))
(defun seq (&rest r)
  "Join a sequence of statement results together."
  (cons 'seq r))

(defun abl-var (type name &key (undo nil))
  (list 'var type name undo))

(defun print-with-indent (destination format-string &rest r)
  (format destination "~a~a"
          (indentation-string)
          (apply #'format `(nil ,format-string ,@r))))

(defun vars (&rest r)
  (seq (map 'list (lambda (vdecl)
                    (funcall #'abl-var vdecl)))))
(defmacro with-indent (n &body body)
  `(progn
     (incf *INDENT-LEVEL* ,n)
     ,@body
     (decf *INDENT-LEVEL* ,n)))

(defun abl-var (type name &key (undo nil))
  (print-with-indent t "DEFINE VARIABLE ~a AS ~a~a.~%"
                     (force name)
                     (force type)
                     (if undo ""
                       " NO-UNDO")))

(defmacro vars (&body body)
  `(progn
     ,@(map 'list (lambda (v)
                    `(abl-var (delay ,(first v)) ,(delay  (second v))))
            body)))

(defun abl-parm (direction type name &key (undo nil))
  (list 'parm direction type name undo))
(defun parms (&rest r)
  (seq
   (map 'list (lambda (pdecl)
                (funcall #'abl-parm pdecl)))))
(defun abl-procedure (name parm-list var-list &rest r)
  (list 'proc name (funcall #'parms parm-list)
         (funcall #'vars var-list)
         r))

(defun abl-assign (&rest r)
  (cons 'assign r))

(defun abl-call (name parm-list)
  (list 'call name (funcall #'parms parm-list)))

(defun abl-handler-block (form &rest handler-list)
  (list 'with-handlers form
        (map 'list #'abl-handler handler-list)))
