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

(defun indent (line &optional (n 1))
  (reverse (cons line
                 (loop for i from 1 to *INDENT-LEVEL* collecting
                       (make-sequence 'string *INDENT-SIZE* :initial-element #\Space)))))

(defun with-indent (n &rest rest)
  (let ((lines '()))
    (incf *INDENT-LEVEL* n)
    (setq lines  (map 'list (lambda (line)
                              (indent line n))
                      rest))
    (decf *INDENT-LEVEL* n)
    lines))

(defun seq (&rest r)
  "Join a sequence of statement results together."
  (cons 'seq r))

(defun abl-var (type name &key (undo nil))
  (list 'var type name undo))

;; (defun
;;   (list  (indent (format nil "DEFINE VARIABLE ~a AS ~a~a."
;;                          name
;;                          type
;;                          (if undo ""
;;                            " NO-UNDO")))))

(defun vars (&rest r)
  (seq (map 'list (lambda (vdecl)
                    (apply #'abl-var vdecl))
            r)))

;; (defmacro vars (&body body)
;;   (cons 'seq
;;         (loop for vdecl in body collecting
;;               (cons 'abl-var vdecl))))

(defun abl-parm (direction type name &key (undo nil))
  (list 'parm direction type name undo))

;; (defun abl-parm (direction type name &key (undo nil))
;;   (list (indent (format nil "DEFINE ~a PARAMETER ~a AS ~a~a."
;;                         direction
;;                         name
;;                         type
;;                         (if undo ""
;;                           " NO-UNDO")))))

(defun parms (&rest r)
  (seq
   (map 'list (lambda (pdecl)
                (apply #'abl-parm pdecl))
        r)))

;; (defmacro parms (&body body)
;;   (cons 'seq
;;         (loop for pdecl in body collecting
;;               (cons 'abl-parm pdecl))))

(defun abl-procedure (name parm-list var-list &rest r)
  (list 'proc name (apply #'parms parm-list)
         (apply #'vars var-list)
         r))

(defun abl-assign (&rest r)
  (cons 'assign r))

(defun abl-call (name parm-list)
  (list 'call name (apply #'parms parm-list)))

(defun abl-handler (e-type var-list handler-form)
  (list 'handler e-type var-list handler-for
m))

(defmacro handler (e-type var-list &rest body)
  `(abl-handler ,e-type (list ,@var-list) body))

(defun abl-handler-block (form &rest handler-list)
  (list 'with-handlers form
        (map 'list #'abl-handler handler-list)))
