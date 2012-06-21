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

(defun var (type name &key (undo nil))
  (list 'var type name undo))

;; (defun
;;   (list  (indent (format nil "DEFINE VARIABLE ~a AS ~a~a."
;;                          name
;;                          type
;;                          (if undo ""
;;                            " NO-UNDO")))))

;; (defmacro vars (&body body)
;;   (cons 'seq
;;         (loop for vdecl in body collecting
;;               (cons 'abl-var vdecl))))

(defun parm (direction type name &key (undo nil))
  (list 'parm direction type name undo))

;; (defun abl-parm (direction type name &key (undo nil))
;;   (list (indent (format nil "DEFINE ~a PARAMETER ~a AS ~a~a."
;;                         direction
;;                         name
;;                         type
;;                         (if undo ""
;;                           " NO-UNDO")))))
(defmacro defdecls (name func)
  `(defmacro ,name (&rest r)
     (let ((decl-list (map 'list (lambda (decl)
                                   `(,',func ,@decl))
                           r)))
       `(seq ,@decl-list))))

;; var- and parm-list macros
(defdecls vars var)
(defdecls parms parm)

;; (defmacro parms (&body body)
;;   (cons 'seq
;;         (loop for pdecl in body collecting
;;               (cons 'abl-parm pdecl))))

(defun procedure (name parm-list var-list &rest r)
  (list 'proc name parm-list var-list r))

(defun assignment (assignment-list)
  (cons 'assignment assignment-list))
  (list 'handler e-type var-list handler-form))

(defmacro handler (e-type var-list &rest body)
  `(abl-handler ,e-type (list ,@var-list) ,body))
(defmacro assign (&rest rest)
  `(assignment (list . ,rest)))

(defun call (name parm-list)
  (list 'call name parm-list))
(defun abl-handler-block (form &rest handler-list)
  (list 'with-handlers form
        (map 'list (lambda (h)
                     (eval `(handler ,@h)))
             handler-list)))
(defun handler (e-type var-list handler-form)
  (list 'handler e-type var-list handler-form))
