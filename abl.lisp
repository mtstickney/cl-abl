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

(defun seqp (thing)
  (and (listp thing)
       (eq (car thing) 'seq)
       (listp (second thing))))

(defun seq-items (seq)
  (second seq))

(defun var (type name &key (undo nil))
  (list 'var type name undo))

(defun varp (thing)
  (and (listp thing)
       (eq (length thing) 4)
       (eq (car thing) 'var)))

(defun var-name (v)
  (third v))
(defun var-type (v)
  (second v))
(defun var-undo (v)
  (fourth v))

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

(defun parmp (thing)
  (and (listp thing)
       (eq (length thing) 4)
       (eq (car thing) 'parm)))

(defun parm-direction (p)
  (second p))
(defun parm-type (p)
  (third p))
(defun parm-name (p)
  (fourth p))
(defun parm-undo (p)
  (fifth p))

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

(defun procedurep (thing)
  (and (listp thing)
       (eq (length thing) 5)
       (eq (car thing) 'proc)
       (and (seqp (procedure-parms thing))
            (every #'parmp (seq-items parm-list)))
       (and (seqp (procedure-vars thing))
            (every #'varp (seq-items var-list)))))

(defun procedure-name (p)
  (second p))
(defun procedure-parms (p)
  (third p))
(defun procedure-vars (p)
  (fourth p))
(defun procedure-body (p)
  (fifth p))

(defun assignment (assignment-list)
  (cons 'assignment assignment-list))

(defun assignmentp (a)
  (and (listp a)
       (eq (car a) 'assign)
       (listp (cadr a))))

(defmacro assign (&rest rest)
  `(assignment (list . ,rest)))

(defun call (name parm-list)
  (list 'call name parm-list))

(defun callp (thing)
  (and (listp thing)
       (eq (length thing) 3)
       (eq (car thing) 'call)))

(defun call-name (c)
  (second c))
(defun call-parm-list (c)
  (third c))

(defun handler (e-type var-list handler-form)
  (list 'handler e-type var-list handler-form))

(defun handlerp (thing)
  (and (listp thing)
       (eq (length thing) 4)
       (eq (car thing) 'handler)))

(defun handler-type (h)
  (second h))
(defun handler-var-list (h)
  (third h))
(defun handler-form (h)
  (fourth h))

(defun abl-scoped-handlers (form handler-list)
  (list 'with-handlers form handler-list))

(defmacro with-handlers (form &rest handler-list)
  (let ((handler-forms (map 'list (lambda (h)
                                    `(handler ,@h))
                            handler-list)))
    `(abl-scoped-handlers ,form ,(cons 'list handler-forms))))

(defun abl-alias (name value)
  (list 'alias name value))

