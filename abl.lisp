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

(defun indentation-string ()
  (make-sequence 'string (* *INDENT-LEVEL* *INDENT-SIZE*) :initial-element #\Space))

(defun print-with-indent (destination format-string &rest r)
  (format destination "~a~a"
          (indentation-string)
          (apply #'format `(nil ,format-string ,@r))))

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
  (print-with-indent t "DEFINE ~a PARAMETER ~a AS ~a~a.~%"
                     (force direction)
                     (force name)
                     (force type)
                     (if undo ""
                       " NO-UNDO")))

(defun abl-parm-list (parm-list)
  (mapc (lambda (p)
          (abl-parm (delay (first p)) (delay (second p)) (delay (third p))))
        parm-list))

;; syntax adjustment
(defmacro parms (&body body)
  `(abl-parm-list (list ,@(map 'list (lambda (l) (cons 'list l))) body)))

(defun abl-procedure (name param-list &rest r)
  (print-with-indent "PROCEDURE ~a:~%" (force name))
  (with-indent 1
               (abl-parm-list param-list)
               (mapc #'force r))
  (print-with-indent "END.~%"))

;; (defmacro procedure (name param-list &body body)
;;   `(abl-procedure (delay ,name) (delay ,param-list))
;;   )

;; (defun statement (s)
;;   (lambda ()
;;     (print-with-indent nil "~a.~%" (funcall s))))

;; (defun define-var (name type &key (undo nil))
;;   (statement
;;    (lambda ()
;;      (print-with-indent nil "~{~a~^ ~}~a"
;;                         `(define variable ,(funcall name) as ,(funcall type))
;;                         (if (not undo)
;;                             " NO-UNDO"
;;                           "")))))

;; (defun define-parm (name type parm-type&key (undo nil))
;;   (unless (eql parm-type :output :input :input-output :return))
;;   (let ((parm-type (cond
;;                ((not (and input-p input))
;;                 :output)
;;                ((not (and output-p output))
;;                 :input)
;;                (t
;;                 :input-output))))
;;     (statement
;;      (lambda ()
;;        (print-with-indent nil "~{~a~^ ~}~a"
;;                           `(define ,parm-type parameter ,(funcall name) as ,(funcall type))
;;                           (if (not undo)
;;                               " NO-UNDO"
;;                             ""))))))

;; (defun seq (&rest r)
;;   (lambda ()
;;     (apply #'concatenate 'string  (map 'list #'funcall r))))

;; (defun dvar (name type &key (undo nil))
;;   (define-var (lambda () name) (lambda () type) :undo undo))

;; (defun dparm (name type &rest r)
;;   (apply #'define-parm `(,(lambda () name) ,(lambda () type) ,@r)))

;; (defun with-block (func var-list &rest body)
;;   "Takes a list of lists, each of which are the arguments to func, and a body of code to follow."
;;   (apply #'seq `(,@(map 'list (lambda (args) (apply func args)) var-list)
;;                  ,(lambda () (make-sequence 'string 1 :initial-element #\Newline))
;;                  ,@body)))

;; (defun with-vars (var-list &rest body)
;;   (with-block #'dvar var-list body))

;; (defun with-parms (parm-list &rest body)
;;   (with-block #'dparm parm-list body))

;; (defun assign ())
