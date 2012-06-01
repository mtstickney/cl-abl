(in-package :cl-user)

(defpackage :com.bitworks.abl
  (:use :cl))

(in-package :com.bitworks.abl)

(defvar *INDENT-LEVEL* 0)
(defvar *INDENT-SIZE* 8)
(defvar *DESTINATION* nil)

(defun indentation-string ()
  (make-sequence 'string (* *INDENT-LEVEL* *INDENT-SIZE*) :initial-element #\Space))

(defun print-with-indent (destination format-string &rest r)
  (format destination "~a~a"
          (indentation-string)
          (apply #'format `(nil ,format-string ,@r))))

(defun statement (s)
  (lambda ()
    (print-with-indent nil "~a.~%" (funcall s))))

(defun define-var (name type &key (undo nil))
  (statement
   (lambda ()
     (print-with-indent nil "~{~a~^ ~}~a"
                        `(define variable ,(funcall name) as ,(funcall type))
                        (if (not undo)
                            " NO-UNDO"
                          "")))))

(defun define-parm (name type &key (input nil input-p) (output nil output-p) (undo nil))
  (unless (or (and input-p input) (and output-p output))
    (error "At least one of :input or :output needs to be supplied and true"))
  (let ((parm-type (cond
               ((not (and input-p input))
                :output)
               ((not (and output-p output))
                :input)
               (t
                :input-output))))
    (statement
     (lambda ()
       (print-with-indent nil "~{~a~^ ~}~a"
                          `(define ,parm-type parameter ,(funcall name) as ,(funcall type))
                          (if (not undo)
                              " NO-UNDO"
                            ""))))))

(defun seq (&rest r)
  (lambda ()
    (apply #'concatenate 'string  (map 'list #'funcall r))))

(defun dvar (name type &key (undo nil))
  (define-var (lambda () name) (lambda () type) :undo undo))

(defun dparm (name type &rest r)
  (apply #'define-parm `(,(lambda () name) ,(lambda () type) ,@r)))

(defun with-block (func var-list &rest body)
  "Takes a list of lists, each of which are the arguments to func, and a body of code to follow."
  (apply #'seq `(,@(map 'list (lambda (args) (apply func args)) var-list)
                 ,(lambda () (make-sequence 'string 1 :initial-element #\Newline))
                 ,@body)))

(defun with-vars (var-list &rest body)
  (with-block #'dvar var-list body))

(defun with-parms (parm-list &rest body)
  (with-block #'dparm parm-list body))

(defun assign ())
