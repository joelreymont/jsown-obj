(in-package :jsown-obj)

(defvar *readers* (make-hash-table :test 'equal))
(defvar *writers* (make-hash-table :test 'equal))

;; Define message structure

(defun make-slot-spec (slot)
  ;; lisp name
  (car slot))

;;; Write JSON request

(defun slot-accessor-name (name slot-name)
  (intern (concatenate 'string (symbol-name name) "-" (symbol-name slot-name))))  

(defun slot-writer-name (slot-name)
  (intern (concatenate 'string "WRITE-" (symbol-name slot-name))))

(defun write-slot (name slot self)
  (destructuring-bind (lisp-name js-name &optional writer) slot
    (let* ((accessor (slot-accessor-name name lisp-name))
           (value (if writer
                      `(funcall ,writer (,accessor ,self))
                      `(,accessor ,self)
                      )))
      `(,js-name ,value)
      )))

(defun make-writer (name slots &optional js-type)
  (let* ((writes (loop for slot in slots
                       collect (write-slot name slot 'self)))
         (decls (unless writes `((declare (ignore self))))))
    (when js-type
      (setq writes (cons `("type" ,js-type) writes)))
    `(defun ,(slot-writer-name `,name) (self)
       ,@decls
       (jsown:new-js ,@writes))
    ))

;;; Read JSON reply

(defun maker-name (name)
  (intern (concatenate 'string "MAKE-" (symbol-name name))))

(defun slot-reader-name (slot-name)
  (intern (concatenate 'string "READ-" (symbol-name slot-name))))

(defun read-slot (name slot self js-obj)
  (destructuring-bind (lisp-name js-name &optional reader) slot
    (let* ( (value `(jsown:val-safe ,js-obj ,js-name)))
      (when reader
        (setq value `(funcall ,reader ,value)))
      `(setf (,(slot-accessor-name `,name `,lisp-name) ,self) ,value)
      )))

(defun make-reader (name slots)
  (let* ((reads (loop for slot in slots
                      collect (read-slot name slot 'self 'js-obj)))
         (decls (unless reads `((declare (ignore js-obj))))))
    `(defun ,(slot-reader-name `,name) (js-obj)
       ,@decls
       (let ((self (,(maker-name `,name))))
         ,@reads
         self)
       )))

(defun accessors (name slots)
  (loop for slot in slots
        collect (slot-accessor-name name (car slot))))

;;; Declare messages and helpers

(defun make-spec (name slots)
  (let ((specs (loop for slot in slots
                     collect (make-slot-spec slot))))
    `(defstruct ,name ,@specs)
    ))

(defun save-reader (js-type reader)
  (setf (gethash js-type *readers*) reader))

(defun save-writer (name writer)
  (setf (gethash name *writers*) writer))

(defmacro defobject (name skip &optional slots opts)
  (declare (ignore skip))
  (when (keywordp (car slots))
    (setq opts slots
          slots nil))
  (destructuring-bind (&key parse js-type) opts
    (let ((extras (when (and js-type parse)
                    `((save-reader ,js-type #',(slot-reader-name `,name)))
                    ))
          (maker (maker-name name))
          (writer (slot-writer-name name))
          (reader (slot-reader-name name))
          (accessors (map 'list (lambda (x) `(quote ,x)) (accessors name slots))))
      `(progn
         ,(make-spec `,name `,slots)
         ,(make-reader `,name `,slots)
         ,(make-writer `,name `,slots `,js-type)
         (save-writer ',name #',writer)
         ,@extras
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (export (list ',name ',maker ',reader ',writer ,@accessors)))
         ))))

(defun from-json (js-obj)
  (let* ((type (jsown:val js-obj "type"))
         (reader (gethash type *readers*)))
    (funcall reader js-obj)
    ))

(defun to-json (o)
  (let* ((writer (gethash (type-of o) *writers*)))
    (funcall writer o)
    ))

;;; Utility functions

(defun read-map (reader)
  (lambda (js-obj)
    (let ((m (make-hash-table :test 'equal)))
      (loop for (k . v) in (cdr js-obj) do
        (setf (gethash k m) (funcall reader v)))
      m
      )))

(defun read-array (reader)
  (lambda (l)
    (let* ((n 0)
           (l (loop for js-obj in l
                    collect (funcall reader js-obj) do
                      (incf n)
                    )))
      (make-array n :initial-contents l)
      )))

(defun read-string-array ()
  (read-array #'identity))

(defun read-obj-array ()
  (read-array #'from-json))
