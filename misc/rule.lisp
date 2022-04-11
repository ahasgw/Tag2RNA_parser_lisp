(defconstant ZERO-POINT 0)
(defconstant NO-BONDS 0)
(defconstant PAIR-NONE 0)
(defconstant PAIR-INTERNAL 1)
(defconstant PAIR-EXTERNAL 2)
(defconstant PAIR-LEFT 4)
(defconstant PAIR-RIGHT 8)

#|
(defconstant %t2d  #*00000001)
(defconstant %t2u  #*00000010)
(defconstant %t3l  #*00000100)
(defconstant %t3r  #*00001000)
(defconstant %t4ld #*00010000)
(defconstant %t4lu #*00100000)
(defconstant %t4rd #*01000000)
(defconstant %t4ru #*10000000)
(defconstant %t2   #*00000011)
(defconstant %t3   #*00001100)
(defconstant %t4   #*11110000)
(defconstant %tall #*11111111)
|#

(defclass parse-info ()
  ((point :initarg :point
          :reader point)
   (bonds :initarg :bonds
          :reader bonds)
   (ptype :initarg :ptype
          :reader :ptype)))

(defun make-parse-info (point bonds ptype)
  (make-instance 'parse-info
    :point point
    :bonds bonds
    :ptype ptype))

(defclass adjunct-tree ()
  ((type :initarg :type
         :reader type)
   (root :initarg :root-node-symbol
         :reader root-node-symbol)
   (tagged :initarg :tagged-node-symbol
           :reader tagged-node-symbol)))

(defun make-atree (type root tagged)
  (make-instance 'adjunct-tree
    :type type
    :root-node-symbol root
    :tagged-node-symbol tagged))

(defclass rule (parse-info adjunct-tree)
  ()
  (:documentation "Element of Recognition Matrix"))

(defun make-rule (atree point bonds ptype)
  (make-instance  'rule
    :point point
    :bonds bonds
    :ptype ptype
    :type (atree-type atree)
    :root-node-symbol (atree-root-node atree)
    :tagged-node-symbol (atree-tagged-node atree)))

(defun make-initial-rules (atrees)
  (mapcar #'(lambda (atree)
              (make-rule atree
                         point
                         bonds
                         ptype))
          atrees))

(defun make-point-table ()
  (let ((point-table (make-hash-table :test 'equal)))
    (setf (gethash '(#\A #\U #\A #\U) point-table) 9)
    (setf (gethash '(#\A #\U #\U #\A) point-table) 9)
    (setf (gethash '(#\A #\U #\C #\G) point-table) 21)
    (setf (gethash '(#\A #\U #\G #\C) point-table) 17)
    (setf (gethash '(#\U #\A #\A #\U) point-table) 11)
    (setf (gethash '(#\U #\A #\U #\A) point-table) 9)
    (setf (gethash '(#\U #\A #\C #\G) point-table) 23)
    (setf (gethash '(#\U #\A #\G #\C) point-table) 18)
    (setf (gethash '(#\C #\G #\A #\U) point-table) 18)
    (setf (gethash '(#\C #\G #\U #\A) point-table) 17)
    (setf (gethash '(#\C #\G #\C #\G) point-table) 29)
    (setf (gethash '(#\C #\G #\G #\C) point-table) 20)
    (setf (gethash '(#\G #\C #\A #\U) point-table) 23)
    (setf (gethash '(#\G #\C #\U #\A) point-table) 21)
    (setf (gethash '(#\G #\C #\C #\G) point-table) 34)
    (setf (gethash '(#\G #\C #\G #\C) point-table) 29)
    point-table))

(defmethod get-point ((type (eql %t2u)) (rule rule) key)
  ())
