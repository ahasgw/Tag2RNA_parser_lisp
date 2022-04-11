;;; TAG2RNA parser
;;;

;;; Grammar class definition
;;;

(defclass grammar () ()
  (:documentation "Bases of all grammar classes"))

;; Tag2RNA grammar class definition
;;

(defclass tag2rna-grammar (grammar)
  ((initial :initarg :initial-symbols
            :reader initial-symbols)
   (final :initarg :final-symbols
          :reader final-symbols)
   (atrees :initarg :atrees
           :reader atrees))
  (:documentation "Tag2RNA grammar class"))

(defun make-tag2rna-grammar (initial final atrees)
  (make-instance 'tag2rna-grammar
    :initial-symbols initial
    :final-symbols final
    :atrees atrees))

(defmacro atree-type (atree) `(first ,atree))
(defmacro atree-root-node (atree) `(second ,atree))
(defmacro atree-tagged-node (atree) `(third ,atree))

(defmethod get-atrees-final-symbol-tagged ((grammar tag2rna-grammar))
  (remove-if
   (complement
    #'(lambda (atree)
        (member (atree-tagged-node atree)
                (final-symbols grammar))))
   (atrees grammar)))

(defmethod get-atrees-by-types ((grammar tag2rna-grammar) types)
  (remove-if
   (complement
    #'(lambda (atree)
        (member (atree-type atree) types)))
   (atrees grammar)))

(defmethod get-atrees-by-types (atrees types)
  (remove-if
   (complement
    #'(lambda (atree)
        (member (atree-type atree) types)))
   atrees))

;;; Recognizer class definition
;;;

(defclass recognizer () ()
  (:documentation "Bases of all recognizer classes"))

(defgeneric set-target-string (recognizer string)
  (:documentation "Set the string into recognizer"))

(defgeneric recognize (recongizer)
  (:documentation "Recognize a given string"))

;; Tag2RNA recognizer class definition

(defclass tag2rna-recognizer (recognizer tag2rna-grammar)
  ((string :accessor target-string
           :initform nil) ;; has " " of it's head.
   (recog-result :reader result-of-recognizing
                 :initform nil))
  (:documentation "Tag2RNA recognizer class"))

(defun make-tag2rna-recognizer (initial final atrees)
  (make-instance 'tag2rna-recognizer
    :initial-symbols initial
    :final-symbols final
    :atrees atrees))

(defmethod set-target-string ((recognizer tag2rna-recognizer) string)
  (setf (target-string recognizer)
        (concatenate 'simple-string " " string)))

(defmethod target-string-length ((recognizer tag2rna-recognizer))
  (1- (length (target-string recognizer))))

(defmethod recognize ((recognizer tag2rna-recognizer))
  (let ((mat (make-recognition-matrix)))
    (initialize-recognition-matrix mat recognizer)
    (compute-every-steps mat recognizer)
    (accept mat recognizer)))

;;; Recognition matrix class definition
;;;

(defclass recognition-matrix ()
  ((strage :initform (make-hash-table :test 'equal)
           :accessor strage))
  (:documentation "Recognition matrix class"))

(defun make-recognition-matrix ()
  (make-instance 'recognition-matrix))

(defmethod get-entry ((mat recognition-matrix) key)
  (gethash key (strage mat)))

(defmethod add-to-entry ((mat recognition-matrix) key item)
;  (pushnew item (gethash key (strage mat))))
  (setf (gethash key (strage mat)) item))

(defmacro pair-p (x y)
  (let ((c1 (gensym))
        (c2 (gensym)))
    `(let ((,c1 (schar string ,x))
           (,c2 (schar string ,y)))
       (or (and (char-equal ,c1 #\A) (char-equal ,c2 #\U))
           (and (char-equal ,c1 #\U) (char-equal ,c2 #\A))
           (and (char-equal ,c1 #\G) (char-equal ,c2 #\C))
           (and (char-equal ,c1 #\C) (char-equal ,c2 #\G))))))

(defmethod initialize-recognition-matrix
           ((mat recognition-matrix)
            (recognizer tag2rna-recognizer))
  (let* ((n (target-string-length recognizer))
         (string (target-string recognizer))
         (atree-final (get-atrees-final-symbol-tagged recognizer))
         (t4r (get-atrees-by-types atree-final '(t4rd t4ru)))
         (t4l (get-atrees-by-types atree-final '(t4ld t4lu)))
         (t3r (get-atrees-by-types atree-final '(t3r)))
         (t3l (get-atrees-by-types atree-final '(t3l)))
         (t2  (get-atrees-by-types atree-final '(t2d t2u))))
    (format t "~%Entering initialize")
    (do ((i 0 (+ i 1)))
        ((= i (1- n)))
      (princ #\.)            ; for express progression
      (do ((p 0 (+ p 1)))
          ((= p (1+ i)))
        (add-to-entry mat '(p p i (1+ i)) t4r))
      (do ((r (1+ i) (+ r 1)))
          ((= r (1+ n)))
        (add-to-entry mat '(i (1+ i) r r) t4l))
      (when (pair-p (+ i 1) (+ i 2))
        (do ((p 0 (+ p 1)))
            ((= p (1+ i)))
          (add-to-entry mat '(p p i (+ i 2)) t3r))
        (do ((r (+ i 2) (+ r 1)))
            ((= r (1+ n)))
          (add-to-entry mat '(i (+ i 2) r r) t3l)))
      (do ((k (+ i 1) (+ k 1)))
          ((= k n))
        (when (pair-p (+ i 1) (+ k 1))
          (add-to-entry mat '(i (+ i 1) k (+ k 1)) t2))))
    (princ #\.)            ; for express progression
    (do ((p 0 (+ p 1)))
        ((= p n))
      (add-to-entry mat '(p p (1- n) n) t4r))
    (add-to-entry mat '((1- n) n n n) t4l)
    )
)

(defmacro compute-step (atrees key)
  (let ((atree-s (gensym))
        (atree-t (gensym)))
    `(dolist (,atree-s ,atrees)
       (dolist (,atree-t (get-entry mat ,key))
         (when (eq (atree-tagged-node ,atree-s)
                   (atree-root-node ,atree-t))
           (add-to-entry mat '(i j k l) ,atree-s)
           (return))))))

(defmethod compute-every-steps
           ((mat recognition-matrix)
            (recognizer tag2rna-recognizer))
  (let* ((n (target-string-length recognizer))
         (string (target-string recognizer))
         (t2u (get-atrees-by-types recognizer '(t2u)))
         (t2d (get-atrees-by-types recognizer '(t2d)))
         (t3l (get-atrees-by-types recognizer '(t3l)))
         (t3r (get-atrees-by-types recognizer '(t3r)))
         (t4ld (get-atrees-by-types recognizer '(t4ld)))
         (t4lu (get-atrees-by-types recognizer '(t4lu)))
         (t4rd (get-atrees-by-types recognizer '(t4rd)))
         (t4ru (get-atrees-by-types recognizer '(t4ru))))
    (format t "~%Entering compute steps")
    (do ((l 0 (+ l 1))) ((= l (1+ n)))
      (princ #\.)            ; for express progression
      (do ((i l (- i 1))) ((= i -1))
        (do ((j i (+ j 1))) ((= j (1+ l)))
          (do ((k l (- k 1))) ((= k (1- j)))
            (when (and (<= 0 i (1- j) (1+ k) l n)  ;; step(1-1)
                       (pair-p j (1+ k)))
              (compute-step t2u '(i (1- j) (1+ k) l)))
            (when (and (<= 0 (1+ i) j k (1- l) n)  ;; step(1-2)
                       (pair-p (1+ i) l))
              (compute-step t2d '((1+ i) j k (1- l))))
            (when (and (<= 0 (1+ i) (1- j) k l n)  ;; step(2-1)
                       (pair-p (1+ i) j))
              (compute-step t3l '((1+ i) (1- j) k l)))
            (when (and (<= 0 i j (1+ k) (1- l) n)  ;; step(2-2)
                       (pair-p (1+ k) l))
              (compute-step t3r '(i j (1+ k) (1- l))))
            (when (<= 0 (1+ i) j k l n)            ;; step(3-1)
              (compute-step t4ld '((1+ i) j k l)))
            (when (<= 0 i (1- j) k l n)            ;; step(3-2)
              (compute-step t4lu '(i (1- j) k l)))
            (when (<= 0 i j k (1- l) n)            ;; step(3-3)
              (compute-step t4rd '(i j k (1- l))))
            (when (<= 0 i j (1+ k) l n)            ;; step(3-4)
              (compute-step t4ru '(i j (1+ k) l)))
            )
          )
        )
      )
    )
  )

(defmethod accept ((mat recognition-matrix)
                   (recognizer tag2rna-recognizer))
  (format t "~%Entering accept")
  (let ((n (target-string-length recognizer))
        (result nil))
    (do ((j 0 (+ j 1)))
        ((or (= j (1+ n)) result))
      (princ #\.)           ; for express progression
      (dolist (atree (get-entry mat '(0 j j n)))
        (when (member (atree-root-node atree)
                      (initial-symbols recognizer))
          (setf result t)
          (return))))
    (format t (if result
                "~%String was recognized."
                "~%String was NOT recognized."))
    result)
)

;;; Parser class definition
;;;

(defclass parser () ()
  (:documentation "Bases of all parser classes"))

(defgeneric parse (parser)
  (:documentation "Parse a given string"))

;; Tag2RNA parser class definition

(defclass tag2rna-parser (parser tag2rna-recognizer)
  ((parse-result :reader result-of-parsing
                 :initform nil))
  (:documentation "Tag2RNA parser class"))

(defun make-tag2rna-parser (initial final atrees)
  (make-instance 'tag2rna-parser
    :initial-symbols initial
    :final-symbols final
    :atrees atrees))

(defmethod parse ((parser tag2rna-parser))
  )

;;;