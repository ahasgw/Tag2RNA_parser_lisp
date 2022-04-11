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

#|
(defmethod get-atrees-by-types ((grammar tag2rna-grammar) types)
  (remove-if
   (complement
    #'(lambda (atree)
        (member (atree-type atree) types)))
   (atrees grammar)))
|#

(defun get-atrees-by-types (atrees types)
  (remove-if
   (complement
    #'(lambda (atree)
        (member (atree-type atree) types)))
   atrees))

;;; Recognizer class definition
;;;

(defclass recognizer () ()
  (:documentation "Bases of all recognizer classes"))

;; Tag2RNA recognizer class definition

(defclass tag2rna-recognizer (recognizer tag2rna-grammar)
  ((string :accessor target-string
           :initform nil) ;; has " " of it's head.
   (recog-info :accessor recog-mat))
  (:documentation "Tag2RNA recognizer class"))

(defun make-tag2rna-recognizer (initial final atrees)
  (make-instance 'tag2rna-recognizer
    :initial-symbols initial
    :final-symbols final
    :atrees atrees))

(defmethod set-target-string ((recognizer tag2rna-recognizer) str)
  (setf (target-string recognizer)
        (concatenate 'simple-string " " str)))

(defmethod target-string-length ((recognizer tag2rna-recognizer))
  (1- (length (target-string recognizer))))

(defmethod pair-p ((recognizer tag2rna-recognizer) x y)
  (let ((c1 (schar (target-string recognizer) x))
        (c2 (schar (target-string recognizer) y)))
    (or (and (char-equal c1 #\A) (char-equal c2 #\U))
        (and (char-equal c1 #\U) (char-equal c2 #\A))
        (and (char-equal c1 #\G) (char-equal c2 #\C))
        (and (char-equal c1 #\C) (char-equal c2 #\G)))))

(defmethod make-recognition-matrix ((recognizer tag2rna-recognizer))
  (setf (recog-mat recognizer)
        (make-hash-table :test 'equal)))

(defmethod get-recog-info ((recognizer tag2rna-recognizer) key)
  (gethash key (recog-mat recognizer)))

(defmethod add-recog-info ((recognizer tag2rna-recognizer) key item)
  (pushnew item (gethash key (recog-mat recognizer))))

(defun recognize (recognizer)
  (progn (make-recognition-matrix recognizer)
         (initialize-recognition-matrix recognizer)
         (compute-every-steps recognizer)
         (accept recognizer)))

(defmethod initialize-recognition-matrix ((recognizer tag2rna-recognizer))
  (let* ((n (target-string-length recognizer))
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
        (add-recog-info recognizer '(p p i (1+ i)) t4r))
      (do ((r (1+ i) (+ r 1)))
          ((= r (1+ n)))
        (add-recog-info recognizer '(i (1+ i) r r) t4l))
      (when (pair-p recognizer (+ i 1) (+ i 2))
        (do ((p 0 (+ p 1)))
            ((= p (1+ i)))
          (add-recog-info recognizer '(p p i (+ i 2)) t3r))
        (do ((r (+ i 2) (+ r 1)))
            ((= r (1+ n)))
          (add-recog-info recognizer '(i (+ i 2) r r) t3l)))
      (do ((k (+ i 1) (+ k 1)))
          ((= k n))
        (when (pair-p recognizer (+ i 1) (+ k 1))
          (add-recog-info recognizer '(i (+ i 1) k (+ k 1)) t2))))
    (princ #\.)            ; for express progression
    (do ((p 0 (+ p 1)))
        ((= p n))
      (add-recog-info recognizer '(p p (1- n) n) t4r))
    (add-recog-info recognizer '((1- n) n n n) t4l)
    )
)

(defmacro compute-step (atrees key)
  (let ((atree-s (gensym))
        (atree-t (gensym)))
    `(dolist (,atree-s ,atrees)
       (dolist (,atree-t (get-recog-info recognizer ,key))
         (when (eq (atree-tagged-node ,atree-s)
                   (atree-root-node ,atree-t))
           (add-recog-info recognizer '(i j k l) ,atree-s)
           (return))))))

(defmethod compute-every-steps ((recognizer tag2rna-recognizer))
  (let* ((n (target-string-length recognizer))
         (tall (atrees recognizer))
         (t2u (get-atrees-by-types tall '(t2u)))
         (t2d (get-atrees-by-types tall '(t2d)))
         (t3l (get-atrees-by-types tall '(t3l)))
         (t3r (get-atrees-by-types tall '(t3r)))
         (t4ld (get-atrees-by-types tall '(t4ld)))
         (t4lu (get-atrees-by-types tall '(t4lu)))
         (t4rd (get-atrees-by-types tall '(t4rd)))
         (t4ru (get-atrees-by-types tall '(t4ru))))
    (format t "~%Entering compute steps")
    (do ((l 0 (+ l 1))) ((= l (1+ n)))
      (princ #\.)            ; for express progression
      (do ((i l (- i 1))) ((= i -1))
        (do ((j i (+ j 1))) ((= j (1+ l)))
          (do ((k l (- k 1))) ((= k (1- j)))
            (when (and (<= 0 i (1- j) (1+ k) l n)  ;; step(1-1)
                       (pair-p recognizer j (1+ k)))
              (compute-step t2u '(i (1- j) (1+ k) l)))
            (when (and (<= 0 (1+ i) j k (1- l) n)  ;; step(1-2)
                       (pair-p recognizer (1+ i) l))
              (compute-step t2d '((1+ i) j k (1- l))))
            (when (and (<= 0 (1+ i) (1- j) k l n)  ;; step(2-1)
                       (pair-p recognizer (1+ i) j))
              (compute-step t3l '((1+ i) (1- j) k l)))
            (when (and (<= 0 i j (1+ k) (1- l) n)  ;; step(2-2)
                       (pair-p recognizer (1+ k) l))
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

(defmethod accept ((recognizer tag2rna-recognizer))
  (format t "~%Entering accept")
  (let ((n (target-string-length recognizer))
        (result nil))
    (do ((j 0 (+ j 1)))
        ((or (= j (1+ n)) result))
      (princ #\.)           ; for express progression
      (dolist (atree (get-recog-info recognizer '(0 j j n)))
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
#|
(defmethod recognize ((recognizer tag2rna-recognizer))
;initials finals atrees string
  (let* ((n (target-string-length string))
         (recognition-matrix-b (recog-mat recognizer))
         (t2u (get-atrees-by-types atrees '(t2u)))
         (t2d (get-atrees-by-types atrees '(t2d)))
         (t3l (get-atrees-by-types atrees '(t3l)))
         (t3r (get-atrees-by-types atrees '(t3r)))
         (t4ld (get-atrees-by-types atrees '(t4ld)))
         (t4lu (get-atrees-by-types atrees '(t4lu)))
         (t4rd (get-atrees-by-types atrees '(t4rd)))
         (t4ru (get-atrees-by-types atrees '(t4ru))))
    (let* ((atree-final (get-atrees-final-symbol-tagged atrees finals))
           (t4r (get-atrees-by-types atree-final '(t4rd t4ru)))
           (t4l (get-atrees-by-types atree-final '(t4ld t4lu)))
           (t3r (get-atrees-by-types atree-final '(t3r)))
           (t3l (get-atrees-by-types atree-final '(t3l)))
           (t2  (get-atrees-by-types atree-final '(t2d t2u))))
      ;; initialize a recognition matrix-b
      (format t "~%Entering initialize")
      (do ((i 0 (+ i 1)))
          ((= i (1- n)))
        (princ #\.)            ; for express progression
        (do ((p 0 (+ p 1)))
            ((= p (1+ i)))
          (setf (matrix-b p p i (1+ i)) t4r))
        (do ((r (1+ i) (+ r 1)))
            ((= r (1+ n)))
          (setf (matrix-b i (1+ i) r r) t4l))
        (when (pair-p (+ i 1) (+ i 2))
          (do ((p 0 (+ p 1)))
              ((= p (1+ i)))
            (setf (matrix-b p p i (+ i 2)) t3r))
          (do ((r (+ i 2) (+ r 1)))
              ((= r (1+ n)))
            (setf (matrix-b i (+ i 2) r r) t3l)))
        (do ((k (+ i 1) (+ k 1)))
            ((= k n))
          (when (pair-p (+ i 1) (+ k 1))
            (setf (matrix-b i (+ i 1) k (+ k 1)) t2))))
      (princ #\.)            ; for express progression
      (do ((p 0 (+ p 1)))
          ((= p n))
        (setf (matrix-b p p (1- n) n) t4r))
      (setf (matrix-b (1- n) n n n) t4l)
      )
    ;; compute every steps
    (format t "~%Entering compute steps")
    (do ((l 0 (+ l 1)))
        ((= l (1+ n)))
      (princ #\.)            ; for express progression
      (do ((i l (- i 1)))
          ((= i -1))
        (do ((j i (+ j 1)))
            ((= j (1+ l)))
          (do ((k l (- k 1)))
              ((= k (1- j)))
            (when (and (<= 0 i (1- j) (1+ k) l n)  ;; step(1-1)
                       (pair-p j (1+ k)))
              (compute-step t2u (matrix-b i (1- j) (1+ k) l)))
            (when (and (<= 0 (1+ i) j k (1- l) n)  ;; step(1-2)
                       (pair-p (1+ i) l))
              (compute-step t2d (matrix-b (1+ i) j k (1- l))))
            (when (and (<= 0 (1+ i) (1- j) k l n)  ;; step(2-1)
                       (pair-p (1+ i) j))
              (compute-step t3l (matrix-b (1+ i) (1- j) k l)))
            (when (and (<= 0 i j (1+ k) (1- l) n)  ;; step(2-2)
                       (pair-p (1+ k) l))
              (compute-step t3r (matrix-b i j (1+ k) (1- l))))
            (when (<= 0 (1+ i) j k l n)            ;; step(3-1)
              (compute-step t4ld (matrix-b (1+ i) j k l)))
            (when (<= 0 i (1- j) k l n)            ;; step(3-2)
              (compute-step t4lu (matrix-b i (1- j) k l)))
            (when (<= 0 i j k (1- l) n)            ;; step(3-3)
              (compute-step t4rd (matrix-b i j k (1- l))))
            (when (<= 0 i j (1+ k) l n)            ;; step(3-4)
              (compute-step t4ru (matrix-b i j (1+ k) l)))
            )
          )
        )
      )
    ;; accept
    (format t "~%Entering accept")
    (let ((result nil))
      (do ((j 0 (+ j 1)))
          ((or (= j (1+ n)) result))
        (princ #\.)           ; for express progression
        (dolist (atree (matrix-b 0 j j n))
          (when (member (atree-root-node atree) initials)
            (setf result t)
            (return))))
      (format t (if result
                  "~%String was recognized."
                  "~%String was NOT recognized."))
      result)
    )
  )
|#