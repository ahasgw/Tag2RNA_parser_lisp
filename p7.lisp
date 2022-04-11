;;; TAG2RNA parser
;;;

;;; Grammar class definition
;;;

(defclass grammar () ()
  (:documentation "Bases of all grammar classes"))

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

;(defmethod describe ((g tag2rna-grammar) stream)
;  (format stream "~&~S is a tag2RNA grammar." g (values)))

;;; Parser class definition
;;;

(defclass parser () ()
  (:documentation "Bases of all parser classes"))

(defgeneric parse-string (parser string)
  (:documentation "Parse for the string"))

(defgeneric parse (parser)
  (:documentation "Returns parsing result"))

;;
(defclass tag2rna-parser (parser tag2rna-grammar)
  ((string :accessor given-string) ;;has " " of it's head.
   (result :reader parse-result))
  (:documentation "Tag2RNA parser class"))

(defun make-tag2rna-parser (initial final atrees)
  (make-instance 'tag2rna-parser
    :initial-symbols initial
    :final-symbols final
    :atrees atrees))

(defmethod parse-string ((parser tag2rna-parser) string)
  (progn
    (setf (given-string parser)
          (concatenate 'simple-string " " string))
    (parse parser)))

(defmethod parse ((parser tag2rna-parser))
  (recognize (initial-symbols parser)
             (final-symbols parser)
             (atrees parser)
             (given-string parser)))

;;
(defmacro atree-type (atree)
  `(first ,atree))

(defmacro atree-root-node (atree)
  `(second ,atree))

(defmacro atree-tagged-node (atree)
  `(third ,atree))

(defun collect-atrees-whose-tagged-node-labeled-by-final-symbol (atrees finals)
  (remove-if
   (complement #'(lambda (atree)
                   (member (atree-tagged-node atree) finals)))
   atrees))

(defun collect-atrees-by-types (atrees types)
  (remove-if
   (complement #'(lambda (atree)
                   (member (first atree) types)))
   atrees))

(defmacro pair-p (x y)
  (let ((c1 (gensym))
        (c2 (gensym)))
    `(let ((,c1 (schar string ,x))
           (,c2 (schar string ,y)))
       (or (and (char-equal ,c1 #\A) (char-equal ,c2 #\U))
           (and (char-equal ,c1 #\U) (char-equal ,c2 #\A))
           (and (char-equal ,c1 #\G) (char-equal ,c2 #\C))
           (and (char-equal ,c1 #\C) (char-equal ,c2 #\G))))))

;; Reconition matrix B (hash table)
;
(defun make-recog-mat-B ()
  (make-hash-table :test 'equal))

(defmacro matrix-b (i j k l)
  `(gethash (list ,i ,j ,k ,l) recognition-matrix-b))

#|
(defmacro compute-step (atrees element-of-b)
  (let ((atree-s (gensym))
        (atree-t (gensym)))
    `(dolist (,atree-s ,atrees)
       (let ((,atree-t 
              (remove-if
               (complement #'(lambda (rule)
                               (eq (atree-tagged-node ,atree-s)
                                   (root-node-symbol rule))))
               ,element-of-b)))
         (maplist #'(lambda (rule)
                      (

         (when (eq (atree-tagged-node ,atree-s)
                   (root-node-symbol ,atree-t))
           (pushnew ,atree-s (matrix-b i j k l))
           (return))))))
|#
;;
(defun recognize (initials finals atrees string)
  (let* ((n (1- (length string)))
         (recognition-matrix-b (make-recog-mat-B))
         (t2u (collect-atrees-by-types atrees '(t2u)))
         (t2d (collect-atrees-by-types atrees '(t2d)))
         (t3l (collect-atrees-by-types atrees '(t3l)))
         (t3r (collect-atrees-by-types atrees '(t3r)))
         (t4ld (collect-atrees-by-types atrees '(t4ld)))
         (t4lu (collect-atrees-by-types atrees '(t4lu)))
         (t4rd (collect-atrees-by-types atrees '(t4rd)))
         (t4ru (collect-atrees-by-types atrees '(t4ru))))
    (let* ((atree-final (collect-atrees-whose-tagged-node-labeled-by-final-symbol atrees finals))
           (t4r (collect-atrees-by-types atree-final '(t4rd t4ru)))
           (t4l (collect-atrees-by-types atree-final '(t4ld t4lu)))
           (t3r (collect-atrees-by-types atree-final '(t3r)))
           (t3l (collect-atrees-by-types atree-final '(t3l)))
           (t2  (collect-atrees-by-types atree-final '(t2d t2u))))
      ;; initialize a recognition matrix-b
      (format t "~%Entering initialize")
      (do ((i 0 (+ i 1)))
          ((= i (1- n)))
        (princ #\.)            ; for express progression
        (do ((p 0 (+ p 1)))
            ((= p (1+ i)))
          (setf (matrix-b p p i (1+ i))
                (make-initial-rules (t4r ZERO-POINT NO-BONDS PAIR-NONE))))
        (do ((r (1+ i) (+ r 1)))
            ((= r (1+ n)))
          (setf (matrix-b i (1+ i) r r)
                (make-initial-rules (t4l ZERO-POINT NO-BONDS PAIR-NONE))))
        (when (pair-p (+ i 1) (+ i 2))
          (do ((p 0 (+ p 1)))
              ((= p (1+ i)))
            (setf (matrix-b p p i (+ i 2))
                  (make-initial-rules (t3r ZEOR-POINT 1 PAIR_RIGHT))))
          (do ((r (+ i 2) (+ r 1)))
              ((= r (1+ n)))
            (setf (matrix-b i (+ i 2) r r)
                  (make-initial-rules (t3l ZERO-POINT 1 PAIR_LEFT)))))
        (do ((k (+ i 1) (+ k 1)))
            ((= k n))
          (when (pair-p (+ i 1) (+ k 1))
            (setf (matrix-b i (+ i 1) k (+ k 1))
                  (make-initial-rules
                   (t2 ZERO-POINT 1 (logior PAIR_INTERNAL
                                            PAIR_EXTERNAL)))))))
      (princ #\.)            ; for express progression
      (do ((p 0 (+ p 1)))
          ((= p n))
        (setf (matrix-b p p (1- n) n)
              (make-initial-rules (t4r ZERO-POINT NO-BONDS PAIR_NONE))))
      (setf (matrix-b (1- n) n n n)
            (make-initial-rules (t4l ZERO-POINT NO-BONDS PAIR_NONE)))
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

;;;