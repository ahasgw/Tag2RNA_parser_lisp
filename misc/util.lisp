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
   (copy-seq atrees)))

(defun collect-atrees-by-types (atrees types)
  (remove-if
   (complement #'(lambda (atree)
                   (member (atree-type atree) types)))
   (copy-seq atrees)))

(defmacro pair-p (x y)
  (let ((c1 (gensym))
        (c2 (gensym)))
    `(let ((,c1 (schar string ,x))
           (,c2 (schar string ,y)))
       (format t "[~A,~A](~A,~A)" ,c1 ,c2 ,x ,y)
       (or (and (char-equal ,c1 #\A) (char-equal ,c2 #\U))
           (and (char-equal ,c1 #\U) (char-equal ,c2 #\A))
           (and (char-equal ,c1 #\G) (char-equal ,c2 #\C))
           (and (char-equal ,c1 #\C) (char-equal ,c2 #\G))))))

;; Ex.(compute-step t2 (aref matrix-b 1 2 3 4))
(defmacro compute-step (atrees element-of-b)
  (let ((atree-s (gensym))
        (atree-t (gensym)))
    `(dolist (,atree-s ,atrees)
       (dolist (,atree-t ,element-of-b)
         (when (eq (atree-tagged-node ,atree-s)
                   (atree-root-node ,atree-t))
           (pushnew ,atree-s (aref matrix-b i j k l))
           (return))))))
       