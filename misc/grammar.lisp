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
