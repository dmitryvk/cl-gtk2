(in-package :gtk)

(defstruct ui-d class props children expansion var initform initializer)

(defstruct ui-prop name value)

(defstruct ui-child v props)

(defun parse-ui-props (list)
  ;; list is ({:prop value}* rest)
  (iter (for x first list then (cddr x))
        (while (keywordp (first x)))
        (for (name value) = x)
        (collect (make-ui-prop :name name :value value) into props)
        (finally (return (values props x)))))

(defun parse-ui-children (list)
  ;; list is (child*)
  ;; child is {ui {:prop value}*}
  (iter (while list)
        (for child = (if (eq :expr (first (first list)))
                         (make-ui-d :var (second (first list)))
                         (parse-ui-description (first list))))
        (for (values props rest) = (parse-ui-props (rest list)))
        (setf list rest)
        (collect (make-ui-child :v child :props props))))

(defun parse-ui-description (description)
  ;; description is (class {:prop value}* child*)
  ;; child is {ui {:prop value}*}
  (let ((class (first description)))
    (multiple-value-bind (props rest) (parse-ui-props (rest description))
      (let ((children (parse-ui-children rest)))
        (make-ui-d :class class :props props :children children)))))

(defun get-ui-d-var (d)
  (let ((prop (find :var (ui-d-props d) :key #'ui-prop-name)))
    (if prop
        (ui-prop-value prop)
        (gensym (format nil "~A-" (symbol-name (ui-d-class d)))))))

(defun get-ui-d-initform (d)
  `(make-instance ',(ui-d-class d)
                  ,@(iter (for prop in (ui-d-props d))
                          (unless (eq (ui-prop-name prop) :var)
                            (appending (list (ui-prop-name prop) (ui-prop-value prop)))))))

(defvar *ui-child-packers* (make-hash-table))

(defmacro def-ui-child-packer (class (var child-def child) &body body)
  `(setf (gethash ',class *ui-child-packers*)
         (lambda (,var ,child-def ,child) ,@body)))

(def-ui-child-packer container (w d child)
  (declare (ignore d))
  `(container-add ,w ,child))

(defun get-ui-child-prop-value (d name required-p context)
  (let ((prop (find name (ui-child-props d) :key #'ui-prop-name)))
    (if (and required-p (null prop))
        (error "~A is a mandatory child property for ~A" name context)
        (when prop (ui-prop-value prop)))))

(def-ui-child-packer box (b d child)
  (let ((expand-prop (find :expand (ui-child-props d) :key #'ui-prop-name))
        (fill-prop (find :fill (ui-child-props d) :key #'ui-prop-name))
        (padding-prop (find :padding (ui-child-props d) :key #'ui-prop-name)))
    `(box-pack-start ,b ,child
                     ,@(when expand-prop (list :expand (ui-prop-value expand-prop)))
                     ,@(when fill-prop (list :fill (ui-prop-value fill-prop)))
                     ,@(when padding-prop (list :padding (ui-prop-value padding-prop))))))

(def-ui-child-packer paned (p d child)
  (let ((resize-prop (find :resize (ui-child-props d) :key #'ui-prop-name))
        (shrink-prop (find :shrink (ui-child-props d) :key #'ui-prop-name)))
    `(if (null (paned-child-1 ,p))
         (paned-pack-1 ,p ,child
                       ,@(when resize-prop (list :resize (ui-prop-value resize-prop)))
                       ,@(when shrink-prop (list :shrink (ui-prop-value shrink-prop))))
         (paned-pack-2 ,p ,child
                       ,@(when resize-prop (list :resize (ui-prop-value resize-prop)))
                       ,@(when shrink-prop (list :shrink (ui-prop-value shrink-prop)))))))

(def-ui-child-packer table (table d child)
  `(table-attach ,table ,child
                 ,(get-ui-child-prop-value d :left t "table packing")
                 ,(get-ui-child-prop-value d :right t "table packing")
                 ,(get-ui-child-prop-value d :top t "table packing")
                 ,(get-ui-child-prop-value d :bottom t "table packing")
                 ,@(let ((x-options (get-ui-child-prop-value d :x-options nil nil)))
                        (when x-options
                          (list :x-options x-options)))
                 ,@(let ((y-options (get-ui-child-prop-value d :y-options nil nil)))
                        (when y-options
                          (list :y-options y-options)))
                 ,@(let ((x-padding (get-ui-child-prop-value d :x-padding nil nil)))
                        (when x-padding
                          (list :x-padding x-padding)))
                 ,@(let ((y-padding (get-ui-child-prop-value d :y-padding nil nil)))
                        (when y-padding
                          (list :y-padding y-padding)))))

(defun get-child-packer-fn (d)
  (iter (for class first (find-class (ui-d-class d)) then (first (c2mop:class-direct-superclasses class)))
        (while class)
        (for packer = (gethash (class-name class) *ui-child-packers*))
        (when packer (return packer))))

(defun get-child-packer (d var)
  (let ((fn (get-child-packer-fn d)))
    (when fn
      (let ((forms (iter (for child in (ui-d-children d))
                         (for child-var = (ui-d-var (ui-child-v child)))
                         (collect (funcall fn var child child-var)))))
        (when forms (cons 'progn forms))))))

(defun get-ui-d-initializer (d var)
  (get-child-packer d var))

(defun set-ui-expansion-1 (d)
  (when (ui-d-class d)
    ;; only direct-vars do not have class
    (setf (ui-d-var d) (get-ui-d-var d)
          (ui-d-initform d) (get-ui-d-initform d))
    (setf (ui-d-initializer d) (get-ui-d-initializer d (ui-d-var d)))))

(defun set-ui-expansion (description)
  (iter (for child in (ui-d-children description))
        (set-ui-expansion (ui-child-v child)))
  (set-ui-expansion-1 description))

(defun flattened-ui-descriptions (d)
  (cons d
        (iter (for child in (ui-d-children d))
              (when (ui-d-class (ui-child-v child))
                (appending (flattened-ui-descriptions (ui-child-v child)))))))

(defmacro let-ui (ui-description &body body)
  (let* ((description (parse-ui-description ui-description))
         (items (flattened-ui-descriptions description)))
    (set-ui-expansion description)
    `(let (,@(iter (for i in items)
                   (collect (list (ui-d-var i)
                                  (ui-d-initform i)))))
       ,@(iter (for i in items)
               (when (ui-d-initializer i)
                 (collect (ui-d-initializer i))))
       ,@body)))
