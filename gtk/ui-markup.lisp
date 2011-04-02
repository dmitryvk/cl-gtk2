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

(defgeneric pack-child (container child &key))

(defmethod pack-child ((w container) child &key)
  (container-add w child))

(defmethod pack-child ((b box) child &key (expand t) (fill t) (padding 0) pack-type position)
  (box-pack-start b child
		  :expand expand
		  :fill fill
		  :padding padding)
  (when pack-type
    (setf (box-child-pack-type b child) pack-type))
  (when position
    (setf (box-child-position b child) position)))

(defmethod pack-child ((p paned) child &key (resize 'default) (shrink t))
  (if (null (paned-child-1 p))
      (paned-pack-1 p child
		    :resize (if (eq resize 'default) nil resize)
		    :shrink shrink)
      (paned-pack-2 p child
		    :resize (if (eq resize 'default) t resize)
		    :shrink shrink)))

(defmethod pack-child ((table table) child &key
		       left right top bottom
		       (x-options '(:expand :fill)) (y-options '(:expand :fill)) (x-padding 0) (y-padding 0))

  (unless left
    (error "left is a mandatory child property for table packing"))
  (unless right
    (error "right is a mandatory child property for table packing"))
  (unless top
    (error "top is a mandatory child property for table packing"))
  (unless bottom
    (error "bottom is a mandatory child property for table packing"))

  (table-attach table child
		:left left
		:right right
		:top top
		:bottom bottom
		:x-options x-options
		:y-options y-options
		:x-padding x-padding
		:y-padding y-padding))

(defmethod pack-child ((w tree-view) child &key)
  (tree-view-append-column w child))

(defmethod pack-child ((w tree-view-column) child &key (expand t) attributes)
  (tree-view-column-pack-start w child :expand expand)
  (iter (for a on attributes by #'cddr)
	(tree-view-column-add-attribute w child
					(first a)
					(second a))))

(defmethod pack-child ((b toolbar) child &key (expand 'default) (homogeneous 'default))
  (toolbar-insert b child -1)
  (unless (eq expand 'default)
    (container-call-set-property b child "expand" expand +g-type-boolean+))
  (unless (eq homogeneous 'default)
    (container-call-set-property b child "homogeneous" homogeneous +g-type-boolean+)))

(defun set-ui-expansion-1 (d)
  (when (ui-d-class d)
    ;; only direct-vars do not have class
    (setf (ui-d-var d) (get-ui-d-var d)
          (ui-d-initform d) (get-ui-d-initform d))))

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
    `(let (,@(iter (for item in items)
                   (collect (list (ui-d-var item)
                                  (ui-d-initform item)))))
       ,@(iter (for item in items)
	       (appending (iter (for child in (ui-d-children item))
				(for child-var = (ui-d-var (ui-child-v child)))
				(let ((props
				       (iter (for p in (ui-child-props child))
					     (appending (list (ui-prop-name p) (ui-prop-value p))))))
				  (collect (list* 'pack-child (ui-d-var item) child-var props))))))
       
       ,@body)))

