(in-package :gobject)

(defvar *lisp-name-package* nil
  "For internal use (used by class definitions generator). Specifies the package in which symbols are interned.")
(defvar *strip-prefix* "")
(defvar *lisp-name-exceptions* nil)
(defvar *generation-exclusions* nil)
(defvar *known-interfaces* (make-hash-table :test 'equal))
(defvar *additional-properties* nil)
(defvar *generated-types* nil)

(defun name->supplied-p (name)
  (make-symbol (format nil "~A-SUPPLIED-P" (symbol-name name))))

(defstruct property name accessor-name readable writable)

(defstruct (gobject-property (:include property)) gname type)

(defstruct (cffi-property (:include property)) type reader writer)

(defmethod make-load-form ((object gobject-property) &optional env)
  (declare (ignore env))
  `(make-gobject-property :name ',(property-name object)
                          :accessor-name ',(property-accessor-name object)
                          :readable ',(property-readable object)
                          :writable ',(property-writable object)
                          :gname ',(gobject-property-gname object)
                          :type ',(gobject-property-type object)))

(defmethod make-load-form ((object cffi-property) &optional env)
  (declare (ignore env))
  `(make-cffi-property :name ',(property-name object)
                       :accessor-name ',(property-accessor-name object)
                       :readable ',(property-readable object)
                       :writable ',(property-writable object)
                       :type ',(cffi-property-type object)
                       :reader ',(cffi-property-reader object)
                       :writer ',(cffi-property-writer object)))

(defun parse-gobject-property (spec)
  (destructuring-bind (name accessor-name gname type readable writable) spec
      (make-gobject-property :name name
                             :accessor-name accessor-name
                             :gname gname
                             :type type
                             :readable readable
                             :writable writable)))

(defun parse-cffi-property (spec)
  (destructuring-bind (name accessor-name type reader writer) spec
    (make-cffi-property :name name
                        :accessor-name accessor-name
                        :type type
                        :reader reader
                        :writer writer
                        :readable (not (null reader))
                        :writable (not (null writer)))))

(defun parse-property (spec)
  (cond
    ((eq (first spec) :cffi) (parse-cffi-property (rest spec)))
    (t (parse-gobject-property spec))))

(defun property->method-arg (property)
  (when (or (gobject-property-p property)
            (and (cffi-property-p property)
                 (property-writable property)))
    (let ((name (property-name property)))
      `(,name nil ,(name->supplied-p name)))))

(defun gobject-property->arg-push (property)
  (assert (typep property 'gobject-property))
  (with-slots (name type gname) property
    `(when ,(name->supplied-p name)
       (push ,gname arg-names)
       (push ,type arg-types)
       (push ,name arg-values))))

(defun cffi-property->initarg (property)
  (assert (typep property 'cffi-property))
  (when (property-writable property)
    (with-slots (accessor-name name type writer) property
      `(when ,(name->supplied-p name)
         (setf (,accessor-name object) ,name)))))

(defun accessor-name (class-name property-name)
  (intern (format nil "~A-~A" (symbol-name class-name)
                  (lispify-name property-name))
          *lisp-name-package*))

(defgeneric property->reader (class property))
(defgeneric property->writer (class property))

(defmethod property->reader (class (property gobject-property))
  (with-slots (accessor-name type gname) property
   `(defmethod ,accessor-name ((object ,class))
      (g-object-call-get-property object ,gname ,type))))

(defmethod property->reader (class (property cffi-property))
  (with-slots (accessor-name type reader) property
    (etypecase reader
      (string `(defmethod ,accessor-name ((object ,class))
                 (foreign-funcall ,reader g-object object ,type)))
      (symbol `(defmethod ,accessor-name ((object ,class))
                 (funcall ',reader object))))))

(defmethod property->writer (class (property gobject-property))
  (with-slots (accessor-name type gname) property
    `(defmethod (setf ,accessor-name) (new-value (object ,class))
       (g-object-call-set-property object ,gname new-value ,type)
       new-value)))

(defmethod property->writer (class (property cffi-property))
  (with-slots (accessor-name type writer) property
    (etypecase writer
      (string `(defmethod (setf ,accessor-name) (new-value (object ,class))
                 (foreign-funcall ,writer g-object object ,type new-value :void)
                 new-value))
      (symbol `(defmethod (setf ,accessor-name) (new-value (object ,class))
                 (funcall ',writer object new-value)
                 new-value)))))

(defun property->accessors (class property export)
  (append (when (property-readable property)
            (list (property->reader class property)))
          (when (property-writable property)
            (list (property->writer class property)))
          (when export
            (list `(export ',(property-accessor-name property)
                           (find-package ,(package-name (symbol-package (property-accessor-name property)))))))))

(defun interface->lisp-class-name (interface)
  (etypecase interface
    (symbol interface)
    (string (or (gethash interface *known-interfaces*)
                (error "Unknown interface ~A" interface)))))

(defun type-initializer-call (type-initializer)
  (etypecase type-initializer
    (string `(if (foreign-symbol-pointer ,type-initializer)
                 (foreign-funcall-pointer
                  (foreign-symbol-pointer ,type-initializer) ()
                  g-type)
                 (warn "Type initializer '~A' is not available" ,type-initializer)))
    (symbol `(funcall ',type-initializer))))

(defun meta-property->slot (class-name property)
  `(,(property-name property)
     :allocation ,(if (gobject-property-p property) :gobject-property :gobject-fn)
     :g-property-type ,(if (gobject-property-p property) (gobject-property-type property) (cffi-property-type property))
     :accessor ,(intern (format nil "~A-~A" (symbol-name class-name) (property-name property)) (symbol-package class-name))
     ,@(when (if (gobject-property-p property)
                 t
                 (not (null (cffi-property-writer property))))
             `(:initarg
               ,(intern (string-upcase (property-name property)) (find-package :keyword))))
     ,@(if (gobject-property-p property)
           `(:g-property-name ,(gobject-property-gname property))
           `(:g-getter ,(cffi-property-reader property)
                       :g-setter ,(cffi-property-writer property)))))

(defmacro define-g-object-class (g-type-name name
                                 (&key (superclass 'g-object)
                                       (export t)
                                       interfaces
                                       type-initializer)
                                 (&rest properties))
  (setf properties (mapcar #'parse-property properties))
  `(progn
     (defclass ,name (,@(when (and superclass (not (eq superclass 'g-object))) (list superclass)) ,@(mapcar #'interface->lisp-class-name interfaces))
       (,@(mapcar (lambda (property) (meta-property->slot name property)) properties))
       (:metaclass gobject-class)
       (:g-type-name . ,g-type-name)
       ,@(when type-initializer
               (list `(:g-type-initializer . ,type-initializer))))
     ,@(when export
             (cons `(export ',name (find-package ,(package-name (symbol-package name))))
                   (mapcar (lambda (property)
                             `(export ',(intern (format nil "~A-~A" (symbol-name name) (property-name property)) (symbol-package name))
                                      (find-package ,(package-name (symbol-package name)))))
                           properties)))))

(defmacro define-g-interface (g-type-name name (&key (export t) type-initializer) &body properties)
  (setf properties (mapcar #'parse-property properties))
  `(progn
     (defclass ,name ()
       (,@(mapcar (lambda (property) (meta-property->slot name property)) properties))
       (:metaclass gobject-class)
       (:g-type-name . ,g-type-name)
       (:g-interface-p . t)
       ,@(when type-initializer
               (list `(:g-type-initializer . ,type-initializer))))
     ,@(when export
             (cons `(export ',name (find-package ,(package-name (symbol-package name))))
                   (mapcar (lambda (property)
                             `(export ',(intern (format nil "~A-~A" (symbol-name name) (property-name property)) (symbol-package name))
                                      (find-package ,(package-name (symbol-package name)))))
                           properties)))
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (gethash ,g-type-name *known-interfaces*) ',name))))

(defun starts-with (name prefix)
  (and prefix (> (length name) (length prefix)) (string= (subseq name 0 (length prefix)) prefix)))

(defun strip-start (name prefix)
  (if (starts-with name prefix)
      (subseq name (length prefix))
      name))

(defun lispify-name (name)
  (with-output-to-string (stream)
    (loop for c across (strip-start name *strip-prefix*)
       for firstp = t then nil
       do (when (and (not firstp) (upper-case-p c)) (write-char #\- stream))
       do (write-char (char-upcase c) stream))))

(defun g-name->name (name)
  (or (second (assoc name *lisp-name-exceptions* :test 'equal))
      (intern (string-upcase (lispify-name name)) *lisp-name-package*)))

(defun property->property-definition (class-name property)
  (let ((name (g-name->name (g-class-property-definition-name property)))
        (accessor-name (accessor-name class-name (g-class-property-definition-name property)))
        (g-name (g-class-property-definition-name property))
        (type (g-type-name (g-class-property-definition-type property)))
        (readable (g-class-property-definition-readable property))
        (writable (and (g-class-property-definition-writable property)
                       (not (g-class-property-definition-constructor-only property)))))
    `(,name ,accessor-name ,g-name ,type ,readable ,writable)))

(defun probable-type-init-name (type-name)
  (with-output-to-string (stream)
    (iter (for c in-string type-name)
          (for prev-c previous c)
          (when (and (not (first-iteration-p))
                     (upper-case-p c)
                     (not (upper-case-p prev-c))
                     (not (char= prev-c #\_)))
            (write-char #\_ stream))
          (write-char (char-downcase c) stream))
    (write-string "_get_type" stream)))

(defun get-g-class-definition (type &optional lisp-name-package)
  (when (and (stringp type) (zerop (g-type-numeric type)))
    (let ((type-init-name (probable-type-init-name type)))
      (when (foreign-symbol-pointer type-init-name)
        (foreign-funcall-pointer (foreign-symbol-pointer type-init-name) () :int))))
  (when *generated-types*
    (setf (gethash (g-type-string type) *generated-types*) t))
  (let* ((*lisp-name-package* (or lisp-name-package *lisp-name-package* *package*))
         (g-type (ensure-g-type type))
         (g-name (g-type-name g-type))
         (name (g-name->name g-name))
         (superclass-g-type (g-type-parent g-type))
         (superclass-name (g-name->name (g-type-name superclass-g-type)))
         (interfaces (g-type-interfaces g-type))
         (properties (class-properties g-type))
         (type-init-name (probable-type-init-name g-name))
         (own-properties
          (sort (copy-list (remove g-type properties :key #'g-class-property-definition-owner-type :test-not #'g-type=))
                #'string< :key #'g-class-property-definition-name)))
    `(define-g-object-class ,g-name ,name 
         (:superclass ,superclass-name
                      :export t
                      :interfaces (,@(sort (mapcar #'g-type-name interfaces) 'string<))
                      ,@(when (and (foreign-symbol-pointer type-init-name)
                                   (not (null-pointer-p (foreign-symbol-pointer type-init-name))))
                              `(:type-initializer ,type-init-name)))
       (,@(mapcar (lambda (property)
                    (property->property-definition name property))
                  own-properties)
          ,@(cdr (find g-name *additional-properties* :key 'car :test 'string=))))))

(defun get-g-interface-definition (interface &optional lisp-name-package)
  (when (and (stringp interface) (zerop (g-type-numeric interface)))
    (let ((type-init-name (probable-type-init-name interface)))
      (when (foreign-symbol-pointer type-init-name)
        (foreign-funcall-pointer (foreign-symbol-pointer type-init-name) () :int))))
  (when *generated-types*
    (setf (gethash (g-type-string interface) *generated-types*) t))
  (let* ((*lisp-name-package* (or lisp-name-package *lisp-name-package* *package*))
         (type (ensure-g-type interface))
         (g-name (g-type-name type))
         (name (g-name->name g-name))
         (properties (sort (copy-list (interface-properties type))
                           #'string< :key #'g-class-property-definition-name))
         (probable-type-initializer (probable-type-init-name g-name)))
    `(define-g-interface ,g-name ,name
         (:export t
                  ,@(when (foreign-symbol-pointer probable-type-initializer)
                          `(:type-initializer ,probable-type-initializer)))
       ,@(append (mapcar (lambda (property)
                           (property->property-definition name property))
                         properties)
                 (cdr (find g-name *additional-properties* :key 'car :test 'string=))))))

(defun get-g-class-definitions-for-root-1 (type)
  (unless (member type *generation-exclusions* :test 'g-type=)
    (iter (when (first-iteration-p)
            (unless (and *generated-types*
                         (gethash (g-type-string type) *generated-types*))
              (appending (list (get-g-class-definition type)))))
          (for child-type in (sort (copy-list (g-type-children type)) #'string< :key #'g-type-string))
          (appending (get-g-class-definitions-for-root-1 child-type)))))

(defun get-g-class-definitions-for-root (type)
  (setf type (ensure-g-type type))
  (get-g-class-definitions-for-root-1 type))

(defvar *referenced-types*)

(defun class-or-interface-properties (type)
  (setf type (ensure-g-type type))
  (cond 
    ((g-type= (g-type-fundamental type) +g-type-object+) (class-properties type))
    ((g-type= (g-type-fundamental type) +g-type-interface+) (interface-properties type))))

(defun get-shallow-referenced-types (type)
  (setf type (ensure-g-type type))
  (remove-duplicates (sort (loop
                              for property in (class-or-interface-properties type)
                              when (g-type= type (g-class-property-definition-owner-type property))
                              collect (g-class-property-definition-type property))
                           #'string<
                           :key #'g-type-string)
                     :test 'equal))

(defun get-referenced-types-1 (type)
  (setf type (ensure-g-type type))
  (loop
     for property-type in (sort (copy-list (get-shallow-referenced-types type)) #'string> :key #'g-type-string)
     do (pushnew property-type *referenced-types* :test 'g-type=))
  (loop
     for type in (sort (copy-list (g-type-children type)) #'string< :key #'g-type-string)
     do (get-referenced-types-1 type)))

(defun get-referenced-types (root-type)
  (let (*referenced-types*)
    (get-referenced-types-1 (ensure-g-type root-type))
    *referenced-types*))

(defun filter-types-by-prefix (types prefix)
  (remove-if-not
   (lambda (type)
     (starts-with (g-type-name (ensure-g-type type)) prefix))
   types))

(defun filter-types-by-fund-type (types fund-type)
  (setf fund-type (ensure-g-type fund-type))
  (remove-if-not
   (lambda (type)
     (equal (g-type-fundamental (ensure-g-type type)) fund-type))
   types))

(defmacro define-g-enum (g-name name (&key (export t) type-initializer) &body values)
  "Defines a GEnum type for enumeration. Generates corresponding CFFI definition.

Example:
@begin{pre}
\(define-g-enum \"GdkGrabStatus\" grab-status () :success :already-grabbed :invalid-time :not-viewable :frozen)
\(define-g-enum \"GdkExtensionMode\" gdk-extension-mode (:export t :type-initializer \"gdk_extension_mode_get_type\")
  (:none 0) (:all 1) (:cursor 2))
@end{pre}
@arg[g-name]{a string. Specifies the GEnum name}
@arg[name]{a symbol. Names the enumeration type.}
@arg[export]{a boolean. If true, @code{name} will be exported.}
@arg[type-initializer]{a @code{NIL} or a string or a function designator.

If non-@code{NIL}, specifies the function that initializes the type: string specifies a C function that returns the GType value and function designator specifies the Lisp function.}
@arg[values]{values for enum. Each value is a keyword or a list @code{(keyword integer-value)}. @code{keyword} corresponds to Lisp value of enumeration, and @code{integer-value} is an C integer for enumeration item. If @code{integer-value} is not specified, it is generated automatically (see CFFI manual)}"
  `(progn
     (defcenum ,name ,@values)
     (register-enum-type ,g-name ',name)
     ,@(when export
             (list `(export ',name (find-package ,(package-name (symbol-package name))))))
     ,@(when type-initializer
             (list `(at-init () ,(type-initializer-call type-initializer))))))

(defun enum-value->definition (enum-value)
  (let ((value-name (intern (lispify-name (enum-item-nick enum-value))
                            (find-package :keyword)))
        (numeric-value (enum-item-value enum-value)))
    `(,value-name ,numeric-value)))

(defun get-g-enum-definition (type &optional lisp-name-package)
  (when (and (stringp type) (zerop (g-type-numeric type)))
    (let ((type-init-name (probable-type-init-name type)))
      (when (foreign-symbol-pointer type-init-name)
        (foreign-funcall-pointer (foreign-symbol-pointer type-init-name) () :int))))
  (when *generated-types*
    (setf (gethash (g-type-string type) *generated-types*) t))
  (let* ((*lisp-name-package* (or lisp-name-package *lisp-name-package* *package*))
         (g-type (ensure-g-type type))
         (g-name (g-type-name g-type))
         (name (g-name->name g-name))
         (items (get-enum-items g-type))
         (probable-type-initializer (probable-type-init-name g-name)))
    `(define-g-enum ,g-name ,name
         (:export t
                  ,@(when (foreign-symbol-pointer probable-type-initializer)
                          (list :type-initializer
                                probable-type-initializer)))
       ,@(mapcar #'enum-value->definition items))))

(defmacro define-g-flags (g-name name (&key (export t) type-initializer) &body values)
  "Defines a GFlags type for enumeration that can combine its values. Generates corresponding CFFI definition. Values of this type are lists of keywords that are combined.

Example:
@begin{pre}
\(define-g-flags \"GdkWindowState\" window-state ()
  (:withdrawn 1)
  (:iconified 2) (:maximized 4) (:sticky 8) (:fullscreen 16)
  (:above 32) (:below 64))
@end{pre}
@arg[g-name]{a string. Specifies the GEnum name}
@arg[name]{a symbol. Names the enumeration type.}
@arg[export]{a boolean. If true, @code{name} will be exported.}
@arg[type-initializer]{a @code{NIL} or a string or a function designator.

If non-@code{NIL}, specifies the function that initializes the type: string specifies a C function that returns the GType value and function designator specifies the Lisp function.}
@arg[values]{values for flags. Each value is a keyword or a list @code{(keyword integer-value)}. @code{keyword} corresponds to Lisp value of a flag, and @code{integer-value} is an C integer for flag. If @code{integer-value} is not specified, it is generated automatically (see CFFI manual)}"
  `(progn
     (defbitfield ,name ,@values)
     (register-flags-type ,g-name ',name)
     ,@(when export
             (list `(export ',name (find-package ,(package-name (symbol-package name))))))
     ,@(when type-initializer
             (list `(at-init () ,(type-initializer-call type-initializer))))))

(defun flags-value->definition (flags-value)
  (let ((value-name (intern (lispify-name (flags-item-nick flags-value))
                            (find-package :keyword)))
        (numeric-value (flags-item-value flags-value)))
    `(,value-name ,numeric-value)))

(defun get-g-flags-definition (type &optional lisp-name-package)
  (when (and (stringp type) (zerop (g-type-numeric type)))
    (let ((type-init-name (probable-type-init-name type)))
      (when (foreign-symbol-pointer type-init-name)
        (foreign-funcall-pointer (foreign-symbol-pointer type-init-name) () :int))))
  (when *generated-types*
    (setf (gethash (g-type-string type) *generated-types*) t))
  (let* ((*lisp-name-package* (or lisp-name-package *lisp-name-package* *package*))
         (g-type (ensure-g-type type))
         (g-name (g-type-name g-type))
         (name (g-name->name g-name))
         (items (get-flags-items g-type))
         (probable-type-initializer (probable-type-init-name g-name)))
    `(define-g-flags ,g-name ,name
         (:export t
                  ,@(when (foreign-symbol-pointer probable-type-initializer)
                          (list :type-initializer
                                probable-type-initializer)))
       ,@(mapcar #'flags-value->definition items))))

(defun maybe-call-type-init (type)
  (when (and (stringp type) (zerop (g-type-numeric type)))
    (let ((type-init-name (probable-type-init-name type)))
      (when (foreign-symbol-pointer type-init-name)
        (foreign-funcall-pointer (foreign-symbol-pointer type-init-name) () :int)))))

(defun get-g-type-definition (type &optional lisp-name-package)
  (maybe-call-type-init type)
  (cond
    ((g-type-is-a type +g-type-enum+) (get-g-enum-definition type lisp-name-package))
    ((g-type-is-a type +g-type-flags+) (get-g-flags-definition type lisp-name-package))
    ((g-type-is-a type +g-type-interface+) (get-g-interface-definition type lisp-name-package))
    ((g-type-is-a type +g-type-object+) (get-g-class-definition type lisp-name-package))
    (t (error "Do not know how to automatically generate type definition for ~A type ~A"
              (g-type-string (g-type-fundamental type))
              (or (g-type-string type) type)))))

(defun generate-types-hierarchy-to-file (file root-type &key include-referenced prefix package exceptions prologue interfaces enums flags objects exclusions additional-properties)
  (if (not (streamp file))
      (with-open-file (stream file :direction :output :if-exists :supersede)
        (generate-types-hierarchy-to-file stream root-type
                                          :prefix prefix
                                          :package package
                                          :exceptions exceptions
                                          :prologue prologue
                                          :include-referenced include-referenced
                                          :interfaces interfaces
                                          :enums enums
                                          :flags flags
                                          :objects objects
                                          :exclusions exclusions
                                          :additional-properties additional-properties))
      (let* ((*generation-exclusions* (mapcar #'ensure-g-type exclusions))
             (*lisp-name-package* (or package *package*))
             (*package* *lisp-name-package*)
             (*strip-prefix* (or prefix ""))
             (*lisp-name-exceptions* exceptions)
             (*print-case* :downcase)
             (*additional-properties* additional-properties)
             (*generated-types* (make-hash-table :test 'equalp))
             (referenced-types (and include-referenced
                                    (filter-types-by-prefix
                                     (get-referenced-types root-type)
                                     prefix))))
        (setf exclusions (mapcar #'ensure-g-type exclusions))
        (when prologue
          (write-string prologue file)
          (terpri file))
        (when include-referenced
          (loop
             for interface in interfaces
             do (loop
                   for referenced-type in (get-shallow-referenced-types interface)
                   do (pushnew referenced-type referenced-types :test 'g-type=)))
          (loop
             for object in objects
             do (loop
                   for referenced-type in (get-shallow-referenced-types object)
                   do (pushnew referenced-type referenced-types :test 'g-type=)))
          (loop
             for enum-type in (filter-types-by-fund-type
                               referenced-types "GEnum")
             for def = (get-g-enum-definition enum-type)
             unless (member enum-type exclusions :test 'g-type=)
             do (format file "~S~%~%" def))
            
          (loop
             for flags-type in (filter-types-by-fund-type
                                referenced-types "GFlags")
             for def = (get-g-flags-definition flags-type)
             unless (member flags-type exclusions :test 'g-type=)
             do (format file "~S~%~%" def)))
        (loop
           with auto-enums = (and include-referenced
                                  (filter-types-by-fund-type
                                   referenced-types "GEnum"))
           for enum in enums
           for def = (get-g-enum-definition enum)
           unless (find enum auto-enums :test 'g-type=)
           do (format file "~S~%~%" def))
        (loop
           with auto-flags = (and include-referenced
                                  (filter-types-by-fund-type
                                   referenced-types "GFlags"))
           for flags-type in flags
           for def = (get-g-flags-definition flags-type)
           unless (find flags-type auto-flags :test 'g-type=)
           do (format file "~S~%~%" def))
        (loop
           for interface in interfaces
           for def = (get-g-interface-definition interface)
           do (format file "~S~%~%" def))
        (loop
           for def in (get-g-class-definitions-for-root root-type)
           do (format file "~S~%~%" def))
        (iter (for object in objects)
              (unless (gethash (g-type-string object) *generated-types*)
                (for def = (get-g-class-definition object))
                (format file "~S~%~%" def))))))