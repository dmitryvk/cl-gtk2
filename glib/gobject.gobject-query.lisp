(in-package :gobject)

(defstruct g-class-property-definition
  name
  type
  readable
  writable
  constructor
  constructor-only
  owner-type)

(defun parse-g-param-spec (param)
  (let ((flags (foreign-slot-value param 'g-param-spec 'flags)))
    (make-g-class-property-definition
     :name (foreign-slot-value param 'g-param-spec
                               'name)
     :type (foreign-slot-value param 'g-param-spec
                               'value-type)
     :readable (not (null (member :readable flags)))
     :writable (not (null (member :writable flags)))
     :constructor (not (null (member :construct flags)))
     :constructor-only (not (null (member :construct-only flags)))
     :owner-type (foreign-slot-value param 'g-param-spec
                                     'owner-type))))

(defun class-properties (g-type)
  (setf g-type (ensure-g-type g-type))
  (let ((g-class (g-type-class-ref g-type)))
    (unwind-protect
         (with-foreign-object (n-properties :uint)
           (let ((params (g-object-class-list-properties g-class n-properties)))
             (unwind-protect
                  (loop
                     for i from 0 below (mem-ref n-properties :uint)
                     for param = (mem-aref params :pointer i)
                     collect (parse-g-param-spec param))
               (g-free params))))
      (g-type-class-unref g-class))))

(defun class-parent (type)
  (g-type-parent (ensure-g-type type)))

(defun interface-properties (g-type)
  (setf g-type (ensure-g-type g-type))
  (let ((g-iface (g-type-default-interface-ref g-type)))
    (unwind-protect
         (with-foreign-object (n-properties :uint)
           (let ((params (g-object-interface-list-properties g-iface n-properties)))
             (unwind-protect
                  (loop
                     for i from 0 below (mem-ref n-properties :uint)
                     for param = (mem-aref params :pointer i)
                     for flags = (foreign-slot-value param 'g-param-spec 'flags)
                     collect (make-g-class-property-definition
                              :name (foreign-slot-value param 'g-param-spec
                                                        'name)
                              :type (foreign-slot-value param 'g-param-spec
                                                        'value-type)
                              :readable (not (null (member :readable flags)))
                              :writable (not (null (member :writable flags)))
                              :constructor (not (null (member :construct flags)))
                              :constructor-only (not (null (member :construct-only flags)))
                              :owner-type (foreign-slot-value param 'g-param-spec
                                                              'owner-type)))
               (g-free params))))
      (g-type-default-interface-unref g-iface))))

(defstruct enum-item
  name value nick)

(defun get-enum-items (type)
  (let ((g-class (g-type-class-ref (ensure-g-type type))))
    (unwind-protect
         (loop
            with n = (foreign-slot-value g-class 'g-enum-class 'n-values)
            with values = (foreign-slot-value g-class 'g-enum-class 'values)
            for i from 0 below n
            for enum-value = (mem-aref values 'g-enum-value i)
            collect (make-enum-item
                     :name (foreign-slot-value enum-value 'g-enum-value
                                               'name)
                     :value (foreign-slot-value enum-value 'g-enum-value
                                                'value)
                     :nick (foreign-slot-value enum-value 'g-enum-value
                                               'nick)))
      (g-type-class-unref g-class))))

(defstruct flags-item
  name value nick)

(defun get-flags-items (type)
  (let ((g-class (g-type-class-ref (ensure-g-type type))))
    (unwind-protect
         (loop
            with n = (foreign-slot-value g-class 'g-flags-class 'n-values)
            with values = (foreign-slot-value g-class 'g-flags-class 'values)
            for i from 0 below n
            for flags-value = (mem-aref values 'g-flags-value i)
            collect (make-flags-item
                     :name (foreign-slot-value flags-value 'g-flags-value
                                               'name)
                     :value (foreign-slot-value flags-value 'g-flags-value
                                                'value)
                     :nick (foreign-slot-value flags-value 'g-flags-value
                                               'nick)))
      (g-type-class-unref g-class))))
