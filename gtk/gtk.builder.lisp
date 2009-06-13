(in-package :gtk)

(defcfun gtk-builder-add-from-file :uint
  (builder g-object)
  (filename :string)
  (error :pointer))

(defun builder-add-from-file (builder filename)
  (gtk-builder-add-from-file builder filename (null-pointer)))

(export 'builder-add-from-file)

(defcfun gtk-builder-add-from-string :uint
  (builder g-object)
  (string :string)
  (length :int)
  (error :pointer))

(defun builder-add-from-string (builder string)
  (gtk-builder-add-from-string builder string -1 (null-pointer)))

(export 'builder-add-from-string)

(defcfun gtk-builder-add-objects-from-file :uint
  (builder g-object)
  (filename :string)
  (object-ids :pointer)
  (error :pointer))

(defun builder-add-objects-from-file (builder filename object-ids)
  (let ((l (foreign-alloc :pointer :count (1+ (length object-ids)))))
    (loop
       for i from 0
       for object-id in object-ids
       do (setf (mem-aref l :pointer i) (foreign-string-alloc object-id)))
    (unwind-protect
         (gtk-builder-add-objects-from-file builder filename l (null-pointer))
      (loop
         for i from 0
         repeat (1- (length object-ids))
         do (foreign-string-free (mem-aref l :pointer i)))
      (foreign-free l))))

(export 'builder-add-objects-from-file)

(defcfun gtk-builder-add-objects-from-string :uint
  (builder g-object)
  (string :string)
  (length :int)
  (object-ids :pointer)
  (error :pointer))

(defun builder-add-objects-from-string (builder string object-ids)
  (let ((l (foreign-alloc :pointer :count (1+ (length object-ids)))))
    (loop
       for i from 0
       for object-id in object-ids
       do (setf (mem-aref l :pointer i) (foreign-string-alloc object-id)))
    (unwind-protect
         (gtk-builder-add-objects-from-string builder string -1 l (null-pointer))
      (loop
         for i from 0
         repeat (1- (length object-ids))
         do (foreign-string-free (mem-aref l :pointer i)))
      (foreign-free l))))

(export 'builder-add-objects-from-string)

(defcfun (builder-get-object "gtk_builder_get_object") g-object
  (builder g-object)
  (name :string))

(export 'builder-get-object)

; TODO: gtk_builder_get_objects

; TOOD: move connect-flags to gobject

(defbitfield connect-flags :after :swapped)

(defcallback builder-connect-func-callback :void
    ((builder g-object) (object g-object) (signal-name (:string :free-from-foreign nil))
     (handler-name (:string :free-from-foreign nil)) (connect-object g-object)
     (flags connect-flags) (data :pointer))
  (restart-case
      (funcall (get-stable-pointer-value data)
               builder object signal-name handler-name connect-object flags)
    (return () nil)))

(defcfun gtk-builder-connect-signals-full :void
  (builder g-object)
  (func :pointer)
  (data :pointer))

(defun builder-connect-signals-full (builder func)
  (with-stable-pointer (ptr func)
    (gtk-builder-connect-signals-full builder (callback builder-connect-func-callback) ptr)))

(export 'builder-connect-signals-full)

(defun builder-connect-signals-simple (builder handlers-list)
  (flet ((connect-func (builder object signal-name handler-name connect-object flags)
           (declare (ignore builder connect-object))
           (let ((handler (find handler-name handlers-list :key 'first :test 'string=)))
             (when handler
               (g-signal-connect object signal-name (second handler) :after (member :after flags))))))
    (builder-connect-signals-full builder #'connect-func)))

(export 'builder-connect-signals-simple)

; TODO: gtk_builder_get_type_from_name

; TODO: gtk_builder_value_from_string

; TODO: gtk_builder_value_from_string_type

(defmethod initialize-instance :after ((builder builder) &key from-file from-string)
  (when from-file
    (builder-add-from-file builder from-file))
  (when from-string
    (builder-add-from-string builder from-string)))
