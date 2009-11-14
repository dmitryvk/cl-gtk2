(in-package :gdk)

(defcstruct %gdk-device
  (parent-instance gobject.ffi::%g-object)
  (name (:string :free-from-foreign nil))
  (source gdk-input-source)
  (mode gdk-input-mode)
  (has-cursor :boolean)
  (num-axes :int)
  (axes :pointer)
  (num-keys :int)
  (keys :pointer))

(define-g-boxed-cstruct gdk-device-key nil
  (keyval :uint)
  (modifiers modifier-type))

(define-g-boxed-cstruct gdk-device-axis nil
  (use axis-use)
  (min :double)
  (max :double))

(defun %gdk-device-name (device)
  (foreign-slot-value (pointer device) '%gdk-device 'name))

(defun %gdk-device-source (device)
  (foreign-slot-value (pointer device) '%gdk-device 'source))

(defun %gdk-device-mode (device)
  (foreign-slot-value (pointer device) '%gdk-device 'mode))

(defun %gdk-device-has-cursor (device)
  (foreign-slot-value (pointer device) '%gdk-device 'has-cursor))

(defun %gdk-device-n-axes (device)
  (foreign-slot-value (pointer device) '%gdk-device 'num-axes))

(defun %gdk-device-n-keys (device)
  (foreign-slot-value (pointer device) '%gdk-device 'num-keys))

(defun %gdk-device-axes (device)
  (let ((n (foreign-slot-value (pointer device) '%gdk-device 'num-axes))
        (axes (foreign-slot-value (pointer device) '%gdk-device 'axes)))
    (iter (for i from 0 below n)
          (for axis = (convert-from-foreign (inc-pointer axes (* i (foreign-type-size 'gdk-device-axis-cstruct)))
                                            '(g-boxed-foreign gdk-device-axis)))
          (collect axis))))

(defun %gdk-device-keys (device)
  (let ((n (foreign-slot-value (pointer device) '%gdk-device 'num-keys))
        (keys (foreign-slot-value (pointer device) '%gdk-device 'keys)))
    (iter (for i from 0 below n)
          (for key = (convert-from-foreign (inc-pointer keys (* i (foreign-type-size 'gdk-device-key-cstruct)))
                                            '(g-boxed-foreign gdk-device-key)))
          (collect key))))

(defmethod print-object ((object gdk-device) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A (~A, ~A)" (gdk-device-name object) (gdk-device-source object) (gdk-device-mode object))))

(defcfun gdk-devices-list (glib:glist (g-object gdk-device) :free-from-foreign nil))

(export 'gdk-devices-list)

(defcfun gdk_device_set_mode :boolean
  (device (g-object gdk-device))
  (mode gdk-input-mode))

(defcfun gdk-device-set-key :void
  (device (g-object gdk-device))
  (index :uint)
  (keyval :uint)
  (modifiers modifier-type))

(export 'gdk-device-set-key)

(defcfun gdk-device-set-axis-use :void
  (device (g-object gdk-device))
  (index :uint)
  (use axis-use))

(export 'gdk-device-set-axis-use)

(defcfun gdk-device-get-core-pointer (g-object gdk-device))

(export 'gdk-device-get-core-pointer)

(defcfun gdk_device_get_state :void
  (device (g-object gdk-device))
  (window (g-object gdk-window))
  (axes (:pointer :double))
  (mask (:pointer modifier-type)))

(defun gdk-device-get-state (device window)
  (with-foreign-objects ((axes :double (%gdk-device-n-axes device)) (mask 'modifier-type))
    (gdk_device_get_state device window axes mask)
    (values (iter (for i from 0 below (%gdk-device-n-axes device))
                  (collect (mem-aref axes :double i)))
            (mem-ref mask 'modifier-type))))

(export 'gdk-device-get-state)

(define-g-boxed-cstruct gdk-time-coord nil
  (time :uint32)
  (axes :double :count 128))

(defcfun gdk_device_get_history :boolean
  (device (g-object gdk-device))
  (window (g-object gdk-window))
  (start :uint32)
  (stop :uint32)
  (events (:pointer (:pointer (:pointer gdk-time-coord-cstruct))))
  (n-events (:pointer :int)))

(defcfun gdk_device_free_history :void
  (events (:pointer (:pointer gdk-time-coord-cstruct)))
  (n-events :int))

(defun gdk-device-get-history (device window start stop)
  (with-foreign-objects ((events :pointer) (n-events :int))
    (when (gdk_device_get_history device window start stop events n-events)
      (prog1
          (iter (with events-ar = (mem-ref events :pointer))
                (for i from 0 below (mem-ref n-events :int))
                (for coord = (mem-aref events-ar '(g-boxed-foreign gdk-time-coord) i))
                (collect coord))
        (gdk_device_free_history (mem-ref events :pointer) (mem-ref n-events :int))))))

(export 'gdk-device-get-history)

(defcfun gdk_device_get_axis :boolean
  (device (g-object gdk-device))
  (axes (:pointer :double))
  (use axis-use)
  (value (:pointer :double)))

(defun gdk-device-get-axis (device axes axis-use)
  (assert (= (%gdk-device-n-axes device) (length axes)))
  (with-foreign-objects ((axes-ar :double (%gdk-device-n-axes device)) (value :double))
    (let ((i 0))
      (map nil
           (lambda (v)
             (setf (mem-aref axes-ar :double i) v)
             (incf i))
           axes))
    (when (gdk_device_get_axis device axes-ar axis-use value)
      (mem-ref value :double))))

(export 'gdk-device-get-axis)

(defcfun gdk-input-set-extension-events :void
  (window (g-object gdk-window))
  (mask :int)
  (mode gdk-extension-mode))

(export 'gdk-input-set-extension-events)
