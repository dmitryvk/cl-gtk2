(in-package :gobject)

(defun emit-signal (object signal-name &rest args)
  "Emits the signal.
@arg[object]{an instance of @class{g-object}. Signal is emitted on this object}
@arg[signal-name]{a string specifying the signal}
@arg[args]{arguments for the signal}
@return{none}"
  (let* ((object-type (g-type-from-object (pointer object)))
         (signal-info (parse-signal-name object-type signal-name)))
    (unless signal-info
      (error "Signal ~A not found on object ~A" signal-name object))
    (let ((params-count (length (signal-info-param-types object))))
      (with-foreign-object (params 'g-value (1+ params-count))
        (set-g-value (mem-aref params 'g-value 0) object object-type :zero-g-value t)
        (iter (for i from 0 below params-count)
              (for arg in args)
              (for type in (signal-info-param-types signal-info))
              (set-g-value (mem-aref params 'g-value (1+ i)) arg type :zero-g-value t))
        (prog1
            (if (= (g-type-numeric (signal-info-return-type signal-info)) +g-type-void+)
                (g-signal-emitv params (signal-info-id signal-info) signal-name (null-pointer))
                (with-foreign-object (return-value 'g-value)
                  (g-value-zero return-value)
                  (g-value-init return-value (signal-info-return-type signal-info))
                  (prog1 (parse-gvalue return-value)
                    (g-value-unset return-value))))
          (iter (for i from 0 below (1+ params-count))
                (g-value-unset (mem-aref params 'g-value i))))))))